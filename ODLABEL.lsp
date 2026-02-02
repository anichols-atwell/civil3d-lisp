;; ------------------------------------------------------------
;; ODLABEL.LSP - Create MTEXT labels from Map 3D Object Data
;; Author: Civil-3D LISP Assistant
;; Version: 1.1 (fixes FBOUNDP error; more robust detection)
;; ------------------------------------------------------------
;; Workflow:
;;  1) Pick one object on the target layer (must have OD).
;;  2) If multiple OD tables exist, choose which table.
;;  3) Choose fields (numbers, comma-separated) or * for all.
;;  4) Labels are created for all objects on that LAYER
;;     that have the chosen OD table.
;;  5) Each chosen field appears on a new line in the MTEXT.
;;
;; Notes:
;;  - Requires Civil 3D / Map 3D (ADE_* Object Data functions).
;;  - MTEXT placed at entity bounding-box center (fallback = entity base point).
;;  - Labels are placed on: OD_LABELS_<source-layer>
;;  - Graceful handling of missing/empty fields (“-”).
;; ------------------------------------------------------------

(defun c:ODLABEL (/ *error*
                    _has-func _oknum _split _trim _strjoin _val->str
                    _bbox-center _ensure-layer _pick-table
                    _choose-fields _has-table _get-rec _get-val
                    oldcmdecho oldos esel ent ed layer
                    tname fields incl-names? ans def-txth txth
                    lbl-layer ss n i ename lines text pt
                    made skipped notab)

  ;; ---------- Error Handler ----------
  (defun *error* (msg)
    (and (= msg "Function cancelled") (setq msg "Cancelled"))
    (if oldcmdecho (setvar 'CMDECHO oldcmdecho))
    (if oldos (setvar 'OSMODE oldos))
    (princ (strcat "\nError: " msg))
    (princ))

  ;; ---------- Utilities ----------
  (vl-load-com)

  ;; Robust: check if a function symbol exists (no fboundp needed)
  (defun _has-func (sym)
    (member sym (atoms-family 1)))

  (defun _oknum (s) (and s (not (wcmatch s "*[^0-9]*"))))

  (defun _trim (s)
    (if s (vl-string-trim " \t\r\n" s)))

  (defun _split (s sep / p r sub)
    (setq r '())
    (while (setq p (vl-string-search sep s))
      (setq sub (substr s 1 p))
      (setq r (cons sub r))
      (setq s (substr s (+ p (strlen sep)) 1)))
    (reverse (cons s r)))

  (defun _strjoin (lst sep / r)
    (if lst
      (progn
        (setq r (car lst) lst (cdr lst))
        (while lst
          (setq r (strcat r sep (car lst))
                lst (cdr lst)))))
    r)

  (defun _val->str (v / t)
    (setq t (type v))
    (cond
      ((= t 'STR) v)
      ((= t 'INT) (itoa v))
      ((= t 'REAL) (rtos v 2 3))
      ((= t 'LIST) (vl-princ-to-string v))
      ((null v) "-")
      (t (vl-princ-to-string v))))

  (defun _bbox-center (ename / vla min max pt)
    (setq vla (vlax-ename->vla-object ename))
    (if (not (vl-catch-all-error-p
               (vl-catch-all-apply 'vlax-invoke-method (list vla 'GetBoundingBox 'min 'max))))
      (progn
        (setq min (vlax-safearray->list (vlax-variant-value min)))
        (setq max (vlax-safearray->list (vlax-variant-value max)))
        (setq pt (mapcar '(lambda (a b) (/ (+ a b) 2.0)) min max))))
    (or pt (cdr (assoc 10 (entget ename))))) ; fallback

  (defun _ensure-layer (lname / )
    (if (not (tblsearch "LAYER" lname))
      (entmakex
        (list '(0 . "LAYER")
              '(100 . "AcDbSymbolTableRecord")
              '(100 . "AcDbLayerTableRecord")
              (cons 2 lname)
              '(70 . 0)
              '(62 . 7)
              '(6 . "CONTINUOUS"))))
    lname)

  (defun _get-rec (ename tname / r)
    (if (and (_has-func 'ade_odgetrecord)
             (setq r (ade_odgetrecord ename tname 0)))
      r
      nil))

  (defun _get-val (ename tname fname / v r)
    (cond
      ((and (_has-func 'ade_odgetfield)
            (setq v (ade_odgetfield ename tname 0 fname))) v)
      ((setq r (_get-rec ename tname)) (cdr (assoc fname r)))
      (t nil)))

  (defun _pick-table (ename / ts i choice)
    (if (not (_has-func 'ade_odgettables))
      nil
      (progn
        (setq ts (ade_odgettables ename))
        (cond
          ((null ts) nil)
          ((= (length ts) 1) (car ts))
          (t
           (princ "\nMultiple Object Data tables found:")
           (setq i 0)
           (foreach t ts
             (princ (strcat "\n  " (itoa (setq i (1+ i))) ") " t)))
           (initget 6)
           (setq choice (getint (strcat "\nEnter table number [1-" (itoa i) "]: ")))
           (if (and choice (>= choice 1) (<= choice i))
             (nth (1- choice) ts)
             nil))))))

  (defun _choose-fields (ename tname / r flds i raw nums picks)
    (setq r (_get-rec ename tname))
    (if (null r)
      nil
      (progn
        (setq flds (mapcar 'car r))
        (princ (strcat "\nAvailable fields in OD table \"" tname "\":"))
        (setq i 0)
        (foreach f flds
          (princ (strcat "\n  " (itoa (setq i (1+ i))) ") " f)))
        (setq raw (getstring "\nEnter field numbers (comma-separated) or * for ALL: "))
        (cond
          ((or (null raw) (= raw "")) nil)
          ((= (strcase raw) "*") flds)
          (t
           ;; parse numbers
           (setq picks
                 (mapcar 'atoi
                         (vl-remove-if '(lambda (x) (= x ""))
                                       (_split (vl-string-translate " " "" raw) ","))))
           (vl-remove nil
             (mapcar
               '(lambda (n) (if (and (>= n 1) (<= n (length flds))) (nth (1- n) flds)))
               picks)))))))

  (defun _has-table (ename tname / ts)
    (and (_has-func 'ade_odgettables)
         (setq ts (ade_odgettables ename))
         (vl-some '(lambda (x) (= (strcase x) (strcase tname))) ts)))

  ;; ---------- Begin ----------
  (setq oldcmdecho (getvar 'CMDECHO))
  (setq oldos (getvar 'OSMODE))
  (setvar 'CMDECHO 0)

  ;; Verify Map 3D OD API presence
  (if (not (_has-func 'ade_odgettables))
    (progn
      (princ "\nThis routine requires Civil 3D/Map 3D (ADE Object Data functions not found).")
      (setvar 'CMDECHO oldcmdecho)
      (setvar 'OSMODE oldos)
      (princ)
      (exit)))

  ;; Select sample object
  (setq esel (entsel "\nSelect an object from the target layer (with Object Data): "))
  (if (null esel)
    (progn
      (princ "\nNothing selected. Cancelled.")
      (setvar 'CMDECHO oldcmdecho)
      (setvar 'OSMODE oldos)
      (princ)
      (exit)))
  (setq ent (car esel))
  (setq ed (entget ent))
  (setq layer (cdr (assoc 8 ed)))
  (princ (strcat "\nUsing layer: " layer))

  ;; Choose OD table
  (setq tname (_pick-table ent))
  (if (null tname)
    (progn
      (princ "\nNo Object Data tables found on the selected object. Cancelled.")
      (setvar 'CMDECHO oldcmdecho)
      (setvar 'OSMODE oldos)
      (princ)
      (exit)))
  (princ (strcat "\nUsing OD table: " tname))

  ;; Choose fields
  (setq fields (_choose-fields ent tname))
  (if (null fields)
    (progn
      (princ "\nNo fields selected. Cancelled.")
      (setvar 'CMDECHO oldcmdecho)
      (setvar 'OSMODE oldos)
      (princ)
      (exit)))
  (princ (strcat "\nFields selected: " (_strjoin fields ", ")))

  ;; Include field names?
  (initget "Yes No")
  (setq ans (getkword "\nInclude field names in label? [Yes/No] <No>: "))
  (setq incl-names? (eq ans "Yes"))

  ;; Text height
  (setq def-txth (getvar 'TEXTSIZE))
  (initget 7)
  (setq txth (getreal (strcat "\nText height <" (rtos def-txth 2 3) ">: ")))
  (if (not txth) (setq txth def-txth))

  ;; Label layer
  (setq lbl-layer (_ensure-layer (strcat "OD_LABELS_" layer)))
  (princ (strcat "\nLabel layer: " lbl-layer))

  ;; Collect targets (entire layer)
  (setq ss (ssget "X" (list (cons 8 layer))))
  (if (or (null ss) (= (sslength ss) 0))
    (progn
      (princ "\nNo objects found on that layer. Cancelled.")
      (setvar 'CMDECHO oldcmdecho)
      (setvar 'OSMODE oldos)
      (princ)
      (exit)))

  (setq n (sslength ss))
  (princ (strcat "\nFound " (itoa n) " object(s) on layer " layer "."))
  (setq i 0 made 0 skipped 0 notab 0)

  ;; Process each entity
  (while (< i n)
    (setq ename (ssname ss i))
    (setq i (1+ i))

    (if (_has-table ename tname)
      (progn
        (setq lines
              (mapcar
                '(lambda (fn / v s)
                   (setq v (_get-val ename tname fn))
                   (setq s (_val->str v))
                   (if incl-names?
                     (strcat fn ": " s)
                     s))
                fields))
        (if lines
          (progn
            (setq text (_strjoin lines "\\P"))
            (setq pt (_bbox-center ename))
            (if (not pt) (setq pt (getvar 'VIEWCTR)))

            ;; Create MTEXT
            (entmakex
              (list
                (cons 0 "MTEXT")
                (cons 100 "AcDbEntity")
                (cons 8 lbl-layer)
                (cons 100 "AcDbMText")
                (cons 10 pt)
                (cons 40 txth)
                (cons 41 (* txth 30.0)) ; width
                (cons 7 (getvar "TEXTSTYLE"))
                (cons 71 5)             ; middle-left
                (cons 72 1)             ; LTR
                (cons 73 1)             ; spacing style: at least
                (cons 44 1.0)           ; spacing factor
                (cons 50 0.0)
                (cons 1 text)))
            (setq made (1+ made)))
          (setq skipped (1+ skipped))))
      (setq notab (1+ notab)))

    (if (= (rem i 50) 0)
      (princ (strcat "\nProcessed " (itoa i) " / " (itoa n) "...")))
  )

  ;; Summary
  (princ
    (strcat
      "\nDone."
      "\n  Labels created: " (itoa made)
      "\n  Objects skipped (no values): " (itoa skipped)
      "\n  Objects without OD table \"" tname "\": " (itoa notab)))

  ;; Restore sysvars
  (setvar 'CMDECHO oldcmdecho)
  (setvar 'OSMODE oldos)
  (princ)
)
