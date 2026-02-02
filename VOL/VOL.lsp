;;; VOL.LSP
;;; CREATES A BASIN SUMMARY TABLE (AUTOCAD TABLE ENTITY) FROM SELECTED CONTOUR POLYLINES.
;;; ALSO COPIES THE SAME DATA (TAB-DELIMITED) TO THE WINDOWS CLIPBOARD FOR EASY PASTE INTO EXCEL.
;;;
;;; TITLE ROW:
;;;   BASIN VOL. TABLE
;;;
;;; COLUMNS:
;;;   ELEVATION (FT) | AREA (SF) | INC. VOLUME (CF) | TOTAL VOLUME (CF)
;;;
;;; VOLUME METHOD: AVERAGE END AREA
;;;   INCVOL = ((AREAPREV + AREACUR)/2) * (ELEVCRU - ELEVPRV)
;;;   TOTALVOL = CUMULATIVE SUM OF INCVOL

(vl-load-com)

(defun _bs:commify-int (s / n out)
  ;; INSERT COMMAS INTO AN INTEGER STRING (NO SIGN, NO DECIMALS).
  (setq n (strlen s)
        out "")
  (while (> n 3)
    (setq out (strcat "," (substr s (- n 2) 3) out)
          n (- n 3)))
  (strcat (substr s 1 n) out)
)

(defun _bs:fmt-num (n dec / s sign p ip dp)
  ;; FORMAT NUMBER WITH OPTIONAL DECIMALS AND THOUSANDS SEPARATORS.
  ;; DEC IS INTEGER >= 0
  (setq sign (if (< n 0.0) "-" ""))
  (setq s (rtos (abs n) 2 dec))
  (setq p (vl-string-search "." s))
  (if p
    (progn
      (setq ip (substr s 1 p))
      (setq dp (substr s (+ p 2)))
      (setq ip (_bs:commify-int ip))
      (strcat sign ip "." dp))
    (progn
      (setq ip (_bs:commify-int s))
      (strcat sign ip)))
)

(defun _bs:set-clipboard (txt / html win clip)
  ;; WINDOWS CLIPBOARD (TEXT)
  (setq html (vlax-create-object "htmlfile"))
  (if html
    (progn
      (setq win  (vlax-get html 'parentWindow))
      (setq clip (vlax-get win  'clipboardData))
      (vl-catch-all-apply 'vlax-invoke (list clip 'setData "Text" txt))
      (vlax-release-object html)))
  txt
)

(defun _bs:get-polyline-elevation (ent / obj elev)
  (setq obj (vlax-ename->vla-object ent))
  (if obj
    (progn
      (setq elev (vla-get-Elevation obj))
      (if elev elev 0.0))
    0.0)
)

(defun _bs:get-text-height ( / h)
  ;; DETERMINES A REASONABLE TEXT HEIGHT AUTOMATICALLY.
  ;; 1. CHECK CANNOSCALEVALUE FOR SCALE-BASED HEIGHT (TARGETING 0.1 ON PAPER).
  ;; 2. CHECK CURRENT TEXT STYLE FOR FIXED HEIGHT.
  ;; 3. FALLBACK TO TEXTSIZE SYSTEM VARIABLE.
  (if (and (getvar "CANNOSCALEVALUE") (> (getvar "CANNOSCALEVALUE") 0.0))
    (setq h (/ 0.1 (getvar "CANNOSCALEVALUE")))
    (progn
      (setq h (cdr (assoc 40 (entget (tblobjname "STYLE" (getvar "TEXTSTYLE"))))))
      (if (or (not h) (<= h 0.0))
        (setq h (getvar "TEXTSIZE"))
      )
    )
  )
  (if (or (not h) (<= h 0.0)) (setq h 2.0)) ;; FINAL SAFETY FALLBACK
  h
)

(defun _bs:modelspace ( / acad doc)
  (setq acad (vlax-get-acad-object))
  (setq doc  (vla-get-ActiveDocument acad))
  (vla-get-ModelSpace doc)
)

(defun _bs:is-closed-lwpoly (ent)
  (= (logand (cdr (assoc 70 (entget ent))) 1) 1)
)

(defun _bs:add-table (inspt textHeight data / ms tbl rowH defaultW acMiddleCenter r c)
  ;; CREATE AND POPULATE AN AUTOCAD TABLE WITH A TITLE ROW AND COLUMN HEADERS.

  (setq ms (_bs:modelspace))
  (setq rowH (* textHeight 1.8))
  (setq defaultW (* textHeight 10.0))
  (setq acMiddleCenter 5)

  ;; ROWS: TITLE + HEADER + DATA
  (setq tbl (vla-AddTable ms (vlax-3d-point inspt) (+ 2 (length data)) 4 rowH defaultW))

  ;; COLUMN WIDTHS (TWEAKABLE)
  (vla-SetColumnWidth tbl 0 (* textHeight 12.0))
  (vla-SetColumnWidth tbl 1 (* textHeight 14.0))
  (vla-SetColumnWidth tbl 2 (* textHeight 16.0))
  (vla-SetColumnWidth tbl 3 (* textHeight 18.0))

  ;; TITLE ROW (ROW 0 IS MERGED ACROSS COLUMNS BY AUTOCAD)
  (vla-SetText tbl 0 0 "BASIN VOL. TABLE")
  (vl-catch-all-apply 'vla-SetCellAlignment (list tbl 0 0 acMiddleCenter))
  (vl-catch-all-apply 'vla-SetCellTextHeight (list tbl 0 0 textHeight))

  ;; HEADER ROW (ROW 1)
  (vla-SetText tbl 1 0 "ELEVATION\\P(FT)")
  (vla-SetText tbl 1 1 "AREA\\P(SF)")
  (vla-SetText tbl 1 2 "INC.\\PVOLUME\\P(CF)")
  (vla-SetText tbl 1 3 "TOTAL\\PVOLUME\\P(CF)")

  ;; ALIGN + HEADER TEXT HEIGHT
  (setq c 0)
  (repeat 4
    (vl-catch-all-apply 'vla-SetCellAlignment (list tbl 1 c acMiddleCenter))
    (vl-catch-all-apply 'vla-SetCellTextHeight (list tbl 1 c textHeight))
    (setq c (1+ c))
  )

  ;; DATA ROWS START AT ROW 2
  (setq r 2)
  (foreach row data
    ;; ROW = (ELEV AREA INCVOL TOTVOL) AS STRINGS
    (vla-SetText tbl r 0 (nth 0 row))
    (vla-SetText tbl r 1 (nth 1 row))
    (vla-SetText tbl r 2 (nth 2 row))
    (vla-SetText tbl r 3 (nth 3 row))

    ;; ALIGN + DATA TEXT HEIGHT
    (setq c 0)
    (repeat 4
      (vl-catch-all-apply 'vla-SetCellAlignment (list tbl r c acMiddleCenter))
      (vl-catch-all-apply 'vla-SetCellTextHeight (list tbl r c textHeight))
      (setq c (1+ c))
    )

    (setq r (1+ r))
  )

  tbl
)

(defun c:VOL ( / *error* ss contour-list idx ent obj elev area
                        sorted-data data-raw data-display cumul-vol
                        prev cur depth inc-vol inspt text-height
                        clip)

  (defun *error* (msg)
    (princ (strcat "\nERROR: " msg))
    (princ))

  (princ "\nSELECT CONTOURS FOR THIS BASIN (MULTIPLE SELECTION ALLOWED): ")
  
  (while (not ss)
    (setq ss (ssget '((0 . "LWPOLYLINE"))))
    (if (not ss)
      (progn
         ;; If nil, user either missed everything or right-clicked/entered to finish.
         ;; For ssget, usually Enter means "finish", but if nothing is selected yet, we should ask.
         (initget "Exit Retry")
         (if (= (getkword "\nNOTHING SELECTED. [Retry/Exit] <Retry>: ") "Exit")
           (progn (princ "\nCANCELLED.") (exit))
           (princ "\nRETRYING SELECTION (Window/Crossing supported)...")
         )
      )
    )
  )

  ;; BUILD (ELEV AREANUMERICORNIL)
  (setq contour-list '()
        idx 0)

  (repeat (sslength ss)
    (setq ent  (ssname ss idx)
          obj  (vlax-ename->vla-object ent)
          elev (_bs:get-polyline-elevation ent))

    (if (_bs:is-closed-lwpoly ent)
      (progn
        (setq area (vla-get-Area obj))
        (setq contour-list (cons (list elev area) contour-list)))
      (progn
        ;; KEEP OPEN CONTOURS IN THE LIST BUT WITHOUT AREA
        (setq contour-list (cons (list elev nil) contour-list))))

    (setq idx (1+ idx)))

  ;; SORT BY ELEVATION ASCENDING
  (setq sorted-data
    (vl-sort contour-list '(lambda (a b) (< (car a) (car b)))))

  ;; COMPUTE INCREMENTAL + CUMULATIVE VOLUMES
  ;; DATA-RAW ROWS: (ELEV AREA INC TOT) NUMERIC WHERE AVAILABLE
  (setq data-raw '()
        cumul-vol 0.0
        idx 0)

  (foreach cur sorted-data
    (cond
      ((= idx 0)
       (setq data-raw (append data-raw (list (list (car cur) (cadr cur) 0.0 0.0)))))
      (t
       (setq prev (nth (1- idx) sorted-data))
       (if (and (numberp (cadr cur)) (numberp (cadr prev)))
         (progn
           (setq depth   (- (car cur) (car prev)))
           (setq inc-vol (* (/ (+ (cadr prev) (cadr cur)) 2.0) depth))
           (setq cumul-vol (+ cumul-vol inc-vol))
           (setq data-raw (append data-raw (list (list (car cur) (cadr cur) inc-vol cumul-vol)))))
         ;; IF AN OPEN CONTOUR IS INVOLVED, KEEP TOTALS BUT LEAVE INC BLANK
         (setq data-raw (append data-raw (list (list (car cur) (cadr cur) nil cumul-vol)))))))

    (setq idx (1+ idx)))

  ;; PREPARE DISPLAY STRINGS FOR TABLE + CLIPBOARD
  (setq data-display '())
  (foreach row data-raw
    (setq data-display
      (append data-display
        (list
          (list
            (_bs:fmt-num (nth 0 row) 2)                       ;; ELEV (2 DECIMALS)
            (if (numberp (nth 1 row)) (_bs:fmt-num (nth 1 row) 0) "OPEN")
            (if (numberp (nth 2 row)) (_bs:fmt-num (nth 2 row) 0) "")
            (_bs:fmt-num (nth 3 row) 0))))))

;; CLIPBOARD STRING (TAB-DELIMITED, EXCEL FRIENDLY) - NO HEADER ROW, NO TRAILING BLANK LINE
(setq clip "")
(if data-display
  (progn
    (setq clip (strcat
      (nth 0 (car data-display)) "\t"
      (nth 1 (car data-display)) "\t"
      (nth 2 (car data-display)) "\t"
      (nth 3 (car data-display))
    ))
    (foreach row (cdr data-display)
      (setq clip (strcat clip "\n"
                         (nth 0 row) "\t"
                         (nth 1 row) "\t"
                         (nth 2 row) "\t"
                         (nth 3 row)
                 ))
    )
  )
)
(_bs:set-clipboard clip)

  ;; AUTOMATICALLY DETERMINE TEXT HEIGHT
  (setq text-height (_bs:get-text-height))
  (princ (strcat "\nAUTOMATIC TEXT HEIGHT: " (rtos text-height 2 2)))

  (setq inspt (getpoint "\nCLICK TO PLACE BASIN SUMMARY TABLE: "))
  (if (not inspt)
    (progn (princ "\nPLACEMENT CANCELLED.") (princ) (exit)))

  ;; CREATE TABLE (TITLE + HEADER + DATA)
  (_bs:add-table inspt text-height data-display)

  (princ (strcat "\nSUCCESS! CREATED BASIN SUMMARY TABLE WITH "
                 (itoa (length data-display)) " CONTOURS."
                 "\nALSO COPIED TAB-DELIMITED DATA TO CLIPBOARD FOR EXCEL."))
  (princ)
)
