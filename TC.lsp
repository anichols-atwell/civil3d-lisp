;;; TC.LSP
;;; Extract start/end elevations from a Civil 3D surface at a user-selected polyline
;;; and copy to clipboard for Excel (tab-delimited, single row, no headers):
;;;   Length(ft) <TAB> StartElev <TAB> EndElev
;;;
;;; Command: TC
;;; ------------------------------------------------------------

(vl-load-com)

(defun _c3d:objname (vlaObj)
  (if vlaObj (strcase (vlax-get-property vlaObj 'ObjectName)) "")
)

(defun _c3d:is-surface-obj (vlaObj / nm)
  (setq nm (_c3d:objname vlaObj))
  (or (wcmatch nm "*AECC*SURFACE*")
      (wcmatch nm "*TIN*SURFACE*")
      (wcmatch nm "*GRID*SURFACE*"))
)

(defun _c3d:is-polyline-ename (e / dxf0)
  (and e
       (setq dxf0 (cdr (assoc 0 (entget e))))
       (or (= dxf0 "LWPOLYLINE")
           (= dxf0 "POLYLINE")))
)

(defun _c3d:clipboard-settext (txt / html pw cd ok)
  ;; Uses htmlfile ActiveX clipboardData interface (common on Windows)
  (setq ok nil)
  (setq html (vl-catch-all-apply 'vlax-create-object (list "htmlfile")))
  (if (not (vl-catch-all-error-p html))
    (progn
      (setq pw (vlax-get-property html 'ParentWindow))
      (setq cd (vlax-get-property pw 'ClipboardData))
      (vl-catch-all-apply 'vlax-invoke-method (list cd 'setData "Text" txt))
      (setq ok T)
      (vlax-release-object html)
    )
  )
  ok
)

(defun _c3d:find-elev-at-xy (surf x y / res)
  ;; Surface.FindElevationAtXY(x,y) -> elevation (real) or nil if outside surface/failure
  (setq res (vl-catch-all-apply 'vlax-invoke-method (list surf 'FindElevationAtXY x y)))
  (if (vl-catch-all-error-p res) nil res)
)

(defun _c3d:fmt (val prec)
  (if (and val (numberp val))
    (rtos val 2 prec)
    ""  ;; blank if not available
  )
)

(defun _c3d:insunits->feet-factor ( / u)
  ;; Returns multiplier to convert drawing units to feet when possible.
  ;; If unknown/unset, returns 1.0 (assume feet).
  (setq u (getvar "INSUNITS"))
  (cond
    ((or (= u 0) (= u 2)) 1.0)                  ; 0=Unitless, 2=Feet
    ((= u 1) (/ 1.0 12.0))                      ; Inches -> feet
    ((= u 6) 3.28083989501312)                  ; Meters -> feet
    ((= u 4) (/ 1.0 304.8))                     ; Millimeters -> feet
    ((= u 5) (/ 1.0 30.48))                     ; Centimeters -> feet
    ((= u 3) 5280.0)                            ; Miles -> feet
    ((= u 7) 0.00328083989501312)               ; Kilometers -> feet
    (T 1.0)
  )
)

(defun _c3d:curve-length-feet (ename / dist pEnd fac)
  ;; Uses curve distance in drawing units, converts to feet where possible.
  (setq fac (_c3d:insunits->feet-factor))
  (setq pEnd (vlax-curve-getEndParam ename))
  (setq dist (vlax-curve-getDistAtParam ename pEnd))
  (if (and dist (numberp dist))
    (* dist fac)
    nil
  )
)

(defun _c3d:prompt-select-surface ( / sel e obj)
  (princ "\nSelect a Civil 3D surface: ")
  (setq sel (entsel))
  (cond
    ((null sel) nil)
    (t
      (setq e   (car sel)
            obj (vlax-ename->vla-object e))
      (if (_c3d:is-surface-obj obj)
        obj
        (progn
          (princ "\nThat object is not a Civil 3D surface. Please try again.")
          (_c3d:prompt-select-surface)
        )
      )
    )
  )
)

(defun _c3d:prompt-select-polyline ( / sel e)
  (princ "\nSelect a polyline to sample start/end: ")
  (setq sel (entsel))
  (cond
    ((null sel) nil)
    (t
      (setq e (car sel))
      (if (_c3d:is-polyline-ename e)
        e
        (progn
          (princ "\nThat object is not a polyline. Please try again.")
          (_c3d:prompt-select-polyline)
        )
      )
    )
  )
)

(defun c:TC ( / *error* oldCmdecho oldOsmode surf pl sp ep sx sy ex ey
                                selev eelev lenFt out ok)

  (defun *error* (msg)
    (if oldCmdecho (setvar "CMDECHO" oldCmdecho))
    (if oldOsmode  (setvar "OSMODE"  oldOsmode))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\nError: " msg))
      (princ "\nCancelled.")
    )
    (princ)
  )

  (setq oldCmdecho (getvar "CMDECHO"))
  (setq oldOsmode  (getvar "OSMODE"))
  (setvar "CMDECHO" 0)

  ;; Select surface
  (setq surf (_c3d:prompt-select-surface))
  (if (null surf) (progn (princ "\nNo surface selected.") (*error* "CANCEL") (exit)))
  (princ (strcat "\nUsing surface object: " (_c3d:objname surf)))

  ;; Select polyline
  (setq pl (_c3d:prompt-select-polyline))
  (if (null pl) (progn (princ "\nNo polyline selected.") (*error* "CANCEL") (exit)))

  ;; Start/end points (WCS)
  (setq sp (vlax-curve-getStartPoint pl))
  (setq ep (vlax-curve-getEndPoint   pl))
  (setq sx (car sp) sy (cadr sp))
  (setq ex (car ep) ey (cadr ep))

  ;; Surface elevations at endpoints
  (setq selev (_c3d:find-elev-at-xy surf sx sy))
  (setq eelev (_c3d:find-elev-at-xy surf ex ey))

  ;; Length in feet (best-effort conversion using INSUNITS)
  (setq lenFt (_c3d:curve-length-feet pl))

  ;; Output order: Length, StartElev, EndElev (no headers)
  (setq out
    (strcat
      (_c3d:fmt lenFt 3) "\t"
      (_c3d:fmt selev 3) "\t"
      (_c3d:fmt eelev 3)
    )
  )

  (setq ok (_c3d:clipboard-settext out))

  (princ "\n----------------------------------------")
  (princ (strcat "\nLength (ft): " (_c3d:fmt lenFt 3)))
  (princ (strcat "\nStart  Elev: " (_c3d:fmt selev 3)))
  (princ (strcat "\nEnd    Elev: " (_c3d:fmt eelev 3)))
  (if ok
    (princ "\nCopied: Length<TAB>StartElev<TAB>EndElev  (Paste into Excel)")
    (progn
      (princ "\nCould not access clipboard. Copy this manually:\n")
      (princ out)
    )
  )
  (princ "\n----------------------------------------")

  (setvar "CMDECHO" oldCmdecho)
  (setvar "OSMODE"  oldOsmode)
  (princ)
)
