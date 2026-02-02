;;; BASINBUILDER.LSP
;;; COMMAND: BASINBUILDER
;;;
;;; PURPOSE:
;;;   USER SELECTS A CLOSED LWPOLYLINE WITH AN ASSIGNED ELEVATION (BASE CONTOUR).
;;;   USER ENTERS SIDE SLOPE H:V (V IS 1, USER ENTERS H), DEPTH INCREMENT (FT),
;;;   AND TOTAL DEPTH (FT).
;;;
;;;   ROUTINE OFFSETS THE POLYLINE EACH INCREMENT BY:
;;;     HORIZONTAL OFFSET = H * VERTICAL STEP
;;;
;;;   ELEVATIONS AUTO-DIRECTION:
;;;     - IF OFFSETS GO OUTWARD (AREA INCREASES), ELEVATION STEPS UP
;;;     - IF OFFSETS GO INWARD  (AREA DECREASES), ELEVATION STEPS DOWN
;;;
;;; OUTPUT:
;;;   CREATES A TABLE "BASIN VOL. TABLE" AND COPIES DATA (NO HEADER, NO TRAILING BLANK LINE)
;;;   TO CLIPBOARD FOR EXCEL.
;;;
;;; TABLE COLUMNS:
;;;   ELEVATION (FT) | AREA (SF) | INC. VOLUME (CF) | TOTAL VOLUME (CF)

(vl-load-com)

(defun _bb:commify-int (s / n out)
  (setq n (strlen s)
        out "")
  (while (> n 3)
    (setq out (strcat "," (substr s (- n 2) 3) out)
          n (- n 3)))
  (strcat (substr s 1 n) out)
)

(defun _bb:fmt-num (n dec / s sign p ip dp)
  (setq sign (if (< n 0.0) "-" ""))
  (setq s (rtos (abs n) 2 dec))
  (setq p (vl-string-search "." s))
  (if p
    (progn
      (setq ip (substr s 1 p))
      (setq dp (substr s (+ p 2)))
      (setq ip (_bb:commify-int ip))
      (strcat sign ip "." dp))
    (progn
      (setq ip (_bb:commify-int s))
      (strcat sign ip)))
)

(defun _bb:set-clipboard (txt / html win clip)
  (setq html (vlax-create-object "htmlfile"))
  (if html
    (progn
      (setq win  (vlax-get html 'parentWindow))
      (setq clip (vlax-get win  'clipboardData))
      (vl-catch-all-apply 'vlax-invoke (list clip 'setData "Text" txt))
      (vlax-release-object html)))
  txt
)

(defun _bb:get-text-height ( / h)
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

(defun _bb:modelspace ( / acad doc)
  (setq acad (vlax-get-acad-object))
  (setq doc  (vla-get-ActiveDocument acad))
  (vla-get-ModelSpace doc)
)

(defun _bb:is-closed-lwpoly (ent)
  (= (logand (cdr (assoc 70 (entget ent))) 1) 1)
)

(defun _bb:get-elev (ent / obj elev)
  (setq obj (vlax-ename->vla-object ent))
  (if obj
    (progn
      (setq elev (vla-get-Elevation obj))
      (if elev elev 0.0))
    0.0)
)

(defun _bb:set-elev (ent elev / obj)
  (setq obj (vlax-ename->vla-object ent))
  (if obj (vl-catch-all-apply 'vla-put-Elevation (list obj elev)))
  ent
)

(defun _bb:get-area (ent / obj)
  (setq obj (vlax-ename->vla-object ent))
  (if obj (vl-catch-all-apply 'vla-get-Area (list obj)) nil)
)

(defun _bb:last-ename ( ) (entlast))

(defun _bb:offset-by-command (ent dist sidept / before after)
  ;; USE COMMAND OFFSET WITH A SIDE POINT. RETURNS NEW ENTITY NAME OR NIL.
  (setq before (_bb:last-ename))
  (command "_.OFFSET" dist ent sidept "")
  (setq after (_bb:last-ename))
  (if (and after (not (eq after before))) after nil)
)

(defun _bb:add-table (inspt textHeight data / ms tbl rowH defaultW acMiddleCenter r c)
  (setq ms (_bb:modelspace))
  (setq rowH (* textHeight 1.8))
  (setq defaultW (* textHeight 10.0))
  (setq acMiddleCenter 5)

  ;; ROWS: TITLE + HEADER + DATA
  (setq tbl (vla-AddTable ms (vlax-3d-point inspt) (+ 2 (length data)) 4 rowH defaultW))

  ;; COLUMN WIDTHS
  (vla-SetColumnWidth tbl 0 (* textHeight 12.0))
  (vla-SetColumnWidth tbl 1 (* textHeight 14.0))
  (vla-SetColumnWidth tbl 2 (* textHeight 16.0))
  (vla-SetColumnWidth tbl 3 (* textHeight 18.0))

  ;; TITLE ROW
  (vla-SetText tbl 0 0 "BASIN VOL. TABLE")
  (vl-catch-all-apply 'vla-SetCellAlignment (list tbl 0 0 acMiddleCenter))
  (vl-catch-all-apply 'vla-SetCellTextHeight (list tbl 0 0 textHeight))

  ;; HEADER ROW
  (vla-SetText tbl 1 0 "ELEVATION\\P(FT)")
  (vla-SetText tbl 1 1 "AREA\\P(SF)")
  (vla-SetText tbl 1 2 "INC.\\PVOLUME\\P(CF)")
  (vla-SetText tbl 1 3 "TOTAL\\PVOLUME\\P(CF)")

  (setq c 0)
  (repeat 4
    (vl-catch-all-apply 'vla-SetCellAlignment (list tbl 1 c acMiddleCenter))
    (vl-catch-all-apply 'vla-SetCellTextHeight (list tbl 1 c textHeight))
    (setq c (1+ c))
  )

  ;; DATA ROWS START AT 2
  (setq r 2)
  (foreach row data
    (vla-SetText tbl r 0 (nth 0 row))
    (vla-SetText tbl r 1 (nth 1 row))
    (vla-SetText tbl r 2 (nth 2 row))
    (vla-SetText tbl r 3 (nth 3 row))

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

(defun _bb:build-summary-from-ents (ents / contour-list sorted-data data-raw data-display
                                       cumul-vol idx prev cur depth inc-vol
                                       obj elev area
                                       clip firstrow)
  ;; ENTS IS A LIST OF ENAMES (CLOSED LWPOLYLINES WITH ELEVATIONS)
  ;; RETURNS DATA-DISPLAY AND COPIES TO CLIPBOARD (NO HEADER, NO TRAILING NEWLINE)

  (setq contour-list '())
  (foreach e ents
    (setq obj (vlax-ename->vla-object e))
    (setq elev (_bb:get-elev e))
    (if (_bb:is-closed-lwpoly e)
      (setq area (vla-get-Area obj))
      (setq area nil))
    (setq contour-list (cons (list elev area) contour-list))
  )

  ;; SORT BY ELEVATION ASC
  (setq sorted-data (vl-sort contour-list '(lambda (a b) (< (car a) (car b)))))

  ;; VOLUME CALCS
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
         (setq data-raw (append data-raw (list (list (car cur) (cadr cur) nil cumul-vol)))))
      ))
    (setq idx (1+ idx))
  )

  ;; DISPLAY STRINGS
  (setq data-display '())
  (foreach row data-raw
    (setq data-display
      (append data-display
        (list
          (list
            (_bb:fmt-num (nth 0 row) 2)
            (if (numberp (nth 1 row)) (_bb:fmt-num (nth 1 row) 0) "OPEN")
            (if (numberp (nth 2 row)) (_bb:fmt-num (nth 2 row) 0) "")
            (_bb:fmt-num (nth 3 row) 0)
          )
        )
      )
    )
  )

  ;; CLIPBOARD (NO HEADER, NO TRAILING NEWLINE)
  (setq clip "")
  (if data-display
    (progn
      (setq firstrow (car data-display))
      (setq clip (strcat (nth 0 firstrow) "\t" (nth 1 firstrow) "\t" (nth 2 firstrow) "\t" (nth 3 firstrow)))
      (foreach row (cdr data-display)
        (setq clip (strcat clip "\n" (nth 0 row) "\t" (nth 1 row) "\t" (nth 2 row) "\t" (nth 3 row)))
      )
    )
  )
  (_bb:set-clipboard clip)

  data-display
)

(defun c:BASINBUILDER ( / *error* ent baseElev baseArea
                         hval depthInc totalDepth
                         sidept steps i vstep dist
                         curEnt newEnt createdEnts
                         elevSign prevArea newArea
                         inspt textHeight dataDisplay)

  (defun *error* (msg)
    (if msg (princ (strcat "\nERROR: " msg)))
    (princ)
  )

  (princ "\nSELECT A CLOSED BASE POLYLINE FOR BASINBUILDER: ")
  
  (while (not ent)
    (setq ent (car (entsel "\nSELECT BASE POLYLINE: ")))
    (if ent
      (progn
        (if (/= (cdr (assoc 0 (entget ent))) "LWPOLYLINE")
          (progn 
             (princ "\nSELECTED OBJECT IS NOT AN LWPOLYLINE.") 
             (setq ent nil) ;; Reset to keep looping
          )
          (if (not (_bb:is-closed-lwpoly ent))
             (progn 
                (princ "\nSELECTED POLYLINE IS NOT CLOSED.") 
                (setq ent nil) ;; Reset to keep looping
             )
          )
        )
      )
      (progn
         ;; If user pressed Enter or Esc (which returns nil for entsel in some cases or just nil if missed)
         ;; We check ERRNO to see if it was a cancel vs a miss-click, 
         ;; BUT for simplicity in LISP, if entsel returns nil, it usually means missed click or enter.
         ;; We will ask if they want to retry or exit if they missed.
         (initget "Exit Retry")
         (if (= (getkword "\nMISSED OR NOTHING SELECTED. [Retry/Exit] <Retry>: ") "Exit")
           (progn (princ "\nCANCELLED.") (exit))
           (princ "\nRETRYING SELECTION...")
         )
      )
    )
  )

  (setq baseElev (_bb:get-elev ent))
  (setq baseArea (_bb:get-area ent))

  (if (= baseElev 0.0)
    (princ "\nWARNING: BASE ELEVATION IS 0.00. IF THIS IS NOT INTENTIONAL, SET AN ELEVATION AND TRY AGAIN.")
  )

  (setq hval (getreal "\nENTER SIDE SLOPE H VALUE FOR H:1 (EXAMPLE 4 FOR 4:1): "))
  (if (or (not hval) (<= hval 0.0))
    (progn (princ "\nINVALID H VALUE. COMMAND CANCELLED.") (princ) (exit))
  )

  (setq depthInc (getreal "\nENTER DEPTH INCREMENT (FT) (EXAMPLE 1 OR 2): "))
  (if (or (not depthInc) (<= depthInc 0.0))
    (progn (princ "\nINVALID DEPTH INCREMENT. COMMAND CANCELLED.") (princ) (exit))
  )

  (setq totalDepth (getreal "\nENTER TOTAL DEPTH TO BUILD (FT) (EXAMPLE 10): "))
  (if (or (not totalDepth) (<= totalDepth 0.0))
    (progn (princ "\nINVALID TOTAL DEPTH. COMMAND CANCELLED.") (princ) (exit))
  )

  (princ "\nPICK A POINT ON THE SIDE YOU WANT THE OFFSETS TO GO (OUTSIDE OR INSIDE): ")
  (setq sidept (getpoint))
  (if (not sidept)
    (progn (princ "\nNO SIDE POINT PICKED. COMMAND CANCELLED.") (princ) (exit))
  )

  (setq createdEnts '())
  (setq curEnt ent)
  (setq elevSign nil)

  ;; FULL STEPS
  (setq steps (fix (/ totalDepth depthInc)))
  (setq i 1)

  (while (<= i steps)
    ;; CALCULATE CUMULATIVE DISTANCE FROM ORIGIN
    (setq vstep (* i depthInc))
    (setq dist (* hval vstep))

    ;; ALWAYS OFFSET FROM THE BASE ENTITY 'ent'
    ;; THIS PREVENTS THE "SIDE POINT" CROSSING ISSUE SEEN WITH INCREMENTAL OFFSETS
    (setq newEnt (_bb:offset-by-command ent dist sidept))

    (if (not newEnt)
      (progn 
         (princ "\nOFFSET FAILED (DEPTH LIMIT REACHED OR GEOMETRY TOO SMALL). STOPPING GENERATION.") 
         (setq i (+ steps 100)) ;; BREAK LOOP
      )
      (progn
        (if (not (_bb:is-closed-lwpoly newEnt))
          (progn 
             (princ "\nOFFSET RESULT IS NOT CLOSED. STOPPING GENERATION.") 
             (entdel newEnt)
             (setq i (+ steps 100)) ;; BREAK LOOP
          )
          (progn
            ;; DETERMINE DIRECTION ON FIRST OFFSET ONLY
            (if (not elevSign)
              (progn
                (setq newArea (_bb:get-area newEnt))
                ;; COMPARE WITH ORIGINAL BASE AREA
                (if (and (numberp newArea) (numberp baseArea))
                  (setq elevSign (if (> newArea baseArea) 1.0 -1.0))
                  (setq elevSign 1.0)
                )
              )
            )

            (_bb:set-elev newEnt (+ baseElev (* elevSign vstep)))
            (setq createdEnts (append createdEnts (list newEnt)))
          )
        )
      )
    )
    
    (setq i (1+ i))
  )

  ;; REMAINDER PARTIAL STEP
  (setq vstep (- totalDepth (* steps depthInc)))
  ;; Only proceed if we have a remainder AND the loop didn't abort (checked by matching steps count roughly or just trying)
  ;; Simpler: If the previous step succeeded (or if steps=0), try this.
  
  (if (and (> vstep 1e-8) (or (= steps 0) (>= (length createdEnts) steps)))
    (progn
      (setq dist (* hval totalDepth)) ;; Cumulative total distance
      (setq newEnt (_bb:offset-by-command ent dist sidept))
      
      (if (and newEnt (_bb:is-closed-lwpoly newEnt))
        (progn
           (if (not elevSign)
              (progn
                (setq newArea (_bb:get-area newEnt))
                (if (and (numberp newArea) (numberp baseArea))
                  (setq elevSign (if (> newArea baseArea) 1.0 -1.0))
                  (setq elevSign 1.0)
                )
              )
           )
           (_bb:set-elev newEnt (+ baseElev (* elevSign totalDepth)))
           (setq createdEnts (append createdEnts (list newEnt)))
        )
        (if newEnt (entdel newEnt)) ;; Cleanup if open
      )
    )
  )

  ;; BUILD SUMMARY FROM ORIGINAL + CREATED
  (setq dataDisplay (_bb:build-summary-from-ents (cons ent createdEnts)))

  ;; AUTOMATICALLY DETERMINE TEXT HEIGHT
  (setq textHeight (_bb:get-text-height))
  (princ (strcat "\nAUTOMATIC TEXT HEIGHT: " (rtos textHeight 2 2)))

  (setq inspt (getpoint "\nCLICK TO PLACE BASIN VOL. TABLE: "))
  (if (not inspt)
    (progn (princ "\nPLACEMENT CANCELLED. NOTE: CONTOURS WERE STILL CREATED.") (princ) (exit))
  )

  (_bb:add-table inspt textHeight dataDisplay)

  (princ (strcat
           "\nSUCCESS! CREATED " (itoa (length createdEnts)) " OFFSET CONTOUR(S)."
           "\nCREATED BASIN VOL. TABLE AND COPIED TAB-DELIMITED DATA TO CLIPBOARD."
         ))
  (princ)
)
