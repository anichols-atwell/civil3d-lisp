;;; ------------------------------------------------------------
;;; VOFFSET.LSP
;;; Command: VOFFSET
;;;
;;; Mimics the standard AutoCAD OFFSET command but applies a 
;;; Vertical Difference (Elevation Delta) to the offset entities.
;;; Now supports pre-selection (noun-verb selection).
;;; ------------------------------------------------------------

(defun c:VOFFSET ( / *error* hdist vdist ent obj baseElev newElev sidept lastEnt newEnt ss i)

  ;; Error handler
  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  (vl-load-com)

  ;; Check for pre-selected objects (noun-verb selection)
  (setq ss (ssget "_I"))
  (if ss (sssetfirst nil nil)) ;; Clear highlight/selection early

  ;; 1. GET HORIZONTAL DISTANCE
  (setq hdist (getdist "\nSpecify offset distance: "))
  (if (or (null hdist) (<= hdist 0.0))
    (progn (princ "\nInvalid distance.") (exit))
  )

  ;; 2. GET VERTICAL DELTA (CHANGE IN ELEVATION)
  (setq vdist (getreal "\nSpecify vertical change (delta elevation): "))
  (if (null vdist)
    (progn (princ "\nInvalid vertical delta.") (exit))
  )

  ;; 3. PROCESS PRE-SELECTION (IF ANY)
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))
        
        (princ (strcat "\nProcessing pre-selected object " (itoa (1+ i)) " of " (itoa (sslength ss)) "..."))
        
        (if (vlax-property-available-p obj 'Elevation)
          (progn
            (setq baseElev (vla-get-Elevation obj))
            (setq newElev (+ baseElev vdist))
            (princ (strcat " (Elev: " (rtos baseElev 2 2) ")"))
            
            (setq sidept (getpoint "\nSpecify point on side to offset: "))
            (if sidept
              (progn
                (setq lastEnt (entlast))
                (command "_.OFFSET" hdist ent sidept "")
                (setq newEnt (entlast))
                (if (and newEnt (not (equal newEnt lastEnt)))
                  (progn
                    (setq obj (vlax-ename->vla-object newEnt))
                    (vla-put-Elevation obj newElev)
                    (princ (strcat " -> New Elevation: " (rtos newElev 2 2)))
                  )
                  (princ "\nOffset failed.")
                )
              )
              (princ "\nNo point selected for this object.")
            )
          )
          (princ "\nObject does not have an Elevation property.")
        )
        (setq i (1+ i))
      )
      (setq ent nil) ;; Reset for the next loop
    )
  )

  ;; 4. MAIN LOOP (FOR ADDITIONAL SELECTIONS)
  (while (setq ent (car (entsel "\nSelect next object to offset or <Exit>: ")))
    (setq obj (vlax-ename->vla-object ent))
    
    (if (vlax-property-available-p obj 'Elevation)
      (progn
        (setq baseElev (vla-get-Elevation obj))
        (setq newElev (+ baseElev vdist))
        (princ (strcat "\nSource Elevation: " (rtos baseElev 2 2)))
        
        (setq sidept (getpoint "\nSpecify point on side to offset: "))
        (if sidept
          (progn
            (setq lastEnt (entlast))
            (command "_.OFFSET" hdist ent sidept "")
            (setq newEnt (entlast))
            (if (and newEnt (not (equal newEnt lastEnt)))
              (progn
                (setq obj (vlax-ename->vla-object newEnt))
                (vla-put-Elevation obj newElev)
                (princ (strcat " -> New Elevation: " (rtos newElev 2 2)))
              )
              (princ "\nOffset failed.")
            )
          )
          (princ "\nNo point selected.")
        )
      )
      (princ "\nSelected object does not have an Elevation property.")
    )
  )
  
  (princ "\nVOFFSET complete.")
  (princ)
)

(princ "\nVOFFSET command loaded.")
(princ)
