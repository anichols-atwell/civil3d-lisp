;;; ------------------------------------------------------------
;;; VOFFSET.LSP
;;; Command: VOFFSET
;;;
;;; Mimics the standard AutoCAD OFFSET command but applies a 
;;; Vertical Difference (Elevation Delta) to the offset entities.
;;; ------------------------------------------------------------

(defun c:VOFFSET ( / *error* hdist vdist ent obj baseElev newElev sidept lastEnt newEnt)

  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  ;; 1. GET HORIZONTAL DISTANCE
  ;; mimal error checking, but robust enough for standard use
  (setq hdist (getdist "\nSpecify offset distance: "))
  (if (or (null hdist) (<= hdist 0.0))
    (progn (princ "\nInvalid distance.") (exit))
  )

  ;; 2. GET VERTICAL DELTA (CHANGE IN ELEVATION)
  (setq vdist (getreal "\nSpecify vertical change (delta elevation): "))
  (if (null vdist)
    (progn (princ "\nInvalid vertical delta.") (exit))
  )

  ;; 3. MAIN LOOP - MIMIC STANDARD COMMAND WORKFLOW
  (while T
    (princ "\nSelect object to offset or <Exit>: ")
    (setq ent (car (entsel)))
    
    (if (null ent)
      (exit) ;; User pressed Enter to finish
      (progn
        ;; Check if valid object for elevation
        (setq obj (vlax-ename->vla-object ent))
        (if (vlax-property-available-p obj 'Elevation)
          (progn
             (setq baseElev (vla-get-Elevation obj))
             (setq newElev (+ baseElev vdist))
             
             ;; Prompt for side
             (setq sidept (getpoint "\nSpecify point on side to offset: "))
             
             (if sidept
               (progn
                 (setq lastEnt (entlast))
                 
                 ;; PERFORM THE OFFSET
                 ;; We use the command interface to handle the "Side" logic naturally
                 (command "_.OFFSET" hdist ent sidept "")
                 
                 (setq newEnt (entlast))
                 
                 ;; DID WE CREATE A NEW ENTITY?
                 (if (and newEnt (not (equal newEnt lastEnt)))
                   (progn
                      ;; UPDATE ELEVATION
                      (setq obj (vlax-ename->vla-object newEnt))
                      (vla-put-Elevation obj newElev)
                      (princ (strcat " New Elevation: " (rtos newElev 2 2)))
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
    )
  )
  (princ)
)

(princ "\nVOFFSET command loaded.")
(princ)
