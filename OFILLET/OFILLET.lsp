(defun c:OFILLET ( / *error* rad inc sel1 ent1 last_ent old_cmdecho)
  ;; Error handler
  (defun *error* (msg)
    (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
    (if (not (wcmatch (strcase msg t) "*break*,*cancel*,*exit*"))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )
  
  (vl-load-com)
  (setq old_cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  
  (princ "\n--- Offset Fillet (Ripple Effect) ---")
  
  ;; Input: Initial Radius
  (initget 4) 
  (setq rad (getdist "\nSpecify initial radius: "))
  (if (not rad) (setq rad 0.0))
  
  ;; Input: Radius Increment
  (setq inc (getreal "\nSpecify radius offset (+ for increase, - for decrease): "))
  (if (not inc) (setq inc 0.0))
  
  (setq last_ent nil)
  
  (princ (strcat "\nradius=" (rtos rad 2 2)))
  
  ;; Main Loop
  (while T
    ;; User Prompts
    (setq sel1 (entsel (strcat "\nSelect first object (Radius " (rtos rad 2 2) "): ")))
    
    (if (null sel1)
      (progn
        (princ "\nNo selection. Exiting.")
        (exit) ; Break loop on empty selection
      )
    )
    
    (setq ent1 (car sel1))
    
    ;; --- Logic: Check for new contour ---
    ;; If we have a previous entity, and this one is DIFFERENT, increment radius.
    (if (and last_ent (not (equal ent1 last_ent)))
      (progn
        (setq rad (+ rad inc))
        ;; Clamp to 0
        (if (< rad 0.0) (setq rad 0.0))
        (princ (strcat "\nNew contour detected. Radius updated to: " (rtos rad 2 2)))
        (setvar "FILLETRAD" rad) ; Update the system variable immediately
      )
    )

    ;; Apply Fillet
    ;; We pass the first selection POINT (cadr sel1) to the command, then 'pause' to let the user
    ;; interactively select the second segment using native AutoCAD behavior.
    (setvar "FILLETRAD" rad)
    (setvar "CMDECHO" 1)
    (command "_.FILLET" (cadr sel1) pause)
    (setvar "CMDECHO" 0)

    ;; Update Tracking
    (setq last_ent ent1)
  )
  
  (setvar "CMDECHO" old_cmdecho)
  (princ)
)

(princ "\nType OFILLET to run.")
