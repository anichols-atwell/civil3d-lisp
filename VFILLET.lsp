;;; ------------------------------------------------------------
;;; VFILLET.LSP
;;; Command: VFILLET
;;;
;;; Performs sequential fillet operations with an automatically 
;;; incrementing (or decrementing) radius.
;;; Designed for contouring applications where radius changes 
;;; based on slope/offset.
;;; ------------------------------------------------------------

(defun c:VFILLET ( / *error* rInit rStep rCurr ent1 ent2)

  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  (princ "\n--- Variable Radius Fillet ---")

  ;; 1. GET INITIAL RADIUS
  (setq rInit (getdist "\nEnter initial radius: "))
  (if (or (null rInit) (< rInit 0.0))
    (progn (princ "\nInvalid radius.") (exit))
  )

  ;; 2. GET RADIUS OFFSET (STEP)
  (setq rStep (getdist "\nEnter Radius Offset (change per fillet): "))
  (if (null rStep)
    (progn (princ "\nInvalid offset.") (exit))
  )

  ;; Optional: Ask direction? 
  ;; For now, we assume positive input means Add. 
  ;; User can enter negative if they really want, but getdist usually returns positive.
  ;; Let's ask if they want to Increase or Decrease if they didn't specify sign?
  ;; Actually, the user prompt said "Enter Radius Offset...: 3". 
  ;; Usually for contours going "up", radius increases.
  ;; We will assume Addition. If they want subtraction, 
  ;; we could add a keyword in the loop or prompt initially.
  ;; Let's add a quick keyword prompt for mode.
  
  (initget "Increase Decrease")
  (setq mode (getkword "\nRadius change Direction [Increase/Decrease] <Increase>: "))
  (if (eq mode "Decrease")
    (setq rStep (- rStep))
  )
 
  (setq rCurr rInit)

  ;; 3. MAIN LOOP
  (while T
    (princ (strcat "\nCurrent Fillet Radius: " (rtos rCurr 2 2)))
    
    (setq ent1 (entsel "\nSelect first object (or Press Enter to Exit): "))
    
    (if (null ent1)
      (exit) ;; Exit on Enter
      (progn
        (setq ent2 (entsel "\nSelect second object: "))
        (if ent2
          (progn
            ;; Perform Fillet
            ;; We set the radius first to ensure it's correct for this pair
            (setvar "FILLETRAD" rCurr)
            (command "_.FILLET" ent1 ent2)
            
            ;; Update Radius for next pass
            (setq rCurr (+ rCurr rStep))
            
            ;; Safety check for negative radius
            (if (< rCurr 0.0)
              (progn
                (princ "\nRadius would become negative. Resetting to 0.")
                (setq rCurr 0.0)
              )
            )
          )
          (princ "\nSecond object not selected. Skipping.")
        )
      )
    )
  )
  (princ)
)

(princ "\nVFILLET command loaded. Type VFILLET to run.")
(princ)
