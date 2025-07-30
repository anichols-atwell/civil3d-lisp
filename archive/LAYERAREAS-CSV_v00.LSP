;;;---------------------------------------------------------------
;;; LAYERAREAS-CSV.lsp - write closed-polyline areas to a CSV
;;;---------------------------------------------------------------
(defun c:LAYERAREAS-CSV ( / lay ss idx ent obj area tot
                             fn file hdl *error*)
  
  ;; Error handler
  (defun *error* (msg)
    (if file (close file))
    (princ (strcat "\nError: " msg))
    (princ)
  )
  
  (vl-load-com)
  
  ;; Ask for layer with validation
  (setq lay (getstring T "\nLayer to analyse: "))
  (if (not lay)
    (progn (princ "\nCancelled.") (princ) (exit)))
  
  (if (not (tblsearch "LAYER" lay))
    (progn 
      (princ (strcat "\nLayer '" lay "' does not exist."))
      (princ)
      (exit)))
  
  ;; Ask for output file
  (setq fn (getfiled "CSV file to create" (strcat lay "_areas.csv") "csv" 1))
  (if (not fn)
    (progn (princ "\nFile selection cancelled.") (princ) (exit)))
  
  ;; Select closed polylines on the specified layer
  (if (setq ss (ssget "_X"
               (list '(0 . "LWPOLYLINE") (cons 8 lay) '(-4 . "&") '(70 . 1))))
    (progn
      ;; Open output file
      (if (setq file (open fn "w"))
        (progn
          (write-line "Handle,Area" file)     ; CSV header
          (setq tot 0.0  idx 0)
          (repeat (sslength ss)
            (setq ent (ssname ss idx)
                  obj (vlax-ename->vla-object ent)
                  area (vla-get-Area obj)
                  hdl  (vla-get-Handle obj))
            (write-line
              (strcat hdl "," (rtos area 2 8))   ; eight-decimal precision
              file)
            (setq tot (+ tot area)
                  idx (1+ idx)))
          (close file)
          (setq file nil)  ; Clear file handle
          (princ (strcat "\nWrote "
                         (itoa (sslength ss))
                         " records to " fn
                         ".  Total area = "
                         (rtos tot 2 2) " sq.units.")))
        (princ (strcat "\nCannot create file: " fn))))
    (princ "\nNo closed polylines found on that layer."))
  (princ))