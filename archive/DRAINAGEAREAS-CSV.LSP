;;;---------------------------------------------------------------
;;; DRAINAGEAREAS-CSV.lsp - Export drainage area polylines with labels to CSV
;;;
;;; Usage: DRAINAGEAREAS-CSV
;;; Prompts for polyline layer, text layer, and output file
;;; Creates CSV with Handle, Drainage Layer, Label Layer, Label ID, Area (SF), Area (AC)
;;; Uses point-in-polygon testing to match text labels with drainage areas
;;;---------------------------------------------------------------

(defun c:DRAINAGEAREAS-CSV ( / *error* poly-layer text-layer ss-poly ss-text 
                               fn file idx ent obj area hdl centroid
                               text-ent text-obj text-pt text-content
                               label-id area-sf area-ac)
  
  ;; Error handler
  (defun *error* (msg)
    (if file (close file))
    (princ (strcat "\nError: " msg))
    (princ)
  )
  
  ;; Point-in-polygon test function (ray casting algorithm)
  (defun point-in-polygon (pt poly-pts / count i j n x y xi yi xj yj)
    (setq count 0
          x (car pt)
          y (cadr pt)
          n (length poly-pts)
          i 0
          j (1- n))
    (while (< i n)
      (setq xi (car (nth i poly-pts))
            yi (cadr (nth i poly-pts))
            xj (car (nth j poly-pts))
            yj (cadr (nth j poly-pts)))
      (if (and (or (and (> yi y) (<= yj y))
                   (and (<= yi y) (> yj y)))
               (< x (+ xj (* (/ (- y yj) (- yi yj)) (- xi xj)))))
        (setq count (1+ count)))
      (setq j i
            i (1+ i)))
    (= (rem count 2) 1)
  )
  
  ;; Get polyline vertices as point list
  (defun get-polyline-points (ent / pts coord)
    (setq pts '())
    (foreach coord (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) (entget ent)))
      (setq pts (append pts (list coord))))
    pts
  )
  
  ;; Find text label inside polyline
  (defun find-text-in-poly (poly-pts ss-text / i text-ent text-pt text-str result)
    (setq result ""
          i 0)
    (if ss-text
      (repeat (sslength ss-text)
        (setq text-ent (ssname ss-text i))
        (setq text-pt (cdr (assoc 10 (entget text-ent))))
        (if (point-in-polygon text-pt poly-pts)
          (progn
            (setq text-str (cdr (assoc 1 (entget text-ent))))
            (if (= result "")
              (setq result text-str)
              (setq result (strcat result "-" text-str)))))
        (setq i (1+ i))))
    (if (= result "") "UNLABELED" result)
  )
  
  (vl-load-com)
  
  ;; Get polyline layer with validation
  (setq poly-layer (getstring "\nLayer containing drainage area polylines: "))
  (if (not poly-layer)
    (progn (princ "\nCancelled.") (princ) (exit)))
  
  (if (not (tblsearch "LAYER" poly-layer))
    (progn 
      (princ (strcat "\nLayer '" poly-layer "' does not exist."))
      (princ) (exit)))
  
  ;; Get text layer with validation  
  (setq text-layer (getstring "\nLayer containing area labels: "))
  (if (not text-layer)
    (progn (princ "\nCancelled.") (princ) (exit)))
  
  (if (not (tblsearch "LAYER" text-layer))
    (progn 
      (princ (strcat "\nLayer '" text-layer "' does not exist."))
      (princ) (exit)))
  
  ;; Get output file
  (setq fn (getfiled "CSV file to create" (strcat poly-layer "_drainage_areas.csv") "csv" 1))
  (if (not fn)
    (progn (princ "\nFile selection cancelled.") (princ) (exit)))
  
  ;; Select closed polylines on the specified layer
  (setq ss-poly (ssget "_X"
                      (list '(0 . "LWPOLYLINE") 
                            (cons 8 poly-layer) 
                            '(-4 . "&") 
                            '(70 . 1))))
  
  (if (not ss-poly)
    (progn 
      (princ (strcat "\nNo closed polylines found on layer '" poly-layer "'."))
      (princ) (exit)))
  
  ;; Select text entities on the specified layer
  (setq ss-text (ssget "_X"
                      (list '(-4 . "<OR")
                            '(0 . "TEXT")
                            '(0 . "MTEXT")
                            '(-4 . "OR>")
                            (cons 8 text-layer))))
  
  (if (not ss-text)
    (princ (strcat "\nWarning: No text entities found on layer '" text-layer "'.")))
  
  ;; Process polylines and create CSV
  (if (setq file (open fn "w"))
    (progn
      ;; Write CSV header
      (write-line "Handle,Drainage_Layer,Label_Layer,Label_ID,Area_SqFt,Area_Acres" file)
      
      (setq idx 0)
      (princ (strcat "\nProcessing " (itoa (sslength ss-poly)) " drainage areas..."))
      
      (repeat (sslength ss-poly)
        (setq ent (ssname ss-poly idx)
              obj (vlax-ename->vla-object ent)
              area (vla-get-Area obj)
              hdl (vla-get-Handle obj)
              area-sf area
              area-ac (/ area 43560.0))
        
        ;; Get polyline points for point-in-polygon test
        (setq poly-pts (get-polyline-points ent))
        
        ;; Find matching text label
        (setq label-id (find-text-in-poly poly-pts ss-text))
        
        ;; Write CSV line
        (write-line
          (strcat hdl ","
                  poly-layer ","
                  text-layer ","
                  label-id ","
                  (rtos area-sf 2 2) ","
                  (rtos area-ac 2 4))
          file)
        
        (setq idx (1+ idx)))
      
      (close file)
      (setq file nil)
      
      (princ (strcat "\nSuccess! Exported " 
                     (itoa (sslength ss-poly))
                     " drainage areas to " fn
                     "\nColumns: Handle, Drainage_Layer, Label_Layer, Label_ID, Area_SqFt, Area_Acres")))
    (princ (strcat "\nError: Cannot create file " fn)))
  
  (princ)
)