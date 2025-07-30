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

  ;; Find closest text to a point (for open polylines)
  (defun find-closest-text (pt ss-text max-dist / i text-ent text-pt text-str dist min-dist closest-text)
    (setq min-dist 1e6
          closest-text ""
          i 0)
    (if ss-text
      (repeat (sslength ss-text)
        (setq text-ent (ssname ss-text i)
              text-pt (cdr (assoc 10 (entget text-ent)))
              dist (distance pt text-pt))
        (if (and (< dist min-dist) (< dist max-dist))
          (progn
            (setq min-dist dist
                  closest-text (cdr (assoc 1 (entget text-ent))))))
        (setq i (1+ i))))
    (if (= closest-text "") "UNLABELED" closest-text)
  )
  
  (vl-load-com)
  
  ;; Get polyline layer by clicking on object
  (setq sel-result (entsel "Select closed polyline from DRAINAGE AREAS layer: "))
  (if (not sel-result)
    (progn (princ "\nSelection cancelled.") (princ) (exit)))
  
  (setq sel-ent (car sel-result)
        poly-layer (cdr (assoc 8 (entget sel-ent))))
  
  (princ (strcat "\nUsing drainage layer: " poly-layer))
  
  ;; Validate it's a polyline (closed or not)
  (setq ent-data (entget sel-ent))
  (if (not (= (cdr (assoc 0 ent-data)) "LWPOLYLINE"))
    (progn 
      (princ "\nError: Selected object is not a polyline.")
      (princ) (exit)))
  
  ;; Check if selected polyline is closed and inform user
  (if (= (logand (cdr (assoc 70 ent-data)) 1) 1)
    (princ "\nSelected polyline is closed - good!")
    (princ "\nWarning: Selected polyline is not closed, but continuing..."))
  
  ;; Get text layer by clicking on object
  (setq sel-result (entsel "Select text label from LABELS layer (A1, A2, etc.): "))
  (if (not sel-result)
    (progn (princ "\nSelection cancelled.") (princ) (exit)))
  
  (setq sel-ent (car sel-result)
        text-layer (cdr (assoc 8 (entget sel-ent))))
  
  (princ (strcat "\nUsing labels layer: " text-layer))
  
  ;; Validate it's a text entity
  (setq ent-data (entget sel-ent)
        ent-type (cdr (assoc 0 ent-data)))
  (if (not (or (= ent-type "TEXT") (= ent-type "MTEXT")))
    (progn 
      (princ "\nError: Selected object is not a text entity.")
      (princ) (exit)))
  
  ;; Get output file
  (setq fn (getfiled "CSV file to create" (strcat poly-layer "_drainage_areas.csv") "csv" 1))
  (if (not fn)
    (progn (princ "\nFile selection cancelled.") (princ) (exit)))
  
  ;; Select all polylines on the specified layer (closed and open)
  (setq ss-poly (ssget "_X"
                      (list '(0 . "LWPOLYLINE") 
                            (cons 8 poly-layer))))
  
  (if (not ss-poly)
    (progn 
      (princ (strcat "\nNo polylines found on layer '" poly-layer "'."))
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
  
  ;; Process polylines and collect data
  (setq data-list '()
        idx 0
        closed-count 0
        open-count 0)
  (princ (strcat "\nProcessing " (itoa (sslength ss-poly)) " drainage areas..."))
  
  (repeat (sslength ss-poly)
    (setq ent (ssname ss-poly idx)
          obj (vlax-ename->vla-object ent)
          ent-data (entget ent)
          hdl (vla-get-Handle obj)
          is-closed (= (logand (cdr (assoc 70 ent-data)) 1) 1))
    
    (princ (strcat "\nProcessing polyline " (itoa (1+ idx)) " - Handle: " hdl))
    
    (if is-closed
      (progn
        ;; Process closed polyline normally
        (setq area (vla-get-Area obj)
              area-sf area
              area-ac (/ area 43560.0)
              closed-count (1+ closed-count))
        
        (princ (strcat " (CLOSED - Area: " (rtos area-sf 2 2) " sf)"))
        
        ;; Get polyline points for point-in-polygon test
        (setq poly-pts (get-polyline-points ent))
        
        ;; Find matching text label
        (setq label-id (find-text-in-poly poly-pts ss-text))
        
        ;; Add data to list for sorting
        (setq data-list (cons (list label-id hdl poly-layer text-layer area-sf area-ac "CLOSED") data-list)))
      (progn
        ;; Handle open polyline - no area calculation possible
        (setq open-count (1+ open-count))
        (princ " (OPEN - No area)")
        
        ;; Try to find nearby text (using centroid approximation)
        (setq poly-pts (get-polyline-points ent))
        (if poly-pts
          (progn
            ;; Calculate approximate centroid for open polyline
            (setq centroid-x (/ (apply '+ (mapcar 'car poly-pts)) (length poly-pts))
                  centroid-y (/ (apply '+ (mapcar 'cadr poly-pts)) (length poly-pts)))
            ;; For open polylines, we'll search for closest text instead of point-in-polygon
            (setq label-id (find-closest-text (list centroid-x centroid-y) ss-text 50.0))) ; within 50 units
          (setq label-id "NO-GEOMETRY"))
        
        ;; Add data with area marked as not calculable
        (setq data-list (cons (list label-id hdl poly-layer text-layer "N/A" "N/A" "OPEN-NO-AREA") data-list))))
    
    (setq idx (1+ idx)))
  
  ;; Report processing summary
  (princ (strcat "\nFound " (itoa closed-count) " closed polylines and " (itoa open-count) " open polylines."))
  (princ (strcat "\nData list contains " (itoa (length data-list)) " items."))
  
  ;; Sort data by Label ID (ascending)
  (setq data-list (vl-sort data-list '(lambda (a b) (< (car a) (car b)))))
  
  ;; Write sorted data to CSV
  (if (setq file (open fn "w"))
    (progn
      ;; Write CSV header
      (write-line "Handle,Drainage_Layer,Label_Layer,Label_ID,Area_SqFt,Area_Acres,Status" file)
      
      ;; Write sorted data
      (foreach data-row data-list
        (setq area-sf-str (if (numberp (nth 4 data-row)) 
                            (rtos (nth 4 data-row) 2 2) 
                            (nth 4 data-row))
              area-ac-str (if (numberp (nth 5 data-row)) 
                            (rtos (nth 5 data-row) 2 4) 
                            (nth 5 data-row)))
        (write-line
          (strcat (cadr data-row) ","           ; Handle
                  (caddr data-row) ","          ; Poly layer
                  (cadddr data-row) ","         ; Text layer  
                  (car data-row) ","            ; Label ID
                  area-sf-str ","               ; Area SF (or "N/A")
                  area-ac-str ","               ; Area AC (or "N/A")
                  (nth 6 data-row))             ; Status
          file))
      
      (close file)
      (setq file nil)
      
      (princ (strcat "\nSuccess! Exported " 
                     (itoa (sslength ss-poly))
                     " drainage areas to " fn
                     "\nColumns: Handle, Drainage_Layer, Label_Layer, Label_ID, Area_SqFt, Area_Acres, Status"
                     "\n" (itoa closed-count) " closed polylines with areas calculated"
                     "\n" (itoa open-count) " open polylines marked as 'OPEN-NO-AREA'")))
    (princ (strcat "\nError: Cannot create file " fn)))
  
  (princ)
)