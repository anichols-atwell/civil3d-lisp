(defun c:BASINAREAS-CSV ( / *error* contour-layer text-layer ss-contour ss-text 
                           fn file idx ent obj area hdl elevation sel-result
                           ent-data sel-ent basin-id area-sf data-list 
                           closed-count open-count volume-data)

  (defun *error* (msg)
    (if file (close file))
    (princ (strcat "\nError: " msg))
    (princ)
  )

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

  (defun get-polyline-points-2d (ent / pts coord)
    (setq pts '())
    (foreach coord (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) (entget ent)))
      (setq pts (append pts (list (list (car coord) (cadr coord))))))
    pts
  )

  (defun get-polyline-elevation (ent / obj elev)
    (setq obj (vlax-ename->vla-object ent))
    (if obj
      (progn
        (setq elev (vla-get-Elevation obj))
        (if elev elev 0.0))
      0.0)
  )

  (defun find-text-in-poly (poly-pts ss-text / i text-ent text-pt text-str result)
    (setq result ""
          i 0)
    (if ss-text
      (repeat (sslength ss-text)
        (setq text-ent (ssname ss-text i))
        (setq text-pt (list (car (cdr (assoc 10 (entget text-ent))))
                           (cadr (cdr (assoc 10 (entget text-ent))))))
        (if (point-in-polygon text-pt poly-pts)
          (progn
            (setq text-str (cdr (assoc 1 (entget text-ent))))
            (if (= result "")
              (setq result text-str)
              (setq result (strcat result "-" text-str)))))
        (setq i (1+ i))))
    (if (= result "") "UNLABELED" result)
  )

  (defun find-closest-text (pt ss-text max-dist / i text-ent text-pt dist min-dist closest-text)
    (setq min-dist 1e6
          closest-text ""
          i 0)
    (if ss-text
      (repeat (sslength ss-text)
        (setq text-ent (ssname ss-text i)
              text-pt (list (car (cdr (assoc 10 (entget text-ent))))
                           (cadr (cdr (assoc 10 (entget text-ent)))))
              dist (distance pt text-pt))
        (if (and (< dist min-dist) (< dist max-dist))
          (progn
            (setq min-dist dist
                  closest-text (cdr (assoc 1 (entget text-ent))))))
        (setq i (1+ i))))
    (if (= closest-text "") "UNLABELED" closest-text)
  )

  (vl-load-com)

  (setq sel-result (entsel "Select polyline contour from RETENTION BASIN layer: "))
  (if (not sel-result)
    (progn (princ "\nSelection cancelled.") (princ) (exit)))

  (setq sel-ent (car sel-result)
        contour-layer (cdr (assoc 8 (entget sel-ent))))

  (princ (strcat "\nUsing contour layer: " contour-layer))

  (setq ent-data (entget sel-ent))
  (if (not (= (cdr (assoc 0 ent-data)) "LWPOLYLINE"))
    (progn 
      (princ "\nError: Selected object is not a polyline.")
      (princ) (exit)))

  (if (= (logand (cdr (assoc 70 ent-data)) 1) 1)
    (princ "\nSelected contour is closed - good!")
    (princ "\nWarning: Selected contour is not closed, but continuing..."))

  (setq sel-result (entsel "Select text label for BASIN ID (Basin A, Basin B, etc.): "))
  (if (not sel-result)
    (progn (princ "\nSelection cancelled.") (princ) (exit)))

  (setq sel-ent (car sel-result)
        text-layer (cdr (assoc 8 (entget sel-ent))))

  (princ (strcat "\nUsing basin labels layer: " text-layer))

  (setq ent-data (entget sel-ent))
  (if (not (or (= (cdr (assoc 0 ent-data)) "TEXT") (= (cdr (assoc 0 ent-data)) "MTEXT")))
    (progn 
      (princ "\nError: Selected object is not a text entity.")
      (princ) (exit)))

  (setq fn (getfiled "CSV file to create" (strcat contour-layer "_retention_basins.csv") "csv" 1))
  (if (not fn)
    (progn (princ "\nFile selection cancelled.") (princ) (exit)))

  (setq ss-contour (ssget "_X" (list '(0 . "LWPOLYLINE") (cons 8 contour-layer))))

  (if (not ss-contour)
    (progn 
      (princ (strcat "\nNo polylines found on layer '" contour-layer "'."))
      (princ) (exit)))

  (setq ss-text (ssget "_X" (list '(-4 . "<OR") '(0 . "TEXT") '(0 . "MTEXT") '(-4 . "OR>") (cons 8 text-layer))))

  (if (not ss-text)
    (princ (strcat "\nWarning: No text entities found on layer '" text-layer "'.")))

  (setq data-list '() idx 0 closed-count 0 open-count 0)
  (princ (strcat "\nProcessing " (itoa (sslength ss-contour)) " contour polylines..."))

  (repeat (sslength ss-contour)
    (setq ent (ssname ss-contour idx)
          obj (vlax-ename->vla-object ent)
          ent-data (entget ent)
          hdl (vla-get-Handle obj)
          elevation (get-polyline-elevation ent))

    (princ (strcat "\nProcessing contour " (itoa (1+ idx)) " - Handle: " hdl))

    (if (and elevation (/= elevation 0.0))
      (setq elev-str (rtos elevation 2 2))
      (setq elev-str "N/A"))

    (if (= (logand (cdr (assoc 70 ent-data)) 1) 1)
      (progn
        (setq area (vla-get-Area obj)
              area-sf area
              closed-count (1+ closed-count))

        (princ (strcat " (CLOSED - Area: " (rtos area-sf 2 2) " sf, Elev: " elev-str ")"))

        (setq poly-pts (get-polyline-points-2d ent))
        (setq basin-id (find-text-in-poly poly-pts ss-text))
        (setq data-list (cons (list basin-id elev-str area-sf hdl contour-layer) data-list)))
      (progn
        (setq open-count (1+ open-count))
        (princ (strcat " (OPEN - No area, Elev: " elev-str ")"))

        (setq poly-pts (get-polyline-points-2d ent))
        (if poly-pts
          (progn
            (setq centroid-x (/ (apply '+ (mapcar 'car poly-pts)) (length poly-pts))
                  centroid-y (/ (apply '+ (mapcar 'cadr poly-pts)) (length poly-pts)))
            (setq basin-id (find-closest-text (list centroid-x centroid-y) ss-text 50.0)))
          (setq basin-id "NO-GEOMETRY"))

        (setq data-list (cons (list basin-id elev-str "N/A" hdl contour-layer) data-list))))

    (setq idx (1+ idx)))

  (princ (strcat "\nFound " (itoa closed-count) " closed contours and " (itoa open-count) " open contours."))
  (princ (strcat "\nData list contains " (itoa (length data-list)) " items."))

  (setq data-list (vl-sort data-list 
    '(lambda (a b) 
       (if (= (car a) (car b))
         (if (and (/= (cadr a) "N/A") (/= (cadr b) "N/A"))
           (< (atof (cadr a)) (atof (cadr b)))
           (< (cadr a) (cadr b)))
         (< (car a) (car b))))))

  (princ "\nCalculating volumes using average-end-area method...")

  ;; Simple basin volume calculation function
  (defun calculate-basin-volumes (basin-list / result cumul-vol i current prev depth incr-vol cumul-af)
    (setq result '()
          cumul-vol 0.0
          i 0)
    (repeat (length basin-list)
      (setq current (nth i basin-list))
      (if (= i 0)
        ;; First contour - no volume
        (setq result (append result (list (list (cadddr current) (nth 4 current) (car current) (cadr current) (caddr current) "N/A" "N/A" "N/A" "N/A"))))
        ;; Calculate volume
        (progn
          (setq prev (nth (1- i) basin-list))
          (if (and (numberp (caddr current)) (numberp (caddr prev)) (/= (cadr current) "N/A") (/= (cadr prev) "N/A"))
            (progn
              (setq depth (- (atof (cadr current)) (atof (cadr prev)))
                    incr-vol (* (/ (+ (caddr prev) (caddr current)) 2.0) depth)
                    cumul-vol (+ cumul-vol incr-vol)
                    cumul-af (/ cumul-vol 43560.0))
              (setq result (append result (list (list (cadddr current) (nth 4 current) (car current) (cadr current) (caddr current) (rtos depth 2 2) (rtos incr-vol 2 2) (rtos cumul-vol 2 2) (rtos cumul-af 2 4))))))
            (setq result (append result (list (list (cadddr current) (nth 4 current) (car current) (cadr current) (caddr current) "N/A" "N/A" "N/A" "N/A")))))))
      (setq i (1+ i)))
    result
  )

  ;; Simple volume calculation without complex functions
  (setq volume-data '())
  (setq current-basin ""
        basin-contours '())

  ;; Group by basin
  (foreach data-row data-list
    (if (= (car data-row) current-basin)
      (setq basin-contours (append basin-contours (list data-row)))
      (progn
        (if (/= current-basin "")
          (setq volume-data (append volume-data (calculate-basin-volumes basin-contours))))
        (setq current-basin (car data-row)
              basin-contours (list data-row)))))

  ;; Process last basin
  (if (/= current-basin "")
    (setq volume-data (append volume-data (calculate-basin-volumes basin-contours))))

  (if (setq file (open fn "w"))
    (progn
      (write-line "Handle,Layer,Basin_ID,Elevation,Area_SqFt,Depth,Incremental_Vol_CF,Cumulative_Vol_CF,Cumulative_Vol_AF" file)

      (foreach vol-row volume-data
        (write-line
          (strcat (car vol-row) ","
                  (cadr vol-row) ","
                  (caddr vol-row) ","
                  (cadddr vol-row) ","
                  (if (numberp (nth 4 vol-row)) (rtos (nth 4 vol-row) 2 2) (nth 4 vol-row)) ","
                  (nth 5 vol-row) ","
                  (nth 6 vol-row) ","
                  (nth 7 vol-row) ","
                  (nth 8 vol-row))
          file))

      (close file)
      (setq file nil)

      (princ (strcat "\nSuccess! Exported " 
                     (itoa (sslength ss-contour))
                     " contours with volume calculations to " fn
                     "\nColumns: Handle, Layer, Basin_ID, Elevation, Area_SqFt, Depth, Incremental_Vol_CF, Cumulative_Vol_CF, Cumulative_Vol_AF"
                     "\n" (itoa closed-count) " closed contours with areas calculated"
                     "\n" (itoa open-count) " open contours marked as 'N/A' area"
                     "\nVolumes calculated using average-end-area method")))
    (princ (strcat "\nError: Cannot create file " fn)))

  (princ)
)