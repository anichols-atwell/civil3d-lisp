;;;  LayerLenExcel.lsp  —  sum line & polyline lengths on one layer
;;;  and optionally export to Excel.
;;;
;;;  Usage:
;;;    1. APPLOAD the file.
;;;    2. Type  LAYERLEN  ↵
;;;    3. Follow prompts.

(vl-load-com)

(defun put-cell (ws r c v)      ; helper: write cell
  (vlax-put-property (vlax-get-property ws 'Cells r c) 'Value2 v))

(defun c:LayerLen (/ layname ent selset total count i e len
                      reply excelPath app wb ws cols)

  ;; ——— choose layer ———
  (setq layname (getstring T "\nEnter layer name <pick>: "))
  (if (or (null layname) (= layname ""))
    (progn (prompt "\nSelect an object on the layer…")
           (setq ent (entsel))
           (if ent (setq layname (cdr (assoc 8 (entget (car ent))))))))

  ;; ——— gather lengths ———
  (if layname
    (progn
      (setq selset (ssget "x"
                          (list (cons 0 "LINE,LWPOLYLINE,POLYLINE")
                                (cons 8 layname))))
      (setq total 0.0 count 0)
      (if selset
        (progn
          (setq count (sslength selset) i 0)
          (repeat count
            (setq e (ssname selset i)
                  len (vlax-curve-getdistatparam e (vlax-curve-getendparam e))
                  total (+ total len)
                  i (1+ i)))
          (prompt (strcat "\nTotal length on layer \"" layname "\": "
                          (rtos total 2 2) " drawing units ("
                          (itoa count) " entities)."))

          ;; ——— export? ———
          (initget "Yes No")
          (setq reply (getkword "\nCreate Excel file? [Yes/No] <No>: "))
          (if (or (null reply) (= reply "")) (setq reply "No"))

          (if (= (substr (strcase reply) 1 1) "Y")
            (progn
              (setq excelPath (getfiled "Save Excel File As"
                                        "LayerLengths.xlsx" "xlsx" 1))
              (if excelPath
                (progn
                  ;; Excel automation — every method uses arg-list style
                  (setq app (vlax-create-object "Excel.Application"))
                  (vlax-put-property app 'Visible :vlax-false)

                  (setq wb (vlax-invoke-method
                             (vlax-get-property app 'Workbooks)
                             'Add '()))
                  (setq ws (vlax-get-property wb 'ActiveSheet))

                  (put-cell ws 1 1 "Layer Name")
                  (put-cell ws 1 2 "Entity Count")
                  (put-cell ws 1 3 "Total Length (dwg units)")

                  (put-cell ws 2 1 layname)
                  (put-cell ws 2 2 count)
                  (put-cell ws 2 3 total)

                  (setq cols (vlax-get-property ws 'Columns))
                  (vlax-invoke-method cols 'AutoFit '())

                  ;; Save (51 = .xlsx)
                  (vlax-invoke-method wb 'SaveAs (list excelPath 51))
                  (vlax-invoke-method wb 'Close (list 0))
                  (vlax-invoke-method app 'Quit '())

                  (foreach o (list cols ws wb app)
                    (if (vlax-objectp o) (vlax-release-object o)))

                  (prompt (strcat "\nExcel file saved: " excelPath))
                )))))
        (prompt "\nNo LINE/LWPOLYLINE entities found on that layer.")))
    (prompt "\nNo layer specified."))
  (princ))
