#|
ukol 1:
CO-MATRIX-VECTOR-PRODUCT - konkurentni soucit matice a vektoru
|#
(defun co-matrix-vector-product (matrix vector)
  (let ((sync-list (make-list (length matrix) :initial-element nil)))
    (co-dotimes (i (length matrix))
      (let ((parcial-result (reduce #'+ 
                                    (mapcar #'* (nth i matrix) vector))))
        (setf (nth i sync-list) t)
        (await (every (lambda (x) (equal x t)) sync-list))
        (setf (nth i vector) parcial-result))))  
  vector)

#|
ukol 2:
CO-MAPCAR - konkurentni varianta mapcaru
|#
(defun co-mapcar (procedure list)
  (let ((result-list (make-list (length list))))
    (co-dotimes (i (length list))
      (setf (nth i result-list)
            (funcall procedure (nth i list))))
    result-list))

#|
ukol 3:
CO-MATRIX-PRODUCT
TBD
|#


   
  



      
      


