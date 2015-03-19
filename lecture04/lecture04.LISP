#|
Lze nahradit pomoci count-if, ktera bere lambda vyraz a list na ktery se aplikuje
|#
(defun count-equal (list element)
  (apply #'+ (mapcar (lambda (x) (if (= element x)
                           1
                         0))
                    list)))

(defun count-less (list element)
  (apply #'+ (mapcar (lambda (x) (if (> element x)
                           1
                         0))
                    list)))
#|
Lze nahradit pomoci subseq, ktera prijma list index, nebo lixt index1 index2 kde index je pocatecni index, kterym ma podlist zacinat, index1 je pocatecni index a index2 je index, kterym podlist konci.
|#
(defun sub-list (lst rightStop)
  (let ((result nil)
        (index 0))
    (dolist (elem lst)
      (if (< index rightStop)
          (progn 
            (setf result (append result (list elem)))
            (setf index (+ index 1)))))
    result))

(defun arg-odevzdano ()
  (let* ((C '(2 4 5 4 6 8 8 8 8 8 8 7 9))
         (D (make-list (length C))))
    (co-dotimes (i (length C))
      (let (k
            count-less
            count-equal
            count-equal-left)
        (setf k (nth i C))
        (setf count-equal-left (count-equal (sub-list C i) k))
        (setf count-equal (count-equal C k))
        (setf count-less (count-less C k))        
        (if (= count-equal 0)
            (setf (nth count-less D) k) 
          (setf (nth (+ count-less count-equal-left) D) k))))
    D))

#|
Verze po face-liftu odstrenenim volani vlastnich metod - pouzitim metod poskytovanych v lispu, odstranenim zbytecneho if, ktery zustal z prvniho navrhu reseni a pak tam "nevadil" a odstranenim zbytecneho ukladani mezivysledku do pameti.
|#
(defun arg-verze2 ()
  (let* ((C '(2 4 5 4 6 8 8 8 8 8 8 7 9))
         (D (make-list (length C))))
    (co-dotimes (i (length C))
      (let (k)
        (setf k (nth i C)) 
          (setf (nth (+ (count-if (lambda (x) (< x k)) C)
                        (count-if (lambda (x) (= x k)) (subseq C 0 i))) D) k)))
    D))
 