#|
sablona od Mgr. Lastovicky
doplnena implementaci...
|#
(defun producer-consumer-1 ()
  (let* ((data '(9 6 3 5 2 2 6 1 9 8))
         buffer
         end)
    (labels ((produce ()
               (pop data))
             (consume (item)               
               (print item)))
      (co-progn
      ;PRODUCER
        (let (item)          
          (loop until end do              
                (setf item (produce))
                (if (not item)
                    (setf end t)
                  (progn
                    (await (not buffer))
                    (setf buffer item)))))
      ;CONSUPER
        (let (item)
          (loop until (and end (not buffer)) do
                (await (or end buffer))
                (if buffer
                    (progn
                      (setf item buffer)
                      (setf buffer nil)
                      (consume item)))))))))

#|
sablona od Mgr. Lastovicky
Pro testovani spravnosti implementace - vraci t nebo nil
Nakopirovana implementace predchozi metody.
|#
(defun test-producer-consumer-1 ()
  (let* ((data (loop for i below 200 collect (random 10)))
         (data-producer (copy-list data))
         data-consumer
         buffer
         end)
    (labels ((produce ()
               (sleep (/ (random 10) 1000))
               (pop data-producer))
             (consume (item)
               (sleep (/ (random 10) 1000))
               (push-end item data-consumer)))
      (co-progn
      ;PRODUCER
        (let (item)          
          (loop until end do              
                (setf item (produce))
                (if (not item)
                    (setf end t)
                  (progn
                    (await (not buffer))
                    (setf buffer item)))))
      ;CONSUPER
        (let (item)
          (loop until (and end (not buffer)) do
                (await (or end buffer))
                (if buffer
                    (progn
                      (setf item buffer)
                      (setf buffer nil)
                      (consume item)))))))
    (equalp data data-consumer)))

#|
sablona od Mgr. Lastovicky
Implementace producent-konzumer pomoci bufferu, ktery funguje jako cyklicky seznam.
|#
(defun producer-consumer-2 ()
  (let* ((data '(9 6 3 5 2 2 6 1 9 8 1 2 3 4 5 6 7 8 9))
         (buffer '(nil nil nil nil nil))
         end)
    (labels ((produce ()
               (pop data))
             (consume (item)               
               (print item)))
      (co-progn
      ;PRODUCER
        (let (item
              (pointer 0))          
          (loop until end do              
                (setf item (produce))
                (if (not item)
                    (setf end t)
                  (progn
                    (await (not (nth pointer buffer)))
                    (setf (nth pointer buffer) item)
                    (setf pointer (mod (+ pointer 1) 5))))))
      ;CONSUPER
        (let (item
              (pointer 0))
          (loop until (and end (not (nth pointer buffer))) do
                (await (or end (nth pointer buffer)))
                (if (nth pointer buffer)
                    (progn
                      (setf item (nth pointer buffer))
                      (setf (nth pointer buffer) nil)
                      (consume item)
                      (setf pointer (mod (+ pointer 1) 5))))))
        ))))


        