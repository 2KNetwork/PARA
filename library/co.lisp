;; -*- mode: lisp; encoding: utf-8; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; co.lisp
;;;
;;; Jednoduchá knihovna pro studium paralelního programování. Slouží pouze ke
;;; cvičným účelům (není efektivní)
;;;

#|
Popis:
-----

Makro await
syntax: await form

Aktivní čekání. Periodicky vyhodnocuje form, dokud není výsledek true.

Příklad:

(await (= x 0))

------------------------------------------------------------------------------

Makro co-progn
syntax: co-progn &body body

Vyhodnotí paralelně (ve více procesech) výrazy v body. Vrátí hodnotu posled-
ního z nich. Než skončí, počká, až všechny vytvořené procesy skončí. Procesy
lze synchronizovat pomocí lokální funkce barrier, která implementuje bariéru.

Příklad:

(co-progn 
  (progn (sleep (random 5))
    (print 1)
    (barrier)
    (print 2))
  (progn (sleep (random 5))
    (print 3)
    (barrier)
    (print 4)))

Vytiskne čísla 1 a 3 (v náhodném pořadí) a pak čísla 2 a 4 (opět v náhodném
pořadí). Vrátí 4.

------------------------------------------------------------------------------

Makro co-dotimes
syntax co-dotimes (var form &optional result) &body body

Funguje podobně jako makro dotimes, ale jednotlivé iterace vyhodnocuje para-
lelně v různých procesech. Než skončí, počká, až všechny procesy skončí.
Procesy lze synchronizovat pomocí lokální funkce barrier, která implementuje 
bariéru.

Příklad:

(co-dotimes (x 10 'hotovo)
  (sleep (random 4.0))
  (print x)
  (barrier)
  (sleep (random 4.0))
  (print x))

Vytiskne v náhodném pořadí čísla od 1 do 9 a potom znovu. Jako výsledek vrátí
symbol hotovo.

------------------------------------------------------------------------------

Funkce make-lock
zjednodušená syntax: make-lock &key name

Vytvoří zámek (případně pojmenovaný name) k použití pro kritickou sekci.

------------------------------------------------------------------------------

Makro with-lock
zjednodušená syntax: with-lock (lock) &body body

Vyhodnotí tělo body v kritické sekci pro zámek lock.

------------------------------------------------------------------------------

Funkce make-semaphore
syntax: make-semaphore &key name count

Vytvoří nový semafor. Nepovinný parametr name lze použít k pojmenování 
semaforu, parametr count k nastavení počáteční hodnoty (default je 1)

------------------------------------------------------------------------------

Funkce semaphore-signal
syntax: semaphore-signal sem

Zvýší hodnotu semaforu sem o 1

------------------------------------------------------------------------------

Funkce semaphore-wait
syntax: semaphore-wait sem &key reason

Sníží hodnotu semaforu sem o 1, případně počká, až je to možné. V proměnné 
reason lze zadat řetězec, popisující důvod čekání (objeví se v prohlížeči
procesů)

|#


(pushnew '(*standard-output* . *standard-output*) 
         mp:*process-initial-bindings*
         :key 'car)

(import '(mp:MAKE-LOCK mp:WITH-LOCK #|mp:make-semaphore|#))

(defmacro await (form)
  "Aktivní čekaní: opakovaně se vyhodnocuje form, dokud nevrátí t"
  `(loop until ,form))

(defmacro loop-while (condition &body body)
  (with-unique-names (res)
    `(loop while ,condition
           for ,res = (progn ,@body)
           finally return ,res)))

#|
(let ((x 10))
  (loop-while (plusp x)
    (print x)
    (decf x)))

(let ((x 10))
  (loop-while (plusp x)
    (print x)
    (decf x)
    nil))
|#

(defmacro loop-until (condition &body body)
  (with-unique-names (res)
    `(loop until ,condition
           for ,res = (progn ,@body)
           finally return ,res)))

#|
(let ((x 10))
  (loop-until (minusp x)
    (print x)
    (decf x)))
|#

(defun prepare-co-progn-body (body result-sym)
  (when body
    (let ((rev (reverse body)))
      (reverse (cons `(setf ,result-sym ,(car rev))
                     (cdr rev))))))

#|
(prepare-co-progn-body '(a b c d) 'res)
|#

(defmacro co-progn (&body body)
  (with-unique-names (end-barrier barrier result)
    `(let ((,end-barrier  (mp:make-barrier ,(1+ (length body))))
           (,barrier  (mp:make-barrier ,(length body)))
           ,result)
       (flet ((barrier () (mp:barrier-wait ,barrier)))
         (let ((processes (list
                           ,@(mapcar (lambda (expr proc-ind)
                                       `(mp:process-run-function (format nil "co-progn process ~s" ,proc-ind)
                                                                 '()
                                                                 (lambda () 
                                                                   (unwind-protect 
                                                                       ,expr 
                                                                     (mp:barrier-wait ,end-barrier)))))
                                     (prepare-co-progn-body body result)
                                     (loop for x below (length body)
                                           collect x)))))
           (unwind-protect 
               (progn (mp:barrier-wait ,end-barrier)
                 ,result)
             (dolist (p processes)
               (mp:process-kill p))))))))

(defmacro co-dotimes ((var count-form &optional result) &body body)
  (with-unique-names (barrier end-barrier count)
    `(let* ((,count ,count-form)
            (,end-barrier (mp:make-barrier (1+ ,count)))
            (,barrier (mp:make-barrier ,count))
            (fun (lambda (,var)
                   (declare (ignorable ,var))
                   (unwind-protect
                       (flet ((barrier () (mp:barrier-wait ,barrier)))
                         ,@body)
                     (mp:barrier-wait ,end-barrier)))))
       (let (processes)
         (dotimes (x ,count)
           (push (mp:process-run-function (format nil "co-dotimes process ~s" x) '() fun x) processes))
         (unwind-protect
             (progn
               (mp:barrier-wait ,end-barrier)
               ,result)
           (dolist (p processes)
             (mp:process-kill p)))))))

#|
(defun test ()
  (let ((x 0))
    (co-dotimes (p 10 x)
      (dotimes (i 1000000)
        (incf x)))))

(co-progn 
  (progn (sleep (random 5))
    (print 1)
    (print 2))
  (progn (sleep (random 5))
    (print 3)
    (print 4)))

(co-progn 
  (progn (sleep (random 5))
    (print 1)
    (barrier)
    (print 2))
  (progn (sleep (random 5))
    (print 3)
    (barrier)
    (print 4)))

(defun test ()
  (co-progn 
    (progn (sleep (random 5))
      (print 1)
      (barrier)
      (print 2))
    (progn (sleep (random 5))
      (print 3)
      (barrier)
      (print 4))))

(co-dotimes (x 10 'hotovo)
  (sleep (random 10.0))
  (print x))

(co-dotimes (x 20)
  (sleep 1)
  (dotimes (y 10) 
    (print "Dobrý den")))

(co-dotimes (x 10 'hotovo)
  (sleep (random 4.0))
  (print x)
  (barrier)
  (sleep (random 4.0))
  (print x))
|#

#|
;; Semafory v LW jsou špatně
(defun semaphore-signal (sem)
  (mp:semaphore-release sem))

(defun semaphore-wait (sem &key reason)
  (mp:semaphore-acquire sem :wait-reason reason))
|#

;; Primitivní náhražka s aktivním čekáním
(defclass semaphore ()
 ((%value :initarg :init-value :initform 0)
  (%p-lock :initform (make-lock))
  (%value-lock :initform (make-lock))))

(defun make-semaphore (&key (count 1))
 (make-instance 'semaphore :init-value count))

(defmethod p ((semaphore semaphore)
             &key (reason (format nil "Waiting for semaphore ~s" semaphore)))
 (with-lock ((slot-value semaphore '%p-lock) :reason reason)
   (mp:process-wait reason (lambda () (plusp (slot-value semaphore '%value))))
   (with-lock ((slot-value semaphore '%value-lock) :reason reason)
     (decf (slot-value semaphore '%value))))
 nil)

(defmethod v ((semaphore semaphore) 
             &key (reason (format nil "Waiting for semaphore ~s" semaphore)))
 (with-lock ((slot-value semaphore '%value-lock) :reason reason)
   (incf (slot-value semaphore '%value)))
 nil)

(defun semaphore-signal (sem)
  (v sem))

(defun semaphore-wait (sem &key reason)
  (p sem :reason reason))
