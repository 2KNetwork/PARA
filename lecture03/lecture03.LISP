#|
sablona od Mgr. Lastovicky
doplnena zakladni implementace...

Aplikovana myslenka je velice jednoducha:

Producer produkuje o sve pomocne pameti cisla - ve chvili, kdy uz nic
nevyprodukuje, nastavi end na t a tim jeho hlavni cyklus konci, 
pokud ale vyprodukuje neco jineho nez nil, pocka az bude prazdnu buffer a ulozi.

Consumer a jeho cyklus probihaji dokud neni nastaveno end na t a take dokud neni
prazny buffer - to proto, ze i kdyz uz je nastaveno end na t, porad jeste muze byt 
v bufferu hodnota a tu je potreba pozrat. Pote ceka dokud neni neco v buffer, 
hodnotu si nacte do lokalni pameti, smaze buffer a zpracuje hodnotu v lokalni
pameti.
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
      ;CONSUMER
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
      ;CONSUMER
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
K synchronizaci se vyuziva nil hodnota seznamu - verze jako na prednace.

Toto reseni je pouze rozsirenim predchoziho... Pouziva se buffer jako list 5 
hodnot, nikoliv pouze 1. Kazdy proces ma vlastni promennou ukazatel, ktera ukazuje,
na ktery prvek bufferu ten ktery proces zrovna ukazuje. Veskere cteni a zapis do 
bufferu je potreba prepsat na cteni a zapis n-te polozky bufferu dle ukazatele 
pointer. Protoze je buffer (pochopitelne) konecny, je potreba pri posouvani 
polohy bufferu (zvysovani hodnoty) aplikovat na novou hodnotu funkci
modulo (velikost bufferu). Tim zajistime prechod z konce bufferu zpet na zacatek.
Zbyly princip ale odpovida predchozimu reseni.
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
      ;CONSUMER
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

#|
sablona od Mgr. Lastovicky
Implementace producent-konzumer pomoci bufferu, ktery funguje jako cyklicky seznam.
K synchronizaci se vyuziva poloha ukazatelu do bufferu.

Myslenka: 
Producent provadi cyklus, dokud neni end nastaveno na t. end je na t
nastaveno pokud dojdou producentovi data. Pokud je hodnota ukazatele, ktery je 
vyuzivan producentem (top) + 1 rovna ukazateli bottom, respektive
(= (mod (+ top 1) 5) bottom), pak je buffer plny a producent ceka az toto
nebude. 

Consumer provadi cyklus dokud neni end nastaveno na t a dokud zaroven neni ukazatel
bottom roven ukazateli top (to je znak toho, ze buffer je prazdny). Consumer
ceka, naopak od producenta, ve chvili kdy je buffer prazdny, tedy neni co cist.To 
nastane ve chvili, kdy jsou ukazatele bottom a top rovny.
|#

(defun producer-consumer-3 ()
  (let* ((data '(9 6 3 5 2 2 6 1 9 8 1 2 3 4 5 6 7 8 9))
         (buffer '(nil nil nil nil nil))
         end
         (top 0)
         (bottom 0))
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
                    (await  (not (= (mod (+ top 1) 5) bottom)))                   
                    (setf (nth top buffer) item)
                    (setf top (mod (+ top 1) 5))))))
      ;CONSUMER
        (let (item)
          (loop until (and end (eql top bottom)) do
                (await (or (not (eql top bottom)) end))
                (if (not (= bottom top))
                    (progn
                      (setf item (nth bottom buffer))                     
                      (consume item)
                      (setf bottom (mod (+ bottom 1) 5))))))
        ))))
