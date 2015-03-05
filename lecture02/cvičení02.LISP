#|
Implementovany scenar poc10:
Oba procesy nactou aktualni hodnotu sdilene promenne n.
Kdyz tak ucini, navzajem na sebe cekaji, ze uz nacetli.
Oba procesy signalizuji pomoci loaded flajek, ze nacetly.
Oba procesy svou hodnotu o 1 zvetsi a ulozi ji do sdilene promenne.
Oba procesy na sebe navzajem cekaji, dokud oba nezahlasi, ze uz zapsali.
Proces 1 vynuluje vlajky procesu 2.
Proces 2 vynuluje vlajky procesu 1.
Protoze proces 1 ceka, az bude mit proces 2 nulove vlajky a protoze je to prave
     proces 1, ktery je nuluje, s jistotou muzeme prohlasit, ze proces 1 nikdy nebude
     pracovat se starou hodnotou vlajek procesu 2, protoze je to on sam, kdo  
     se stara o jejich smazani. Stejne to plati i naopak.
|#
(defun poc10 ()
  (let ((n 0)
        (first-loaded)
        (first-wrote)
        (second-loaded)
        (second-wrote))
    (co-progn
      (let (tmp)
        (dotimes (i 10)
          (setf tmp n)
          (setf first-loaded t)
          (await second-loaded)
          (setf n (+ tmp 1))
          (setf first-wrote t)
          (await second-wrote)
          (setf second-loaded nil)
          (setf second-wrote nil)))          
      (let (tmp)
        (dotimes (i 10)
          (setf tmp n)
          (setf second-loaded t)
          (await first-loaded)
          (setf n (+ tmp 1))
          (setf second-wrote t)
          (await first-wrote)
          (setf first-loaded nil)
          (setf first-wrote nil))))
    n))

#|
Implementovany scenar poc2:
Oba procesy nactou 0.
Druhy proces ceka, dokud prvni proces neohlasi fffp.
Prvni proces se v predposlednim cyklu zastavi, ohlasi fffp a ceka az druhy proces
     ohlasi sw1 - az zapise 1.
Druhy proces zapise 1, ohlasi zapis hodnoty pomoci sw1 a ceka az prvni proces hodnotu
     nacte a ohlasito pomoci fl1.
Prvni proces nacte 1, ohlasi fl1 a ceka az druhy proces dokonci praci - sfa.
Druhy proces iteruje az do posledniho cyklu, kdy poslednim radkem ohlasi, ze skoncil
     pomoci sfa.
Prvni vlakno zapise 2 a konci.
|#
(defun poc2 ()
  (let ((n 0)
        fffp ; first finished first part  
        fl1 ; first loaded 1
        sfa ; second finished all
        sl0 ; second loaded 0
        sw1) ; second wrote 1
    (co-progn
      (let (tmp)
        (dotimes (i 10)
          (await sl0)
          (setf tmp n) 
          (if (= i 9)
              (and (setf fl1 t)
                   (await sfa)))                     
          (setf n (+ tmp 1))
          (if (= i 8)
              (and (setf fffp t)
                   (await sw1)))))         
      (let (tmp)
        (dotimes (i 10)          
          (setf tmp n) 
          (setf sl0 t)
          (await fffp)
          (setf n (+ tmp 1))
          (setf sw1 t)
          (if (= i 0)
              (await fl1))
          (if (= i 9)              
              (setf sfa t)))))
    n))


             
             
             