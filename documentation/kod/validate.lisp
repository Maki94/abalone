
(defun find-second-neighbours (ball balls new-balls) ;ball = (B 4); returns ( ((D 1) (D 2)) ((E 2) (F 7)) )
  (cond
    ((null new-balls) nil)
    ((find-ball (car new-balls) balls) (append (list  (list ball (car new-balls))) (find-second-neighbours ball balls (cdr new-balls))))
    (t (find-second-neighbours ball balls (cdr new-balls)))))

(defun all-neighbours2 (single-balls balls); single-balls = ((I 5) (I 6) (H 4) (H 5) (H 6) (G 4) (G 5) (E 3) (E 4) (D 4) (C 5) (C 6) (B 4) (B 5) (B 6) (A 4) (A 5))
 ;"returns ((I 5) (I 6))  ((I 5) (H 4))  ((I 5) (H 5))  ((I 6) (I 5))  ((I 6) (H 5))  ((I 6) (H 6))  ((H 4) (H 5))  ((H 4) (I 5)) ((H 4) (G 4))  ((H 5) (H 6))  ((H 5) (I 6))  ((H 5) (I 5))  ((H 5) (H 4))  ((H 5) (G 4))  ((H 5) (G 5))  ((H 6) (I 6))  ((H 6) (H 5))  ((H 6) (G 5))  ((G 4) (G 5))  ((G 4) (H 5))  ((G 4) (H 4))  ((G 5) (H 6))  ((G 5) (H 5))  ((G 5) (G 4))  ((E 3) (E 4))  ((E 4) (E 3))  ((E 4) (D 4))  ((D 4) (E 4))  ((C 5) (C 6))  ((C 5) (B 4))  ((C 5) (B 5))  ((C 6) (C 5))  ((C 6) (B 5))  ((C 6) (B 6))  ((B 4) (B 5))  ((B 4) (C 5))  ((B 4) (A 4))  ((B 5) (B 6))  ((B 5) (C 6))  ((B 5) (C 5))  ((B 5) (B 4))  ((B 5) (A 4))  ((B 5) (A 5))  ((B 6) (C 6))  ((B 6) (B 5))  ((B 6) (A 5))  ((A 4) (A 5))  ((A 4) (B 5))  ((A 4) (B 4))  ((A 5) (B 6))  ((A 5) (B 5))  ((A 5) (A 4))"
  (if single-balls
    (append (find-second-neighbours (car single-balls) balls (get-all-new-tacka (car single-balls)))
                (all-neighbours2 (cdr single-balls) balls))))

(defun find-third-neighbours (two-ball balls new-balls)
  (cond
    ((null new-balls) nil)
    ((find-ball (car new-balls) balls) (append (list (append two-ball (list (car new-balls)))) (find-third-neighbours  two-ball balls (cdr new-balls))))
    (t (find-third-neighbours two-ball balls (cdr new-balls)))))

(defun get-all-third-new-tacka (ball1 ball2) ; ball1 = (I 6) ball2 = (I 5)
  (cond ((equal (car ball1) (car ball2))
            (list (append  (list (car ball1)) (list (1+ (max (cadr ball1) (cadr ball2)))))
                (append (list (car ball1)) (list (1- (min (cadr ball1) (cadr ball2)))))))
        ((equal (cadr ball1) (cadr ball2))
            (list (append (list (next-char (max-cmp-char (car ball1) (car ball2)))) (cdr ball1))
                (append (list (prev-char (min-cmp-char (car ball1) (car ball2)))) (cdr ball1))))
        (t  (list (append (list (next-char (max-cmp-char (car ball1) (car ball2)))) (list (1+ (max (cadr ball1) (cadr ball2)))))
                (append (list (prev-char (min-cmp-char (car ball1) (car ball2)))) (list (1- (min (cadr ball1) (cadr ball2)))))))))

(defun all-neighbours3 (two-balls balls)
  (if two-balls
    (append (find-third-neighbours (car two-balls) balls (remove-invalid-list (get-all-third-new-tacka (caar two-balls) (cadar two-balls))))
                (all-neighbours3 (cdr two-balls) balls))))

(defun make-command (balls) ; balls = (((D 1) (D 2) (D 3)) .. .(...))
  (if balls
    (append  (all-command-combinations (car balls)) (make-command (cdr balls)))))

(defun new-third-tacka (smer mx mn) ; vraca tacku u slucaju da su stvoreni uslovi da moze da odgurne
  (remove-invalid-list (list (cond
    ((and (equal (car mx) (car mn)) (= smer desno)) (list (car mx) (1+ (cadr mx))))
    ((and (equal (car mx) (car mn)) (= smer levo)) (list (car mx) (1- (cadr mn))))
    ((and (= (cadr mx) (cadr mn)) (= smer gore-levo)) (list (next-char (car mx)) (cadr mx)))
    ((and (= (cadr mx) (cadr mn)) (= smer dole-desno)) (list (prev-char (car mn)) (cadr mn)))
    ((and (not (equal (car mx) (car mn))) (not (= (cadr mx) (cadr mn))) (= smer gore-desno)) (list (next-char (car mx)) (1+ (cadr mx))))
    ((and (not (equal (car mx) (car mn))) (not (= (cadr mx) (cadr mn))) (= smer dole-levo))(list (prev-char (car mn)) (1- (cadr mn))))))))

(defun move-two-without-pushing (stanje mx mn smer)
  (cond
    ((and (equal (car mx) (car mn)) (= smer desno))
      (valid-state-one (state-empty stanje) mx smer))
    ((and (equal (car mx) (car mn)) (= smer levo))
      (valid-state-one (state-empty stanje) mn smer))

    ((and (not (equal (car mx) (car mn))) (not (= (cadr mx) (cadr mn))) (= smer gore-desno))
      (valid-state-one (state-empty stanje) mx smer))
    ((and (not (equal (car mx) (car mn))) (not (= (cadr mx) (cadr mn))) (= smer dole-levo))
      (valid-state-one (state-empty stanje) mn smer))

    ((and (= (cadr mx) (cadr mn)) (= smer gore-levo))
      (valid-state-one (state-empty stanje) mx smer))
    ((and (= (cadr mx) (cadr mn)) (= smer dole-desno))
      (valid-state-one (state-empty stanje) mn smer))

    (t (and (valid-state-one (state-empty stanje) mn smer) (valid-state-one (state-empty stanje) mx smer)))))

(defun move-three-without-pushing (stanje mx mid mn smer)
  (cond
    ((and (equal (car mx) (car mn)) (= smer desno))
      (valid-state-one (state-empty stanje) mx smer))
    ((and (equal (car mx) (car mn)) (= smer levo))
      (valid-state-one (state-empty stanje) mn smer))

    ((and (not (equal (car mx) (car mn))) (not (= (cadr mx) (cadr mn))) (= smer gore-desno))
      (valid-state-one (state-empty stanje) mx smer))
    ((and (not (equal (car mx) (car mn))) (not (= (cadr mx) (cadr mn))) (= smer dole-levo))
      (valid-state-one (state-empty stanje) mn smer))

    ((and (= (cadr mx) (cadr mn)) (= smer gore-levo))
      (valid-state-one (state-empty stanje) mx smer))
    ((and (= (cadr mx) (cadr mn)) (= smer dole-desno))
      (valid-state-one (state-empty stanje) mn smer))

    (t (and (valid-state-one (state-empty stanje) mn smer) (valid-state-one (state-empty stanje) mid smer) (valid-state-one (state-empty stanje) mx smer)))))

(defun valid-state-one (empty tacka smer)
  (let* ((new-tacka (get-new-tacka smer tacka)))
        (cond ((out-of-table new-tacka) t)
              ((find-ball new-tacka empty) t)
              (t nil))))

(defun valid-state-two (stanje mxt mnt smer player) ; tacka1 min, tacka2 max
  (let*(  (opp-balls (player-state stanje (not player)))
          (balls (player-state stanje player))
          (empty (state-empty stanje))
          (new-tacka (car (new-third-tacka smer mxt mnt)))) ; imace vrednost samo ako je slucaj da moze da gurne
          (cond
            ((null new-tacka) (move-two-without-pushing stanje mxt mnt smer)); tada ne moze da odgurne
            ((find-ball new-tacka balls) nil) ; ako je sledeca tacka iste boje, onda  ne moze da izvrsi taj potez
            ((find-ball new-tacka empty) t) ; ako je sledeca tacka empty, onda moze da izvrsi taj potez
            (t (let* ((next-tacka (car (new-third-tacka smer (max-tacka new-tacka mxt) (min-tacka new-tacka mnt)))))
                  (cond
                    ;((not (find-ball new-tacka opp-balls)) t)
                    ((null next-tacka) t) ; onda moze da je odgurne, obuhvata slucaj i da izgura sa terena
                    ((and (find-ball new-tacka opp-balls) (not (find-ball next-tacka balls)) (not (find-ball next-tacka opp-balls))) t)
                    (t nil)))))))

(defun valid-state-three (stanje t1 t2 t3 smer player)
  (let*(  (opp-balls (player-state stanje (not player)))
          (balls (player-state stanje player))
          (empty (state-empty stanje))
          (new-tacka (car (new-third-tacka smer t1 t3)))) ; imace vrednost u slucaju da moze da gurne
          (cond
              ((null new-tacka) (move-three-without-pushing stanje t1 t2 t3 smer)) ; tada ne moze da odgurne i poziva se odgovarajuca funkcija
              ((find-ball new-tacka balls) nil) ; ako moze da gura i sledeca tacka je iste boje, onda potez nije validan
              ((find-ball new-tacka empty) t) ; ako moze da gura i sledeca tacka je prazna, ona se moze pomeriti u tom pravcu
              (t (let* ((next-tacka (car (new-third-tacka smer (max-tacka new-tacka t1) (min-tacka new-tacka t3))))) ; slucaj kada je sledeca tacka suprotne boje
                    (cond
                      ((null next-tacka) t) ; slucaj obuhvata kada moze da izgura sa terena jednu lopticu
                      ((find-ball next-tacka balls) nil) ; ako je iza protivnicke loptice nasa loptica tada ne moze da odgurne
                      ((find-ball next-tacka empty) t) ; ako je prazna iza nje onda moze da odgurne bez problema
                      (t (let* ((next-next-tacka (car (new-third-tacka smer (max-tacka t1 next-tacka) (min-tacka t3 next-tacka)))))
                            (cond
                              ((null next-next-tacka) t)
                              ((find-ball next-next-tacka balls) nil)
                              ((find-ball next-next-tacka empty) t)
                              (t nil))))))))))

(defun try-set (stanje command player) ; command = (((D 1) (D 2)) desno)
  (let* ( (p1 (caar command))
          (p2 (cadar command))
          (p3 (caddar command))
          (direction (cadr command)))
          (cond
                (p3 (valid-state-three stanje p1 p2 p3 direction player)) 
                (p2 (valid-state-two stanje p1 p2 direction player))
                (p1 (valid-state-one (state-empty stanje) p1 direction)))))

(defun valid-commands (stanje command player)
  (if command
    (append (if (try-set stanje (car command) player) (list (car command)) nil)
             (valid-commands stanje (cdr command) player))))

 ;(setq test2 '((((G 6) (G 5) (G 4)) 6) (((G 6) (G 5) (G 4)) 1) (((G 6) (G 5) (G 4)) 4) ))
 ;(valid-cmds2 (valid-commands stanje test2 player))
