(defun get-new-tacka (smer tacka)
  (cond   ((= desno smer)			(cons  (car tacka) (list (1+ (cadr tacka)))))
          ((= gore-desno smer)(cons  (next-char (car tacka)) (list (1+ (cadr tacka)))))
          ((= gore-levo smer)	(cons  (next-char (car tacka)) (list (cadr tacka))))
          ((= levo smer)			(cons  (car tacka)(list (1- (cadr tacka)))))
          ((= dole-levo smer)	(cons  (prev-char (car tacka)) (list (1- (cadr tacka)))))
          ((= dole-desno smer)(cons  (prev-char (car tacka)) (list (cadr tacka))))))

(defun move-state-one (stanje tacka player smer)
  (let*((new-tacka (get-new-tacka smer tacka))
        (opp-balls (player-state stanje (not player)))
        (balls (player-state stanje player))
  			(empty (state-empty stanje)))

        (if player
          (get-state
             (add-point (remove-point balls tacka) new-tacka)
             opp-balls
             (add-point (remove-point empty new-tacka) tacka))
          (get-state
              balls
              (add-point (remove-point opp-balls tacka) new-tacka)
              (add-point (remove-point empty new-tacka) tacka)))))

(defun move-state-two (stanje tacka1 tacka2 player smer)
  (if (and (equal tacka1 tacka2) (or (equal smer desno) (equal smer levo)))
    (move-state-one (move-state-one stanje tacka1 player smer) tacka2 player smer)
    (let*((new-tacka (get-new-tacka smer tacka2))
          (white (state-white stanje))
          (black (state-black stanje))
          (removed-black (remove-point black new-tacka))
          (removed-white (remove-point white new-tacka)))

          (move-state-one (move-state-one
            (if (and (equal removed-black black) (equal removed-white white))
                stanje
                (move-state-one stanje new-tacka (not player) smer))
            tacka2 player smer) tacka1 player smer))))

(defun move-state-three (stanje tacka1 tacka2 tacka3 player smer)
  (if (and (equal3 tacka1 tacka2 tacka3) (or (equal smer desno) (equal smer levo)))
    (move-state-one (move-state-one (move-state-one stanje tacka3 player smer) stanje tacka2 player smer) tacka1 player smer)
    (let*((new-tacka1 (get-new-tacka smer tacka3))
          (new-tacka2 (get-new-tacka smer new-tacka1))
          (white (state-white stanje))
          (black (state-black stanje))
          (removed-black (remove-point (remove-point black new-tacka1) new-tacka2))
          (removed-white (remove-point (remove-point white new-tacka1) new-tacka2)))

          (move-state-one (move-state-one (move-state-one
            (if (and (equal removed-black black) (equal removed-white white))
                stanje
                (move-state-one (move-state-one stanje new-tacka2 (not player) smer)
                  new-tacka1 (not player) smer))
            tacka3 player smer) tacka2 player smer) tacka1 player smer))))
