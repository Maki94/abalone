(require "newMain.lisp")

(defun find-second-neightbours (ball balls new-balls) ; returns ( ((D 1) (D 2)) ((E 2) (F 7)) )
  (cond
    ((null new-balls) nil)
    ((not (find (car new-balls) balls)) (find-second-neightbours ball balls (cdr new-balls)))
    (t (append ((list ball (car new-balls))
          (find-second-neightbours ball balls (cdr new-balls)))))))

(defun all-neightbours2 (single-balls balls)
  (if single-balls
    (append (find-second-neightbours (car single-balls) balls (get-all-new-tacka (car single-balls)))
                (all-neightbours2 ((cdr single-balls) balls)))))

(defun find-third-neightbours (two-ball balls new-balls)
  (cond
    ((null new-balls) nil)
    ((not (find-ball (car new-balls) balls)) (find-third-neightbours ball ))))

(defun get-all-third-new-tacka (ball1 ball2)
  (cond ((equal (car ball1) (car ball2))
            (list (append (car ball1) (list (1+ (max (car ball1) (car ball2)))))
                (append (car ball1) (list (1- (min (car ball1) (car ball2)))))))
        ((equal (cadr ball1) (cadr ball2))
            (list (append (list (next-char (max (car ball1) (car ball2)))) (cdr ball1))
                (append (list (prev-char (min (car ball1) (car ball2)))) (cdr ball1))))
        (t  (list (append (list (next-char (max (car ball1) (car ball2)))) (list (1+ (max (car ball1) (car ball2)))))
                (append (list (prev-char (min (car ball1) (car ball2)))) (list (1- (min (car ball1) (car ball2)))))))))

(defun all-neightbours3 (two-balls balls)
  (if two-balls
    (append (find-third-neightbours (car two-balls) balls (get-all-third-new-tacka (car two-balls))) ; TODO: find-third-neightbours
                (all-neightbours3 ((cdr two-balls) balls)))))

(defun all-command-combinations (balls)
  (list (append balls (list desno))
        (append balls (list gore-desno))
        (append balls (list gore-levo))
        (append balls (list levo))
        (append balls (list dole-levo))
        (append balls (list dole-desno))))

(defun make-command (balls) ; balls = (((D 1) (D 2) (D 3)) .. .(...))
  (if balls3
    (append (all-command-combinations (car balls)) (make-command (cdr balls)))))

(defun postavi (stanje oznaka tacke smer)
  (cond ((caddr tacke) (move-state-three stanje (car tacke) (cadr tacke) (caddr tacke) oznaka smer))
        ((cadr tacke) (move-state-two stanje (car tacke) (cadr tacke) oznaka smer))
        (t (move-state-one stanje (car tacke) oznaka smer))))

(defun valid-state-one (stanje tacka oznaka smer)
  (find-ball (get-new-tacka smer tacka) (state-empty stanje)))

(defun player-state (stanje player)
  (if player (state-white stanje) (state-black stanje)))

(defun new-third-tacka (smer mx mn)
  (cond
    ((and (equal (car mx) (car mn)) (= smer desno)) (list (car mx) (1+ (cadr mx))))
    ((and (equal (car mx) (car mn)) (= smer levo)) (list (car mx) (1- (cadr mn))))
    ((and (= (cadr mx) (cadr mn)) (= smer gore-levo)) (list (next-char (car mx)) (car mx)))
    ((and (= (cadr mx) (cadr mn)) (= smer dole-desno)) (list (prev-char (car mn)) (car mn)))
    ((and (not (equal (car mx) (car mn))) (not (= (cadr mx) (cadr mn))) (= smer gore-desno)) (list (next-char (car mx)) (1+ (cadr mx))))
    ((and (not (equal (car mx) (car mn))) (not (= (cadr mx) (cadr mn))) (= smer dole-levo))(list (prev-char (car mn)) (1- (car mn))))))

(defun can-push (new-tacka next-tacka empty opp-balls smer)
  (let* ( (c1 (car new-tacka))
          (c2 (car next-tacka))
          (v1 (cadr new-tacka))
          (v2 (cadr next-tacka)))
  ((if(or((and (equal c1 c2) (= smer desno))
        (and (equal c1 c2) (= smer levo))
        (and (= v1 v2) (= smer gore-levo))
        (and (= v1 v2) (= smer dole-desno))
        (and (not (equal c1 c2)) (not (= v1 v2)) (= smer gore-desno))
        (and (not (equal c1 c2)) (not (= v1 v2)) (= smer dole-levo))))
      (or (find-ball new-tacka empty)
        (and (find-ball new-tacka opp-balls))
          (find-ball next-tacka empty))
      nil))))

(defun max-tacka (t1 t2) (if (less t1 t2) t2 t1))

(defun min-tacka (t1 t2) (if (less t1 t2) t1 t2))

(defun valid-state-two (stanje mnt mxt smer player) ; tacka1 min, tacka2 max
  (let* ( (opp-balls (player-state stanje (not player)))
          (empty (state-empty stanje))

          (new-tacka (new-third-tacka smer mxt mnt))
          (next-tacka (new-third-tacka smer (max-tacka new-tacka mxt) (min-tacka new-tacka mnt))))
          (if (can-push new-tacka next-tacka empty opp-balls smer) t
              (and (find-ball (get-new-tacka smer mxt) empty)
                  (find-ball (get-new-tacka smer mnt) empty)))))

(defun less (t1 t2)
  (cond ( ((< (car t1) (car t2)) t)
          ((> (car t1) (car t2)) nil)
          ((< (cadr t1) (cadr t2)) t)
          (t nil))))

(defun sort-tacke (tacke) ; min mid max
  (let* ( (t1 (car tacke))
          (t2 (cadr tacke))
          (t3 (caddr tacke)))
          (cond
            ((null t3) (list (max-tacka t1 t2) (max-tacka t1 t2)))
            (t
              (if (less p1 p2)
                (if (less p1 p3)
                  (if (less p2 p3)
                    (list p1 p2 p3)
                    (list p1 p3 p2))
                  (list p3 p1 p2))
                (if (less p2 p3)
                  (if (less p1 p3)
                    (list p2 p1 p3)
                    (list p2 p3 p1))
                  (list p3 p2 p1))))
            )))
(defun new-fourth-tacka ())

(defun can-push3 (new-tacka next-tacka next-next-tacka empty opp-balls smer)
  (let* ( (c1 (car next-tacka))
          (c2 (car next-next-tacka))
          (v1 (cadr next-tacka))
          (v2 (cadr next-next-tacka)))
          ((if(or((and (equal c1 c2) (= smer desno))
                  (and (equal c1 c2) (= smer levo))
                  (and (= v1 v2) (= smer gore-levo))
                  (and (= v1 v2) (= smer dole-desno))
                  (and (not (equal c1 c2)) (not (= v1 v2)) (= smer gore-desno))
                  (and (not (equal c1 c2)) (not (= v1 v2)) (= smer dole-levo))))
                (or (find-ball new-tacka empty)
                    (and (find-ball new-tacka opp-balls)
                      (or (find-ball next-tacka empty)
                        (and (find-ball next-tacka opp-balls)
                          (find-ball next-next-tacka empty)))))
                nil))))

(defun valid-state-three (stanje t1 t2 t3 smer player) ; t1 min, t3 max
  (let* ( (opp-balls (player-state stanje (not player)))
          (empty (state-empty stanje))
          (new-tacka (new-third-tacka p3 p1 smer))

          (next-tacka (new-third-tacka (max-tacka new-tacka t3) (min-tacka new-tacka t1) smer)))
          (next-next-tacka (new-third-tacka (max-tacka next-tacka t3) (min-tacka new-tacka t1) smer))

          (if (can-push3 new-tacka next-tacka next-next-tacka empty opp-balls smer) t
              (and (find-ball (get-new-tacka smer t1) empty)
                  (find-ball (get-new-tacka smer t2) empty)
                  (find-ball (get-new-tacka smer t3) empty)))))

(defun try-set (stanje command player) ; command = (((D 1) (D 2)) desno)
  (let* ( (sorted-tacke (sort-tacke (car tacke)))
          (p1 (car sorted-tacke))
          (p2 (cadr sorted-tacke))
          (p3 (caddr sorted-tacke))
          (direction (cadr command)))
          (cond (p3 (valid-state-three stanje p1 p2 p3 player direction))
                (p2 (valid-state-two stanje p1 p2 player direction))
                (p1 (valid-state-one stanje p1 player direction)))))

(defun valid-commands (stanje command player)
  (if command  (append (try-set stanje (car command) player)
                        (valid-commands stanje (cdr command player)))))

(defun action-handler (stanje single-balls balls player) ; return all valid moves
  (let* ((neightbours2 (all-neightbours2 single-balls balls))
        (neightbours3 (all-neightbours3 neightbours2 balls))
        (command3 (make-command neightbours3))
        (command2 (make-command neightbours2))
        (command1 (make-command single-balls))
        (valid-cmds3 (valid-commands stanje command3 player))
        (valid-cmds2 (valid-commands stanje command2 player))
        (valid-cmds1 (valid-commands stanje command1 player)))
        (list valid-cmds3 valid-cmds2 valid-cmds1)))

(defun make-single (list temp) ; list = D; temp = (1 2 3 4); return ((D 1) (D 2)..)
  (if temp (append (list D (car temp)) (make-single list (cdr temp)))))

(defun single-balls (list)
  (if list (append (make-single (caar list) (cdar list)) (single-balls (cdr list)))))

(defun actions (stanje player) ; NOTE: returns all valid moves, NOTE: make a wrapper to be set with unique commands
    (action-handler
      stanje
      (single-balls (player-state stanje player))
      (player-state stanje player) player)
)
;(setf poc (init-state))
;(print-list poc)
;(format t "~%~%~%")
;
;(print-list (unesi poc t))
