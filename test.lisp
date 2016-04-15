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
  (cond
    ((equal (car ball1) (car ball2))
      (list (cons (car ball1) (list (1+ (max (car ball1) (car ball2)))))
            (cons (car ball1) (list (1- (min (car ball1) (car ball2)))))))
    ()
  )
)

(defun all-neightbours3 (two-balls balls)
  (if two-balls
    (append (find-third-neightbours (car two-balls) balls (get-all-third-new-tacka (car two-balls))) ; TODO: find-third-neightbours
                (all-neightbours3 ((cdr two-balls) balls))))
)

(defun fun (stanje single-balls balls) ; return all valid moves
  (let* (
    (neightbours2 (all-neightbours2 single-balls balls))
    (neightbours3 (all-neightbours3 neightbours2 balls)) ; TODO: all-neightbours3
    (moves3 (make-command3 neightbours3 balls))  ; TODO: make-command3
    (moves2 (make-command2 neightbours2 balls))  ; TODO: make-command2
    (moves1 (make-command1 single-balls balls))  ; TODO: make-command1
    )(
    list moves3 moves2 moves1
    ))
)


(defun actions (stanje player) ; TODO: returns all valid moves
  (if player
    (fun stanje (single-balls (state-white stanje) (state-white stanje)) ; todo single-balls
    (fun stanje (single-balls (state-black stanje) (state-black stanje)) ; todo fun
    ))

;(setf poc (init-state))
;(print-list poc)
;(format t "~%~%~%")
;
;(print-list (unesi poc t))
