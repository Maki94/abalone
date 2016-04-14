(require "newMain.lisp")

(defun find-neightbour (ball balls new-balls) ; todo find
  (cond
    ((null new-balls) nil)
    ((not (find (car new-balls) balls)) (find-neightbour ball balls (cdr new-balls)))
    (t (append ((list ball (car new-balls))
          (find-neightbour ball balls (cdr new-balls)))))
  ))

(defun all-neightbours2 (single-balls balls)
  (if balls
    (t (append (find-neightbour (car single-balls) balls)) (all-neightbours2 ((cdr single-balls) balls)))
))

(defun fun (stanje single-balls balls) ; return all valid moves
  (let* (
    (neightbours2 (all-neightbours2 single-balls balls)) ; todo: all-neightbours2
    (neightbours3 (all-neightbours3 neightbours2 balls)) ; todo: all-neightbours3
    (moves3 (make-command3 neightbours3 balls))  ; todo: make-command3
    (moves2 (make-command2 neightbours2 balls))  ; todo: make-command2
    (moves1 (make-command1 single-balls balls))  ; todo: make-command1
    )(
    list moves3 moves2 moves1
    ))
)


(defun actions (stanje player) ; TODO: returns all valid moves
  (if player
    (fun stanje (single-balls (state-white stanje) (state-white stanje)) ; todo single-balls
    (fun stanje (single-balls (state-black stanje) (state-black stanje)) ; todo fun
    )
)

;(setf poc (init-state))
;(print-list poc)
;(format t "~%~%~%")
;
;(print-list (unesi poc t))
