(require "constants.lisp")
(require "external_functions.lisp")

(defun print-list (list)(cond((atom list)(format t " ~d " list))( t (dolist (x list)(format t " ~d " x)))))
(defun sortiraj (ls op) (cond ((null ls) '()) (t (dodaj (car ls) (sortiraj (cdr ls) op) op))))
(defun dodaj (el ls op) (cond  ((null ls) (list el)) ((apply op (list (cadr el) (cadr (car ls)))) (cons el ls)) (t  (cons (car ls) (dodaj el (cdr ls) op)))))
(defun pridruzi (graph ls) (cond  ((null ls) '()) (t (cons (list (car ls) (cadr (assoc (car ls) graph))) (pridruzi graph (cdr ls))))))
(defun dodaj-potomke (graph cvor cvorovi)(cond ((null graph) '())((equal (caar graph) cvor)(novi-cvorovi (cadar graph) cvorovi))(t (dodaj-potomke (cdr graph) cvor cvorovi))))
(defun novi-cvorovi (potomci cvorovi)(cond ((null potomci) '())((member (car potomci) cvorovi)(novi-cvorovi (cdr potomci) cvorovi))(t (cons (car potomci)(novi-cvorovi (cdr potomci) cvorovi)))))

(defun set-union2 (l1 l2)
 (cond ((null l2) l1)
       ((member (car l2) l1) (set-union2 l1 (cdr l2)))
       (t (cons (car l2) (set-union2 l1 (cdr l2))))))

(defun set-difference2 (s1 s2)
 "l1\l2 , D E \ A D G"
	(cond
		((null s1) nil)
		((member (car s1) s2) (set-difference2 (cdr s1) s2))
		(t (cons (car s1) (set-difference2 (cdr s1) s2)))))

(defun difference-union (l1 l2 l3)
  "l1 \ (l2 union l3)"
	(set-difference2 l1 (set-union2 l2 l3))
)

(defun without-last (l)
    (reverse (cdr (reverse l)))
)
