(defun print-list (list)(cond((atom list)(format t " ~d " list))( t (dolist (x list)(format t " ~d " x)))))
(defun sortiraj (ls op) (cond ((null ls) '()) (t (dodaj (car ls) (sortiraj (cdr ls) op) op))))
(defun dodaj (el ls op) (cond  ((null ls) (list el)) ((apply op (list (cadr el) (cadr (car ls)))) (cons el ls)) (t  (cons (car ls) (dodaj el (cdr ls) op)))))
(defun pridruzi (graph ls) (cond  ((null ls) '()) (t (cons (list (car ls) (cadr (assoc (car ls) graph))) (pridruzi graph (cdr ls))))))
(defun dodaj-potomke (graph cvor cvorovi)(cond ((null graph) '())((equal (caar graph) cvor)(novi-cvorovi (cadar graph) cvorovi))(t (dodaj-potomke (cdr graph) cvor cvorovi))))
(defun novi-cvorovi (potomci cvorovi)(cond ((null potomci) '())((member (car potomci) cvorovi)(novi-cvorovi (cdr potomci) cvorovi))(t (cons (car potomci)(novi-cvorovi (cdr potomci) cvorovi)))))
(defun without-last (l) (reverse (cdr (reverse l))))
(defun set-union2 (l1 l2) (cond ((null l2) l1) ((member (car l2) l1) (set-union2 l1 (cdr l2))) (t (cons (car l2) (set-union2 l1 (cdr l2))))))
(defun set-difference2 (s1 s2) (cond ((null s1) nil) ((member (car s1) s2) (set-difference2 (cdr s1) s2)) (t (cons (car s1) (set-difference2 (cdr s1) s2)))))
(defun difference-union (l1 l2 l3) (set-difference2 l1 (set-union2 l2 l3)))

(defun my-or (x y) (or x y))
(defun equal3 (p1 p2 p3)
	(and (equal p1 p2) (equal p2 p3)))
(defun occupied (black white) 	; "pravi listu zauzetih elemenata"
	(if (and white black)
		(cons (list (caar white) (set-union2 (cadar black) (cadar white)))
						(occupied (cdr black) (cdr white)))))


(defun get-empty (occupied row)
	(cond
		((null occupied) nil)
		(t (cons  (list (caar occupied) (set-difference2 row (cadar occupied)))
								(get-empty (cdr occupied) (append
																								(if (= (1- (car row)) 0) nil (list (1- (car row))))
																								(if (= (1- (car row)) 0) (without-last row) row)))))))


(defun get-state (white black empty)
	(make-state
		:white white
		:black black
		:empty empty))

(defun remove-point (state-paremeter tacka) "CHECKED"
		(apply 'list (mapcar (lambda (x)	(if (equal (car x) (car tacka))
																				(list (car x) (remove (cadr tacka) (cadr x))) x))
																				 state-paremeter)))

(defun add-point (state-paremeter tacka)		"CHECKED"
		(apply 'list (mapcar (lambda (x)	(if (equal (car x) (car tacka))
																				(list (car x) (append (cdr tacka) (cadr x))) x))
																				 state-paremeter)))

(defun stampaj (stanje) "CHECKED" ; TODO Ponovo implementirati kada se sredi GUI
	(format t "~%White:~%")
	(print-list (state-white stanje))
	(format t "~%Black:~%")
	(print-list (state-black stanje))
	(format t "~%Empty:~%")
	(print-list (state-empty stanje)))

(defun kraj (stanje) "CHECKED"
	(or (not (reduce #'my-or (mapcar (lambda (x) (cadr x)) (state-white stanje))))
			(not (reduce #'my-or (mapcar (lambda (x) (cadr x)) (state-black stanje))))))

(defun prev-char (ch) (cadr (member ch (car (init-rows)))))
(defun next-char (ch) (cadr (member ch  (reverse (car (init-rows))))))
(defun get-all-new-tacka (tacka)
  (list (get-new-tacka desno tacka)
        (get-new-tacka gore-desno tacka)
        (get-new-tacka gore-levo tacka)
        (get-new-tacka levo tacka)
        (get-new-tacka dole-levo tacka)
        (get-new-tacka dole-desno tacka)
    )
)
