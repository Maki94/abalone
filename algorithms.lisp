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

(defun print-commands ()
	(format t "
	desno: 1
	gore-desno: 2
	gore-levo: 3
	levo: 4
	dole-levo: 5
	dole-desno: 6 ~%" ))

(defun my-or (x y) (or x y))
(defun equal3 (p1 p2 p3)
	(and (equal p1 p2) (equal p2 p3)))
(defun occupied (stanje) (occupied-handler (state-black stanje) (state-white stanje)))
(defun occupied-handler (black white) 	; "pravi listu zauzetih elemenata"
	(if (and white black)
		(cons (list (caar white) (set-union2 (cadar black) (cadar white)))
						(occupied-handler (cdr black) (cdr white)))))

(defun get-empty (stanje)
	(get-empty-handler (occupied-handler (state-black stanje) (state-white stanje)) '(5 6 7 8 9)))

(defun get-empty-handler (occupied row)
	(cond
		((null occupied) nil)
		(t (cons  (list (caar occupied) (set-difference2 row (cadar occupied)))
								(get-empty-handler (cdr occupied) (append
																								(if (= (1- (car row)) 0) nil (list (1- (car row))))
																								(if (= (1- (car row)) 0) (without-last row) row)))))))


(defun get-state (white black)
	(make-state
		:white white
		:black black))

(defun remove-point (state-paremeter tacka) "CHECKED"
		(apply 'list (mapcar (lambda (x)	(if (equal (car x) (car tacka))
																				(list (car x) (remove (cadr tacka) (cadr x))) x))
																				 state-paremeter)))

(defun add-point (state-paremeter tacka)		"CHECKED"
		(if (out-of-table tacka)
				 state-paremeter
				(apply 'list (mapcar (lambda (x)
					(if (equal (car x) (car tacka))
					 (list (car x) (sort  (append (cadr x) (cdr tacka)) #'<))
						x))
				 state-paremeter))))

(defun stampaj (stanje) "CHECKED"
	(format t "~%White:~%")
	(print-list (state-white stanje))
	(format t "~%Black:~%")
	(print-list (state-black stanje)))

(defun kraj (stanje) "CHECKED"
	(or (not (reduce #'my-or (mapcar (lambda (x) (cadr x)) (state-white stanje))))
			(not (reduce #'my-or (mapcar (lambda (x) (cadr x)) (state-black stanje))))))

(defun prev-char (ch) (cadr (member ch (car (init-rows)))))

(defun next-char (ch) (cadr (member ch  (reverse (car (init-rows))))))

(defun remove-invalid-list (list) ; list ='((I 6) (NIL 6) (NIL 5) (I 4) (H 4) (H 5))), returns ((I 6) (H 4) (H 5))
	(cond
		((null list) nil)
		((or
			(null (caar list))
			(< (cadar list) 1)
			(> (cadar list) 9)
			(and (equal (caar list) 'A) (> (cadar list) 5))
			(and (equal (caar list) 'B) (> (cadar list) 6))
			(and (equal (caar list) 'C) (> (cadar list) 7))
			(and (equal (caar list) 'D) (> (cadar list) 8))

			(and (equal (caar list) 'F) (< (cadar list) 2))
			(and (equal (caar list) 'G) (< (cadar list) 3))
			(and (equal (caar list) 'H) (< (cadar list) 4))
			(and (equal (caar list) 'I) (< (cadar list) 5))
			) (remove-invalid-list (cdr list)))
			(t (cons (car list) (remove-invalid-list (cdr list))))))

(defun get-all-new-tacka (tacka) ; returns '((I 6) (NIL 6) (NIL 5) (I 4) (H 4) (H 5)))
  (remove-invalid-list
	 			(list (get-new-tacka desno tacka)
        (get-new-tacka gore-desno tacka)
        (get-new-tacka gore-levo tacka)
        (get-new-tacka levo tacka)
        (get-new-tacka dole-levo tacka)
        (get-new-tacka dole-desno tacka)))
				 )

(defun find-ball (target list) ; target = ((d 1)) list = (()(D 1 2)())
	(member (cadr target) (cadr (assoc (car target) list))))

(defun ret-first (c1 c2 list)
	(cond ((null list) nil)
				((equal c1 (car list)) c1)
				((equal c2 (car list)) c2)
				(t (ret-first c1 c2 (cdr list)))))

(defun cmp-char< (c1 c2)
		(cond ((equal c1 c2) nil)
					((equal c1 (ret-first c1 c2 (reverse (car (init-rows))))) t)))

(defun cmp-char> (c1 c2)
		(cond ((equal c1 c2) nil)
					((equal c1 (ret-first c1 c2 (car (init-rows)))) t)))

(defun max-cmp-char (c1 c2)
	(cond ((equal c1 c2) c1)
				((equal c1 (ret-first c1 c2 (car (init-rows)))) c1)
				(t c2)))

(defun min-cmp-char (c1 c2)
	(if (equal c1 (max-cmp-char c2 c1)) c2 c1))
;
(defun less (t1 t2)
  (cond   ((cmp-char< (car t1) (car t2)) t)
          ((cmp-char> (car t1) (car t2)) nil)
          ((< (cadr t1) (cadr t2)) t)
          (t nil)))
(defun above (t1 t2)
  (if (less t1 t2) t2 t1))

;(defun sort-tacke (tacke) ; min mid max
;  (if (caddr tacke)
;    (if (equal (max-tacka (car tacke) (cadr tacke)) (car tacke))
;        (if (equal (max-tacka (car tacke) (caddr tacke)) (car tacke))
;          (list (car tacke) (max-tacka (cadr tacke) (caddr tacke)) (min-tacka (cadr tacke) (caddr tacke)))
;          (list (caddr tacke) (car tacke) (cadr tacke)))
;        (if (equal (max-tacka (cadr tacke) (caddr tacke)) (cadr tacke))
;          (list (cadr tacke) (max-tacka (car tacke) (caddr tacke)) (min-tacka (car tacke) (caddr tacke)))
;          (list (caddr tacke) (cadr tacke) (car tacke))))
;      (list (max-tacka (car tacke) (cadr tacke)) (min-tacka (car tacke) (cadr tacke)))))

;(defun sort-tacke (tacke) ; min mid max
;  (let* ( (p1 (car tacke))
;          (p2 (cadr tacke))
;          (p3 (caddr tacke)))
;          (if (p3)
;            (if (equal (max-tacka p1 p2) p1)
;                (if (equal (max-tacka p1 p3) p1)
;                  (list p1 (max-tacka p2 p3) (min-tacka p2 p3))
;                  (list p3 p1 p2))
;                (if (equal (max-tacka p2 p3) p2)
;                  (list p2 (max-tacka p1 p3) (min-tacka p1 p3))
;                  (list p3 p2 p1)))
;              (list (max-tacka p1 p2) (min-tacka p1 p2)))))

(defun sort-tacke (tacke )
	(cond
		( (= (length tacke) 3)
				(if (equal (max-tacka (car tacke) (cadr tacke)) (car tacke))
						(if (equal (max-tacka (car tacke) (caddr tacke)) (car tacke))
							(list (car tacke) (max-tacka (cadr tacke) (caddr tacke)) (min-tacka (cadr tacke) (caddr tacke)))
							(list (caddr tacke) (car tacke) (cadr tacke)))
						(if (equal (max-tacka (cadr tacke) (caddr tacke)) (cadr tacke))
							(list (cadr tacke) (max-tacka (car tacke) (caddr tacke)) (min-tacka (car tacke) (caddr tacke)))
							(list (caddr tacke) (cadr tacke) (car tacke)))))
		( (= (length tacke) 2) (list (max-tacka (car tacke) (cadr tacke)) (min-tacka (car tacke) (cadr tacke))))
		(t tacke)))


(defun max-tacka (t1 t2) (if (less t1 t2) t2 t1))

(defun min-tacka (t1 t2) (if (less t1 t2) t1 t2))

(defun player-state (stanje player)
  (if player (state-white stanje) (state-black stanje)))
;
(defun make-set-from-list (list)
  (cond ((null list) nil)
        ((my-find (car list) (cdr list)) (make-set-from-list (cdr list)))
        (t (append (list (car list)) (make-set-from-list (cdr list))))))

(defun my-find (el list)
  (if list (if (equal el (car list)) t (my-find el (cdr list)))))

(defun sort-lista-tacke (lista)
  (if lista
    (cons (sort-tacke (car lista)) (sort-lista-tacke (cdr lista)))))
(defun make-single (node temp) ; node = D; temp = (1 2 3 4); return ((D 1) (D 2)..)
  (if temp
    (cons (list node (car temp)) (make-single node (cdr temp)))))

(defun single-balls (list) ; returns ((I 5) (I 6) (H 4) (H 5) (H 6) (G 4) (G 5) (E 3) (E 4) (D 4) (C 5) (C 6) (B 4) (B 5) (B 6) (A 4) (A 5))
  (if list
     (append (make-single (caar list) (cadar list)) (single-balls (cdr list)))))

(defun out-of-table (tacka)
	(or(null tacka)
		 (null (car tacka))
		 (< (cadr tacka) 1)
		 (> (cadr tacka) 9)
		 (and (equal (car tacka) 'A) (> (cadr tacka) 5))
		 (and (equal (car tacka) 'B) (> (cadr tacka) 6))
		 (and (equal (car tacka) 'C) (> (cadr tacka) 7))
		 (and (equal (car tacka) 'D) (> (cadr tacka) 8))
		 (and (equal (car tacka) 'F) (< (cadr tacka) 2))
		 (and (equal (car tacka) 'G) (< (cadr tacka) 3))
		 (and (equal (car tacka) 'H) (< (cadr tacka) 4))
		 (and (equal (car tacka) 'I) (< (cadr tacka) 5)) ))
;
(defun all-command-combinations (balls)
  (list (cons balls (list desno))
        (cons balls (list gore-desno))
        (cons balls (list gore-levo))
        (cons balls (list levo))
        (cons balls (list dole-levo))
        (cons balls (list dole-desno))))
