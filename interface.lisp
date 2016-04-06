(require "algorithms.lisp")

;(defun get-empty (white black rows columns column state) "state = -1"
;	(if rows
;		(cons	(list	(car rows)
;					(difference-union   (member column (if (equal (car rows) 0) (without-last columns) columns))
;										(cadr (assoc (car rows) black))
;										(cadr (assoc (car rows) white))))
;					(get-empty white black (cdr rows)
;					(if (= state 0) (without-last columns) columns)
;					(+ state column)
;					(if (equal (car rows) 'D) 0 state)))
;	)
;)
(defun get-empty (occupied row)
	(cond
		((null occupied) nil)
		(t (cons  (list (caar occupied) (set-difference2 row (cadar occupied)))
								(get-empty (cdr occupied) (append
																								( if (= (1- (car row)) 0) nil (list (1- (car row))))
																								( if (= (1- (car row)) 0) (without-last row) row)))))
	)
)
(defun occupied (black white)
	"pravi listu zauzetih elemenata"
	(if (and white black)
		(cons (list (caar white) (set-union2 (cadar black) (cadar white)))
						(occupied (cdr black) (cdr white)))
	)
)
;
;(defun get-table (white black empty rows)
;  (if rows
;    (cons  (list (car rows) (get-fields (cadar white) (cadar black) (cadar empty) 1))
;        (get-table (cdr white)(cdr black) (cdr empty) (cdr rows)))))
;(defun get-fields (white-row black-row empty-row col)
;  ; vraca ((5 w)(6 w)(7 e)(8 b)(9 b))
;  (cond
;    ((= col 10) nil)
;    (( equal (car white-row) col)   (cons (list col 'W) (get-fields (cdr white-row) black-row empty-row (1+ col))))
;    (( equal (car black-row) col)   (cons (list col 'B) (get-fields white-row (cdr black-row) empty-row (1+ col))))
;    (( equal (car empty-row) col)  (cons (list col 'E) (get-fields white-row black-row (cdr empty-row) (1+ col))))
;    (t                 (cons (list col 'U) (get-fields white-row black-row empty-row (1+ col))))))

;(defun print-table (table)
;  (mapcar (lambda (x) (print-row (cadr x))) table))


;(defun execute (smer color p1 p2 p3)
;	;(execute dole-levo 'W (C 4) (C 5) nil)
;	;(if (cond
;	;			((equal smer dole-levo) 	(if (dole-levo? p1 p2 p3) 			(move-dole-levo color p1 p2 p3) t))
;	;
;	;			((equal smer dole-desno) 	(if (dole-desno? p1 p2 p3) 			(move-dole-desno color p1 p2 p3) t))
;	;
;	;			((equal smer levo) 				(if (levo? p1 p2 p3) 						(move-levo color p1 p2 p3) t))
;	;
;	;			((equal smer gore) 				(if (gore? p1 p2 p3) 						(move-gore color p1 p2 p3) t))
;	;
;	;			((equal smer gore-desno) 	(if (gore-desno? p1 p2 p3) 			(move-gore-desno color p1 p2 p3) t))
;	;
;	;			((equal smer gore-levo) 	(if (gore-levo? p1 p2 p3) 			(move-gore-levo color p1 p2 p3) t)))
;	;
;	;			(execute-error (unvalid-move smer color p1 p2 p3)))
;)

(defun unesi-potez (potez white black)
	(if (equal potez 'W)
		(unesi white)
		(unesi black)
	)
)
(defun kraj (white black)
	(apply 'and (mapcar(lambda (x) (if (cdr x) t )) (append white black)))
)
(defun pronadji-boju (white black p)
	(cond
		((null p) t)
		((assoc))
	)
)
(defun ista-boja? (white black p1 p2 p3)
	(and
		(equal (pronadji-boju white black p1))
		(equal (pronadji-boju white black p2))
		(equal (pronadji-boju white black p3))
	)
)

(defun valid? (white black potez)
	(and 	(or (equal potez 1) (equal potez 2) (equal potez 3) (equal potez 4) (equal potez 5) (equal potez 6))
				(not (null (caar potez)))
				(if (and (null (cadar potez)) (cddar potez)) nil t)
				(ista-boja? white black (caar potez) (cadar potez) (cddar potez))
	)
)
(defun postavi (white black potez)
	(if (valid? white black potez)
			(execute white black potez)
			(error "Unet nevalidan potez")
	)
)

(defun unesi (white black)
	(progn
		(format t "~%Unesite potez(potez oblika (((D 4) (E 5) nil) 5)): ")
		(postavi white black (read)) ; "potez oblika (((D 4) (E 5) (F 6)) 5)"
	)
)
;(trace get-empty)
;	(setq empty (get-empty white black rows columns '5 '-1)) ; empty = (A (7))  (B NIL)  (C (3 6 9))  (D (2 3 4 5 6 7 8))  (E (1 2 3 4 5 6 	7 8 9))  (F (1 2 3 4 5 6 7 8))  (G (1 4 7))  (H NIL)  (I (3))
;(untrace get-empty)
(setq occupieded (occupied black white))
(trace get-empty)
	(print-list (get-empty occupieded '(5 6 7 8 9)))
(untrace get-empty)
;(setq table (get-table white black empty rows))


(defun init-state ()
	(make-state
		:white white
		:black black
		:empty (get-empty (occupied black white) '(5 6 7 8 9))
	)
)

(defun iks-oks ()
	(let* ((stanje '(---------))
		(prvi (progn
			(format t "~%Unesite ko igraprvi (w ili b): ")
			(read)))
		(racunar(progn
			(format t "~%Unesite r ako racunar igra prvi (r ili c): ")
			(read)))
		(igrac (if (equal prvi 'w) t '()))
		(auto (if (equal racunar 'r) t '())))
	(igraj stanje igrac auto)))

(defun igraj (stanje igrac auto)
	(let* ((nstanje (if (not auto)
										;(car (minimax stanje 4 igrac t))
										(unesi stanje igrac)))) ; potez je odigran
					(progn (stampaj nstanje)					; odstampaj tabelu
					(if (not (kraj nstanje))					; ako nije kraj igre
						(let* ((nnstanje (if (not auto)
															;(unesi nstanje (not igrac))
															(car (minimax nstanje 4 (not igrac) t)))))
							(progn (stampaj nnstanje)
								(if (not (kraj nnstanje))
								(igraj nnstanje igrac auto))))))))
;(print-table table)
