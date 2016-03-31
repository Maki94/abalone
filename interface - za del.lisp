(require "algorithms.lisp")

(defun get-empty (white black rows columns column state) "state = -1"
	(if rows
		(cons  	(list	(car rows)
					(difference-union   (member column (if (equal (car rows) 0) (without-last columns) columns))
										(cadr (assoc (car rows) black))
										(cadr (assoc (car rows) white))))
				(get-empty white black (cdr rows)
					(if (= state 0) (without-last columns) columns)
					(+ state column)
					(if (equal (car rows) 'D) 0 state)))
	)
)

(defun get-table (white black empty rows)
  (if rows
    (cons  (list (car rows) (get-fields (cadar white) (cadar black) (cadar empty) 1))
        (get-table (cdr white)(cdr black) (cdr empty) (cdr rows)))))

(defun get-fields (white-row black-row empty-row col)
  ; vraca ((5 w)(6 w)(7 e)(8 b)(9 b))
  (cond
    ((= col 10) nil)
    (( equal (car white-row) col)   (cons (list col 'W) (get-fields (cdr white-row) black-row empty-row (1+ col))))
    (( equal (car black-row) col)   (cons (list col 'B) (get-fields white-row (cdr black-row) empty-row (1+ col))))
    (( equal (car empty-row) col)  (cons (list col 'E) (get-fields white-row black-row (cdr empty-row) (1+ col))))
    (t                 (cons (list col 'U) (get-fields white-row black-row empty-row (1+ col))))))

(defun print-table (table)
  (mapcar (lambda (x) (print-row (cadr x))) table))

(defun execute (smer color p1 p2 p3)
	;(execute dole-levo 'W (C 4) (C 5) nil)
	;(if (cond
	;			((equal smer dole-levo) 	(if (dole-levo? p1 p2 p3) 			(move-dole-levo color p1 p2 p3) t))
	;
	;			((equal smer dole-desno) 	(if (dole-desno? p1 p2 p3) 			(move-dole-desno color p1 p2 p3) t))
	;
	;			((equal smer levo) 				(if (levo? p1 p2 p3) 						(move-levo color p1 p2 p3) t))
	;
	;			((equal smer gore) 				(if (gore? p1 p2 p3) 						(move-gore color p1 p2 p3) t))
	;
	;			((equal smer gore-desno) 	(if (gore-desno? p1 p2 p3) 			(move-gore-desno color p1 p2 p3) t))
	;
	;			((equal smer gore-levo) 	(if (gore-levo? p1 p2 p3) 			(move-gore-levo color p1 p2 p3) t)))
	;
	;			(execute-error (unvalid-move smer color p1 p2 p3)))
)


(setq empty (get-empty white black rows columns '5 '-1)) ; empty = (A (7))  (B NIL)  (C (3 6 9))  (D (2 3 4 5 6 7 8))  (E (1 2 3 4 5 6 	7 8 9))  (F (1 2 3 4 5 6 7 8))  (G (1 4 7))  (H NIL)  (I (3))
(setq table (get-table white black empty rows))

(print-table table)
