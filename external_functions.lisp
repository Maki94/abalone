;(defun print-row (row)
;	"row = ((1 u))"
;  (mapcar (lambda (x) (print-field (last x))) row)
;  (format t "~%"))
;
;(defun print-field (field)
;  ; (print-list field)
;  (cond
;    ((equal (car field) 'U) (princ "   "))
;    ((equal (car field) 'W) (princ " w "))
;    ((equal (car field) 'B) (princ " b "))
;    ((equal (car field) 'E) (princ " - "))
;    ;(t (princ "nil"))
;		))
;
;		;(defun get-table (white black empty rows)
;		;  (if rows
;		;    (cons  (list (car rows) (get-fields (cadar white) (cadar black) (cadar empty) 1))
;		;        (get-table (cdr white)(cdr black) (cdr empty) (cdr rows)))))
;		;(defun get-fields (white-row black-row empty-row col)
;		;  ; vraca ((5 w)(6 w)(7 e)(8 b)(9 b))
;		;  (cond
;		;    ((= col 10) nil)
;		;    (( equal (car white-row) col)   (cons (list col 'W) (get-fields (cdr white-row) black-row empty-row (1+ col))))
;		;    (( equal (car black-row) col)   (cons (list col 'B) (get-fields white-row (cdr black-row) empty-row (1+ col))))
;		;    (( equal (car empty-row) col)  (cons (list col 'E) (get-fields white-row black-row (cdr empty-row) (1+ col))))
;		;    (t                 (cons (list col 'U) (get-fields white-row black-row empty-row (1+ col))))))
;
;		;(defun print-table (table)
;		;  (mapcar (lambda (x) (print-row (cadr x))) table))
;
;		;(trace get-empty)
;		;	(setq empty (get-empty white black rows columns '5 '-1)) ; empty = (A (7))  (B NIL)  (C (3 6 9))  (D (2 3 4 5 6 7 8))  (E (1 2 3 4 5 6 	7 8 9))  (F (1 2 3 4 5 6 7 8))  (G (1 4 7))  (H NIL)  (I (3))
;		;(untrace get-empty)
;
;		(setq occupieded (occupied black white))
;		(trace get-empty)
;			(print-list (get-empty occupieded '(5 6 7 8 9)))
;		(untrace get-empty)
;		;(setq table (get-table white black empty rows))
