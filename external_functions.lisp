(defun print-row (row)
	"row = ((1 u))"
  (mapcar (lambda (x) (print-field (last x))) row)
  (format t "~%"))

(defun print-field (field)
  ; (print-list field)
  (cond
    ((equal (car field) 'U) (princ "   "))
    ((equal (car field) 'W) (princ " w "))
    ((equal (car field) 'B) (princ " b "))
    ((equal (car field) 'E) (princ " - "))
    ;(t (princ "nil"))
		))
