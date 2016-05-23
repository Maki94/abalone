(require "main.lisp")

(defun proceni-stanje (stanje player)
  (progn
    (defparameter *T1-RULES* '( (if (NijeIzbacen ?x) then (NaTabeli 'el ?x)) ))
    (defparameter *T1-FACTS* (apply 'list (mapcar (lambda (x)	(list 'NijeIzbacen 'b)) (single-balls (player-state stanje player)))))
    (prepare-knowledge *T1-RULES* *T1-FACTS* 10)
    (count-results '(NaTabeli 'el ?x))))

(print-list (proceni-stanje poc t))
