(require "main.lisp")

(defun nova-stanja (stanje player akcije)
  (if akcije
    (let* (	(potez (car akcije))
            (smer (cadr potez))
            (tacke (car potez))
            (izlaz (postavi stanje player tacke smer)))

            (append (list izlaz) (nova-stanja stanje player (cdr akcije))))))
(defun proceni-stanje (state) (random 10000))

(defun min-stanje (stanja)
  (min-stanje-i (car stanja) (cdr stanja)))

(defun min-stanje-i (minStanje stanja)
  (cond
    ((null stanja) minStanje)
    ((< (cadar stanja) (cadr minStanje)) (min-stanje-i (car stanja) (cdr stanja)))
    (t (min-stanje-i minStanje (cdr stanja)))))

(defun max-stanje (stanja)
  (max-stanje-i (car stanja) (cdr stanja)))

(defun max-stanje-i (maxStanje stanja)
  (cond
    ((null stanja) maxStanje)
    ((> (cadar stanja) (cadr maxStanje)) (max-stanje-i (car stanja) (cdr stanja)))
    (t (max-stanje-i maxStanje (cdr stanja)))))

(defun max-s (s1 s2)
  (if (>= (cadr s1) (cadr s2)) s1 s2))

(defun min-s (s1 s2)
  (if (<= (cadr s1) (cadr s2)) s1 s2))

(defun max-value (stanje dubina alpha-stanje beta-stanje)
		(cond
			((zerop dubina) (list stanje (proceni-stanje stanje)))
			(t (progn
					(mapcar (lambda (x)
						(progn
							(setq alpha-stanje (max-s alpha-stanje (min-value x (1- dubina) alpha-stanje beta-stanje)))
              ;(format t "alpha-stanje=") (print-list alpha-stanje) (format t "~%")
							(if (>= (cadr alpha-stanje) (cadr beta-stanje)) (return-from max-value beta-stanje)))
						) (nova-stanja stanje t (actions stanje t)))
					alpha-stanje))))
;
(defun min-value (stanje dubina alpha-stanje beta-stanje)
		(cond
			((zerop dubina) (list stanje (proceni-stanje stanje)))
			(t (progn
					(mapcar (lambda (x)
						(progn
              (setq beta-stanje (min-s beta-stanje (max-value x (1- dubina) alpha-stanje beta-stanje)))
              ;(format t "beta-stanje=") (print-list beta-stanje) (format t "~%")
							(if (<= (cadr beta-stanje) (cadr alpha-stanje)) (return-from min-value alpha-stanje)))
						)(nova-stanja stanje nil (actions stanje nil)))
					beta-stanje))))


(setq alpha-s (list poc -1))
(setq beta-s (list  poc 200000))
;(trace max-value)
;(trace min-value)
;(trace max-stanje)
;(trace min-stanje)
;(trace max-stanje-i)
;(trace min-stanje-i)
(print-table  (car (max-value poc 1 alpha-s beta-s)))
