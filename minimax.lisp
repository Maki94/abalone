(require "main.lisp")

(defun nova-stanja (stanje player akcije)
  (if akcije
    (let* (	(potez (car akcije))
            (smer (cadr potez))
            (tacke (car potez))
            (izlaz (postavi stanje player tacke smer)))

            (append (list izlaz) (nova-stanja stanje player (cdr akcije))))))

(defun proceni-stanje (state) (random 1000))

(defun max-stanje (stanje)
  (max-stanje-i (cdr stanje) (car stanje)))

(defun max-stanje-i (lsv stanje-vrednost)
  (cond
    ((null lsv) stanje-vrednost)
    ((> (cadar lsv) (cadr stanje-vrednost)) (max-stanje-i (cdr lsv) (car lsv)))
    (t (max-stanje-i (cdr lsv) stanje-vrednost))))

(defun min-stanje (stanje)
  (min-stanje-i (cdr stanje) (car stanje)))

(defun min-stanje-i (lsv stanje-vrednost)
  (cond
    ((null lsv) stanje-vrednost)
    ((< (cadar lsv) (cadr stanje-vrednost)) (min-stanje-i (cdr lsv) (car lsv)))
    (t (min-stanje-i (cdr lsv) stanje-vrednost))))

(defun minimax (stanje dubina player)
  (let* ((lp (nova-stanja stanje player (actions stanje player)))
        (f (if player 'max-stanje 'min-stanje)))
        (cond
          ((or (zerop dubina) (null lp)) (list stanje (proceni-stanje stanje)))
          (t (apply f (list (mapcar (lambda (x) (minimax x (1- dubina) (not player))) lp)))))))

;(print-list poc)
(print-table poc)
(setq akss (actions poc t))
(setq aks (subseq akss 48 58))
;(print-list (actions poc t))
(defun  test-first (akcije)
  (if akcije
    (progn
            (print-list (car akcije))
            ;(print-list (cdar akcije))
            (format t "~%")
            (print-table (postavi poc t (caar akcije) (cadar akcije)))
            (test-first (cdr akcije)))))

(format t "~%")
;(trace postavi)
  ;(trace move-state-three)
    ;(trace move-state-two)
    ;(trace move-state-one)
;(print-table (postavi poc t '((G 6) (G 5) (G 4)) 1))

;(trace print-table)
;(test-first aks)

;(trace minimax)
;(trace proceni-stanje)
;(trace nova-stanja)
;(trace minimax)
(print-table (car (minimax poc 1 t)))
;(car (minimax poc 1 t))
