(require "main.lisp")

(defun nova-stanja (stanje player akcije)
  (if akcije
    (let* (	(potez (car akcije))
            (smer (cadr potez))
            (tacke (car potez))
            (izlaz (postavi stanje player tacke smer)))

            (append (list izlaz) (nova-stanja stanje player (cdr akcije))))))

(defun proceni-stanje (state) (random 10000))

(defun max-stanje (stanje)
  (max-stanje-i (cdr stanje) (car stanje)))

(defun max-stanje-i (lsv stanje-vrednost)
  (cond
    ((null lsv) stanje-vrednost)
    ((> (cadar lsv) (cadr stanje-vrednost)) (max-stanje-i (cdr lsv) (car lsv)))
    (t (max-stanje-i (cdr lsv) stanje-vrednost))))

(defun min-stanje (lista-stanja) ; lista-stanja = ((stanje heruistika) (stanje heruistika) ...)
  (min-stanje-i (cdr lista-stanja) (car lista-stanja)))

(defun min-stanje-i (lsv stanje-vrednost) ; lsv = vrednost, stanje-vrednost = stanje
  (cond
    ((null lsv) stanje-vrednost)
    ((< (cadar lsv) (cadr stanje-vrednost)) (min-stanje-i (cdr lsv) (car lsv)))
    (t (min-stanje-i (cdr lsv) stanje-vrednost))))
;
;(defun minimax (stanje dubina player)
;  (let* ((akcije (actions stanje player)) ; akcije su svi validni potezi koji se mogu odigrati iz datog stanja
;        (lp (nova-stanja stanje player akcije))
;        (f (if player 'max-stanje 'min-stanje)))
;        (cond
;          ((or (zerop dubina) (null lp)) (list stanje (proceni-stanje stanje)))
;          (t (apply f (list (mapcar (lambda (x) (minimax x (1- dubina) (not player))) lp)))))))

;(print-list poc)
(print-table poc)
(setq akss (actions poc t))
(setq aks (subseq akss 48 58))
;(print-list (actions poc t))
;(defun  test-first (akcije)
;  (if akcije
;    (progn
;            (print-list (car akcije))
;            ;(print-list (cdar akcije))
;            (format t "~%")
;            (print-table (postavi poc t (caar akcije) (cadar akcije)))
;            (test-first (cdr akcije)))))
;
;(format t "~%")

;(defun max-value (stanje a b dubina max-dubina player)
;  (if (= dubina max-dubina)
;   (proceni-stanje stanje)
;   (progn (mapcar (lambda (S)
;     (let* ((a (max a (min-value S a b (1+ dubina) max-dubina player)))) (if (>= a b) (return-from max-value b)))
;     ) (nova-stanja stanje player (actions stanje player)))
;    a)))
;
;
;(defun min-value (stanje a b dubina max-dubina player)
;  (if (= dubina max-dubina)
;   (proceni-stanje stanje)
;   (progn (mapcar (lambda (S)
;     (let ((new-a (min a (max-value S a b (1+ dubina) max-dubina player)))) (if (<= b new-a) (return-from min-value new-a)))
;     ) (nova-stanja stanje player (actions stanje player)))
;    b)))

;(defun alphabeta (stanje depth alpha beta player)
;  (cond
;    ((= depth 0) (proceni-stanje stanje))
;    (player
;      (progn
;        (mapcar (lambda (x) (progn
;          (setq alpha (max alpha (alphabeta x (1- depth) alpha beta (not player))))
;          (if (<= beta alpha) (return-from alphabeta alpha))
;          ))(nova-stanja stanje player (actions stanje player)))
;        alpha))
;    ((not player)
;        (progn
;          (mapcar (lambda (x)(progn
;            (setq beta (min beta (alphabeta x (1- depth) alpha beta (not player))))
;            (if (<= beta alpha) (return-from alphabeta beta))
;            )) (nova-stanja stanje player (actions stanje player)))
;            beta))
;    (t (error "bad call"))))
;(setq mm (minimax poc 1 t))
;(print-table (car (minimax poc 1 t)))

;(print-list (max-value poc 0 2000 1 2 t))
