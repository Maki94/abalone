
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
  (print-list stanje)
  (max-stanje-i (cdr stanje) (car stanje)))

(defun max-stanje-i (lsv stanje-vrednost)
(progn
(format t "(cadar lsv) = ~d" (cadar lsv))
(format t " cadar stanje-vrednost = ")
(print-list   (cadar  stanje-vrednost))
  (cond
    ((null lsv) stanje-vrednost)
    ((> (cadar lsv) (cadr stanje-vrednost)) (max-stanje-i (cdr lsv) (car lsv)))
    (t (max-stanje-i (cdr lsv) stanje-vrednost))))
)
(defun min-stanje (lista-stanja) ; lista-stanja = ((stanje heruistika) (stanje heruistika) ...)
  (min-stanje-i (cdr lista-stanja) (car lista-stanja)))

(defun min-stanje-i (lsv stanje-vrednost) ; lsv = vrednost, stanje-vrednost = stanje
(progn
(format t "(cadar lsv) = ~d" (cadar lsv))
(format t " cadar stanje-vrednost = ")
(print-list   (cadar  stanje-vrednost))
  (cond
    ((null lsv) stanje-vrednost)
    ((< (cadar lsv) (cadar stanje-vrednost)) (min-stanje-i (cdr lsv) (car lsv)))
    (t (min-stanje-i (cdr lsv) stanje-vrednost))))
)
(defun minimax (stanje dubina player)
  (let* ((akcije (actions stanje player)) ; akcije su svi validni potezi koji se mogu odigrati iz datog stanja
        (lp (nova-stanja stanje player akcije))
        (f (if player 'max-stanje 'min-stanje)))
        (cond
          ((or (zerop dubina) (null lp)) (list stanje (proceni-stanje stanje)))
          (t (apply f (list (mapcar (lambda (x) (minimax x (1- dubina) (not player))) lp)))))))

(defun alphabeta (stanje depth alpha beta player)
  (cond
    ((= depth 0) (list stanje (proceni-stanje stanje)))
    (player (progn
              (mapcar (lambda (x) (progn
                (setq pom (alphabeta x (1- depth) alpha beta (not player)))
                (setq alpha (max alpha (cadr pom)))
                (if (<= beta alpha) (return-from alphabeta (list (car pom) alpha)))
                )) (nova-stanja stanje player (actions stanje player)))
              (list (car pom) alpha)))
    (t   (progn
            (mapcar (lambda (x) (progn
              (setq pomb (alphabeta x (1- depth) alpha beta (not player)))
              (setq beta (min beta (cadr pomb)))
              (if (<= beta alpha) (return-from alphabeta (list (car pomb) beta)))
              )) (nova-stanja stanje player (actions stanje player)))
              (list (car pomb) beta)))))

(defun max-value (stanje dubina alpha-stanje beta-stanje)
  (let* ((lp (nova-stanja stanje t (actions stanje t))))
		(cond
			((or (zerop dubina) (null lp)) (list stanje (proceni-stanje stanje)))
			(t (progn
					(mapcar (lambda (x)
						(progn
              (setq tempMax (min-stanje (list alpha-stanje (min-value x (1- dubina) alpha-stanje beta-stanje))))
              (format t "alpha-stanje=") (print-list alpha-stanje)
              (format t "tmpMax=") (print-list tempMax)
							(setq alpha-stanje (list tempMax (proceni-stanje stanje)))
							(if (>= (cadar alpha-stanje) (cadar beta-stanje)) (return-from max-value beta-stanje)))
						) lp)
					alpha-stanje)))))
;
(defun min-value (stanje dubina alpha-stanje beta-stanje)
  (let* ((lp (nova-stanja stanje nil (actions stanje nil))))
		(cond
			((or (zerop dubina) (null lp)) (list stanje (proceni-stanje stanje)))
			(t (progn
					(mapcar (lambda (x)
						(progn
              (setq tempMin (max-stanje (list beta-stanje (max-value x (1- dubina) alpha-stanje beta-stanje))))
              (format t "alpha-stanje") (print-list alpha-stanje)
              (format t "tempMin") (print-list tempMin)
              (setq beta-stanje (list tempMin (proceni-stanje stanje)))
							(if (<= (cadar beta-stanje) (cadar alpha-stanje)) (return-from min-value alpha-stanje)))
						)lp)
					beta-stanje)))))

;(setq alphabeta-list (alphabeta poc 3 -1 200000000 t))
;(print-table poc)
;(print-table  (car alphabeta-list)) (format t "~%") (print-list  (cadr alphabeta-list))

(setq alpha-s (list (list poc -1)))
(setq beta-s (list (list poc 200000)))
;(trace max-value)
(trace min-value)
(trace max-stanje)
(trace max-stanje-i)
(trace min-stanje)
(trace min-stanje-i)
(print-table  (max-value poc 1 alpha-s beta-s))

;(trace minimax)
;(trace max-stanje)
;(trace max-stanje-i)
;(minimax poc 1 t)

"max-stanje ulaz
(MAX-STANJE
 '((s1  1874)
   (#S(STATE :WHITE ((I (6 5)) (H (5 4 6)) (G (5 6)) (F NIL) (E NIL) (D NIL) (C (5 6)) (B (4 5 6)) (A (4 5)))
       :BLACK ((I (8 9)) (H (7 8 9)) (G (7 8)) (F (5)) (E NIL) (D NIL) (C (2 3)) (B (1 2 3)) (A (1 2))))
    346)
   (#S(STATE :WHITE ((I (5)) (H (5 4 6)) (G (4 5 6)) (F (3)) (E NIL) (D NIL) (C (5 6)) (B (4 5 6)) (A (4 5)))
       :BLACK ((I (8 9)) (H (7 8 9)) (G (7 8)) (F (5)) (E NIL) (D NIL) (C (2 3)) (B (1 2 3)) (A (1 2))))
    2230)))"
"max-stanje-i ulaz
(MAX-STANJE-I
 '((#S(STATE :WHITE ((I (6 5)) (H (5 4 6)) (G (5 6)) (F NIL) (E NIL) (D NIL) (C (5 6)) (B (4 5 6)) (A (4 5)))
       :BLACK ((I (8 9)) (H (7 8 9)) (G (7 8)) (F (5)) (E NIL) (D NIL) (C (2 3)) (B (1 2 3)) (A (1 2))))
    346)
   (#S(STATE :WHITE ((I (5)) (H (5 4 6)) (G (4 5 6)) (F (3)) (E NIL) (D NIL) (C (5 6)) (B (4 5 6)) (A (4 5)))
       :BLACK ((I (8 9)) (H (7 8 9)) (G (7 8)) (F (5)) (E NIL) (D NIL) (C (2 3)) (B (1 2 3)) (A (1 2))))
    2230)
   (#S(STATE :WHITE ((I (5 6)) (H (5 4 6)) (G (4 6)) (F NIL) (E NIL) (D NIL) (C (5 6)) (B (4 5 6)) (A (4 5)))
       :BLACK ((I (8 9)) (H (7 8 9)) (G (7 8)) (F (5)) (E NIL) (D NIL) (C (2 3)) (B (1 2 3)) (A (1 2))))
    2137)"
