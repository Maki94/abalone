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

(defun minimax (stanje dubina player)
  (let* ((akcije (actions stanje player)) ; akcije su svi validni potezi koji se mogu odigrati iz datog stanja
        (lp (nova-stanja stanje player akcije))
        (f (if player 'max-stanje 'min-stanje)))
        (cond
          ((or (zerop dubina) (null lp)) (list stanje (proceni-stanje stanje)))
          (t (apply f (list (mapcar (lambda (x) (minimax x (1- dubina) (not player))) lp)))))))

(defun alphabeta (stanje depth alpha beta player)
  (cond
    ((= depth -1) (list stanje (proceni-stanje stanje)))
    (player (progn
              (mapcar (lambda (x) (progn
                (setq pom (alphabeta x (1- depth) alpha beta (not player)))
                (setq alpha (max alpha (cadr pom)))
                ;(setq alpha (max alpha (cadr (alphabeta x (1- depth) alpha beta (not player)))))
                (if (<= beta alpha) (return-from alphabeta (list (car pom) alpha)))
                )) (nova-stanja stanje player (actions stanje player)))
              (list (car pom) alpha)))
    (t   (progn
            (mapcar (lambda (x) (progn
              (setq pomb (alphabeta x (1- depth) alpha beta (not player)))
              (setq beta (min beta (cadr pomb)))
              ;(setq beta (min beta (cadr (alphabeta x (1- depth) alpha beta (not player)))))
              (if (<= beta alpha) (return-from alphabeta (list (car pomb) beta)))
              )) (nova-stanja stanje player (actions stanje player)))
              (list (car pomb) beta)))))
;(trace alphabeta)
;(print-list (alphabeta poc 2 -1 2000 t))
(defun test-procena (sledbenici)
  (if sledbenici
    (progn
      (format t "~%")
      (print-list (proceni-stanje (car sledbenici)))
      (format t "~%")
      (stampaj (car sledbenici))
      (format t "~%")
      (test-procena (cdr sledbenici)))))
;(test-procena (nova-stanja poc t (actions poc t)))
(setq alphabeta-list (alphabeta poc 1 -1 200000000 t))
(print-table  (car alphabeta-list)) (format t "~%") (print-list  (cadr alphabeta-list))
;(print-table (car (minimax poc 1 t)))

;(print-list (max-value poc 0 2000 1 2 t))
