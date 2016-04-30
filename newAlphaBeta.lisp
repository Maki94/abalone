(require "minimax.lisp")

(defun minimax (stanje dubina player)
  (let*
    ((akcije (actions stanje player))
    (lp (nova-stanja stanje player akcije))
    (f (if player 'max-value 'min-value)))
    (cond
      ((or (= 0 dubina) (null lp)) (list stanje (proceni-stanje stanje)))
      (t (apply f (list (mapcar (lambda (x) (minimax x (1- dubina) (not player))) lp) dubina - 2000000 player))))) )
(defun max-value (stanje-list)
  (alphabeta (stanje-list)
)
(defun max-stanje (stanje)
  (max-stanje-i (cdr stanje) (car stanje)))

(defun max-stanje-i (lsv stanje-vrednost)
  (cond
    ((null lsv) stanje-vrednost)
    ((> (cadar lsv) (cadr stanje-vrednost)) (max-stanje-i (cdr lsv) (car lsv)))
    (t (max-stanje-i (cdr lsv) stanje-vrednost))))
(defun alphabeta-handler (stanje-list depth alpha beta player)
  (alphabeta ())
)
(defun alphabeta (stanje-list depth alpha beta player)
  (cond
    ((= depth 0) (list stanje (proceni-stanje stanje)))
    (player (progn
              (mapcar (lambda (x) (progn
                (setq pom (alphabeta x (1- depth) alpha beta (not player)))
                ;(print-list pom)
                (setq alpha (max alpha (cadr pom)))
                ;(setq alpha (max alpha (cadr (alphabeta x (1- depth) alpha beta (not player)))))
                (if (<= beta alpha) (return-from alphabeta (list (car pom) alpha)))
                )) stanje-list)
              (list (car pom) alpha)))
    (t   (progn
            (mapcar (lambda (x) (progn
              (setq pomb (alphabeta x (1- depth) alpha beta (not player)))
              ;(print-list pomb)
              (setq beta (min beta (cadr pomb)))
              ;(setq beta (min beta (cadr (alphabeta x (1- depth) alpha beta (not player)))))
              (if (<= beta alpha) (return-from alphabeta (list (car pomb) beta)))
              )) (nova-stanja stanje player (actions stanje player)))
              (list (car pomb) beta)))))

(setq mm (minimax poc 1 t))

(print-list mm)
