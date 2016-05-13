(require "main.lisp")
(defun nova-stanja (stanje player akcije)
  (if akcije
    (let* (	(potez (car akcije))
            (smer (cadr potez))
            (tacke (car potez))
            (izlaz (postavi stanje player tacke smer)))

            (append (list izlaz) (nova-stanja stanje player (cdr akcije))))))

(defun proceni-stanje (state) (random 10000))

(defun max-s (s1 s2) (if (>= (cadr s1) (cadr s2)) s1 s2))

(defun min-s (s1 s2) (if (<= (cadr s1) (cadr s2)) s1 s2))

(defun alphabeta (stanje dubina alpha-stanje beta-stanje player)
  (cond
    ((zerop dubina) (list stanje (proceni-stanje stanje)))
    (player
        (mapcar (lambda (x)
            (setq alpha-stanje (max-s alpha-stanje (alphabeta-rest x (1- dubina) alpha-stanje beta-stanje (not player))))
            (if (>= (cadr alpha-stanje) (cadr beta-stanje)) (return-from alphabeta beta-stanje)))
           (nova-stanja stanje player (actions stanje player)))
        alpha-stanje)
    (t
      (mapcar (lambda (x)
            (setq beta-stanje (min-s beta-stanje (alphabeta-rest x (1- dubina) alpha-stanje beta-stanje (not player))))
            (if (<= (cadr beta-stanje) (cadr alpha-stanje)) (return-from alphabeta alpha-stanje)))
          (nova-stanja stanje player (actions stanje player)))
        beta-stanje)))

(defun alphabeta-rest (stanje dubina alpha-stanje beta-stanje player)
  (cond
    ((zerop dubina) (list stanje (proceni-stanje stanje)))
    (player
        (mapcar (lambda (x)
            (setq alpha-stanje (max-s alphabeta-rest-stanje (alphabeta x (1- dubina) alpha-stanje beta-stanje (not player))))
            (if (>= (cadr alpha-stanje) (cadr beta-stanje)) (return-from alphabeta-rest (list stanje (proceni-stanje stanje)))))
           (nova-stanja stanje player (actions stanje player)))
        alpha-stanje)
    (t
      (mapcar (lambda (x)
            (setq beta-stanje (min-s beta-stanje (alphabeta x (1- dubina) alpha-stanje beta-stanje (not player))))
            (if (<= (cadr beta-stanje) (cadr alpha-stanje)) (return-from alphabeta-rest (list stanje (proceni-stanje stanje)))))
          (nova-stanja stanje player (actions stanje player)))
        beta-stanje)))

(setq alpha-s (list poc -1))
(setq beta-s (list  poc 200000))
(print-table poc)
(print-table  (car (alphabeta poc 2 alpha-s beta-s t)))
