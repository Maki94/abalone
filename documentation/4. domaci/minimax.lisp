;(require "main.lisp")
(defun nova-stanja (stanje player akcije)
  (if akcije
    (append (list (postavi stanje player (car (car akcije)) (cadr (car akcije)))) (nova-stanja stanje player (cdr akcije)))))

(defun proceni-stanje (stanje player)
  (progn
    (defparameter *T1-RULES* '( (if (NijeIzbacen ?x) then (NaTabeli 'el ?x)) ))
    (defparameter *T1-FACTS* (apply 'list (mapcar (lambda (x)	(list 'NijeIzbacen 'b)) (single-balls (player-state stanje player)))))
    (prepare-knowledge *T1-RULES* *T1-FACTS* 10)
    (- 1000 (count-results '(NaTabeli 'el ?x)))))


(defun max-s (s1 s2) (if (>= (cadr s1) (cadr s2)) s1 s2))

(defun min-s (s1 s2) (if (<= (cadr s1) (cadr s2)) s1 s2))

(defun alphabetaNew (stanje dubina alpha-stanje beta-stanje player)
  (cond
    ((zerop dubina) (list stanje (proceni-stanje stanje player)))
    (player
        (mapcar (lambda (x)
            (setq alpha-stanje (max-s alpha-stanje (alphabeta-rest x  (1- dubina) alpha-stanje beta-stanje (if (= 1  dubina) player (not player)))))
            (if (>= (cadr alpha-stanje) (cadr beta-stanje)) (return-from alphabetaNew beta-stanje)))
           (nova-stanja stanje player (actions stanje player)))
        alpha-stanje)
    (t
      (mapcar (lambda (x)
            (setq beta-stanje (min-s beta-stanje (alphabeta-rest x (1- dubina) alpha-stanje beta-stanje (if (= 1  dubina) player (not player)))))
            (if (<= (cadr beta-stanje) (cadr alpha-stanje)) (return-from alphabetaNew alpha-stanje)))
          (nova-stanja stanje player (actions stanje player)))
        beta-stanje)))


(defun alphabeta-rest (stanje dubina alpha-stanje beta-stanje player)
  (cond
    ((zerop dubina) (list stanje (proceni-stanje stanje player)))
    (player
        (mapcar (lambda (x)
            (setq alpha-stanje (max-s alpha-stanje
              (alphabeta-rest x (1- dubina) alpha-stanje beta-stanje (not player))
             ))
            (if (>= (cadr alpha-stanje) (cadr beta-stanje)) (return-from alphabeta-rest (list stanje (proceni-stanje stanje (not player))))))
           (nova-stanja stanje (not player) (actions stanje player)))
        alpha-stanje)
    (t
      (mapcar (lambda (x)
            (setq beta-stanje (min-s beta-stanje
              (alphabeta-rest x (1- dubina) alpha-stanje beta-stanje (not player))
              ))
            (if (<= (cadr beta-stanje) (cadr alpha-stanje)) (return-from alphabeta-rest (list stanje (proceni-stanje stanje (not player))))))
          (nova-stanja stanje (not player) (actions stanje player)))
        beta-stanje)))

;(setq start-taimer (get-universal-time))
;
;(print-table  (car (alphabeta poc 4 alpha-s beta-s t)))
;
;(format t "~% vreme ~d" (- (get-universal-time) start-taimer))
