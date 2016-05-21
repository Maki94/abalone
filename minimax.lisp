;(require "main.lisp")
(defun nova-stanja (stanje player akcije)
  (if akcije
    (append (list (postavi stanje player (car (car akcije)) (cadr (car akcije)))) (nova-stanja stanje player (cdr akcije)))))

;(defun proceni-stanje (state) (random 10000))
;(defparameter *T1-RULES* '(
;  (if (Jak ?x) then (Voli 'Milica ?x))
;  (if (SkupaKola ?x) then (Voli 'Milica ?x))
;  (if (Sportista ?x) then (Jak ?x))
;  (if (Zdrav ?x) then (Sportista ?x))
;  (if (Voli 'Milica ?x) then (Voli ?x 'Milica))))
;
;(defparameter *T1-FACTS* '(
;  (Zdrav 'Milan) ))
;
;(prepare-knowledge *T1-RULES* *T1-FACTS* 10)
;(infer '(Voli 'Milan 'Milica))


(defun proceni-stanje (state)
  (random 10000))

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
            (setq alpha-stanje (max-s alpha-stanje (alphabeta-rest x (1- dubina) alpha-stanje beta-stanje (not player))))
            (if (>= (cadr alpha-stanje) (cadr beta-stanje)) (return-from alphabeta-rest (list stanje (proceni-stanje stanje)))))
            ;(if (> (cadr alpha-stanje) (cadr beta-stanje)) (return-from alphabeta-rest (list stanje (proceni-stanje stanje)))))
           (nova-stanja stanje player (actions stanje player)))
        alpha-stanje)
    (t
      (mapcar (lambda (x)
            (setq beta-stanje (min-s beta-stanje (alphabeta-rest x (1- dubina) alpha-stanje beta-stanje (not player))))
            ;(if (< (cadr beta-stanje) (cadr alpha-stanje)) (return-from alphabeta-rest (list stanje (proceni-stanje stanje)))))
            (if (<= (cadr beta-stanje) (cadr alpha-stanje)) (return-from alphabeta-rest (list stanje (proceni-stanje stanje)))))
          (nova-stanja stanje player (actions stanje player)))
        beta-stanje)))

;(setq alpha-s (list poc -1))
;(setq beta-s (list  poc 200000))
;
;(setq start-taimer (get-universal-time))
;
;(print-table  (car (alphabeta poc 4 alpha-s beta-s t)))
;
;(format t "~% vreme ~d" (- (get-universal-time) start-taimer))
