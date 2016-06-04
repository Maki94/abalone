;(require "main.lisp")
(defun nova-stanja (stanje player akcije)
  (if akcije
    (append (list (postavi stanje player (car (car akcije)) (cadr (car akcije)))) (nova-stanja stanje player (cdr akcije)))))

;(defun proceni-stanje (stanje player)
;  (progn
;    (defparameter *T1-RULES* '( (if (NijeIzbacen ?x) then (NaTabeli 'el ?x)) ))
;    (defparameter *T1-FACTS* (apply 'list (mapcar (lambda (x)	(list 'NijeIzbacen 'b)) (single-balls (player-state stanje player)))))
;    (prepare-knowledge *T1-RULES* *T1-FACTS* 10)
;    (- 1000 (count-results '(NaTabeli 'el ?x)))))


(defun proceni-stanje (balls)
  (+
    (hierarhija-broj-kuglica (single-balls balls))
    (- 10000 (* 100 (length (single-balls balls))))
  ))

(defun hierarhija-broj-kuglica (single)
  (- 10000 (* 500 (length single))))


(defun hierarhija-pozicija (balls)
  (apply '+ (mapcar (lambda (x)
    (cond
      ( (equal (car x) 'A) 25)
      ( (equal (car x) 'I) 25)
      ( (equal (cadr x) '1) 25)
      ( (equal (cadr x) '9) 25)

      ( (equal x '(H 4)) 25)
      ( (equal x '(G 3)) 25)
      ( (equal x '(F 2)) 25)

      ( (equal x '(B 6)) 25)
      ( (equal x '(C 7)) 25)
      ( (equal x '(D 8)) 25)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ( (equal (car x) 'B) 17)
      ( (equal (car x) 'H) 17)
      ( (equal (cadr x) '8) 17)
      ( (equal (cadr x) '2) 17)

      ( (equal x '(G 4)) 17)
      ( (equal x '(F 3)) 17)

      ( (equal x '(C 6)) 17)
      ( (equal x '(D 7)) 17)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ( (equal (car x) 'G) 10)
      ( (equal (car x) 'C) 10)
      ( (equal (cadr x) '7) 10)
      ( (equal (cadr x) '3) 10)

      ( (equal x '(F 4)) 10)

      ( (equal x '(D 8)) 10)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ( (equal x '(E 5)) 0) ; centar

      (t 3)))               ; pored centra
   (single-balls balls))))

(defun max-s (s1 s2) (if (>= (cadr s1) (cadr s2)) s1 s2))

(defun min-s (s1 s2) (if (<= (cadr s1) (cadr s2)) s1 s2))

(defun alphabetaNew (stanje dubina alpha-stanje beta-stanje player playerProceni)
  (cond
    ((zerop dubina) (list stanje (proceni-stanje (player-state stanje playerProceni)) ))
    (player
        (mapcar (lambda (x)
            (setq alpha-stanje (max-s alpha-stanje (alphabeta-rest x  (1- dubina) alpha-stanje beta-stanje player playerProceni)))
            (if (>= (cadr alpha-stanje) (cadr beta-stanje)) (return-from alphabetaNew beta-stanje)))
           (nova-stanja stanje player (actions stanje player)))
        alpha-stanje)
    (t
      (mapcar (lambda (x)
            (setq beta-stanje (min-s beta-stanje (alphabeta-rest x (1- dubina) alpha-stanje beta-stanje (not player) playerProceni)))
            (if (<= (cadr beta-stanje) (cadr alpha-stanje)) (return-from alphabetaNew alpha-stanje)))
          (nova-stanja stanje player (actions stanje player)))
        beta-stanje)))


(defun alphabeta-rest (stanje dubina alpha-stanje beta-stanje player playerProceni)
  (cond
    ((zerop dubina) (list stanje (proceni-stanje (player-state stanje playerProceni)) ))
    (player
        (mapcar (lambda (x)
            (setq alpha-stanje (max-s alpha-stanje
              (alphabeta-rest x (1- dubina) alpha-stanje beta-stanje player playerProceni)
             ))
            (if (>= (cadr alpha-stanje) (cadr beta-stanje)) (return-from alphabeta-rest (list stanje (proceni-stanje (player-state stanje playerProceni))))))
           (nova-stanja stanje (not player) (actions stanje player)))
        alpha-stanje)
    (t
      (mapcar (lambda (x)
            (setq beta-stanje (min-s beta-stanje
              (alphabeta-rest x (1- dubina) alpha-stanje beta-stanje (not player) playerProceni)
              ))
            (if (<= (cadr beta-stanje) (cadr alpha-stanje)) (return-from alphabeta-rest (list stanje (proceni-stanje (player-state stanje playerProceni))))))
          (nova-stanja stanje (not player) (actions stanje player)))
        beta-stanje)))

;(setq start-taimer (get-universal-time))
;
;(print-table  (car (alphabeta poc 4 alpha-s beta-s t)))
;
;(format t "~% vreme ~d" (- (get-universal-time) start-taimer))
