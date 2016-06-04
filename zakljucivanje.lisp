(require "main.lisp")

;(defun proceni-stanje (stanje player)
;  (progn
;    (defparameter *T1-RULES* '( (if (NijeIzbacen ?x) then (NaTabeli 'el ?x)) ))
;    (defparameter *T1-FACTS* (apply 'list (mapcar (lambda (x)	(list 'NijeIzbacen 'b)) (single-balls (player-state stanje player)))))
;    (prepare-knowledge *T1-RULES* *T1-FACTS* 10)
;    (count-results '(NaTabeli 'el ?x))))

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

(setq poc (init-state))
(setq w '((I (5 6)) (H (4 5 6)) (G (4 5 6)) (F nil) (E nil) (D nil) (C (5 6)) (B (4 5 6)) (A (4 5))))

(setq w1 '((I (6)) (H (4 5 6)) (G (4 5 6)) (F nil) (E nil) (D nil) (C (5 6)) (B (4 5 6)) (A (4 5))))

(print-list
  (proceni-stanje w)
  ;(hierarhija-broj-kuglica (single-balls w))
)

  (print-list
    (proceni-stanje w1)
    ;(hierarhija-broj-kuglica (single-balls w1))
    )

;(print-list (broj-na-ivici (init-white)))
;(print-list (proceni-stanje poc t))
