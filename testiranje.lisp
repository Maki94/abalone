(require "algorithms.lisp")
(require "constants.lisp")
(require "init.lisp")
(require "navigation.lisp")
(require "validate.lisp")
(require "stampanje.lisp")
(require "inference_engine.lisp")
(require "minimax.lisp")

(defun abalone ()
	(let* (	(stanje (init-state))
					(prvi (progn
						(format t "~%Unesite ko igra prvi (w ili b): ")
						(read)))
					(racunar (progn
						(format t "~%Unesite r ako racunar igra prvi (r ili c): ")
						(read)))
					(player (if (equal prvi 'w) t nil))
					(auto (if (equal racunar 'r) t nil)))
				(progn
					(print-table stanje)
					(igraj stanje player auto))))


(defun igraj (stanje igrac auto)
	(let* (	(nstanje (if auto
											(car (alphabetaNew stanje 2 (list stanje -1) (list stanje 2000000) igrac igrac))
											(unesi stanje igrac))))
					(progn
						(print-table nstanje)
						(if (not (kraj nstanje))
							(progn
								;(print-table nstanje)
							(let* (	(nnstanje (if auto
																		(unesi nstanje (not igrac))
																		(car (alphabetaNew nstanje 2 (list nstanje -1) (list nstanje 2000000) (not igrac) (not igrac))))))
										(progn
											(print-table nnstanje)
											(if (not (kraj nnstanje))
													(igraj nnstanje igrac auto)))))))) )

(defun unesi (stanje player) "CHECKED"
	(progn	(format t "~%Unesite potez (potez oblika (((D 4) (E 5) nil) 5)): ")
					(let* (	(potez (read))
									(smer (cadr potez))
									(tacke (car potez))
									(izlaz (postavi stanje player (sort-tacke tacke) smer)))
									izlaz)))
									;(if (equal stanje izlaz) (unesi stanje player) izlaz))))

(defun postavi (stanje player tacke smer)
  (cond ((caddr tacke) (move-state-three stanje (car tacke) (cadr tacke) (caddr tacke) player smer))
        ((cadr tacke) (move-state-two stanje (car tacke) (cadr tacke) player smer))
        (t (move-state-one stanje (car tacke) player smer))))

(defun actions (stanje player)
  (let* ( (single-balls (single-balls (player-state stanje player)))
          (balls (player-state stanje player))
          (neighbours2 (make-set-from-list (sort-lista-tacke (all-neighbours2 single-balls balls))))
          (neighbours3 (make-set-from-list (sort-lista-tacke (all-neighbours3 neighbours2 balls)))))
          (append
						(valid-commands stanje (make-command neighbours3) player))
						(valid-commands stanje (make-command neighbours2) player)
						(valid-commands stanje (make-command (mapcar 'list single-balls)) player)))



(defun results (state actions)
	(apply 'list (mapcar (lambda (x)	(list '1 x)) actions) ))

;(abalone)

(defun proceni-stanje (balls)
  (+
    (hierarhija-broj-kuglica (single-balls balls))
    ;(- 10000 (* 100 (length (single-balls balls))))
    (hierarhija-pozicija balls)
  ))

(defun hierarhija-broj-kuglica (single)
  (- 50000 (* 500 (length single))))

(defun hierarhija-pozicija (balls)
  (apply '+ (mapcar (lambda (x)
  (progn
    ;(print-list x)
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
        )
   (single-balls balls))))

(setq poc (init-state))
(setq w '((I (5 6)) (H (4 5 6)) (G (4 5 6)) (F nil) (E nil) (D nil) (C (5 6)) (B (4 5 6)) (A (4 5))))

(setq w-step '((I (5 6)) (H (4 5 6)) (G (5 6)) (F (4)) (E nil) (D nil) (C (5 6)) (B (4 5 6)) (A (4 5))))

(setq w1 '((I (6)) (H (4 5 6)) (G (4 5 6)) (F nil) (E nil) (D nil) (C (5 6)) (B (4 5 6)) (A (4 5))))

(trace  hierarhija-pozicija)
(print-list
  (proceni-stanje w)
  ;(hierarhija-broj-kuglica (single-balls w))
)
(print-list
  (proceni-stanje w-step)
)
  (print-list
    (proceni-stanje w1)
    ;(hierarhija-broj-kuglica (single-balls w1))
    )

;(print-list (broj-na-ivici (init-white)))
;(print-list (proceni-stanje poc t))
