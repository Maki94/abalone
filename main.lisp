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
											(car (alphabetaNew stanje 4 (list stanje -1) (list stanje 2000000) igrac igrac))
											(unesi stanje igrac))))
					(progn
						(print-table nstanje)
						(if (not (kraj nstanje))
							(progn
								;(print-table nstanje)
							(let* (	(nnstanje (if auto
																		(unesi nstanje (not igrac))
																		(car (alphabetaNew nstanje 4 (list nstanje -1) (list nstanje 2000000) (not igrac) (not igrac))))))
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

(abalone)
