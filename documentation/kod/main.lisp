(require "algorithms.lisp")
(require "constants.lisp")
(require "init.lisp")
(require "navigation.lisp")
(require "validate.lisp")

(defun abalone ()
	(let* ((stanje (init-state))
		(prvi (progn
			(format t "~%Unesite ko igra prvi (w ili b): ")
			(read)))
		(racunar (progn
			(format t "~%Unesite r ako racunar igra prvi (r ili c): ")
			(read)))
		(igrac (if (equal prvi 'w) t '()))
		(auto (if (equal racunar 'r) t '())))
	(igraj stanje igrac auto)))

(defun igraj (stanje igrac auto)
	(let* ((nstanje (if t
										(unesi stanje igrac))))
					(progn (stampaj nstanje)
						(if (not (kraj nstanje))
							(let* ((nnstanje (if t
																(unesi nstanje (not igrac)))))
								(progn (stampaj nnstanje)
									(if (not (kraj nnstanje))
									(igraj nnstanje igrac auto))))))))

;e.e (igraj pocetno t nil) ; najpre igram prvi kaol beli igrac, a nakon toga igram kao crni kompijuter

(defun unesi (stanje igrac) "CHECKED"
	(progn	(format t "~%Unesite potez (potez oblika (((D 4) (E 5) nil) 5)): ")
					(let* (	(potez (read))
									(smer (cadr potez))
									(tacke (car potez))
									(izlaz (postavi stanje igrac tacke smer)))
									(if (equal stanje izlaz) (unesi stanje igrac) izlaz))))

(defun postavi (stanje oznaka tacke smer) ; NOTE: make wrapper for sorting moves
  (cond ((caddr tacke) (move-state-three stanje (car tacke) (cadr tacke) (caddr tacke) oznaka smer))
        ((cadr tacke) (move-state-two stanje (car tacke) (cadr tacke) oznaka smer))
        (t (move-state-one stanje (car tacke) oznaka smer))))

(defun actions (stanje player)
  (let* ( (single-balls (single-balls (player-state stanje player)))
          (balls (player-state stanje player))
          (neighbours2 (make-set-from-list (sort-lista-tacke (all-neighbours2 single-balls balls))))
          (neighbours3 (make-set-from-list (sort-lista-tacke (all-neighbours3 neighbours2 balls))))
          (command3 (make-command neighbours3))
          (command2 (make-command neighbours2))
          (command1 (make-command (mapcar 'list single-balls)))
          (valid-cmds1 (valid-commands stanje command1 player))
          (valid-cmds2 (valid-commands stanje command2 player))
          (valid-cmds3 (valid-commands stanje command3 player)))
          (append valid-cmds3 valid-cmds2 valid-cmds1)))

(defun results (stanje akcija)) ; TODO: svakom stanju za datu akciju pridruziti odredjenu heruistiku
(defun terminal-test (stanje))	; TODO: prepraviti funkciju za zavrsetak igre
(defun utility (stanje player)) ; TODO: ako je beli pobedio vratiti +1, a ako je crni -1


(setq poc (init-state))
(print-list poc)
;(format t "~%~%~%")

;(print-list (unesi poc t))

(print-list (actions poc t))
