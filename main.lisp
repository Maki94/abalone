(require "algorithms.lisp")
(require "constants.lisp")
(require "init.lisp")
(require "navigation.lisp")
(require "validate.lisp")
(require "stampanje.lisp")

(defun abalone ()
	(let* ((stanje (init-state))
		(prvi (progn
			(format t "~%Unesite ko igra prvi (w ili b): ")
			(read)))
		(racunar (progn
			(format t "~%Unesite r ako racunar igra prvi (r ili c): ")
			(read)))
		(player (if (equal prvi 'w) t '()))
		(auto (if (equal racunar 'r) t '())))
	(igraj stanje player auto)))

(defun igraj (stanje player auto)
	(let* ((nstanje (if t
										(unesi stanje player))))
					(progn (stampaj nstanje)
						(if (not (kraj nstanje))
							(let* ((nnstanje (if t
																(unesi nstanje (not player)))))
								(progn (stampaj nnstanje)
									(if (not (kraj nnstanje))
									(igraj nnstanje player auto))))))))

;e.e (igraj pocetno t nil) ; najpre igram prvi kaol beli igrac, a nakon toga igram kao crni kompijuter

(defun unesi (stanje player) "CHECKED"
	(progn	(format t "~%Unesite potez (potez oblika (((D 4) (E 5) nil) 5)): ")
					(let* (	(potez (read))
									(smer (cadr potez))
									(tacke (car potez))
									(izlaz (postavi stanje player tacke smer)))
									(if (equal stanje izlaz) (unesi stanje player) izlaz))))

(defun postavi (stanje player tacke smer) ; NOTE: make wrapper for sorting moves
  (cond ((caddr tacke) (move-state-three stanje (car tacke) (cadr tacke) (caddr tacke) player smer))
        ((cadr tacke) (move-state-two stanje (car tacke) (cadr tacke) player smer))
        (t (move-state-one stanje (car tacke) player smer))))

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

"((((H 6) (H 5) (H 4)) 4) (((I 6) (H 5) (G 4)) 2) (((I 6) (H 5) (G 4)) 5)
 (((I 5) (H 5) (G 5)) 3) (((I 5) (H 5) (G 5)) 6) (((I 6) (H 6) (G 6)) 3)
 (((I 6) (H 6) (G 6)) 6) (((G 6) (G 5) (G 4)) 1) (((G 6) (G 5) (G 4)) 4)
 (((G 6) (G 5) (G 4)) 5) (((G 6) (G 5) (G 4)) 6) (((B 6) (B 5) (B 4)) 1)
 (((C 6) (B 5) (A 4)) 2) (((C 6) (B 5) (A 4)) 5) (((C 5) (B 5) (A 5)) 3)
 (((C 5) (B 5) (A 5)) 6)
	 (((I 6) (I 5)) 1) (((I 6) (I 5)) 2) (((I 6) (I 5)) 3)
 (((I 6) (I 5)) 4) (((I 5) (H 4)) 2) (((I 5) (H 4)) 3) (((I 5) (H 4)) 4)
 (((I 5) (H 4)) 5) (((I 6) (H 5)) 2) (((I 5) (H 5)) 3) (((H 5) (H 4)) 4)
 (((I 6) (H 6)) 2) (((I 6) (H 6)) 3) (((H 5) (G 4)) 5) (((H 4) (G 4)) 3)
 (((H 4) (G 4)) 4) (((H 4) (G 4)) 5) (((H 4) (G 4)) 6) (((H 6) (G 5)) 2)
 (((H 6) (G 5)) 5) (((H 5) (G 5)) 6) (((G 5) (G 4)) 4) (((G 5) (G 4)) 5)
 (((G 5) (G 4)) 6) (((H 6) (G 6)) 6) (((G 6) (G 5)) 5) (((G 6) (G 5)) 6)
 (((C 6) (C 5)) 1) (((C 6) (C 5)) 2) (((C 6) (C 5)) 3) (((C 6) (C 5)) 4)
 (((C 5) (B 4)) 2) (((C 5) (B 4)) 3) (((C 5) (B 4)) 5) (((C 6) (B 5)) 2)
 (((C 5) (B 5)) 3) (((C 6) (B 6)) 1) (((C 6) (B 6)) 2) (((C 6) (B 6)) 3)
 (((C 6) (B 6)) 6) (((B 6) (B 5)) 1) (((B 5) (A 4)) 5) (((B 4) (A 4)) 3)
 (((B 4) (A 4)) 5) (((B 4) (A 4)) 6) (((B 6) (A 5)) 1) (((B 6) (A 5)) 2)
 (((B 6) (A 5)) 5) (((B 6) (A 5)) 6) (((B 5) (A 5)) 6) (((A 5) (A 4)) 1)
 (((A 5) (A 4)) 4) (((A 5) (A 4)) 5) (((A 5) (A 4)) 6)
	 (((I 5)) 2) (((I 5)) 3)
 (((I 5)) 4) (((I 6)) 1) (((I 6)) 2) (((I 6)) 3) (((H 4)) 3) (((H 4)) 4)
 (((H 4)) 5) (((H 6)) 2) (((G 4)) 4) (((G 4)) 5) (((G 4)) 6) (((G 5)) 5)
 (((G 5)) 6) (((G 6)) 5) (((G 6)) 6) (((C 5)) 2) (((C 5)) 3) (((C 5)) 4)
 (((C 6)) 1) (((C 6)) 2) (((C 6)) 3) (((B 4)) 3) (((B 4)) 5) (((B 6)) 1)
 (((B 6)) 2) (((B 6)) 6) (((A 4)) 4) (((A 4)) 5) (((A 4)) 6) (((A 5)) 1)
 (((A 5)) 5) (((A 5)) 6))"
(defun results (state actions) ; NOTE: svakom stanju za datu akciju pridruziti odredjenu heruistiku koja je mnogo bolja od ove
	(apply 'list (mapcar (lambda (x)	(list '1 x)) actions) ))

"( (1 (((H 6) (H 5) (H 4)) 4)) (1 (((I 6) (H 5) (G 4)) 2)) (1 (((I 6) (H 5) (G 4)) 5)) )"



(setq poc (init-state))
(setq akcije (actions poc t))
(setq res (results poc akcije))

;(print-list poc)
;(print-table poc)
;(format t "~%~%~%")

;(print-list (unesi poc t))

;(print-list (actions poc t))
