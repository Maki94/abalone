(require "algorithms.lisp")
(require "constants.lisp")
(require "init.lisp")
(require "newNavigation.lisp")


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

(defun postavi (stanje oznaka tacke smer) ; TODO: make wrapper for sorting moves
  (cond ((caddr tacke) (move-state-three stanje (car tacke) (cadr tacke) (caddr tacke) oznaka smer))
        ((cadr tacke) (move-state-two stanje (car tacke) (cadr tacke) oznaka smer))
        (t (move-state-one stanje (car tacke) oznaka smer))))

(defun actions (stanje player) ; TODO: returns all valid moves
)
(defun results (stanje akcija))
(defun terminal-test (stanje))
(defun utility (stanje player))



(setf poc (init-state))
(print-list poc)
(format t "~%~%~%")
(print-list (unesi poc t))
