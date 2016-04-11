(require "external_functions.lisp")
(require "algorithms.lisp")
(require "constants.lisp")
(require "init.lisp")
(require "navigate.lisp")

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

(defun postavi (stanje oznaka tacke smer); "CHECKED" (tacke = ((D 4) (E 3) nil))
	(cond
		((= desno smer)			(update-state-desno stanje tacke oznaka))								; "CHECKED"
		((= gore-desno smer)(update-state-gore-desno stanje tacke oznaka))					;
		((= gore-levo smer)	(update-state-gore-levo stanje tacke oznaka))						;
		((= levo smer)			(update-state-levo stanje tacke oznaka))								;
		((= dole-levo smer)	(update-state-dole-levo stanje tacke oznaka))						;
		((= dole-desno smer)(update-state-dole-desno stanje tacke oznaka))					;
		(t (error "~%Uneti smer nije validan!!!"))))

(defun update-state-desno (stanje tacke oznaka) "CHECKED"
	(cond
		((caddr tacke) (move-state-three-right stanje (car tacke) (cadr tacke) (caddr tacke) oznaka))
		((cadr tacke) (move-state-two-right stanje (car tacke) (cadr tacke) oznaka))
		(t (move-state-one-right stanje (car tacke) oznaka))))

(defun update-state-gore-desno (stanje tacke oznaka)
	(cond
		((caddr tacke) (move-state-three-up-right stanje (car tacke) (cadr tacke) (caddr tacke) oznaka))
		((cadr tacke) (move-state-two-up-right stanje (car tacke) (cadr tacke) oznaka))
		(t (move-state-one-up-right stanje (car tacke) oznaka))))

(defun update-state-gore-levo (stanje tacke oznaka)
	(cond
		((caddr tacke) (move-state-three-up-left stanje (car tacke) (cadr tacke) (caddr tacke) oznaka))
		((cadr tacke) (move-state-two-up-left stanje (car tacke) (cadr tacke) oznaka))
		(t (move-state-one-up-left stanje (car tacke) oznaka))))

(defun update-state-levo (stanje tacke oznaka)
	(cond
		((caddr tacke) (move-state-three-left stanje (car tacke) (cadr tacke) (caddr tacke) oznaka))
		((cadr tacke) (move-state-two-left stanje (car tacke) (cadr tacke) oznaka))
		(t (move-state-one-left stanje (car tacke) oznaka))))

(defun update-state-dole-levo (stanje tacke oznaka)
	(cond
		((caddr tacke) (move-state-three-down-left stanje (car tacke) (cadr tacke) (caddr tacke) oznaka))
		((cadr tacke) (move-state-two-down-left stanje (car tacke) (cadr tacke) oznaka))
		(t (move-state-one-down-left stanje (car tacke) oznaka))))

(defun update-state-dole-desno (stanje tacke oznaka)
	(cond
		((caddr tacke) (move-state-three-down-right stanje (car tacke) (cadr tacke) (caddr tacke) oznaka))
		((cadr tacke) (move-state-two-down-right stanje (car tacke) (cadr tacke) oznaka))
		(t (move-state-one-down-right stanje (car tacke) oznaka))))

;TESTING
(setf poc (init-state))
(print-list poc)
(format t "~%~%~%")
;(igraj poc t nil) ; najpre igram prvi kaol beli igrac, a nakon toga igram kao crni kompijuter
;(trace postavi)
	;(trace update-state-desno)
	;(trace move-state-two-right)
	;(trace move-state-one-right)
	;(stampaj (unesi poc t))
	(print-list (unesi poc t))
		;(untrace move-state-one-right)
	;(untrace move-state-two-right)
	;(untrace update-state-desno)
;(trace postavi)
