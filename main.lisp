(require "external_functions.lisp")
(require "algorithms.lisp")
(require "constants.lisp")
(require "init.lisp")

(defun abalone () ; beli uvek igra prvi, ako igrac ima True igra prvi, a ako auto ima True onda racunar igra prvi
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

(defun igraj (stanje igrac auto)																								; igrac je True ako igra white, auto je True ako igra komijuter
	(let* ((nstanje (if t																								; nstanje sadri strukturu nakon odigranog poteza
										(unesi stanje igrac)))) 																		; unesi vraca strukturu tipa state, koja vraca novo stanje
					(progn (stampaj nstanje)																							; odstampaj tabelu nakon odigranog poteza
						(if (not (kraj nstanje))																						; ako nije kraj igre
							(let* ((nnstanje (if t																					; i ne igra kompijuter, onda igra neko drugi umesto njega
																(unesi nstanje (not igrac)))))											; odigraj ponovo potez
								(progn (stampaj nnstanje)																				; na kraju stapaj ovo stanje
									(if (not (kraj nnstanje))																			; da li je kraj?
									(igraj nnstanje igrac auto))))))))														; rekurzivno dok neko ne izgubi

;(igraj pocetno t nil) ; najpre igram prvi kaol beli igrac, a nakon toga igram kao crni kompijuter


(defun unesi (stanje igrac) "CHECKED"
	(progn	(format t "~%Unesite potez (potez oblika (((D 4) (E 5) nil) 5)): ")
					(let* (	(potez (read))
									(smer (cadr potez))
									(tacke (car potez))
									(izlaz (postavi stanje igrac tacke smer)))
									(if (equal stanje izlaz) (unesi stanje igrac) izlaz))))

(defun postavi (stanje oznaka tacke smer); "CHECKED" (tacke = ((D 4) (E 3) nil)) 					; moja modifikovana funkcija, treba da vrati strukturu stanje sa izmenama
	(cond
		((= desno smer)			(update-state-desno stanje tacke oznaka))								; "CHECKED"
		((= gore-desno smer)(update-state-gore-desno stanje tacke oznaka))					; TODO
		((= gore-levo smer)	(update-state-gore-levo stanje tacke oznaka))						; TODO
		((= levo smer)			(update-state-levo stanje tacke oznaka))								; TODO
		((= dole-levo smer)	(update-state-dole-levo stanje tacke oznaka))						; TODO
		((= dole-desno smer)(update-state-dole-desno stanje tacke oznaka))					; TODO
		(t (error "~%Uneti smer nije validan!!!"))))

(defun update-state-desno (stanje tacke oznaka) "CHECKED"
	(cond
		((caddr tacke) (move-state-three-right stanje (car tacke) (cadr tacke) (caddr tacke) oznaka))
		((cadr tacke) (move-state-two-right stanje (car tacke) (cadr tacke) oznaka))
		(t (move-state-one-right stanje (car tacke) oznaka))))

(defun move-state-three-right (stanje tacka1 tacka2 tacka3 oznaka) "CHECKED"
	(if (equal3 (car tacka1) (car tacka2) (car tacka3))
		(let*((new-tacka1 (cons  (car tacka3) (list (1+ (cadr tacka3)))))
					(new-tacka2 (cons  (car new-tacka1) (list (1+ (cadr new-tacka1)))))
					(white (state-white stanje))
					(black (state-black stanje))
					(removed-black (remove-point (remove-point black new-tacka1) new-tacka2))
					(removed-white (remove-point (remove-point white new-tacka1) new-tacka2)))

					(move-state-one-right (move-state-one-right (move-state-one-right
						(if (and (equal removed-black black) (equal removed-white white))
								stanje
								(move-state-one-right (move-state-one-right stanje new-tacka2 (not oznaka))
									new-tacka1 (not oznaka)))
						tacka3 oznaka) tacka2 oznaka) tacka1 oznaka))
		(move-state-one-right (move-state-one-right (move-state-one-right stanje
		 tacka1 oznaka) tacka2 oznaka) tacka3 oznaka)))
;(move-state-two-right stanje new-tacka1 new-tacka2 (not oznaka)))

(defun move-state-two-right (stanje tacka1 tacka2 oznaka) "CHECKED"
	(if (equal (car tacka1) (car tacka2)) ; ako se nalaze u istoj koloni mogu da guraju
		(let*((new-tacka (cons  (car tacka2) (list (1+ (cadr tacka2)))))
					(white (state-white stanje))
					(black (state-black stanje))
					(removed-black (remove-point black new-tacka))
					(removed-white (remove-point white new-tacka)))

					(move-state-one-right (move-state-one-right
						(if (and (equal removed-black black) (equal removed-white white))
								stanje
								(move-state-one-right stanje new-tacka (not oznaka)))
						tacka2 oznaka) tacka1 oznaka))
	(move-state-one-right (move-state-one-right stanje tacka1 oznaka) tacka2 oznaka)))

(defun move-state-one-right (stanje tacka oznaka) "CHECKED"
	(let*((new-tacka (cons  (car tacka) (list (1+ (cadr tacka)))))
				(white (state-white stanje))
				(black (state-black stanje))
				(empty (state-empty stanje)))
				(if oznaka
					(get-state
						 (add-point (remove-point white tacka) new-tacka)
						 black
						 (add-point (remove-point empty new-tacka) tacka))
					(get-state
							white
							(add-point (remove-point black tacka) new-tacka)
						 	(add-point (remove-point empty new-tacka) tacka))
				 )))

(defun kraj (stanje) "CHECKED"
	(or (not (reduce #'my-or (mapcar (lambda (x) (cadr x)) (state-white stanje))))
			(not (reduce #'my-or (mapcar (lambda (x) (cadr x)) (state-black stanje))))))

(defun stampaj (stanje) "CHECKED" ; TODO Ponovo implementirati kada se sredi GUI
	(format t "~%White:~%")
	(print-list (state-white stanje))
	(format t "~%Black:~%")
	(print-list (state-black stanje))
	(format t "~%Empty:~%")
	(print-list (state-empty stanje)))

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
