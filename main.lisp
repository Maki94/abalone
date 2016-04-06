(require "init.lisp")

(defun abalone ()
	(let* ((stanje (init-state))
		(prvi (progn
			(format t "~%Unesite ko igraprvi (w ili b): ")
			(read)))
		(racunar (progn
			(format t "~%Unesite r ako racunar igra prvi (r ili c): ")
			(read)))
		(igrac (if (equal prvi 'w) t '()))
		(auto (if (equal racunar 'r) t '())))
	(igraj stanje igrac auto)))

(defun igraj (stanje igrac auto)																								; igrac je True ako igra white, auto je True ako igra komijuter
	(let* ((nstanje (if (not auto)																								; nstanje sadri strukturu nakon odigranog poteza
									; (car (minimax stanje 4 igrac t))
										(unesi stanje igrac)))) 																		; unesi vraca strukturu tipa state, koja vraca novo stanje
					(progn (stampaj nstanje)																							; odstampaj tabelu nakon odigranog poteza
						(if (not (kraj nstanje))																						; ako nije kraj igre
							(let* ((nnstanje (if auto																					; i ne igra kompijuter, onda igra neko drugi umesto njega
																(unesi nstanje (not igrac))											; odigraj ponovo potez
																;(car (minimax nstanje 4 (not igrac) t)))))
								(progn (stampaj nnstanje)																				; na kraju stapaj ovo stanje
									(if (not (kraj nnstanje))																			; da li je kraj?
									(igraj nnstanje igrac auto))))))))														; rekurzivno dok neko ne izgubi

(defun unesi (stanje igrac)
	(progn	(format t "~%Unesite potez (potez oblika (((D 4) (E 5) nil) 5)): ")
					(let* (	(potez (read))
									(smer (cdr potez))
									(tacke (car potez))
									(oznaka (if igrac 'w 'b))
									(izlaz (postavi stanje oznaka tacke smer)))
									(if (equal stanje izlaz) (unesi stanje igrac) izlaz)))

(defun postavi (stanje oznaka tacke smer); (tacke = ((D 4) (E 3) nil)) 					; moja modifikovana funkcija, treba da vrati strukturu stanje sa izmenama
	(if (valid stanje)
		(if (equal oznaka 'w)
			(update-state-white stanje tacke smer)
			(update-state-black stanje tacke smer))
		(error "Potez nije validan")))


(defun update-state-white (stanje tacke smer) ; vraca izmenjeno stanje

)

(defun kraj (stanje)
	(and 	(apply 'or (mapcar (lambda (x) (if (cdr x) t )) stanje-white))
				(apply 'or (mapcar (lambda (x) (if (cdr x) t )) stanje-black))))

(defun stampaj (stanje)
	(format t "~%White:~%")
	(print-list stanje-white)
	(format t "~%Black:~%")
	(print-list stanje-black)
	(format t "~%Empty:~%")
	(print-list stanje-empty))



(setq occupieded (occupied black white))
(trace get-empty)
	(print-list (get-empty occupieded '(5 6 7 8 9)))
(untrace get-empty)
