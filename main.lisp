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
									(oznaka (if igrac 'w 'b))
									(izlaz (postavi stanje oznaka tacke smer)))
									(if (equal stanje izlaz) (unesi stanje igrac) izlaz))))


(defun postavi (stanje oznaka tacke smer); "CHECKED" (tacke = ((D 4) (E 3) nil)) 					; moja modifikovana funkcija, treba da vrati strukturu stanje sa izmenama
	(cond
		((= desno smer)			(update-state-desno stanje tacke oznaka))								; "CHECKED"
		((= gore-desno smer)(update-state-gore-desno stanje tacke oznaka))
		((= gore-levo smer)	(update-state-gore-levo stanje tacke oznaka))
		((= levo smer)			(update-state-levo stanje tacke oznaka))
		((= dole-levo smer)	(update-state-dole-levo stanje tacke oznaka))
		((= dole-desno smer)(update-state-dole-desno stanje tacke oznaka))
		(t (error "~%Unet smer nije validan!!!"))))

(defun update-state-desno (stanje tacke oznaka) "CHECKED"
	(cond
		((caddr tacke) nil); TODO ako su tri tacke
		((cadr tacke) t); TODO ako su 2 tacke
		(t (move-state-one-right stanje (car tacke) oznaka))))

(defun move-state-two-right (stanje tacka1 tacka2 oznaka); TODO izmeniti svuda da je oznaka True za WHITE, a nil za BLACK
	(if (equal (car tacka1) (car tacka2))
		(let*((new-tacka (cons  (car tacka) (list (1+ (cadr tacka))))) ; TODO nakon izmene svuda, optimalozovati algoritam
					(pushed-tacka (cons (car tacka) (list (1+ (cadr new-tacka)))))
					(white (state-white stanje))
					(black (state-black stanje))
					(empty (state-empty stanje))
					(removed-black (remove-point black new-tacka))
					(removed-white (remove-point white new-tacka))

					(if (equal oznaka 'w)
						(move-state-one-right (move-state-one-right
									(if (equal removed-black black)
												stanje
												(move-state-one-right stanje new-tacka 'b)) tacka2 'w) tacka1 'w)
						(move-state-one-right (move-state-one-right
									(if (equal removed-white white)
												stanje
												(move-state-one-right stanje new-tacka 'w)) tacka2 'b) tacka1 'b)

	(move-state-one-right (move-state-one-right stanje tacka1 oznaka) tacka2 oznaka)
)
(defun move-state-one-right (stanje tacka oznaka) "CHECKED"
	(let*((new-tacka (cons  (car tacka) (list (1+ (cadr tacka)))))
				(white (state-white stanje))
				(black (state-black stanje))
				(empty (state-empty stanje)))
				(if (equal oznaka 'w)
					(get-state
						 (add-point (remove-point white tacka) new-tacka)
						 black
						 (add-point (remove-point empty new-tacka) tacka))
					(get-state
							white
							(add-point (remove-point black tacka) new-tacka)
						 	(add-point (remove-point empty new-tacka) tacka))
				 )))

(defun remove-point (state-paremeter tacka) "CHECKED"
		(apply 'list (mapcar (lambda (x)	(if (equal (car x) (car tacka))
																				(list (car x) (remove (cadr tacka) (cadr x))) x))
																				 state-paremeter)))
(defun add-point (state-paremeter tacka)		"CHECKED"
		(apply 'list (mapcar (lambda (x)	(if (equal (car x) (car tacka))
																				(list (car x) (append (cdr tacka) (cadr x))) x))
																				 state-paremeter)))


(defun kraj (stanje) "CHECKED"
	(or (not (reduce #'my-or (mapcar (lambda (x) (cadr x)) (state-white stanje))))
			(not (reduce #'my-or (mapcar (lambda (x) (cadr x)) (state-black stanje))))))

(defun stampaj (stanje) "CHECKED" ; TODO Ponov implementirati kada se sredi GUI
	(format t "~%White:~%")
	(print-list (state-white stanje))
	(format t "~%Black:~%")
	(print-list (state-black stanje))
	(format t "~%Empty:~%")
	(print-list (state-empty stanje)))



(setq occupieded (occupied black white))
(trace get-empty)
	(print-list (get-empty occupieded '(5 6 7 8 9)))
(untrace get-empty)
