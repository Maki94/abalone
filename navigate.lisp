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
						 	(add-point (remove-point empty new-tacka) tacka)))))

;;;+++++++++++++++++++ NAVIGATE LEFT +++++++++++++++++++

(defun move-state-three-left (stanje tacka1 tacka2 tacka3 oznaka) "CHECKED"
	(if (equal3 (car tacka1) (car tacka2) (car tacka3))
		(let*((new-tacka1 (cons  (car tacka3) (list (1- (cadr tacka3)))))
					(new-tacka2 (cons  (car new-tacka1) (list (1- (cadr new-tacka1)))))
					(white (state-white stanje))
					(black (state-black stanje))
					(removed-black (remove-point (remove-point black new-tacka1) new-tacka2))
					(removed-white (remove-point (remove-point white new-tacka1) new-tacka2)))

					(move-state-one-left (move-state-one-left (move-state-one-left
						(if (and (equal removed-black black) (equal removed-white white))
								stanje
								(move-state-one-left (move-state-one-left stanje new-tacka2 (not oznaka))
									new-tacka1 (not oznaka)))
						tacka3 oznaka) tacka2 oznaka) tacka1 oznaka))
		(move-state-one-left (move-state-one-left (move-state-one-left stanje
		 tacka1 oznaka) tacka2 oznaka) tacka3 oznaka)))

(defun move-state-two-left (stanje tacka1 tacka2 oznaka) "CHECKED"
	(if (equal (car tacka1) (car tacka2)) ; ako se nalaze u istoj koloni mogu da guraju
		(let*((new-tacka (cons  (car tacka2) (list (1- (cadr tacka2)))))
					(white (state-white stanje))
					(black (state-black stanje))
					(removed-black (remove-point black new-tacka))
					(removed-white (remove-point white new-tacka)))

					(move-state-one-left (move-state-one-left
						(if (and (equal removed-black black) (equal removed-white white))
								stanje
								(move-state-one-left stanje new-tacka (not oznaka)))
						tacka2 oznaka) tacka1 oznaka))
	(move-state-one-left (move-state-one-left stanje tacka1 oznaka) tacka2 oznaka)))

(defun move-state-one-left (stanje tacka oznaka) "CHECKED"
	(let*((new-tacka (cons  (car tacka) (list (1- (cadr tacka)))))
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
						 	(add-point (remove-point empty new-tacka) tacka)))))
;;;+++++++++++++++++++ NAVIGATE UP-RIGHT +++++++++++++++++++

(defun move-state-three-up-right (stanje tacka1 tacka2 tacka3 oznaka)
	(let*((new-tacka1 (cons  (next-char (car tacka3)) (list (1+ (cadr tacka3)))))
				(new-tacka2 (cons  (next-char (car new-tacka1)) (list (1+ (cadr new-tacka1)))))
				(white (state-white stanje))
				(black (state-black stanje))
				(removed-black (remove-point (remove-point black new-tacka1) new-tacka2))
				(removed-white (remove-point (remove-point white new-tacka1) new-tacka2)))

				(move-state-one-up-right (move-state-one-up-right (move-state-one-up-right
					(if (and (equal removed-black black) (equal removed-white white))
							stanje
							(move-state-one-up-right (move-state-one-up-right stanje new-tacka2 (not oznaka))
								new-tacka1 (not oznaka)))
					tacka3 oznaka) tacka2 oznaka) tacka1 oznaka)))

(defun move-state-two-up-right (stanje tacka1 tacka2 oznaka)
	(let*((new-tacka (cons  (next-char (car tacka2)) (list (1+ (cadr tacka2)))))
				(white (state-white stanje))
				(black (state-black stanje))
				(removed-black (remove-point black new-tacka))
				(removed-white (remove-point white new-tacka)))

				(move-state-one-up-right (move-state-one-up-right
					(if (and (equal removed-black black) (equal removed-white white))
							stanje
							(move-state-one-up-right stanje new-tacka (not oznaka)))
					tacka2 oznaka) tacka1 oznaka)))

(defun move-state-one-up-right (stanje tacka oznaka)
	(let*((new-tacka (cons  (next-char (car tacka)) (list (1+ (cadr tacka)))))
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
;;;+++++++++++++++++++ NAVIGATE UP-left +++++++++++++++++++

(defun move-state-three-up-left (stanje tacka1 tacka2 tacka3 oznaka)
	(let*((new-tacka1 (cons  (next-char (car tacka3)) (list (cadr tacka3))))
				(new-tacka2 (cons  (next-char (car new-tacka1)) (list (cadr new-tacka1))))
				(white (state-white stanje))
				(black (state-black stanje))
				(removed-black (remove-point (remove-point black new-tacka1) new-tacka2))
				(removed-white (remove-point (remove-point white new-tacka1) new-tacka2)))

				(move-state-one-up-left (move-state-one-up-left (move-state-one-up-left
					(if (and (equal removed-black black) (equal removed-white white))
							stanje
							(move-state-one-up-left (move-state-one-up-left stanje new-tacka2 (not oznaka))
								new-tacka1 (not oznaka)))
					tacka3 oznaka) tacka2 oznaka) tacka1 oznaka)))

(defun move-state-two-up-left (stanje tacka1 tacka2 oznaka)
	(let*((new-tacka (cons  (next-char (car tacka2)) (list (cadr tacka2))))
				(white (state-white stanje))
				(black (state-black stanje))
				(removed-black (remove-point black new-tacka))
				(removed-white (remove-point white new-tacka)))

				(move-state-one-up-left (move-state-one-up-left
					(if (and (equal removed-black black) (equal removed-white white))
							stanje
							(move-state-one-up-left stanje new-tacka (not oznaka)))
					tacka2 oznaka) tacka1 oznaka)))

(defun move-state-one-up-left (stanje tacka oznaka)
	(let*((new-tacka (cons  (next-char (car tacka)) (list (cadr tacka))))
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
;;;;+++++++++++++++++++ NAVIGATE DOWN-RIGHT +++++++++++++++++++

(defun move-state-three-down-right (stanje tacka1 tacka2 tacka3 oznaka)
	(let*((new-tacka1 (cons  (prev-char (car tacka3)) (list (cadr tacka3))))
				(new-tacka2 (cons  (prev-char (car new-tacka1)) (list (cadr new-tacka1))))
				(white (state-white stanje))
				(black (state-black stanje))
				(removed-black (remove-point (remove-point black new-tacka1) new-tacka2))
				(removed-white (remove-point (remove-point white new-tacka1) new-tacka2)))

				(move-state-one-down-right (move-state-one-down-right (move-state-one-down-right
					(if (and (equal removed-black black) (equal removed-white white))
							stanje
							(move-state-one-down-right (move-state-one-down-right stanje new-tacka2 (not oznaka))
								new-tacka1 (not oznaka)))
					tacka3 oznaka) tacka2 oznaka) tacka1 oznaka)))

(defun move-state-two-down-right (stanje tacka1 tacka2 oznaka)
	(let*((new-tacka (cons  (prev-char (car tacka2)) (list (cadr tacka2))))
				(white (state-white stanje))
				(black (state-black stanje))
				(removed-black (remove-point black new-tacka))
				(removed-white (remove-point white new-tacka)))

				(move-state-one-down-right (move-state-one-down-right
					(if (and (equal removed-black black) (equal removed-white white))
							stanje
							(move-state-one-down-right stanje new-tacka (not oznaka)))
					tacka2 oznaka) tacka1 oznaka)))

(defun move-state-one-down-right (stanje tacka oznaka)
	(let*((new-tacka (cons  (prev-char (car tacka)) (list (cadr tacka))))
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
;;;;;+++++++++++++++++++ NAVIGATE DOWN-LEFT +++++++++++++++++++

(defun move-state-three-down-left (stanje tacka1 tacka2 tacka3 oznaka)
	(let*((new-tacka1 (cons  (prev-char (car tacka3)) (list (1- (cadr tacka3)))))
				(new-tacka2 (cons  (prev-char (car new-tacka1)) (list (1- (cadr new-tacka1)))))
				(white (state-white stanje))
				(black (state-black stanje))
				(removed-black (remove-point (remove-point black new-tacka1) new-tacka2))
				(removed-white (remove-point (remove-point white new-tacka1) new-tacka2)))

				(move-state-one-down-left (move-state-one-down-left (move-state-one-down-left
					(if (and (equal removed-black black) (equal removed-white white))
							stanje
							(move-state-one-down-left (move-state-one-down-left stanje new-tacka2 (not oznaka))
								new-tacka1 (not oznaka)))
					tacka3 oznaka) tacka2 oznaka) tacka1 oznaka)))

(defun move-state-two-down-left (stanje tacka1 tacka2 oznaka)
	(let*((new-tacka (cons  (prev-char (car tacka2)) (list (1- (cadr tacka2)))))
				(white (state-white stanje))
				(black (state-black stanje))
				(removed-black (remove-point black new-tacka))
				(removed-white (remove-point white new-tacka)))

				(move-state-one-down-left (move-state-one-down-left
					(if (and (equal removed-black black) (equal removed-white white))
							stanje
							(move-state-one-down-left stanje new-tacka (not oznaka)))
					tacka2 oznaka) tacka1 oznaka)))

(defun move-state-one-down-left (stanje tacka oznaka)
	(let*((new-tacka (cons  (prev-char (car tacka)) (list (1- (cadr tacka)))))
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
