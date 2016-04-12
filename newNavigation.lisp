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
