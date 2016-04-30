(defun init-rows ()
	(list '(I H G F E D C B A)))


(defun init-columns ()
	(list '(1 2 3 4 5 6 7 8 9)))


(defun init-white ()
	 '((I (5 6)) (H (4 5 6)) (G (4 5 6)) (F nil) (E nil) (D nil) (C (5 6)) (B (4 5 6)) (A (4 5))))


(defun init-black ()
	'((I (8 9)) (H (7 8 9)) (G (7 8)) (F (5)) (E nil) (D nil) (C (2 3)) (B (1 2 3)) (A (1 2))))


(defun init-empty ()
	(get-empty (occupied (init-black) (init-white)) '(5 6 7 8 9)))

(defun init-state ()
	(make-state
		:white (init-white)
		:black (init-black)))

(defstruct state
	white
	black)
