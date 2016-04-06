(defconstant desno 1)
(defconstant gore-desno 2)
(defconstant gore-levo 3)
(defconstant levo 4)
(defconstant dole-levo 5)
(defconstant dole-desno 6)

(setq rows '(I H G F E D C B A))
(setq columns '(1 2 3 4 5 6 7 8 9))
(setq white '((I (5 6)) (H (4 5 6)) (G (4 5)) (F nil) (E nil) (D nil) (C (5 6)) (B (4 5 6)) (A (4 5))))
(setq black '((I (8 9)) (H (7 8 9)) (G (7 8)) (F nil) (E nil) (D nil) (C (2 3)) (B (1 2 3)) (A (1 2))))

(defstruct state
	white
	black
	empty)
