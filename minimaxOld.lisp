(defun nova-stanja (stanje player akcije)
  (if akcije
    (append (list (postavi stanje player (car (car akcije)) (cadr (car akcije)))) (nova-stanja stanje player (cdr akcije)))))

(defun proceni-stanje (state) (random 10000))

(defun max-stanje (stanje)
  (max-stanje-i (cdr stanje) (car stanje)))

(defun max-stanje-i (lsv stanje-vrednost)
  (cond
    ((null lsv) stanje-vrednost)
    ((> (cadar lsv) (cadr stanje-vrednost)) (max-stanje-i (cdr lsv) (car lsv)))
    (t (max-stanje-i (cdr lsv) stanje-vrednost))))

(defun min-stanje (lista-stanja) ; lista-stanja = ((stanje heruistika) (stanje heruistika) ...)
  (min-stanje-i (cdr lista-stanja) (car lista-stanja)))

(defun min-stanje-i (lsv stanje-vrednost) ; lsv = vrednost, stanje-vrednost = stanje
  (cond
    ((null lsv) stanje-vrednost)
    ((< (cadar lsv) (cadr stanje-vrednost)) (min-stanje-i (cdr lsv) (car lsv)))
    (t (min-stanje-i (cdr lsv) stanje-vrednost))))

(defun minimax (stanje dubina player)
  (let* ((akcije (actions stanje player)) ; akcije su svi validni potezi koji se mogu odigrati iz datog stanja
        (lp (nova-stanja stanje player akcije))
        (f (if player 'max-stanje 'min-stanje)))
        (cond
          ((or (zerop dubina) (null lp)) (list stanje (proceni-stanje stanje)))
          (t (apply f (list (mapcar (lambda (x) (minimax x (1- dubina) (not player))) lp)))))))
