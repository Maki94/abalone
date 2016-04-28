(defun nova-stanja (stanje player akcije)
  (if akcije
    (let* (	(potez (car akcije))
            (smer (cadr potez))
            (tacke (car potez))
            (izlaz (postavi stanje player tacke smer)))

            (append (list izlaz) (nova-stanja stanje player (cdr akcije))))))

(defun proceni-stanje (state actions) 1)

(defun max-stanje (stanje)
  (max-stanje-i (cdr stanje) (car stanje)))

(defun max-stanje-i (lsv stanje-vrednost)
  (cond
    ((null lsv) stanje-vrednost)
    ((> (cadar lsv) (cadr stanje-vrednost)) (max-stanje-i (cdr lsv) (car lsv)))
    (t (max-stanje-i (cdr lsv) stanje-vrednost))))

(defun min-stanje (stanje)
  (min-stanje-i (cdr stanje) (car stanje)))

(defun min-stanje-i (lsv stanje-vrednost)
  (cond
    ((null lsv) stanje-vrednost)
    ((< (cadar lsv) (cadr stanje-vrednost)) (min-stanje-i (cdr lsv) (car lsv)))
    (t (min-stanje-i (cdr lsv) stanje-vrednost))))

(defun minimax (stanje dubina moj-potez)
  (let ((lp (nova-stanja stanje))
        (f (if moj-potez 'max-stanje 'min-stanje)))
        (cond
          ((or (zerop dubina) (null lp)) (list stanje (proceni-stanje stanje)))
          (t (apply f (list (mapcar (lambda (x) (minimax x (1- dubina) (not moj-potez))) lp)))))))
