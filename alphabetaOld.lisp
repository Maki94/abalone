(defun alphabeta (stanje dubina alpha-stanje beta-stanje player)
        (cond
          ((zerop dubina) (list stanje (proceni-stanje stanje)))
          (player
              (mapcar (lambda (x)
                  (setq alpha-stanje (max-s alpha-stanje (alphabeta x (1- dubina) alpha-stanje beta-stanje (not player))))
                  (if (>= (cadr alpha-stanje) (cadr beta-stanje)) (return-from alphabeta beta-stanje)))
                 (nova-stanja stanje player (actions stanje player)))
              alpha-stanje)
          (t
            (mapcar (lambda (x)
                  (setq beta-stanje (min-s beta-stanje (alphabeta x (1- dubina) alpha-stanje beta-stanje (not player))))
                  (if (<= (cadr beta-stanje) (cadr alpha-stanje)) (return-from alphabeta alpha-stanje)))
                (nova-stanja stanje player (actions stanje player)))
              beta-stanje)))

(defun max-value (stanje dubina alpha-stanje beta-stanje)
		(cond
			((zerop dubina) (list stanje (proceni-stanje stanje)))
			(t (progn
					(mapcar (lambda (x)
						(progn
							(setq alpha-stanje (max-s alpha-stanje (min-value x (1- dubina) alpha-stanje beta-stanje)))
              (format t "alpha-stanje=") (print-list alpha-stanje) (format t "~%")
							(if (>= (cadr alpha-stanje) (cadr beta-stanje)) (return-from max-value beta-stanje)))
						) (nova-stanja stanje t (actions stanje t)))
					alpha-stanje))))
;
(defun min-value (stanje dubina alpha-stanje beta-stanje)
		(cond
			((zerop dubina) (list stanje (proceni-stanje stanje)))
			(t (progn
					(mapcar (lambda (x)
						(progn
              (setq beta-stanje (min-s beta-stanje (max-value x (1- dubina) alpha-stanje beta-stanje)))
              ;(format t "beta-stanje=") (print-list beta-stanje) (format t "~%")
							(if (<= (cadr beta-stanje) (cadr alpha-stanje)) (return-from min-value alpha-stanje)))
						)(nova-stanja stanje nil (actions stanje nil)))
					beta-stanje))))
