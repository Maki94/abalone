(defun print-table (stanje)
  (let* ((gap 4)
        (white (state-white stanje))
        (black (state-black stanje)))
        (progn  (print-empty-fields (+ 4  gap))
                (format t "5 6 7 8 9 ~%")
                (print-table-handler white black gap t)
                (print-empty-fields (+ 6  gap))
                (format t "1 2 3 4 5 ~%"))))

(defun print-empty-fields (gap)
  (if (>= gap 0) (progn (format t " ") (print-empty-fields (1- gap)))))

(defun print-row (white-row black-row start end) ; -row = (5 6)
  (cond
    ((= start end) nil)
    ((find start white-row) (progn (format t "w ") (print-row (delete start white-row) black-row (1+ start) end)))
    ((find start black-row) (progn (format t "b ") (print-row white-row (delete start black-row) (1+ start) end)))
    (t (progn (format t "- ") (print-row white-row black-row (1+ start) end)))))

(defun print-table-handler (white black gap down) ; init gap = 4; down = t
  (if white
    (progn
      (print-list (caar white))
      (print-empty-fields gap)
      (if (or (= gap 0) (not down)) (format t "  ") (format t "~D " gap))
      (cond
        ((= gap 0) (print-row (cadar white) (cadar black) 1 10))
        (down (print-row (cadar white) (cadar black) (1+ gap) 10))
        (t (print-row (cadar white) (cadar black) 1 (- 10 gap))))
      (if (or (= gap 0) down) (format t "~%") (format t "~D~%" (- 10 gap)))
      (cond
        ((= gap 0) (print-table-handler (cdr white) (cdr black) 1 nil))
        (down (print-table-handler (cdr white) (cdr black) (1- gap) t))
        (t (print-table-handler (cdr white) (cdr black) (1+ gap) nil))))))
