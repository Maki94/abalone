;(require "minimax.lisp")
;
;(defun minimax (stanje dubina player)
;  (let*
;    ((akcije (actions stanje player))
;    (lp (nova-stanja stanje player akcije))
;    (f (if player 'max-value 'min-value)))
;    (cond
;      ((or (= 0 dubina) (null lp)) (list stanje (proceni-stanje stanje)))
;      (t (apply f (list (mapcar (lambda (x) (minimax x (1- dubina) (not player))) lp) dubina - 2000000 player stanje))))) )
;(defun max-value (stanje-list)
;  (alphabeta (stanje-list)
;)
;(defun max-stanje (stanje)
;  (max-stanje-i (cdr stanje) (car stanje)))
;
;(defun max-stanje-i (lsv stanje-vrednost)
;  (cond
;    ((null lsv) stanje-vrednost)
;    ((> (cadar lsv) (cadr stanje-vrednost)) (max-stanje-i (cdr lsv) (car lsv)))
;    (t (max-stanje-i (cdr lsv) stanje-vrednost))))
;
;(defun alphabeta (stanje-list depth alpha beta player stanje)
;  (cond
;    ((= depth 0) (list stanje (proceni-stanje stanje)))
;    (player (progn
;              (mapcar (lambda (x) (progn
;                (setq pom (alphabeta x (1- depth) alpha beta (not player)))
;                ;(print-list pom)
;                (setq alpha (max alpha (cadr pom)))
;                ;(setq alpha (max alpha (cadr (alphabeta x (1- depth) alpha beta (not player)))))
;                (if (<= beta alpha) (return-from alphabeta (list (car pom) alpha)))
;                )) stanje)
;              (list (car pom) alpha)))
;    (t   (progn
;            (mapcar (lambda (x) (progn
;              (setq pomb (alphabeta x (1- depth) alpha beta (not player)))
;              ;(print-list pomb)
;              (setq beta (min beta (cadr pomb)))
;              ;(setq beta (min beta (cadr (alphabeta x (1- depth) alpha beta (not player)))))
;              (if (<= beta alpha) (return-from alphabeta (list (car pomb) beta)))
;              )) (nova-stanja stanje player (actions stanje player)))
;              (list (car pomb) beta)))))
;
;(setq mm (minimax poc 1 t))
;
;(print-list mm)

;;;----------------------------------------------------------------------
;;; Tic-Tac-Toe with Minimax search and alpha-beta pruning.
;;;      by Chuck Anderson, based on algorithm in Russell & Norvig.
;;;----------------------------------------------------------------------

;;;----------------------------------------------------------------------
;;;Main function to call to start a game.
;;;----------------------------------------------------------------------
(defun ttt (player other-player board)
  (print-board board)
  (cond ((winner? other-player board) (print-winner other-player))
	((terminal? board) (print-draw))
	(t (ttt other-player player
		(make-move (get-move board player other-player) player board)))))

;;;----------------------------------------------------------------------
;;;Structure to represent a player
;;;----------------------------------------------------------------------
(defstruct (player (:print-function print-player))
  name
  marker				;X or O
  strategy				;function from board to position
  (eval-func #'utility-func)
  (cutoff-func #'cutoff-func))

(defun print-player (p stream depth)
  (format stream "~&Player ~a ~a" (player-name p) (player-marker p)))

;;;----------------------------------------------------------------------
;;;Functions for testing for winner and for terminal state
;;;----------------------------------------------------------------------
(defun winner? (player board)
  (some #'(lambda (win)
	    (every #'(lambda (p)
		       (eql (nth (- p 1) board) (player-marker player)))
		   win))
	'((1 2 3)(4 5 6)(7 8 9) (1 4 7)(2 5 8)(3 6 9) (1 5 9)(3 5 7))))

(defun terminal? (board)
  (not (member '- board)))

;;;----------------------------------------------------------------------
;;;Print results feedback
;;;----------------------------------------------------------------------
(defun print-winner (player)
  (format t "~&           Congratulations, ~a wins!" (player-name player)))

(defun print-draw ()
  (format t "~&           The game is a draw."))

;;;----------------------------------------------------------------------
;;;Update state of board
;;;----------------------------------------------------------------------
(defun make-move (move player board)
  (setf (nth (- move 1) board) (player-marker player))
  board)

;;;----------------------------------------------------------------------
;;;Apply the current player's strategy to get move
;;;----------------------------------------------------------------------
(defun get-move (board player other-player)
  (funcall (player-strategy player) board player other-player))

;;;----------------------------------------------------------------------
;;;Default evaluation function is just this utility function
;;;----------------------------------------------------------------------
(defun utility-func (board player other-player player-to-eval)
  (cond ((winner? player-to-eval board) 1)
	((winner? (if (eql player-to-eval player)
		      other-player
		    ;;else
		    player)
		  board) -1)
	(t 0)))

;;;----------------------------------------------------------------------
;;;Cut off function.  Now only true when true terminal state encountered
;;;----------------------------------------------------------------------
(defun cutoff-func (board player other-player)
  (or (winner? player board)
      (winner? other-player board)
      (not (find '- board))))

;;;----------------------------------------------------------------------
;;;Strategies
;;;----------------------------------------------------------------------

;;;Prompt human for move strategy.
(defun strategy-human (board player other-player)
  (princ "              What is your move? ")
  (loop for move = (read)
      until (and (> move 0) (< move 10))
      do (princ "                 Must be between 1 and 9.  Your move? ")
      finally (return move)))

;;;Random valid move strategy.
(defun strategy-random (board player other-player)
  (let ((dashes (loop for i from 0 for p in board if (eql p '-) collect i)))
    (1+ (nth (random (length dashes)) dashes))))

;;;----------------------------------------------------------------------
;;; alpha-beta-search, implemented using algorithm on page 132 of
;;; Russel and Norvig's text.
;;;----------------------------------------------------------------------

;;;----------------------------------------------------------------------
;;;Structure to represent a node for alpha beta search
;;;----------------------------------------------------------------------
(defstruct (node (:print-function node-print))
  (state '(- - - - - - - - -))
  move
  (depth 0))

(defun node-print (s stream d)
  (print-board (node-state s)))

(defun print-board (board)
  (format t "~%~%~{~a ~a ~a~%~a ~a ~a~%~a ~a ~a~}~%" board))

;;;----------------------------------------------------------------------
;;;Choose best next action using minimax search with alpha-beta pruning
;;;----------------------------------------------------------------------
(defun strategy-alpha-beta (board player other-player)
  (alpha-beta-search
   (make-node :state board) player other-player #'ttt-successors))

(defconstant inf excl::*infinity-single*)
(defconstant -inf (* -1 inf))

;;;----------------------------------------------------------------------
;;;alpha-beta-search returns the best successor of node, given an indicator
;;;of which player's (t or nil) turn it is, and the functions specifying
;;;the game.
;;;----------------------------------------------------------------------
(defun alpha-beta-search (node player other-player succ-func)
  (loop
      with best-value = -inf
      with best-node
      for s in (funcall succ-func node player)
      for s-value =
	(min-value s other-player player -inf inf player succ-func)
      if (> s-value best-value) do (setf best-value s-value
					 best-node s)
      finally (return (node-move best-node))))

;;;----------------------------------------------------------------------
;;; Returns the value of a node by maximizing over its successors
;;;----------------------------------------------------------------------
(defun max-value (node player other-player alpha beta player-to-eval succ-func)
  (cond ((funcall (player-cutoff-func player-to-eval)
		  (node-state node) player other-player)
	 (funcall (player-eval-func player-to-eval)
		  (node-state node) player other-player player-to-eval))
	(t (loop
	       for s in (funcall succ-func node player)
	       do (setf alpha
		    (max alpha (min-value s other-player player alpha beta 
					  player-to-eval succ-func)))
	       if (>= alpha beta) do (return beta)
	       finally (return alpha)))))

;;;----------------------------------------------------------------------
;;; Returns the value of a node by minimizing over its successors
;;;----------------------------------------------------------------------
(defun min-value (node player other-player alpha beta player-to-eval succ-func)
  (cond ((funcall (player-cutoff-func player-to-eval)
		  (node-state node) player other-player)
	 (funcall (player-eval-func player-to-eval)
		  (node-state node) player other-player player-to-eval))
	(t (loop
	       for s in (funcall succ-func node player)
	       do (setf beta
		    (min beta (max-value s other-player player alpha beta
					player-to-eval succ-func)))
	       if (>= alpha beta) do (return alpha)
	       finally (return beta)))))

;;;----------------------------------------------------------------------
(defun ttt-successors (node player)
  (loop
      with state = (node-state node)
      for mark in state
      for i from 0
      if (eql mark '-) collect (make-node
				:state
				(let ((new-state (copy-list state)))
				  (setf (nth i new-state)
				    (player-marker player))
				  new-state)
				:move (1+ i)
				;;depth currently not used anywhere
				:depth (1+ (node-depth node)))))

;;;----------------------------------------------------------------------
;;;----------------------------------------------------------------------
;;; Examples
;;;----------------------------------------------------------------------

#|

(ttt (make-player :name "Chuck" :marker 'X :strategy #'strategy-human)
     (make-player :name "Elvis" :marker 'O :strategy #'strategy-random)
     '(- - - - - - - - -))

(ttt (make-player :name "A" :marker 'X :strategy #'strategy-random)
     (make-player :name "B" :marker 'O :strategy #'strategy-random)
     '(- - - - - - - - -))

(ttt (make-player :name "A-B" :marker 'X :strategy #'strategy-alpha-beta)
     (make-player :name "Random" :marker 'O :strategy #'strategy-random)
     '(- - - - - - - - -))

(ttt (make-player :name "Me" :marker 'X :strategy #'strategy-human)
     (make-player :name "Alpha-Beta" :marker 'O :strategy #'strategy-alpha-beta)
     '(- - - - - - - - -))

(ttt
 (make-player :name "Alpha-Beta" :marker 'X :strategy #'strategy-alpha-beta)
 (make-player :name "Human" :marker 'O :strategy #'strategy-human)
     '(- - - - - - - - -))

(ttt
 (make-player :name "Xab" :marker 'X :strategy #'strategy-alpha-beta)
 (make-player :name "Oab" :marker 'O :strategy #'strategy-alpha-beta)
     '(- - - - - - - - -))

output from this last example:

- - -
- - -			X X O
- - -			- O -
			X - -

X - -
- - -			X X O
- - -			O O -
			X - -

X - -
- O -			X X O
- - -			O O X
			X - -

X X -
- O -			X X O
- - -			O O X
			X O -

X X O
- O -			X X O
- - -			O O X
			X O X
			           The game is a draw.
			NIL
|#
