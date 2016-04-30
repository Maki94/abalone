(require "minimax.lisp")
"
function alphabeta(node, depth, α, β, Player)
    if  depth = 0 or node is a terminal node
        return score
    if  Player = MaxPlayer
        for each child of node
            α := max(α, alphabeta(child, depth-1, α, β, not(Player) ))
            if β ≤ α
                break                             (* Beta cut-off *)
        return α
    else
        for each child of node
            β := min(β, alphabeta(child, depth-1, α, β, not(Player) ))
            if β ≤ α
                break                             (* Alpha cut-off *)
        return β
"
(defun alphabeta (stanje depth alpha beta player)
  (cond
    ((= depth -1) (list stanje (proceni-stanje stanje)))
    (player (progn
              (mapcar (lambda (x) (progn
                (setq pom (alphabeta x (1- depth) alpha beta (not player)))
                (setq alpha (max alpha (cadr pom)))
                ;(setq alpha (max alpha (cadr (alphabeta x (1- depth) alpha beta (not player)))))
                (if (<= beta alpha) (return-from alphabeta (list (car pom) alpha)))
                )) (nova-stanja stanje player (actions stanje player)))
              (list (car pom) alpha)))
    (t   (progn
            (mapcar (lambda (x) (progn
              (setq pomb (alphabeta x (1- depth) alpha beta (not player)))
              (setq beta (min beta (cadr pomb)))
              ;(setq beta (min beta (cadr (alphabeta x (1- depth) alpha beta (not player)))))
              (if (<= beta alpha) (return-from alphabeta (list (car pomb) beta)))
              )) (nova-stanja stanje player (actions stanje player)))
              (list (car pomb) beta)))))
;(trace alphabeta)
;(print-list (alphabeta poc 2 -1 2000 t))
(defun test-procena (sledbenici)
  (if sledbenici
    (progn
      (format t "~%")
      (print-list (proceni-stanje (car sledbenici)))
      (format t "~%")
      (stampaj (car sledbenici))
      (format t "~%")
      (test-procena (cdr sledbenici)))))
;(test-procena (nova-stanja poc t (actions poc t)))
(setq alphabeta-list (alphabeta poc 2 -1 200000000 t))
(print-table  (car alphabeta-list)) (format t "~%") (print-list  (cadr alphabeta-list))
;(setq alphabeta-list (alphabeta (car alphabeta-list) 1 -1 200000000 t))
;(trace alphabeta)
;(print-table  (car alphabeta-list)) (format t "~%") (print-list  (cadr alphabeta-list))
