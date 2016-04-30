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
    ((= depth 0) (proceni-stanje stanje))
    (player
      (progn
        (mapcar (lambda (x)
          (progn
          (setq alpha (max alpha (alphabeta x (1- depth) alpha beta (not player))))
          (if (<= beta alpha) (return-from alphabeta alpha))
          )
          ) (nova-stanja stanje player (actions stanje player)))
        alpha))
    ((not player)
        (progn
          (mapcar (lambda (x)
            (progn
            (setq beta (min beta (alphabeta x (1- depth) alpha beta (not player))))
            (if (<= beta alpha) (return-from alphabeta beta))
            )
            ) (nova-stanja stanje player (actions stanje player)))
            beta))
    (t error "pozvan je T")))
;
;(defun alphabeta (stanje depth alpha beta player)
;  (cond ((= depth 0) (proceni-stanje stanje))
;        (player (progn (mapcar (lambda (child)  (progn
;                      (setq a (max alpha (alphabeta child (1- depth) alpha beta (not player))))
;                      ;(if (<= beta a) (return-from alphabeta a))
;                      )
;                  ) (nova-stanja stanje player (actions stanje player)))
;                  (return-from alphabeta alpha)))
;        (t (mapcar (progn (lambda (child) (progn
;          (print-list alpha)
;          (print-list alpha)
;          (print-list alpha)
;                      (setq b (min beta (alphabeta child (1- depth) alpha beta (not player))))
;                      ;(if (<= b alpha) (return-from alphabeta b))
;                      )
;                  ) (nova-stanja stanje player (actions stanje player))
;                  (return-from alphabeta beta))))))

;(trace alphabeta)
(print-list (alphabeta poc 2 -1 2000 t))
