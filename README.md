# Abalone
Two-player board game *Abalone*. The purpose of the project is to apply heuristic based search algorithms for problem solving. 

## Workflow
-  Initial state
-   Player(s)  
      - Which player has the move
-   Actions(s)
      - Returns the set of all legal moves in a state
-   Result(s, a)
      - Defines the result of a move.
-   Terminal-Test(s)
      - A terminal test, which is true when the game is over and false otherwise.
-   Utility(s, p)
      - A utility function defines numeric value (+1 white won, -1 black won)

> The `initial state`, `Actions`, and `Result` functions define the game tree for the game.
