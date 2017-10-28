# Abalone

## Project workflow
1.  Initial state
      - Which specifies how the game is set up at the start
-   Player(s)  
      - Defines which player has the move in a state
-   Actions(s)
      - Returns the set of legal moves in a state
-   Result(s, a)
      - The transition model, which defines the result of a move.
-   Terminal-Test(s)
      - A terminal test, which is true when the game is over and false otherwise. States where the game has ended are called terminal state
-   Utility(s, p)
      - A utility function defines the final numeric value for a game that ends in terminal state (+1 white won, -1 black won)

> Note: The `initial state`, `Actions`, and `Result` functions define the game tree for the game.

## Decisions
1.  Decisions
