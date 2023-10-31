(*
  File: pruning_table.mli
  Module purpose: store A* heuristic to enable graph search pruning.
  Status: not started.

  Detailed description:
    These tables will hold the A* heuristic for the graph search and
    enable the solver to prune off cube states that are not easily solved.
    The heuristic is some integer for each coordinate value, which is
    not predictable and must be calculated and stored.

  Dependencies:
    None
*)


(*
  Pruning tables are made by starting from goal state and applying all moves.
  Never overwrite values in the table unless they are the initial value (inf).

  For each cube, need to apply all symmetries and check which are an identical
  permutation.

  Then fill in all cubes as one move further away from the goal state.
*)

module Phase1 :
  sig

  (*
    Needs implementation.
  *)

  end