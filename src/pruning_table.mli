(*
  File: pruning_table.mli
  Module purpose: store A* heuristic to enable graph search pruning.
  Status: not started.

  Detailed description:
    These tables will hold the A* heuristic for the graph search and
    enable the solver to prune off cube states that are not easily solved.
    The hueristic is some integer for each coordinate value, which is
    not predictable and must be calculated and stored.

  Dependencies:
    None
*)