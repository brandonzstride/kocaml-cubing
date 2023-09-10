
type t

val mult : t -> t -> t
val inverse : t -> t

(*
  Symmetry S and perm P => S * P * S^-1   
*)
val conjugate : t -> Perm.t -> Perm.t 

(*
  How the symmetry S acts on a move m by S * m * S^-1
*)
val on_move : t -> Move.Fixed_move.t -> Move.Fixed_move.t

val to_rank : t -> int
val n : int 
