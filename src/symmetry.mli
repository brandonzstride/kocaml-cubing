
type t
(* Compose the two symmetries. The second is applied first. *)
val mult : t -> t -> t
val inverse : t -> t
(* how a symmetry s acts on a perm p by s * p * s^-1 *)
val on_perm : t -> Perm.t -> Perm.t
(* How a symmetry s acts on a move m by s * m * s^-1 *)
val on_move : t -> Move.Fixed_move.t -> Move.Fixed_move.t
val to_rank : t -> int
val of_rank : int -> t
val n : int
val next : t -> t option
val zero : t
val all : t list
