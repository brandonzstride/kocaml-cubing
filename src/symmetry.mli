
module type S = 
  sig
    type t
    val mult : t -> t -> t
    val inverse : t -> t
    (* how a symmetry s acts on a perm p by s * p * s^-1 *)
    val on_perm : t -> Perm.t -> Perm.t
    (* How a symmetry s acts on a move m by s * m * s^-1 *)
    val on_move : t -> Move.Fixed_move.t -> Move.Fixed_move.t
    val to_rank : t -> int
    val n : int
    val next : t -> t option
    val zero : t
  end

(* Issue is that coordinate currently refers to this S, but
   I need it to more generically refer to any of type S. *)
module S : S

module type Memoization =
  sig
    val is_already_saved : bool
    val save_location : string
  end

module Memoize (S : S) (_ : Memoization) : S with type t := S.t
