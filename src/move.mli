(*

We use the "is replaced by" representation for moves on the cubie level.

So F(URF) = UFL means that what was in UFL will be moved to URF.

A move will map every cubie on the cube to some other cubie with new orientation.
   
*)

module T : sig
  type t = Cubie.t -> Cubie.t
end

type t = T.t

module Faceturn : sig
  type t = U | R | F | D | B | L [@@deriving variants]

  val to_move : t -> T.t
end

(* This is the signature for single face turns. *)
(* We do not allow count to be zero in `all` or in `to_rank` *)
module Fixed_move : 
  sig
    type t = { faceturn : Faceturn.t ; count : Modular_int.Z4.t }
    val all : t list
    val n : int (* number of possible moves -- length of `n` -- number of non-identity moves *)
    val to_rank : t -> int (* not defined on count = 0 *)
    val to_move : t -> T.t
  end
