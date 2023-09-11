(*

We use the "is replaced by" representation for moves on the cubie level.

So F(URF) = UFL means that what was in UFL will be moved to URF.

A move will map every cubie on the cube to some other cubie with new orientation.
   
*)

module T : sig
  type t = Cubie.t -> Cubie.t
end

module Faceturn : sig
  type t = U | R | F | D | B | L [@@deriving variants]

  val to_move : t -> T.t
end

(* I might consider making a `Non_identity_move` to track the 18 moves we use when searching *)

module Fixed_move : sig
  type t = { faceturn: Faceturn.t ; count : Modular_int.Z4.t }
  [@@deriving enumerate]

  val n : int (* number of possible fixed moves *) (* I should consider removing the identity move *)
  val to_rank : t -> int
  val to_move : t -> T.t
end

type t = T.t