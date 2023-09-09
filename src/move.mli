(*

We use the "is replaced by" representation for moves on the cubie level.

So F(URF) = UFL means that what was in UFL will be moved to URF.
   
*)

module T : sig
  type t = Cubie.t -> Cubie.t
end

module Twist : sig
  type t = U | R | F | D | B | L

  val to_move : t -> T.t
end

module Fixed_moves : sig
  type t = { twist: Twist.t ; count : Modular_int.Z4.t }
  [@@deriving enumerate]

  val to_move : t -> T.t
end

(* We only have so many moves, so I should consider that the move type is a variant *)
(* I prefer that the domain includes orientation *)
(* type t = Cubie.t -> Cubie.t *)
type t = T.t
(* should I define multiplication? It would be much easier to define multiplication on a list; harder on a function *)