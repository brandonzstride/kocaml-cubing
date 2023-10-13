(*
  File: move.mli
  Module purpose: represents faceturns on the Rubik's cube, i.e. valid moves.
  Status: complete.

  Detailed description:
    This module describes some move on the cube. These moves might be
    turns of the faces or compositions of faceturns. Arbitrary moves
    are supported by the type as functions from cubies to cubies with
    orientation, but they are not the intended purpose of this module.
    See the `Perm` module for such a purpose.

    A move uses the "is replaced by" notation: if a move is called f, then
    f(URF) = (UFL, 1) means that the URF cubie position on the cube will get
    replaced by the cubie that was in the UFL position, and it will gain
    orientation 1.
  
  Dependencies:
    Cubie -- moves directly permute the cubies
    Modular_int -- powers of up to four faceturns make up a move
*)

module T :
  sig
    type t = Cubie.t -> Cubie.With_orientation.t
  end

type t = T.t

val id : t

(* Since domain is finite and behavior of moves are limited, we can compare *)
val equal : t -> t -> bool
val equal_without_orientation : t -> t -> bool

(* Compose the two moves. The left is applied first, so (f * g)(x) ~= g(f(x)) *)
(* This isn't typical, but it's noted in the permutation wikipedia that this is sometimes done *)
val ( * ) : t -> t -> t

module Faceturn : sig
  type t = U | R | F | D | B | L [@@deriving variants, sexp, compare]

  val to_move : t -> T.t
end

(* This is the signature for multiple turns of one face *)
(* We do not allow count to be zero in `all` or in `to_rank` *)
module Fixed_move : 
  sig
    type t = { faceturn : Faceturn.t ; count : Modular_int.Z4.t } [@@deriving sexp, compare]
    val all : t list (* all non-identity moves *)
    val all_g1 : t list (* all non-identity moves that generate g1 *)
    val n : int (* number of possible moves -- length of `all` -- number of non-identity moves *)
    val to_rank : t -> int (* not defined on count = 0 *)
    val to_move : t -> T.t

    val random_list : int -> t list
    val random_g1_list : int -> t list
  end
