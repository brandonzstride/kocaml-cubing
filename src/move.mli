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

    There are two submodules holding the fixed moves. These are All_fixed_move,
    which is all the moves and their multiples that generate the Rubik's cube group,
    and G1_fixed_move, which is all the moves and their multiples that generate the
    G1 subgroup of the Rubik's cube group. These are split so that 
  
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

module type Generator =
  sig
    type t [@@deriving enumerate, sexp, compare]
    val to_move : t -> T.t
    val to_rank : t -> int
  end

(* These are to expose the generator types. There has got to be a more elegant way to do this... *)
(* module G_T :
  sig
    type t = U | R | F | D | B | L
  end

module G1_T :
  sig
    type t = U | D | R2 | F2 | B2 | L2
  end *)

(* These moves generate the whole cube *)
module Generator : Generator
(* These generate the G1 subgroup of the cube *)
module G1_generator : Generator

module Fixed_move :
  sig
    (* This is the super type. All S.t can convert to and from it *)
    (* TODO: consider giving this full implementation so users aren't using G below *)
    module Super :
      sig
        type t
      end

    module type S =
      sig
        module Generator : Generator
        type t [@@deriving sexp, compare]
        val of_gen : Generator.t -> t (* count = 1 *)
        val of_generator_and_count : Generator.t -> int -> t
        val to_generator_and_count : t -> (Generator.t * int)
        val all : t list (* all non-identity moves *)
        val n : int (* number of non-identity moves *)
        val to_rank : t -> int (* not defined on identity moves *)
        val to_move : t -> T.t

        (* allow conversions to and from every possible fixed move *)
        (* This makes all seem like a subtype of All_fixed_move *)
        val to_super_t : t -> Super.t
        val of_super_t : Super.t -> t

        (* TODO: exchange this for quickcheck *)
        val random_list : int -> t list
      end

    (* Fixed moves on the whole Rubik's cube group G. This can act as Super. *)
    module G : S with type t = Super.t and type Generator.t = Generator.t
    (* Fixed moves on the subgroup G1 *)
    module G1 : S with type Generator.t = G1_generator.t
  end