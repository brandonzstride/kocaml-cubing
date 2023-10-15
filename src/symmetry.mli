(*
  File: symmetry.mli
  Module purpose: represent symmetries of the cube.
  Status: incomplete; needs reflection corner orientation

  Detailed description:
    This module describes the symmetries of the cube under rotations
    and reflections. There are 48 total symmetries of a cube, but only
    16 preserve the horizontal plane. That is, only 16 don't take the
    middle horizontal slice of the cube to some other slice.

    The UD slice is used as a coordinate, and if a symmetry disrupted
    the slice, then the coordinate would not be well-defined.

    The implementation is very hidden, and the symmetries are memoized
    upon startup. The space of symmetries is small, so this memoization
    is extremely fast.

  IMPORTANT NOTE:
    I currently don't use the reflection. In the long-run, I would like
    to use it.

  Dependencies:
    Move -- symmetries are represented as moves
    Perm -- symmetries can act on permutations
    Cubie -- because this uses moves
    Lookup_table -- symmetries are memoized upon startup for better efficiency
*)

type t
(* Compose the two symmetries. The left is applied first. *)
val mult : t -> t -> t
val inverse : t -> t
(* how a symmetry s acts on a perm p by s * p * s^-1 *)
val on_perm : t -> Perm.t -> Perm.t
(* How a symmetry s acts on a move m by s * m * s^-1 *)
val on_fixed_move : t -> Move.Fixed.Super.t -> Move.Fixed.Super.t
val to_rank : t -> int
val of_rank : int -> t
val id : t
val n : int
val next : t -> t option
val all : t list
val random : unit -> t (* for testing *)