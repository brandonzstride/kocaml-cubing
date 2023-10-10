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
*)

type t
(* Compose the two symmetries. The left is applied first. *)
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
