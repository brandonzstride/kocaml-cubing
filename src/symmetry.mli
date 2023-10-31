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

val id : t
(** [id] is the identity symmetry that does nothing to a cube. *)

val n : int
(** [n] is the number of unique supported symmetries, including the identity symmetry. *)

val mult : t -> t -> t
(** [mult a b] composes the two symmetries [a] and [b], where the left symmetry [a]
    is applied first. *)

val inverse : t -> t
(** [inverse s] is the symmetry [s'] such that [mult s s'] and [mult s' s] are the
    identity symmetry. *)

val on_perm : t -> Perm.t -> Perm.t
(** [on_perm s p] is the resulting permutation after conjugation by [s].
    i.e. it is s * p * s^-1 *)

val on_fixed_move : t -> Move.Fixed.Super.t -> Move.Fixed.Super.t
(** [on_fixed_move s m] is the resulting fixed move after conjugation by [s].
    i.e. it is s * m * s^-1 *)

val to_rank : t -> int
(** [to_rank s] sends [s] to a unique integer between 0 and [n], and it is invertible
    by [of_rank]. *)

val of_rank : int -> t
(** [of_rank x] gets the symmetry of rank [x], where [x] must be between 0 and [n]. It
    is invertible by [to_rank]. *)

val next : t -> t option
(** [next s] gives the symmetry with the next largest rank after [s], or [None] if [s]
    has the maximum rank. *)

val all : t list
(** [all] is a list of all symmetries, including the identity symmetry. *)

module Exposed_for_testing :
  sig
    val random : unit -> t
  end