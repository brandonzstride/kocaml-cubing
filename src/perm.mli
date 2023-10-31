(*
  File: perm.mli
  Module purpose: represent an arbitrary unsolved Rubik's cube.
  Status: complete.

  Detailed description:
    An arbitrary unsolved Rubik's cube is a permutation of the cubies
    (with some orientation). This module represents such a cube as a
    `Move.t`. This module does not consider how faceturns make up
    such a cube, but it does allow faceturns to act on the cube.

    The goal of this whole project is to find the inverse to a permutation
    as a composition of faceturns.

  Other considerations:
    Maybe combine this with `Cube` under a submodule.

  Dependencies:
    Move -- a permutation is exactly a move on the identity cube
*)

type t = Move.t

val identity : t
(** [identity] is the solved cube state. *)

val is_identity : t -> bool
(** [is_identity p] is true if and only if [p] is the solved cube. *)

val random : unit -> t
(** [random ()] is a random well-formed permutation created by some random moves. *)

val perform_move : t -> Move.t -> t
(** [perform_move p m] composes [p] and [m] as moves, where [m] is performed
    *after* the permutation is applied to the solved cube. *)

val perform_fixed_move : t -> Move.Fixed.Super.t -> t
(** [perform_fixed_move p m] performs the fixed move [m] after the permutation [p]. *)

val perform_fixed_move_list : t -> Move.Fixed.Super.t list -> t
(** [perform_fixed_move_list p ls] performs the moves in [ls] in the order given after the permutation [p]. *)

val of_move_list : Move.t list -> t
(** [of_move_list ls] performs the list of moves [ls] on the identity permutation in the order given. *)

val to_corners_list : t -> Cubie.With_orientation.Corner.t list
(** [to_corners_list p] is a list of corner cubies with orientations as they are mapped
    to by the permutation [p] in the order of the [Cubie.Corner] variant. *)

val to_edges_list : t -> Cubie.With_orientation.Edge.t list
(** [to_edges_list p] is a list of edge cubies with orientations as they are mapped to
    by the permutation [p] in the order of the [Cubie.Edge] variant. *)

val to_ud_edges_list : t -> Cubie.With_orientation.Edge.t list
(** [to_ud_edges_list p] is the list [to_edges_list p] filter to only UD edges. *)

val to_ud_slice_edges_list : t -> Cubie.With_orientation.Edge.t list
(** [to_ud_slice_edges_list p] is the list [to_edges_list p] filter to only UD slice edges. *)

val pp : t -> string
(** [pp p] "pretty prints" the permutation [p] to a string. *)