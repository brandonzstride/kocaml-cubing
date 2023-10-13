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
val is_identity : t -> bool
val perform_move : t -> Move.t -> t (* Move is performed after permutation *) 
val perform_fixed_move : t -> Move.Fixed_move.t -> t (* Fixed move is performed after the permutation *)
val perform_fixed_move_list : t -> Move.Fixed_move.t list -> t (* Performs moves in the order given *)
val of_move_list : Move.t list -> t (* earlier moves get performed first *)
val to_corners_list : t -> Cubie.With_orientation.Corner.t list
val to_edges_list : t -> Cubie.With_orientation.Edge.t list
val to_ud_edges_list : t -> Cubie.With_orientation.Edge.t list
val to_ud_slice_edges_list : t -> Cubie.With_orientation.Edge.t list
val pp : t -> string