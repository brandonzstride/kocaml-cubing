(*
  File: coordinate.mli
  Module purpose: describes aspects of Rubik's cube with integers.
  Status: nearly complete; symmetry coordinates need testing.

  Detailed description:
    This module holds all the coordinates that describe a cube. These
    coordinates can be memoized for faster computation (so that a more
    "physical" and computationally expensive representation of the cube
    can be discarded) and converted to symmetry coordinates.

    This module supports conversions between permutations and coordinates.
    See `Perm` module.

    More description and motivation is in another comment below. Definitions
    of the coordinates are in the `coordinate.ml` file.

  Expected usage:
    The `Cube` module uses a record of some coordinates to holistically
    represent a cube. Other modules need not refer to coordinates directly.
*)

(*
  What are coordinates?
  Coordinates are unique values assigned to some aspect of a cube's
  state. They help us succinctly describe a cube and focus on only one
  feature at a time. For example, the Twist coordinate describes the
  orientation of the cube's corners, and the Corner_perm coordinate
  separately describes the permutation of the corners. Together, these
  two coordinates completely determine the state of the cube's corners.
  
  It can be slow to operate on a coordinate because it must be mapped to
  some valid cube state, and then the cube is directly manipulated, and
  then the cube is coverted back to a coordinate. So why are they helpful?
  They are helpful because they are quicker to write down than a cube, and
  there are few enough of them that all operations can be precomputed.
  In this way, we can operate only on coordinates and forget the cube
  altogether once its initial coordinates have been computed.

  In this module, I provide implementations of the coordinates as they
  act directly on the cube, and I provide functors to memoize the results
  of coordinate operations. Once the results are memoized, it should be
  very fast to operate on coordinates.

  There are two types of coordinates:
  1. Raw coordinates
  2. Symmetry coordinates.

  Both coordinates map one-to-one with a feature of the cube state. In fact,
  a raw coordinate and symmetry coordinate contain exactly the same information.
  However, a symmetry coordinate makes use of the cube symmetries to group
  symmetric cubes so that fewer results need to be memoized. Only one cube per
  equivalence class needs to be precomputed, and symmetric cubes are efficiently
  computed based on that result. A symmetry coordinate is effectively a
  reorganization of the raw coordinate, and then some coordinates are hidden so
  that only one representative of each equivalence class is exposed.
*)

module type T =
  sig
    (* The type is hidden, but not really because sexp gives insight *)
    type t [@@deriving sexp, compare]
    (* The coordinate of rank zero *)
    val zero : t
    (* `n` is the number of possible coordinates *)
    val n : int
    (* Get the next largest coordinate, or None if there is no next largest. *)
    val next : t -> t option
    (* Get the integer rank of this coordinate *)
    val to_rank : t -> int
    (* Gets the coordinate from the integer rank *)
    val of_rank : int -> t
    (* Finds a representative permutation for the coordinate *)
    val to_perm : t -> Perm.t
    (* Calculates the coordinate for a given permuation *)
    val of_perm : Perm.t -> t
    (* Gets the resulting coordinate after applying a move *)
    val perform_fixed_move : t -> Move.Fixed_move.t -> t
    (* Gets the resulting coordinate after applying a symmetry like S * P * S^-1 *)
    val perform_symmetry : t -> Symmetry.t -> t
    (* Gets a list of all coordinates. USE THIS SPARINGLY *)
    val all : unit -> t list
  end

module type Memo_params =
  sig
    (*
      `Is_saved          => can just read the results from the files.
      `Needs_computation => must compute results and then save to files.   
    *)
    val status : [> `Is_saved | `Needs_computation ]
    (* the absolute filepath to where the move table is saved *)
    val move_filepath : string
    (* the absolute filepath to where the symmetry table is saved *)
    val symmetry_filepath : string
  end

module type Sym_memo_params =
  sig
    val status : [> `Is_saved | `Needs_computation ]
    val move_filepath : string
    val class_to_rep_filepath : string
    val rep_to_class_filepath : string
  end

(*
  A coordinate has a base functionality described within T and included
  within the coordinate. The user has the option to make a memoized version
  of a coordinate or a symmetry coordinate. Both require some computation
  to make and expose the exact same functionality under the signature T.
*)
module type Coordinate =
  sig
    module Raw : T
    module Make_memoized_coordinate (_ : Memo_params)     : T
    (* The below will break on Twist because it uses corner orientation *)
    module Make_symmetry_coordinate (_ : Sym_memo_params) : T
  end

(* Phase 1 coordinates *)
module Twist         : Coordinate
module Flip          : Coordinate (* only exposed for testing *)
module UD_slice      : Coordinate (* only exposed for testing *)
module Flip_UD_slice : Coordinate

(* Phase 2 coordinates *)
(* Note that these are defined for moves outside of the G1 generators *)
(* TODO: restrict fixed moves to only be G1 generators *)
module Edge_perm     : Coordinate
module Corner_perm   : Coordinate
module UD_slice_perm : Coordinate