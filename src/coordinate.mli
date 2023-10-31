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

  Dependencies:
    Perm -- coordinates are represent an aspect of a perm
    Move -- coordinates can be acted on by moves
    Cubie -- coordinates are calculated from the cubies of a perm
    Symmetry -- for reduction by symmetry equivalence classes
    Lookup_table -- for memoization for increase efficiency

  Other comments:
    I currently have to pass around a Move.Fixed_move.S for the coordinates to
    work with. I couldn't get it to work naturally with functors. What it feels
    like I need is a module type functor (i.e. a functor that returns a module
    type), but this doesn't exist as far as I'm aware.
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

(*
  The signature of any raw coordinate...   
*)
module type S =
  sig
    (* Stores the cube moves that this coordinate supports *)
    module Fixed_move : Move.Fixed.S
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
    val perform_fixed_move : t -> Fixed_move.t -> t
    (* Gets the resulting coordinate after applying a symmetry like S * P * S^-1 *)
    val perform_symmetry : t -> Symmetry.t -> t
    (* Gets a list of all coordinates. USE THIS SPARINGLY *)
    val all : unit -> t list
  end

(*
  Symmetry coordinates have been reduced to symmetry equivalence classes.
*)
module type Sym_S = 
  sig
    include S
    (* Get the symmetry that converts it to the representative of the symmetry class *)
    val get_symmetry : t -> Symmetry.t
    (* Gets cubes that are identical but have different symmetry coordinates *)
    val get_identical_cubes : t -> t list
  end

module type Params =
  sig
    val status :
      [ `Is_saved_at_directory of string
      | `Compute_and_save_at_directory of string
      | `Compute ]
  end

(* Phase 1 coordinates use all fixed moves that generate the group G *)
module type Phase1_S     = functor (_ : Params) -> (S with module Fixed_move = Move.Fixed.G)
module type Phase1_sym_S = functor (_ : Params) -> (Sym_S with module Fixed_move = Move.Fixed.G)

(* Phase 2 coordinates use only the moves that generate the subgroup G1 *)
module type Phase2_S     = functor (_ : Params) -> (S with module Fixed_move = Move.Fixed.G1)
module type Phase2_sym_S = functor (_ : Params) -> (Sym_S with module Fixed_move = Move.Fixed.G1)

(* Exposed coordinates for regular use *)
(* Phase 1*)
module Twist         : Phase1_S
module Flip_UD_slice : Phase1_sym_S

(* Phase 2 *)
module Edge_perm     : Phase2_S
module UD_slice_perm : Phase2_S
module Corner_perm   : Phase2_sym_S

module Using_config () :
  sig
    (* Phase 1 *)
    module Twist         : S with module Fixed_move = Move.Fixed.G
    module Flip_UD_slice : Sym_S with module Fixed_move = Move.Fixed.G

    (* Phase 2 *)
    module Edge_perm     : S with module Fixed_move = Move.Fixed.G1
    module UD_slice_perm : S with module Fixed_move = Move.Fixed.G1
    module Corner_perm   : Sym_S with module Fixed_move = Move.Fixed.G1

  end

(* Raw coordinates that are only exposed for testing and not to be used during solving *)
module Exposed_for_testing :
  sig
    module type Phase1_raw_S = S with module Fixed_move = Move.Fixed.G
    module type Phase2_raw_S = S with module Fixed_move = Move.Fixed.G1

    (* Phase 1 *)
    module Twist         : Phase1_raw_S
    module Flip          : Phase1_raw_S
    module UD_slice      : Phase1_raw_S
    module Flip_UD_slice : Phase1_raw_S

    (* Phase 2 *)
    module Edge_perm     : Phase2_raw_S
    module Corner_perm   : Phase2_raw_S
    module UD_slice_perm : Phase2_raw_S
  end