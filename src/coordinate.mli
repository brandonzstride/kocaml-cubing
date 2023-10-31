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
    module Fixed_move : Move.Fixed.S
    (** [Fixed_move] stores the cube moves that this coordinate supports *)

    type t [@@deriving sexp, compare]
    (** [t] is hidden, but sexp gives insight... *)

    val zero : t
    (** [zero] is the coordinate of rank zero. It is always the solved state. *)

    val n : int
    (** [n] is the number of visible coordinates to the user. *)

    val next : t -> t option
    (** [next x] gives the next largest coordinate after [x], or [None] if there is no next largest. *)

    val to_rank : t -> int
    (** [to_rank x] sends [x] to a unique integer rank, which is invertible with [of_rank]. *)

    val of_rank : int -> t
    (** [of_rank i] gets the unique coordinate with rank [i], which is invertible with [to_rank]. *)

    val to_perm : t -> Perm.t
    (** [to_perm x] finds a representative permutation for the coordinate [x]. *)

    val of_perm : Perm.t -> t
    (** [of_perm p] calculates the coordinate for the given permutation [p]. *)

    val perform_fixed_move : t -> Fixed_move.t -> t
    (** [perform_fixed_move x m] gets the resulting coordinate after applying the fixed move [m]
        to the permutation represented by coordinate [x]. *)

    val perform_symmetry : t -> Symmetry.t -> t
    (** [perform_symmetry x s] gets the resulting coordinate after applying the symmetry [s] to
        the permutation represented by coordinate [x]. *)

    val all : unit -> t list
    (** [all ()] is a list of all coordinates. USE THIS SPARINGLY because it can be slow. *)
  end

(*
  Symmetry coordinates have been reduced to symmetry equivalence classes.
*)
module type Sym_S = 
  sig
    include S
    (** Sym_S has everything that S has, and more. *)

    val get_symmetry : t -> Symmetry.t
    (** [get_symmetry x] gets the symmetry that converts the coordinate [x] to the
        representative of its equivalence class. *)

    val get_identical_cubes : t -> t list
    (** [get_identical_cubes x] gets a list of all coordinates (including [x]) that
        represent an identical underlying permutation but have different symmetry coordinates.
        
        E.g. the solved cube is identical to itself under all symmetries, so there is a unique
        symmetry coordinate for each symmetry of the solved cube, even though they are all the same. *)
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