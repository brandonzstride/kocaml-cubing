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
  The coordinates interact with and depend on the symmetries of the cube.
  The symmetry functions can be either calculated or memoized, so the
  coordinates need to receive a Symmetry.S module in order to adapt to
  make use of the memoized behavior.
*)

module Make (Sym : Symmetry.S) :
  sig

    module type Coordinate =
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
        val perform_symmetry : t -> Sym.t -> t
        (* Gets a list of all coordinates. USE THIS SPARINGLY *)
        val all : unit -> t list
      end

    module type Sym_coordinate =
      sig
        (*
          The only accessible coordinates through `next` or `all` are
          representatives of the equivalence classes.
        *)
        include Coordinate

        (*
          Gets the symmetry that converts the coordinate to the representative
          of its equivalence class.
          i.e. if the perm is P, then it is converted to representative by S * P * S^-1
        *)
        val get_symmetry : t -> Sym.t
      end

    (*
      With no memoization at all, it is very slow to get the rank of a symmetry
      coordinate. This module type provides a structure to work with symmetry
      coordinates before memoization. It provides all that is necessary to begin
      the memoization.   
    *)
    module type Sym_base =
      sig
        module Raw : Coordinate
        type t 
        val of_raw : Raw.t -> t
        (* Gets the representative raw coordinate for the equivalence class of the given t *)
        val get_rep : t -> Raw.t
        (* Gets which symmetry S converts P to the representative R by S * P * S^-1 = R *)
        val get_sym : t -> Sym.t
        val perform_fixed_move : t -> Move.Fixed_move.t -> t
        val perform_symmetry : t -> Sym.t -> t
        (* This is VERY computatationally expensive. Refrain from using this *)
        val all : unit -> t list
      end

    (*
      I need to consider if these should all be raw, and the solver converts
      to sym coordinates as needed. For now, just say that some are left as
      Sym_base instead of a full-on symmetry coordinate.
    *)
    module Phase1 : 
      sig
        module Twist              : Coordinate
        module Flip               : Coordinate (* only exposed for testing *)
        module UD_slice           : Coordinate (* only exposed for testing *)
        module Flip_UD_slice_raw  : Coordinate (* only exposed for testing *)
        module Flip_UD_slice      : Sym_base
      end

    module Phase2 : 
      sig
        module Edge_perm       : Coordinate
        module Corner_perm_raw : Coordinate    (* only exposed for testing *)
        module Corner_perm     : Sym_base
        module UD_slice_perm   : Coordinate
      end

    module type Memo_params =
      sig
        (*
          `Is_already_saved   => can just read the results from the files.
          `Computation_needed => must compute the results and then save to files.
        *)
        val status : [> `Is_already_saved | `Computation_needed]
        (* absolute string filepath to where the move table is saved *)
        val move_save_location : string 
        (* absolute string filepath to where the symmetry table is saved *)
        val symmetry_save_location : string 
      end

    module type Sym_memo_params =
      sig
        include Memo_params (* symmetry_save_location not actually needed *)
        val class_to_rep_location : string
        val rep_to_class_location : string
      end

    (*
      A memoized coordinate has exactly the same behavior to external users
      as the given coordinate, but it uses precomputed results and lookup tables
      to run faster.   
    *)
    module Make_memoized     (_ : Coordinate) (_ : Memo_params)     : Coordinate
    module Make_memoized_sym (_ : Sym_base)   (_ : Sym_memo_params) : Sym_coordinate

  end
