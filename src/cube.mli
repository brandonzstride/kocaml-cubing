(*
  File: cube.mli
  Module purpose: efficiently represent a whole cube.
  Status: in progress.

  Detailed description:
    This module will represent a cube in some phase of the solving algorithm,
    and it supports moves and symmetries on the cube. Such actions will
    be efficient and will use memoized versions of the coordinates.

    The cube can be in Phase 1 or Phase 2 of the solving algorithm. This module
    helps the user recognize which phase it is in and if the cube has reached
    the goal state.

    This currently does not support choosing non-symmetry coordinates to
    represent the cube during the solving.

  Expected usage:
    The solver creates a cube from a permutation to efficiently make moves and
    check for solvedness. The solver should handle converting back to a

  Other comments:
    Might support functionality without memoization to compare speed.
*)

module type S =
  sig
    (* Yes, this jargon again... See Coordinate *)
    module Fixed_move :
      sig
        type t
        val of_super_t : Move.Fixed.Super.t -> t
      end

    type t
    (** Coordinates are used to represent a cube in some phase of the solving algorithm. *)

    val n : int
    (** [n] is the size of the cube space. *)

    val is_goal_state : t -> bool
    (** [is_goal_state cube] is true if and only if the cube is in the goal state of the
        corresponding phase of the algorithm.
    
        Phase 1 goal state : the cube is in the G1 subgroup
        Phase 2 goal state : the cube is solved *)

    (* discards information about some parts of the cube to partially represent *)
    val of_perm : Perm.t -> (t, string) result 
    (** [of_perm p] gets a [t] from the permutation if the permutation is appropriately
        in the solved state of the previous algorithm phase.
        
        This discards information about some parts of the cube, and it partially represents
        the cube only to sufficiently solve the corresponding phase of the algorithm. *)

    (* gets rank as it's expected to be in the pruning table *)
    val to_rank : t -> int
    (** [to_rank cube] sends the cube to a unique integer between 0 and [n] (see above).
        All cubes of a symmetry class have the same rank. *)

    (* performs a single move on the cube *)
    val perform_fixed_move : t -> Fixed_move.t -> t
    (** [perform_fixed_move cube m] gets the resulting cube after applying the fixed move [m]
        to the permutation underlying the given [cube]. *)

    val get_identical_cubes : t -> t list
    (** [get_identical_cubes cube] gets a list of all cubes that have the same underyling
        permutation as [cube] but might be distinct in [t]. *)

    module Exposed_for_testing :
      sig
        val to_perm : t -> Perm.t
      end
  end

(* Supports Twist and Flip_UD_slice coordinates *)
module Phase1 : S with type Fixed_move.t = Move.Fixed.G.t

(* Supports Edge_perm, Corner_perm, and UD_slice_perm coordinates *)
module Phase2 : S with type Fixed_move.t = Move.Fixed.G1.t
