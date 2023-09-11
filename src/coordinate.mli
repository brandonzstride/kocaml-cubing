
module type Raw =
  sig
    type t
    (* find a representative permutation for the coordinate *)
    val invert : t -> Perm.t
    (* calculate the coordinate for a given permutation *)
    val calculate : Perm.t -> t
    (* calculate the resulting coordinate after applying one of the regular, fixed moves *)
    val perform_fixed_move : t -> Move.Fixed_move.t -> t
    (* Cube P with coordinate x and symmetry S. Gets coordinate of S * P * S^-1 *)
    val perform_symmetry : t -> Symmetry.t -> t
  end

(* somewhere need to put class to representative array *)
module type Sym = Raw (* the capabilities don't change *)

module type Is_memoized =
  sig
    val is_memoized : bool
  end

module type S_raw =
  sig
    module Make (_ : Is_memoized) : Raw
  end

module type S_sym =
  sig
    module Make (_ : Is_memoized) : Sym
  end

(*
  Coordinates have either been memoized (i.e. saved in a lookup table) or not.
  We create the modules by telling them if they should load from memory.
*)

module Phase1 :
  sig

    module Twist : S_raw

    (* consider hiding Flip and UDSlice *)
    module Flip : S_raw

    module UD_slice : S_raw
    
    module Flip_UD_slice_raw : S_raw

    module Flip_UD_slice : S_sym

  end

module Phase2 : 
  sig

    (* Only the UD face edges *)
    module Edge_perm : S_raw

    module Corner_perm_raw : S_raw

    module Corner_perm : S_sym

    module UD_slice_perm : S_raw

  end