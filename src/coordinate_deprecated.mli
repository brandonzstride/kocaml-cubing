
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
    (* Find the next largest coordinate. This is useful for creating lookup tables. *)
    val next : t -> t option
    (* Rank the coordinates as integers *)
    val to_rank : t -> int
    (* The smallest coordinate *)
    val zero : t
    (* How many coordinates there are *)
    val n : int
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

(* I would like to have a module that memoizes a coordinate. Also
   a module that takes a raw coordinate and makes it a sym coordinate.
   For the memoization, we really just iterate through the coodinates,
   apply all moves, and save lookup table.
   What this needs is a zero, next, perform_fixed_move.

   To get a sym coordinate from a raw, it needs to know how many equivalence
   classes there are. Then it iterates over all raw coordinates and groups by
   equivalence class. It does this by taking the raw coordinate, applying all
   symmetries, mapping the least of the resulting coordinates to the set of all
   that were hit. If we are iterating over the raw coordinates and come across
   one that is already found in an eq class, then we don't need to perform symmetries
   because they should all be the same. This is done by mapping each coordinate to
   whether it's been found.
   After this is done, we have a mapping of coordinate to eq class. Then sort by
   coordinate and get class index to representative mapping. Therefore, it shouldn't actually
   need to know how many classes there are.
   To calculate moves on these coordinates, we just need how the representative coordinate
   is affected by a move and how a move is affected by a symmetry and how two symmetries
   are multiplied.
   But since the move table will just map representative coordinates to some sym coordinate,
   and there is still work to do, we can't just assume that memoizing the results on
   all visible coordinates is enough.

   So this needs perform_symmetry, to_rank (to tell which coordinates were hit
   by a symmetry), coordinates affected by moves, moves by symmetries, and symmetry
   multiplication.

   To handle memoization on sym coordinates, we need a way to perform extra conversions
   outside the lookup. 


   I think I should convert this all from having is_memoized pass in to just memoizing results.
   This will need to be different for raw versus sym coordinates.
   
   *)


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