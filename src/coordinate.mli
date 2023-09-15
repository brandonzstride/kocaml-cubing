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
    val perform_symmetry : t -> Symmetry.S.t -> t
    (* Find the next largest coordinate. This is useful for creating lookup tables. *)
    val next : t -> t option
    (* Rank the coordinates as integers *)
    val to_rank : t -> int
    (* The smallest coordinate *)
    val zero : t
    (* How many coordinates there are *)
    val n : int
  end

(* Should sym coordinates always be memoized? *)
(* They always need a class to representative array. *)
(* If they don't have a class to rep array, then they just apply
   all symmetries to the perm and calculate coordinate to find the
   rep, then use provided module to compute resulting coordinate,
   which also needs to find rep in order to compute resulting sym
   coordinate of just the original rep. Then do sym move stuff *)
(* If we do choose to memoize, we'll need to save class to rep array,
   and rep to resulting sym array. *)
(* Two options here:
   1. Sym is fast to make from Raw but has slow operations because
      it is not memoized in any way
   2. Sym is slow to make from Raw but has fast operations because is all
      memoized.
  In the first case, we slowly create sym table because operations are slow.
  So this first case is less efficient overall but more natural, and it also
  allows us to compare hard computation speeds versus sped up computation
  via lookup tables. I think I will go with the first because it enables more
  encapsulation. I will clean up this comment to explain the choice.
*)

(*
  -------------------- 
  SYMMETRY COORDINATES
  --------------------

  Since some coordinates are large, we use cube symmetries to cut down
  on the number of possibilities by about a factor of 16. The idea is this:
  combine all coordinates that reach each other via some symmetry conjugation
  into an equivalence class.

  Symmetry coordinates are computed from raw coordinates approximately like the following:
  * Perform all symmetries on the coordinate, and let the smallest resulting coordinate
    represent the class.
  * The index of a coordinate in an equivalence class is the rank of the symmetry
    that converts that coordinate to the representative.

  The process is like this:
  * Iterate through all raw coordinates.
  * For each coordinate, apply all symmetries. The resulting set of coordinates is an equivalence class.
  * Mark off any raw coordinates that are found in this equivalence class as not needing to be
    processed in the future.
  * Map the smallest raw coordinate (the representative) to the class.
  * Order the classes by the representative.
  * The index in this ordering is the symmetry coordinate for the representative.
  * Each raw coordinate is then stored as a representative symmetry coordinate and a symmetry that converts
    the raw coordinate to the representative's raw coordinate.

  We see that it is necessary to compute and order the representatives so that we can get a symmetry coordinate.
  For this reason, it is slow to create this module. However, this is all that is memoized:
  * A map from representative's raw coordinate to symmetry coordinate.
  * A map from representative's symmetry coordinate to raw coordinate.

  Upon memoization, the following will still need to be calculated and store:
  * Map from rep sym coord and move to resulting sym coord.

  This two-step process of creating the symmetry coordinate module and then memoizing it is not the most
  efficient for the memoization process because the symmetry coordinate's calculations will be very slow
  (it must constantly do conversions from raw coordinate to representative coordinate), but I favor this
  two-step approach for easier thinking and more encapsulation; if it's done all at once, it will be easy
  to get lost in the big mix of tasks at hand.

  Another option to consider:
  What I can also do is make it super duper slow to compute eq class rank but store the representative
  as a raw coordinate. Then in memoization, I save the map that takes representative raw coordinate to index.
  OR I just have map that instead of taking index it takes rep raw coord, and that is lookup table.
  This will make it much faster to make the module, but the memoized results will be slightly slower because
  whenever mapping to rank or whatnot, it always must lookup the class index from the representative.
  The difference with this method is entirely within the implementation, I think, so I can ignore it for now.
  Further: this will make it so that making the Sym module is way faster and there is little startup time.
  The only reason I can't change the way a representative is stored is because I need the types to be the
  same between a memoized and non-memoized module.
  I think this slower speed of having to lookup index from rep raw coord will be worth the encapsulation.
  It takes about half a second (0.41-0.43s) to lookup an index from a raw coord for all 64430 eq classes
  based on a silly test of creating a tree map and iterating through all keys I made to find value for each.
*)

module type Sym =
  sig
    type t
    val invert : t -> Perm.t
    val calculate : Perm.t -> t
    val perform_fixed_move : t -> Move.Fixed_move.t -> t
    val perform_symmetry : t -> Symmetry.S.t -> t
    (* Get the representative of the next class, not just the next coordinate *)
    val next_representative : t -> t option
    (* Gets the rank of the eq class of the coordinate *)
    (* Equivalent to getting some representative raw coordinate for the class *)
    val to_eq_class_rank : t -> int
    (* Gets which symmetry converts the coordinate to the representative *)
    (* i.e. if the perm is P, then it is converted to representative by S * P * S^-1 *)
    val get_symmetry : t -> Symmetry.S.t
    (* Gets the first representative *)
    val zero_representative : t
    (* Number of equivalence classes i.e. number of representatives *)
    val n : int
  end

module Sym_of_raw (_ : Raw) : Sym

(*
  -----------
  MEMOIZATION   
  -----------

  Coordinates are memoized using lookup tables. All coordinates that are
  reachable via `zero` and `next` get saved at some unique index in a lookup
  table. This allows for fast moves without any calculations at the cube level.

  The memoized modules will have the same signature as the non-memoized modules.
  The only difference is that they are performing lookups instead of coordinate
  calculations.

  When a memoized module is created, it is told whether it needs to calculate
  the lookup table or if the table already exists. This is done via the
  Memoization module type.
*)

module type Memoization =
  sig
    val is_already_saved : bool
    val save_location : string (* absolute string filepath *)
  end

(* I think I might not enforce the type to be the same,
   but rather make all users of this a functor where I 
   have to pass in the coordinate modules *)

module Memoize_raw (R : Raw) (_ : Memoization) : Raw with type t := R.t

module Memoize_sym (S : Sym) (_ : Memoization) : Sym with type t := S.t

(* All coordinates will be memoized eventually. *)

module Phase1 :
  sig
    module Twist : Raw
    module Flip : Raw (* exposed only for testing *)
    module UD_slice : Raw (* exposed only for testing *)
    module Flip_UD_slice : Sym
  end

module Phase2 :
  sig
    module Edge_perm : Raw
    module Corner_perm : Sym
    module UD_slice_perm : Raw
  end