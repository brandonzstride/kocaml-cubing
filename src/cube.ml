open Core

module type S :
  sig
    type t
    val is_goal_state : t -> bool
    val of_perm : Perm.t -> (t, string) result 
    val to_perm : t -> Perm.t (* for testing purposes *)
    val of_rank : int -> t option
    val to_rank : t -> int
  end

(* Make a combination of two coordinates, one raw and one sym *)
module Make (Raw : Coordinate.S) (Sym : Coordinate.Sym_S) : S =
  struct
    type t =
      { raw : Raw.t
      ; sym : Sym.t }

    let is_goal_state ({ raw ; sym } : t) : bool =
      Raw.rank raw = 0 && Sym.rank sym = 0

    (* assume the coordinates don't conflict *)
    let to_perm ({ raw ; sym } : t ) : Perm.t =
      Move.(Raw.to_perm raw * Sym.to_perm sym) 

    let of_perm (p : Perm.t) : (t, string) result =
      (* TODO: Coordinate.of_perm might want to use result because this is always Ok currently *)
      Ok { raw = Raw.of_perm p ; sym = Sym.of_perm p }

    (* Need to know which is smaller to support better caching. *)
    let is_raw_smaller = Raw.n < Sym.n

    let convert_to_rep ({ raw ; sym } : t) : t =
      let s = Sym.get_symmetry sym in
      { raw = Raw.perform_symmetry raw s ; sym = Sym.perform_symmetry sym s }

    (* TODO: consider symmetry. Need to convert to rep and apply sym to raw *)
    let to_rank =
      if is_raw_smaller then
        function { raw ; sym } ->
          (* use convert_to_rep *)
          Raw.n * Sym.to_rank sym + Raw.to_rank (Raw.perform_symmetry raw (Sym.get_symmetry sym))
      else
        function { raw ; sym } ->
          Sym.n * Raw.to_rank raw + Sym.to_rank sym
      
    let of_rank =
      if is_raw_smaller then
        fun x ->
          { raw = x mod Raw.n
          ; sym = x / Raw.n }
      else
        fun x ->
          { raw = x / Sym.n 
          ; sym = x mod Sym.n }
  end

(* not compiling because we need some saved coordinates for this to work.
   I should have already computed the Twist memoized and the Flip_UD_slice
   symmetry coordinate. *)
module Phase1 = Make (Coordinate.Twist.Raw) (Coordinate.Flip_UD_slice_sym)