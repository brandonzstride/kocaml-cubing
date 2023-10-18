open Core

module type S =
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
      Raw.to_rank raw = 0 && Sym.to_rank sym = 0

    (* assume the coordinates don't conflict *)
    let to_perm ({ raw ; sym } : t ) : Perm.t =
      Move.(Raw.to_perm raw * Sym.to_perm sym) 

    let of_perm (p : Perm.t) : (t, string) result =
      (* TODO: Coordinate.of_perm might want to use result because this is always Ok currently *)
      Ok { raw = Raw.of_perm p ; sym = Sym.of_perm p }

    (* Need to know which is smaller to support better caching. *)
    let is_raw_smaller = Raw.n < Sym.n

    (* let convert_to_rep ({ raw ; sym } : t) : t =
      let s = Sym.get_symmetry sym in
      { raw = Raw.perform_symmetry raw s ; sym = Sym.perform_symmetry sym s } *)

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
          Some { raw = Raw.of_rank @@ x mod Raw.n
               ; sym = Sym.of_rank @@ x / Raw.n }
      else
        fun x ->
          Some { raw = Raw.of_rank @@ x / Sym.n 
               ; sym = Sym.of_rank @@ x mod Sym.n }
  end

module P : Coordinate.Params =
  struct
    let status = `Is_saved_at_directory "./src/kocaml_cubing_lib/coordinates/"
  end

module Twist = Coordinate.Twist (P)
module Flip_UD_slice = Coordinate.Flip_UD_slice (P)

module Phase1 = Make (Twist) (Flip_UD_slice)