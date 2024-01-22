open Core

module type S =
  sig
    module Fixed_move :
      sig
        type t
        val of_super_t : Move.Fixed.Super.t -> t
      end
    type t
    val n : int
    val is_goal_state : t -> bool
    val of_perm : Perm.t -> (t, string) result 
    val to_rank : t -> int
    val perform_fixed_move : t -> Fixed_move.t -> t
    val get_identical_cubes : t -> t list
    module Exposed_for_testing :
      sig
        val to_perm : t -> Perm.t
      end
  end

(* Make a combination of two coordinates, one raw and one sym *)
module Make
  (Raw : Coordinate.S)
  (Sym : Coordinate.Sym_S with type Fixed_move.t = Raw.Fixed_move.t)
  : S with type Fixed_move.t = Raw.Fixed_move.t =
  struct
    module Fixed_move = Raw.Fixed_move

    type t =
      { raw : Raw.t
      ; sym : Sym.t }

    let n = Raw.n * Sym.n

    let is_goal_state ({ raw ; sym } : t) : bool =
      Raw.to_rank raw = 0 && Sym.to_rank sym = 0

    (* This assumes it is always possible to get from perm *)
    let of_perm (p : Perm.t) : (t, string) result =
      Ok { raw = Raw.of_perm p ; sym = Sym.of_perm p }

    (* Need to know which is smaller to support better caching. *)
    let is_raw_smaller = Raw.n < Sym.n

    let convert_to_rep ({ raw ; sym } : t) : t =
      let s = Sym.get_symmetry sym in
      { raw = Raw.perform_symmetry raw s ; sym = Sym.perform_symmetry sym s }

    let to_rank =
      if is_raw_smaller then
        function x ->
          (* use convert_to_rep *)
          let { raw ; sym } = convert_to_rep x in
          Raw.n * Sym.to_rank sym + Raw.to_rank raw
      else
        function x ->
          let { raw ; sym } = convert_to_rep x in
          Sym.n * Raw.to_rank raw + Sym.to_rank sym

    let perform_fixed_move { raw ; sym } (m : Fixed_move.t) : t =
      { raw = Raw.perform_fixed_move raw m ; sym = Sym.perform_fixed_move sym m }

    let get_identical_cubes { raw ; sym } : t list =
      sym
      |> Sym.get_identical_cubes
      |> List.map ~f:(fun s -> { raw ; sym = s })

    module Exposed_for_testing =
      struct
        (* assume the coordinates don't conflict *)
        let to_perm ({ raw ; sym } : t ) : Perm.t =
          Move.(Sym.to_perm sym * Raw.to_perm raw) 
      end
  end

(* This is really slow in utop because it loads everything on startup *)
module C = Coordinate.Using_config ()

(*
  Note that Twist and Flip_UD_slice are defined on all cubes, so
  `of_perm` needs no special considerations
*)
module Phase1 = Make (C.Twist) (C.Flip_UD_slice)

module Phase2 =
  struct
    (* Only two of the coordinates are visible to the user, but 
       Phase2 still needs to consider UD_slice_perm to be solved *)
    module Visible = Make (C.Edge_perm) (C.Corner_perm)

    module Fixed_move = Visible.Fixed_move

    module UD = C.UD_slice_perm

    type t =
      { visible : Visible.t (* users of the module can only see the part *)
      ; ud      : UD.t }

    let n = Visible.n (* UD does not contribute it size *)

    let is_goal_state { visible ; ud } =
      Visible.is_goal_state visible && UD.to_rank ud = 0

    let of_perm (p : Perm.t) : (t, string) result =
      p
      |> Phase1.of_perm (* is always Ok -- see comment above module Phase1 *)
      |> Result.ok_or_failwith
      |> Phase1.is_goal_state
      |> function
         | true -> Ok { visible = Visible.of_perm p |> Result.ok_or_failwith ; ud = UD.of_perm p }
         | false -> Error "perm is not in G1, so phase2 coordinate is not defined"

    (* UD does not contribute to rank because it makes the scale too large *)
    let to_rank x =
      Visible.to_rank x.visible

    let perform_fixed_move { visible ; ud } (m : Fixed_move.t) : t =
      { visible = Visible.perform_fixed_move visible m ; ud = UD.perform_fixed_move ud m }

    (*
      ud is a raw coordinate and thus doesn't have any identical cubes.   
    *)
    let get_identical_cubes { visible ; ud } : t list =
      visible
      |> Visible.get_identical_cubes
      |> List.map ~f:(fun v -> { visible = v ; ud })

    module Exposed_for_testing =
      struct
        let to_perm { visible ; ud } : Perm.t =
          Move.(Visible.Exposed_for_testing.to_perm visible * UD.to_perm ud)
      end

  end