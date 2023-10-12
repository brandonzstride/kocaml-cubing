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

module Phase1 (Twist : Coordinate.T) (Flip_UD_slice : Coordinate.T) : S =
  struct
    type t =
      { twist         : Twist.t
      ; flip_ud_slice : Flip_UD_slice.t }


    let is_goal_state ({ twist ; flip_ud_slice } : t) : bool =
      Twist.to_rank twist = 0 && Flip_UD_slice.to_rank flip_ud_slice = 0
      
    let to_perm ({ twist ; flip_ud_slice } : t) : bool =
      (* twist and flip_ud_slice don't conflict, so safely multiply the permutations *)
      (* ... and note that permutations are moves, so I use Move.( * ) *)
      Move.(Twist.to_perm twist * Flip_UD_slice.to_perm flip_ud_slice)

    let of_perm (p : Perm.t) : (t, string) result =
      (* all perms can be sent to a Phase1 coordinate *)
      Ok { twist = Twist.of_perm p ; flip_ud_slice = Flip_UD_slice.of_perm p }

    (* For caching, we want to keep similar cubes as close together as possible, so we
       need to know which coordinate is smaller *)
    let is_twist_smaller = Twist.n < Flip_UD_slice.n

    (* TODO: the ranks need to consider the non-symmetry coordinate under the same symmetry *)
    let to_rank =
      if is_twist_smaller then
        function { twist ; flip_ud_slice } ->
          Twist.n * Flip_UD_slice.to_rank flip_ud_slice + Twist.to_rank twist
      else
        function { twist ; flip_ud_slice } ->
          Flip_UD_slice.n * Twist.to_rank twist + Flip_UD_slice.to_rank flip_ud_slice

    let of_rank =
      if is_twist_smaller then
        function x ->
          { twist = Twist.of_rank (x mod Twist.n)
          ; flip_ud_slice = Flip_UD_slice.of_rank (x / Twist.n) }
      else
        function x ->
          { twist = Twist.of_rank (x / Flip_UD_slice.n)
          ; flip_ud_slice = Flip_UD_slice.of_rank (x mod Flip_UD_slice.n) }

  end
