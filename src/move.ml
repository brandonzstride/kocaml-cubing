module T = struct
  type t = Cubie.t -> Cubie.t
end

include T

(* operation is composition, where b is first *)
let ( * ) a b = fun x -> b x |> a 

module Twist = struct
  type t = U | R | F | D | B | L [@@deriving enumerate]

  (* These will be defined as constants *)
  let to_move _ = fun x -> x (* temporarily say that the move is identity *)
end

module Fixed_moves = struct
  type t = { twist: Twist.t ; count: Modular_int.Z4.t } [@@deriving enumerate]

  let to_move { twist ; count } =
    let m = Twist.to_move twist in
    match Modular_int.Z4.to_int count with
    | 0 -> Core.Fn.id
    | 1 -> m
    | 2 -> m * m
    | _ -> m * m * m
end