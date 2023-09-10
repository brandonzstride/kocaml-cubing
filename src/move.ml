module T = struct
  type t = Cubie.t -> Cubie.t
end

include T

(* operation is composition, where b is first *)
let ( * ) a b = fun x -> b x |> a 

module Faceturn = struct
  type t = U | R | F | D | B | L [@@deriving enumerate, variants]

  (* These will be defined as constants *)
  let to_move _ = function
    | Cubie.Corner { c ; o } -> Cubie.Corner { c ; o = Modular_int.Z3.(o * of_int 1) }
    | Cubie.Edge   { e ; o } -> Cubie.Edge   { e ; o = Modular_int.Z2.(o * of_int 1) }
    (* ^ This is placeholder *)

  (* let to_move = function
  | U -> begin function
    (* we use the "is replaced by" format, so when U is turned cw, UBR goes to URF and keeps the same orientation *)
    | Cubie.Corner { c = Cubie.Corner_facelet.URF ; o } -> Cubie.Corner { c = Cubie.Corner_facelet.UBR ; o }
    | and so on.. 
  | R
  | F
  | D
  | B
  | L *)
  
end

module Fixed_move = struct
  type t = { faceturn: Faceturn.t ; count: Modular_int.Z4.t } [@@deriving enumerate]

  (* assigns a unique integer to each move *)
  let to_rank { faceturn ; count } =
    Core.Int.(Faceturn.Variants.to_rank faceturn * 4 + Modular_int.Z4.to_int count)
    
  (* `all` is defined by enumerate *)
  let n = List.length all

  let to_move { faceturn ; count } =
    let m = Faceturn.to_move faceturn in
    match Modular_int.Z4.to_int count with
    | 0 -> Core.Fn.id
    | 1 -> m
    | 2 -> m * m
    | _ -> m * m * m
end