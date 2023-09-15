open Core

module T = struct
  type t = Cubie.t -> Cubie.t
end

include T

let equal (m1 : t) (m2 : t) : bool =
  let rec aux = function
  | [] -> true
  | hd :: tl -> Cubie.compare (m1 hd) (m2 hd) = 0 && aux tl
  in
  aux Cubie.all


(* Compose the two moves. But since we use "is replaced by", if we want to first
   do the move b and then do a, we must first see what a's cubies are replaced by,
  and then feed those into b. i.e. we do it in reverse order *)
let ( * ) a b = fun x -> a x |> b 

module Faceturn =
  struct
    type t = U | R | F | D | B | L [@@deriving enumerate, variants]

    (* While it's typically more readable to give each match case its own line
        in this specific situation, it seems better to group them on a line *)
    let to_move = 
      let open Cubie in
      let open Cubie.Edge_facelet in
      let open Cubie.Corner_facelet in function
      | U -> begin function
        (* we use the "is replaced by" format, so when U is turned cw, UR goes to UF and keeps the same orientation *)
        | Edge { e ; o } ->
          let e' = begin match e with
          | UR -> UB | UF -> UR | UL -> UF | UB -> UL | _ -> e end
          in Edge { e = e' ; o } 
        (* we use the "is replaced by" format, so when U is turned cw, UBR goes to URF and keeps the same orientation *)
        | Corner { c ; o } -> 
          let c' = begin match c with
          | URF -> UBR | UFL -> URF | ULB -> UFL | UBR -> ULB | _ -> c end
          in Corner { c = c' ; o }
        end
      | R -> begin function
        | Edge { e ; o } ->
          let e' = begin match e with
          | UR -> FR | BR -> UR | DR -> BR | FR -> DR | _ -> e end
          in Edge { e = e' ; o } 
        | Corner { c ; o } -> 
          let c', d_o = begin match c with
          | URF -> DFR, 2 | UBR -> URF, 1 | DRB -> UBR, 2 | DFR -> DRB, 1 | _ -> c, 0 end
          in Corner { c = c' ; o = Modular_int.Z3.(o + of_int d_o) }
        end
      | F -> begin function
        | Edge { e ; o } -> 
          let e', d_o = begin match e with
          | UF -> FL, 1 | FR -> UF, 1 | DF -> FR, 1 | FL -> DF, 1 | _ -> e, 0 end
          in Edge { e = e' ; o = Modular_int.Z2.(o + of_int d_o) }
        | Corner { c ; o} ->
          let c', d_o = begin match c with
          | URF -> UFL, 1 | UFL -> DLF, 2 | DLF -> DFR, 1 | DFR -> URF, 2 | _ -> c, 0 end
          in Corner { c = c' ; o = Modular_int.Z3.(o + of_int d_o) }
        end
      | D -> begin function
        | Edge { e ; o } ->
          let e' = begin match e with
          | DR -> DF | DF -> DL | DL -> DB | DB -> DR | _ -> e end
          in Edge { e = e' ; o }
        | Corner { c ; o } ->
          let c' = begin match c with
          | DFR -> DLF | DLF -> DBL | DBL -> DRB | DRB -> DFR | _ -> c end
          in Corner { c = c' ; o }
        end
      | B -> begin function
        | Edge { e ; o } -> 
          let e', d_o = begin match e with
          | UB -> BR, 1 | BL -> UB, 1 | DB -> BL, 1 | BR -> DB, 1 | _ -> e, 0 end
          in Edge { e = e' ; o = Modular_int.Z2.(o + of_int d_o) }
        | Corner { c ; o } ->
          let c', d_o = begin match c with
          | ULB -> UBR, 1 | UBR -> DRB, 2 | DRB -> DBL, 1 | DBL -> ULB, 2 | _ -> c, 0 end
          in Corner { c = c' ; o = Modular_int.Z3.(o + of_int d_o) }
        end
      | L -> begin function
        | Edge { e ; o } ->
          let e' = begin match e with
          | UL -> BL | FL -> UL | DL -> FL | BL -> DL | _ -> e end
          in Edge { e = e' ; o }
        | Corner { c ; o } ->
          let c', d_o = begin match c with
          | UFL -> ULB, 1 | ULB -> DBL, 2 | DBL -> DLF, 1 | DLF -> UFL, 2 | _ -> c, 0 end
          in Corner { c = c' ; o = Modular_int.Z3.(o + of_int d_o) }
        end
    
  end

module Fixed_move = 
  struct
    type t = { faceturn : Faceturn.t ; count : Modular_int.Z4.t } [@@deriving enumerate]

    let to_rank x = 
      assert (Modular_int.Z4.to_int x.count <> 0);
      Int.(Faceturn.Variants.to_rank x.faceturn * 3 + Modular_int.Z4.to_int x.count)
      
    (* all defined by enumerate *)
    let all = all |> List.filter ~f:(fun x -> Modular_int.Z4.compare x.count (Modular_int.Z4.of_int 0) = 0)

    let n = List.length all
    
    let to_move { faceturn ; count } =
      let m = Faceturn.to_move faceturn in
      match Modular_int.Z4.to_int count with
      | 0 -> Fn.id (* we do allow count to be zero here. *)
      | 1 -> m
      | 2 -> m * m
      | _ -> m * m * m
  end