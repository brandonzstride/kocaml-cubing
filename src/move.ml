open Core

module T = struct
  type t = Cubie.t -> Cubie.t
end

include T


(* operation is composition, where b is first *)
let ( * ) a b = fun x -> b x |> a 

module Faceturn =
  struct
    type t = U | R | F | D | B | L [@@deriving enumerate, variants]
    let to_move = 
      let open Cubie in function
      | U -> begin function
        (* we use the "is replaced by" format, so when U is turned cw, UBR goes to URF and keeps the same orientation *)
        | Corner { c = URF ; o } -> Corner { c = UBR ; o = Modular_int.Z3.dec o }
        | x -> x 
        end
      | R -> Fn.id
      | F -> Fn.id
      | D -> Fn.id
      | B -> Fn.id
      | L -> Fn.id
    
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