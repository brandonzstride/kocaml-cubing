open Core

module T =
  struct
    type t = Cubie.t -> Cubie.With_orientation.t
  end

include T

let id = Cubie.With_orientation.of_cubie

let equal (m1 : t) (m2 : t) : bool =
  List.for_all Cubie.all ~f:(fun x -> Cubie.With_orientation.compare (m1 x) (m2 x) = 0)


(** Compose a and b by first applying a, then applying b *)
let ( * ) a b = 
  function
  | Cubie.Edge _ as x ->
    (** Safe to use edge_exn if the move is well-formed and maps edges to edges *)
    let bx = b x |> Cubie.With_orientation.edge_exn in
    let abx = a (Cubie.Edge bx.e) |> Cubie.With_orientation.edge_exn in
    Cubie.With_orientation.Edge Cubie.With_orientation.Edge.{ e = abx.e ; o = Modular_int.Z2.(bx.o + abx.o) }
  | Cubie.Corner _ as x ->
    (** Safe to use corner_exn if the move is well-formed and maps corners to corners *)
    let bx = b x |> Cubie.With_orientation.corner_exn in
    let abx = a (Cubie.Corner bx.c) |> Cubie.With_orientation.corner_exn in
    Cubie.With_orientation.Corner Cubie.With_orientation.Corner.{ c = abx.c ; o = Modular_int.Z3.(bx.o + abx.o) }

module Faceturn =
  struct
    type t = U | R | F | D | B | L [@@deriving enumerate, variants, sexp]

    (* While it's typically more readable to give each match case its own line
        in this specific situation, it seems better to group them on a line *)
    let to_move = 
      let open Cubie in
      let open Cubie.Edge in
      let open Cubie.Corner in
      let open Modular_int in
      function
      | U -> begin function
        (* we use the "is replaced by" format, so when U is turned cw, UR goes to UF and keeps the same orientation *)
        | Edge e ->
          let e' = begin match e with
          | UR -> UB | UF -> UR | UL -> UF | UB -> UL | _ -> e end
          in Cubie.With_orientation.Edge { e = e' ; o = Z2.zero } 
        (* we use the "is replaced by" format, so when U is turned cw, UBR goes to URF and keeps the same orientation *)
        | Corner c -> 
          let c' = begin match c with
          | URF -> UBR | UFL -> URF | ULB -> UFL | UBR -> ULB | _ -> c end
          in Cubie.With_orientation.Corner { c = c' ; o = Z3.zero }
        end
      | R -> begin function
        | Edge e ->
          let e' = begin match e with
          | UR -> FR | BR -> UR | DR -> BR | FR -> DR | _ -> e end
          in Cubie.With_orientation.Edge { e = e' ; o = Z2.zero } 
        | Corner c -> 
          let c', o = begin match c with
          | URF -> DFR, 2 | UBR -> URF, 1 | DRB -> UBR, 2 | DFR -> DRB, 1 | _ -> c, 0 end
          in Cubie.With_orientation.Corner { c = c' ; o = Z3.of_int o }
        end
      | F -> begin function
        | Edge e -> 
          let e', o = begin match e with
          | UF -> FL, 1 | FR -> UF, 1 | DF -> FR, 1 | FL -> DF, 1 | _ -> e, 0 end
          in Cubie.With_orientation.Edge { e = e' ; o = Z2.of_int o }
        | Corner c ->
          let c', o = begin match c with
          | URF -> UFL, 1 | UFL -> DLF, 2 | DLF -> DFR, 1 | DFR -> URF, 2 | _ -> c, 0 end
          in Cubie.With_orientation.Corner { c = c' ; o = Z3.of_int o }
        end
      | D -> begin function
        | Edge e ->
          let e' = begin match e with
          | DR -> DF | DF -> DL | DL -> DB | DB -> DR | _ -> e end
          in Cubie.With_orientation.Edge { e = e' ; o = Z2.zero }
        | Corner c ->
          let c' = begin match c with
          | DFR -> DLF | DLF -> DBL | DBL -> DRB | DRB -> DFR | _ -> c end
          in Cubie.With_orientation.Corner { c = c' ; o = Z3.zero }
        end
      | B -> begin function
        | Edge e -> 
          let e', o = begin match e with
          | UB -> BR, 1 | BL -> UB, 1 | DB -> BL, 1 | BR -> DB, 1 | _ -> e, 0 end
          in Cubie.With_orientation.Edge { e = e' ; o = Z2.of_int o }
        | Corner c ->
          let c', o = begin match c with
          | ULB -> UBR, 1 | UBR -> DRB, 2 | DRB -> DBL, 1 | DBL -> ULB, 2 | _ -> c, 0 end
          in Cubie.With_orientation.Corner { c = c' ; o = Z3.of_int o }
        end
      | L -> begin function
        | Edge e ->
          let e' = begin match e with
          | UL -> BL | FL -> UL | DL -> FL | BL -> DL | _ -> e end
          in Cubie.With_orientation.Edge { e = e' ; o = Z2.zero }
        | Corner c ->
          let c', o = begin match c with
          | UFL -> ULB, 1 | ULB -> DBL, 2 | DBL -> DLF, 1 | DLF -> UFL, 2 | _ -> c, 0 end
          in Cubie.With_orientation.Corner { c = c' ; o = Z3.of_int o }
        end
    
  end

module Fixed_move = 
  struct
    type t = { faceturn : Faceturn.t ; count : Modular_int.Z4.t } [@@deriving enumerate, sexp]

    let to_rank x = 
      assert (Modular_int.Z4.to_int x.count <> 0);
      Int.(Faceturn.Variants.to_rank x.faceturn * 3 + Modular_int.Z4.to_int x.count)
      
    (* all defined by enumerate *)
    let all = all |> List.filter ~f:(fun x -> Modular_int.Z4.compare x.count Modular_int.Z4.zero <> 0)

    let n = List.length all
    
    let to_move { faceturn ; count } =
      let m = Faceturn.to_move faceturn in
      match Modular_int.Z4.to_int count with
      | 0 -> id (* we do allow count to be zero here. *)
      | 1 -> m
      | 2 -> m * m
      | _ -> m * m * m
  end