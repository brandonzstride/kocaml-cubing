open Core

module Corner_facelet = struct 
  type t = URF | UFL | ULB | UBR | DFR | DLF | DBL | DRB [@@deriving enumerate, compare, variants]

  let max = List.length all
  let pp = Variants.to_name
end

module Edge_facelet = struct
  (* Facelets are written starting with reference facelet. *)
  type t = UR | UF | UL | UB | DR | DF | DL | DB | FR | FL | BL | BR [@@deriving enumerate, compare, variants]

  let is_ud_slice = function FR | FL | BL | BR -> true | _ -> false
  let is_ud_edge  = Fn.non is_ud_slice
  let max = List.length all
  let all_ud_edges = List.filter all ~f:is_ud_edge 
  let all_ud_slice_edges = List.filter all ~f:is_ud_slice 
  let pp = Variants.to_name
end

module Corner = struct
  (* Corners will have orientation in Z3 *)
  type t = { c : Corner_facelet.t ; o : Modular_int.Z3.t } [@@deriving compare]

  let all = List.map Corner_facelet.all ~f:(fun c -> { c ; o = Modular_int.Z3.of_int 0 })
  let pp { c ; o } =
    Corner_facelet.pp c ^ (Modular_int.Z3.to_int o |> string_of_int)
end

module Edge = struct
  (* Edges will have orientation in Z2 *)
  type t = { e : Edge_facelet.t ; o : Modular_int.Z2.t } [@@deriving compare]

  let zero_ori = fun e -> { e ; o = Modular_int.Z2.of_int 0 }

  let is_ud_slice { e ; o=_ } = Edge_facelet.is_ud_slice e
  let is_ud_edge  { e ; o=_ } = Edge_facelet.is_ud_edge e
  let all = List.map Edge_facelet.all ~f:zero_ori
  let all_ud_edges = List.map Edge_facelet.all_ud_edges ~f:zero_ori
  let all_ud_slice_edges = List.map Edge_facelet.all_ud_slice_edges ~f:zero_ori
  let pp { e ; o } =
    Edge_facelet.pp e ^ (Modular_int.Z2.to_int o |> string_of_int)
end

type t =
  | Corner of Corner.t
  | Edge of Edge.t
  [@@deriving enumerate, compare]

let is_ud_slice = function Edge e -> Edge.is_ud_slice e | _ -> false
let is_ud_edge  = function Edge e -> Edge.is_ud_edge e | _ -> false
let pp = function
  | Corner c -> Corner.pp c
  | Edge   e -> Edge.pp   e
 