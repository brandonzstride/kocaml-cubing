
open Core

module T =
  struct
    module Corner =
      struct
        type t = URF | UFL | ULB | UBR | DFR | DLF | DBL | DRB [@@deriving enumerate, compare, variants]
        let n = List.length all
        let pp = Variants.to_name
      end

    module Edge =
      struct
        type t = UR | UF | UL | UB | DR | DF | DL | DB | FR | FL | BL | BR [@@deriving enumerate, compare, variants]
        let n = List.length all
        let is_ud_slice = function FR | FL | BL | BR -> true | _ -> false
        let is_ud_edge  = Fn.non is_ud_slice
        let all_ud_edges = List.filter all ~f:is_ud_edge
        let all_ud_slice_edges = List.filter all ~f:is_ud_slice
        let pp = Variants.to_name
      end

    type t =
      | Corner of Corner.t
      | Edge of Edge.t
      [@@deriving enumerate, compare]
  end

include T

let is_ud_slice = function
  | Edge e -> Edge.is_ud_slice e
  | _ -> false

let is_ud_edge = function
  | Edge e -> Edge.is_ud_edge e
  | _ -> false

let pp = function
  | Corner c -> "Corner " ^ Corner.pp c
  | Edge e -> "Edge " ^ Edge.pp e

let edge_exn = function Edge e -> e | _ -> failwith "Expected edge in `edge_exn`, but got corner"
let corner_exn = function Corner c -> c | _ -> failwith "Expected corner in `corner_exn`, but got edge"


module With_orientation =
  struct

    module Corner =
      struct
        type t = { c : T.Corner.t ; o : Modular_int.Z3.t } [@@deriving compare]
        let pp { c ; o } = T.Corner.pp c ^ Int.to_string (Modular_int.Z3.to_int o)
      end

    module Edge =
      struct
        type t = { e : T.Edge.t ; o : Modular_int.Z2.t } [@@deriving compare]
        let pp { e ; o } = T.Edge.pp e ^ Int.to_string (Modular_int.Z2.to_int o)
      end

    type t =
      | Corner of Corner.t
      | Edge of Edge.t
      [@@deriving compare]

    let pp = function
      | Corner c -> "Corner " ^ Corner.pp c
      | Edge e -> "Edge " ^ Edge.pp e

    let of_cubie = function
    | T.Edge e -> Edge { e ; o = Modular_int.Z2.zero }
    | T.Corner c -> Corner { c ; o = Modular_int.Z3.zero }

    let to_cubie = function
    | Edge e -> T.Edge e.e
    | Corner c -> T.Corner c.c

    let is_ud_slice = function
    | Edge e -> T.Edge.is_ud_slice e.e
    | _ -> false

    let is_ud_edge = function
    | Edge e -> T.Edge.is_ud_edge e.e
    | _ -> false

    let edge_exn = function Edge e -> e | _ -> failwith "Expected edge in `edge_exn`, but got corner"
    let corner_exn = function Corner c -> c | _ -> failwith "Expected corner in `corner_exn`, but got edge"

  end