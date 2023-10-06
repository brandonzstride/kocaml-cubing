
module Corner =
  struct
    type t = { c : Cubie.Corner.t ; o : Modular_int.Z3.t } [@@deriving compare]
    let pp { c ; o } = Cubie.Corner.pp c ^ Int.to_string (Modular_int.Z3.to_int o)
  end

module Edge =
  struct
    type t = { e : Cubie.Edge.t ; o : Modular_int.Z2.t } [@@deriving compare]
    let pp { e ; o } = Cubie.Edge.pp e ^ Int.to_string (Modular_int.Z2.to_int o)
  end

type t =
  | Corner of Corner.t
  | Edge of Edge.t
  [@@deriving compare]

let pp = function
  | Corner c -> "Corner " ^ Corner.pp c
  | Edge e -> "Edge " ^ Edge.pp e

let of_cubie = function
| Cubie.Edge e -> Edge { e ; o = Modular_int.Z2.zero }
| Cubie.Corner c -> Corner { c ; o = Modular_int.Z3.zero }

let is_ud_slice = function
| Edge e -> Cubie.Edge.is_ud_slice e.e
| _ -> false

let is_ud_edge = function
| Edge e -> Cubie.Edge.is_ud_edge e.e
| _ -> false

let edge_exn = function Edge e -> e | _ -> failwith "Expected edge in `edge_exn`, but got corner"
let corner_exn = function Corner c -> c | _ -> failwith "Expected corner in `corner_exn`, but got edge"