(* This `include` defines the cubies with no orientation *)
include Cubie_intf

module With_orientation =
  struct
    module Corner =
      struct
        type t = { c : Cubie_intf.Corner.t ; o : Modular_int.Z3.t } [@@deriving compare]
        let pp { c ; o } = Cubie_intf.Corner.pp c ^ Int.to_string (Modular_int.Z3.to_int o)
      end

    module Edge =
      struct
        type t = { e : Cubie_intf.Edge.t ; o : Modular_int.Z2.t } [@@deriving compare]
        let pp { e ; o } = Cubie_intf.Edge.pp e ^ Int.to_string (Modular_int.Z2.to_int o)
      end

    type t =
      | Corner of Corner.t
      | Edge of Edge.t
      [@@deriving compare]

    let pp = function
      | Corner c -> "Corner " ^ Corner.pp c
      | Edge e -> "Edge " ^ Edge.pp e

    let of_cubie = function
    | Cubie_intf.Edge e -> Edge { e ; o = Modular_int.Z2.zero }
    | Cubie_intf.Corner c -> Corner { c ; o = Modular_int.Z3.zero }
    
    let is_ud_slice = function
    | Edge e -> Cubie_intf.Edge.is_ud_slice e.e
    | _ -> false

    let is_ud_edge = function
    | Edge e -> Cubie_intf.Edge.is_ud_edge e.e
    | _ -> false

    let edge_exn = function Edge e -> e | _ -> failwith "Expected edge in `edge_exn`, but got corner"
    let corner_exn = function Corner c -> c | _ -> failwith "Expected corner in `corner_exn`, but got edge"
  end
