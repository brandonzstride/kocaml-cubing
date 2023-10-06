open Core

(*
  Actually implement the cubie interface (it's just meant to be a cubie with no orientation)   
*)

module type T =
  sig
    module Corner :
      sig
        type t = URF | UFL | ULB | UBR | DFR | DLF | DBL | DRB [@@deriving enumerate, compare]
        val n : int 
        val pp : t -> string
      end

    module Edge :
      sig
        type t = UR | UF | UL | UB | DR | DF | DL | DB | FR | FL | BL | BR [@@deriving enumerate, compare]
        val is_ud_slice : t -> bool
        val is_ud_edge : t -> bool
        val n : int
        val all_ud_edges : t list
        val all_ud_slice_edges : t list
        val pp : t -> string
      end

    type t =
      | Corner of Corner.t
      | Edge of Edge.t
      [@@deriving enumerate, compare]

    val is_ud_slice : t -> bool
    val is_ud_edge : t -> bool
    val pp : t -> string
    val edge_exn : t -> Edge.t
    val corner_exn : t -> Corner.t
  end

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