
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

include T