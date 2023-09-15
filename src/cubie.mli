
module Corner_facelet : sig
  (* Facelets are written in clockwise order, starting with reference facelet *)
  (*
    enumerate defines
      all : t list   
    
    compare defines
      compare : t -> t -> int
  *)
  type t = URF | UFL | ULB | UBR | DFR | DLF | DBL | DRB [@@deriving enumerate, compare]
  val max : int 
end

module Edge_facelet : sig
  (* Facelets are written starting with reference facelet. *)
  type t = UR | UF | UL | UB | DR | DF | DL | DB | FR | FL | BL | BR [@@deriving enumerate, compare]
  val max : int
  val all_ud_edges : t list
  val all_ud_slice_edges : t list
end

module Corner : sig
  (* Corners will have orientation in Z3 *)
  type t = { c : Corner_facelet.t ; o : Modular_int.Z3.t } [@@deriving compare]

  val all : t list (* all corners with 0 orientation *)
end

module Edge : sig
  (* Edges will have orientation in Z2 *)
  type t = { e : Edge_facelet.t ; o : Modular_int.Z2.t } [@@deriving compare]

  val all : t list (* all edges with 0 orientation *)
  val all_ud_edges : t list
  val all_ud_slice_edges : t list
end

(* Compare has all corners less than edges. *)
type t =
  | Corner of Corner.t
  | Edge of Edge.t
  [@@deriving enumerate, compare]