
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
  val all_rev : t list
end

module Edge_facelet : sig
  (* Facelets are written starting with reference facelet. *)
  type t = UR | UF | UL | UB | DR | DF | DL | DB | FR | FL | BL | BR [@@deriving enumerate, compare]
  val max : int
  val all_rev : t list
end

module Corner : sig
  (* Corners will have orientation in Z3 *)
  (* We'll run into a problem where Z3 is the result of a functor but isn't a subtype of Modular_int.t *)
  type t = { c : Corner_facelet.t ; o : Modular_int.Z3.t }
end

module Edge : sig
  (* Edges will have orientation in Z2 *)
  type t = { e : Edge_facelet.t ; o : Modular_int.Z2.t }
end

type t =
  | Corner of Corner.t
  | Edge of Edge.t