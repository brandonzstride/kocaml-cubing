module Corner_facelet = struct 
  type t = URF | UFL | ULB | UBR | DFR | DLF | DBL | DRB [@@deriving enumerate, compare]

  let max = Core.List.length all
  let all_rev = Core.List.rev all
end

module Edge_facelet = struct
  (* Facelets are written starting with reference facelet. *)
  type t = UR | UF | UL | UB | DR | DF | DL | DB | FR | FL | BL | BR [@@deriving enumerate, compare]

  let max = Core.List.length all
  let all_rev = Core.List.rev all
end

module Corner = struct
  (* Corners will have orientation in Z3 *)
  type t = { c : Corner_facelet.t ; o : Modular_int.Z3.t }
end

module Edge = struct
  (* Edges will have orientation in Z2 *)
  type t = { e : Edge_facelet.t ; o : Modular_int.Z2.t }
end

type t =
  | Corner of Corner.t
  | Edge of Edge.t