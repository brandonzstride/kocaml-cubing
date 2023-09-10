open Core

module Corner_facelet = struct 
  type t = URF | UFL | ULB | UBR | DFR | DLF | DBL | DRB [@@deriving enumerate, compare]

  let max = List.length all
  let all_rev = List.rev all
end

module Edge_facelet = struct
  (* Facelets are written starting with reference facelet. *)
  type t = UR | UF | UL | UB | DR | DF | DL | DB | FR | FL | BL | BR [@@deriving enumerate, compare]

  let max = List.length all
  let all_rev = List.rev all
  let all_ud_edges = [ UR ; UF ; UL ; UB ; DR ; DF ; DL ; DB ]
  let all_ud_slice_edges = [ FR ; FL ; BL ; BR ]
end

module Corner = struct
  (* Corners will have orientation in Z3 *)
  type t = { c : Corner_facelet.t ; o : Modular_int.Z3.t } [@@deriving compare]

  let all = List.map Corner_facelet.all ~f:(fun c -> { c ; o = Modular_int.Z3.of_int 0 })
  let all_rev = List.rev all
end

module Edge = struct
  (* Edges will have orientation in Z2 *)
  type t = { e : Edge_facelet.t ; o : Modular_int.Z2.t } [@@deriving compare]

  let zero_ori = fun e -> { e ; o = Modular_int.Z2.of_int 0 }

  let all = List.map Edge_facelet.all ~f:zero_ori
  let all_rev = List.rev all
  let all_ud_edges = List.map Edge_facelet.all_ud_edges ~f:zero_ori
  let all_ud_slice_edges = List.map Edge_facelet.all_ud_slice_edges ~f:zero_ori
end

type t =
  | Corner of Corner.t
  | Edge of Edge.t
  [@@deriving enumerate]
