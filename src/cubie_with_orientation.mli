
(*
  I would prefer that this is a submodule of cubie
  (so it's Cubie_with_orientation), but the types don't
  work out nicely.
*)

module Corner:
  sig
    type t = { c : Cubie.Corner.t ; o : Modular_int.Z3.t } [@@deriving compare]
    val pp : t -> string
  end

module Edge :
  sig
    type t = { e : Cubie.Edge.t ; o : Modular_int.Z2.t } [@@deriving compare]
    val pp : t -> string
  end

type t =
  | Corner of Corner.t
  | Edge of Edge.t
  [@@deriving compare]

val of_cubie : Cubie.t -> t (** Gives zero orientation *)
val is_ud_slice : t -> bool
val is_ud_edge : t -> bool
val edge_exn : t -> Edge.t
val corner_exn : t -> Corner.t
val pp : t -> string