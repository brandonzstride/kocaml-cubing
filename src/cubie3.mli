
(** It seems like this doesn't work, and I'll need to think of another way because
    users can't find Cubie.Edge.t, or similar, because they only find Cubie_intf.Edge.t
    when using Cubie. *)
include Cubie_intf.T

module With_orientation :
  sig
    module Corner:
      sig
        (** References Cubie.Corner--not a recursive type *)
        type t = { c : Cubie_intf.Corner.t ; o : Modular_int.Z3.t } [@@deriving compare]
        val pp : t -> string
      end

    module Edge :
      sig
        (** References Cubie.Edge--not a recursive type *)
        type t = { e : Cubie_intf.Edge.t ; o : Modular_int.Z2.t } [@@deriving compare]
        val pp : t -> string
      end

    type t =
      | Corner of Corner.t
      | Edge of Edge.t
      [@@deriving compare]

    val of_cubie : Cubie_intf.t -> t (** Gives zero orientation *)
    val is_ud_slice : t -> bool
    val is_ud_edge : t -> bool
    val edge_exn : t -> Edge.t
    val corner_exn : t -> Corner.t
    val pp : t -> string
  end