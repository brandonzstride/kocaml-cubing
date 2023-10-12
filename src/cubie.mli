(*
  File: cubie.mli
  Module purpose: describes "sub-cubes"/"cubies" of the Rubik's cube.
  Status: complete.

  Detailed description:
    The cubies are the "little cubes" on the Rubik's cube. They are
    corner pieces and edge pieces, and they are labeled with the faces
    of the cube they touch. For example, the URF cubie is the corner
    on the up, right, and front faces of the cube. A cubie has an
    orientation with respect to the solved state.

    To be extra clear: the cubie's orientation is defined as how many
    clockwise twists its reference facelet is from the solved position
    of the reference facelet. The reference facelet is the facelet that
    makes up the first character of the cubie (e.g. "U" in the URF cubie).

  Expected usage:
    Moves and coordinates need to interact directly with cubiess. This
    module satisfies the need to work hands-on with the raw cube.
*)

module T :
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
  end

module Edge = T.Edge
module Corner = T.Corner

(* Must reveal this so that Cubie.Corner and Cubie.Edge can be found as constructors *)
type t = T.t = 
  | Corner of Corner.t
  | Edge of Edge.t
  [@@deriving enumerate, compare]


val is_ud_slice : t -> bool
val is_ud_edge : t -> bool
val pp : t -> string
val edge_exn : t -> Edge.t
val corner_exn : t -> Corner.t

module With_orientation :
  sig
    module Corner:
      sig
        type t = { c : T.Corner.t ; o : Modular_int.Z3.t } [@@deriving compare]
        val pp : t -> string
      end

    module Edge :
      sig
        type t = { e : T.Edge.t ; o : Modular_int.Z2.t } [@@deriving compare]
        val pp : t -> string
      end

    type t =
      | Corner of Corner.t
      | Edge of Edge.t
      [@@deriving compare]

    val of_cubie : T.t -> t (** Gives zero orientation *)
    val to_cubie : t -> T.t (** Ignores orientation *)
    val is_ud_slice : t -> bool
    val is_ud_edge : t -> bool
    val edge_exn : t -> Edge.t
    val corner_exn : t -> Corner.t
    val pp : t -> string
  end