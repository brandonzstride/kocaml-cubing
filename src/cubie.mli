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
    Moves and coordinates need to interact directly with cubies. This
    module satisfies the need to work hands-on with the raw cube.

  Dependencies:
    Modular_int -- for orientations of edges (Z2) and corners (Z3)
*)

module T :
  sig
    module Corner :
      sig
        type t = URF | UFL | ULB | UBR | DFR | DLF | DBL | DRB [@@deriving enumerate, compare]
        (** [t] represents a corner cubie by the faces of the cube it touches.
            
            E.g. URF touches the Up, Right, and Front faces. *)

        val n : int 
        (** [n] is the number of corner cubies. *)

        val pp : t -> string
        (** [pp c] "pretty prints" the corner cubie [c] to a string. *)
      end

    module Edge :
      sig
        type t = UR | UF | UL | UB | DR | DF | DL | DB | FR | FL | BL | BR [@@deriving enumerate, compare]
        (** [t] represents an edge cubie by the faces of the cube it touches. *)

        val is_ud_slice : t -> bool
        (** [is_ud_slice e] is true if and only if the cubie [e] is in the slice between
            the U and D faces of the cube. i.e. it is in the middle horizontal slice. *)

        val is_ud_edge : t -> bool
        (** [is_ud_edge e] is true if and only if the cubie [e] is in the U or D faces. *)

        val n : int
        (** [n] is the number of edge cubies. *)

        val all_ud_edges : t list
        (** [all_ud_edges] is a list of all edges [e] that satisfy [is_ud_edge e]. *)

        val all_ud_slice_edges : t list
        (** [all_ud_slice_edges] is a list of all edges [e] that satisfy [is_ud_slice e]. *)

        val pp : t -> string
        (** [pp e] "pretty prints" the edge cubie [e] to a string. *)
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
(** [is_ud_slice cubie] is true if and only if [cubie] is an edge [e] that satisfies
    [Edge.is_ud_slice e]. *)

val is_ud_edge : t -> bool
(** [is_ud_edge cubie] is true if and only if [cubie] is an edge [e] that satisfies
    [Edge.is_ud_edge e]. *)

val pp : t -> string
(** [pp cubie] "pretty prints" the edge cubie to a string. *)

val edge_exn : t -> Edge.t
(** [edge_exn cubie] gets the underlying [Edge.t] from [cubie], or fails with exception. *)

val corner_exn : t -> Corner.t
(** [corner_exn cubie] gets the underlying [Corner.t] from [cubie], or fails with exception. *)

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

    val of_cubie : T.t -> t
    (** [of_cubie cubie] applies zero orientation to [cubie]. *)

    val to_cubie : t -> T.t
    (** [to_cubie cubie_with_orientation] strips the orientation from [cubie_with_orientation]. *)

    (* For the following functions, find the representative function earlier in this file. *)
    val is_ud_slice : t -> bool
    val is_ud_edge : t -> bool
    val edge_exn : t -> Edge.t
    val corner_exn : t -> Corner.t
    val pp : t -> string
  end