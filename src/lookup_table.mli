(*
  We use lookup tables to save results of computationally expensive operations.
  Some of these operations are:
  * Map coordinate and move to new coordinate
  * Map coordinate and symmetry to new coordinate
  * Map representative symmetry coordinate and move to new symmetry coordinate
  * Map coordinate (potentially tuple of coordinate) to some integer
  * Map two symmetries to new symmetry

  The way we will represent this is by N-dimensional arrays, and each dimension has a to_rank function.

  To create a lookup table, we will suppose that everything is already in a list, and we save exactly as is.

*)

module type Key =
  sig
    type t
    val to_rank : t -> int
    val n : int
  end

module type Return_type =
  sig
    type t [@@deriving sexp]
  end

module Make1D (Key : Key) (R : Return_type) :
  sig
    type t
    val from_file : string -> t
    val to_file : t -> string -> unit
    val create : R.t list -> t
    val lookup : t -> Key.t -> R.t
  end

module Make2D (Key1 : Key) (Key2 : Key) (R : Return_type) :
  sig
    type t
    val from_file : string -> t
    val to_file : t -> string -> unit
    val create : R.t list list -> t
    val lookup : t -> Key1.t -> Key2.t -> R.t
  end
