(*
  We use lookup tables to save results of computationally expensive operations.
  Some of these operations are:
  * Map coordinate and move to new coordinate
  * Map coordinate and symmetry to new coordinate
  * Map representative symmetry coordinate and move to new symmetry coordinate
  * Map coordinate (potentially tuple of coordinate) to some integer
  * Map two symmetries to new symmetry

  The way we will represent this is by N-dimensional arrays, and each dimension has a to_rank function.

*)

module type Key =
  sig
    type t
    val to_rank : t -> int
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
    val of_list : R.t list -> t
    val create : 'a list -> f:('a -> R.t) -> t
    val lookup : t -> Key.t -> R.t
    val get_n : t -> int
  end

module Make2D (Key1 : Key) (Key2 : Key) (R : Return_type) :
  sig
    type t
    val from_file : string -> t
    val to_file : t -> string -> unit
    val create : ?n1:int -> ?n2:int -> 'a list -> 'b list -> f:('a -> 'b -> R.t) -> t
    val lookup : t -> Key1.t -> Key2.t -> R.t
  end
