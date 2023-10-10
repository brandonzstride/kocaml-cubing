(*
  File: modular_int.mli
  Module purpose: holds finite cyclic groups.
  Status: complete.

  Detailed description:
    This holds the "Z mod n" groups. The Rubik's cube group
    relies on these groups for turns of the faces and for
    cubie orientations.

    Addition is the group operation, but multiplication is
    implemented.
*)

module type S = sig
  type t [@@deriving sexp]
  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
  val inverse : t -> t (* additive inverse *)
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val of_int : int -> t
  val to_int : t -> int
  val n : int
  val all : t list
  val zero : t
end

module Z2 : S
module Z3 : S
module Z4 : S 

(* Must call this _ because the Group is not used in signature *)
module Make (_ : sig
    val n : int
  end) : S