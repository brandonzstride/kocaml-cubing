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

  Dependencies:
    None
*)

module type S = sig
  type t [@@deriving sexp]
  val n : int
  (** [n] is the size of the group. *)

  val zero : t
  (** [zero] is the 0 element of the group; the additive identity. *)

  val ( + ) : t -> t -> t
  (** [a + b] is integer addition of [a] and [b], mod by [n]. *)

  val ( * ) : t -> t -> t
  (** [a * b] is integer multiplication of [a] and [b], mod by [n]. *)

  val inverse : t -> t
  (** [inverse x] is the additive inverse of the modular int [x]. *)

  val equal : t -> t -> bool
  (** [equal a b] is true if and only if [a] and [b] are the same mod [n]. *)

  val of_int : int -> t
  (** [of_int x] gets the [x] mod [n]. *)

  val to_int : t -> int
  (** [to_int x] casts the modular int [x] to an integer in Z. *)

  val all : t list
  (** [all] is a sorted list of all elements in the group. *)

  val compare : t -> t -> int
end

module Z2 : S
module Z3 : S
module Z4 : S 

(* Must call this _ because the Group is not used in signature *)
module Make (_ : sig
    val n : int
  end) : S