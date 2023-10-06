module type S = sig
  type t [@@deriving sexp]
  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
  val inverse : t -> t
  val int_equal : t -> int -> bool
  val int_compare : t -> int -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val of_int : int -> t
  val to_int : t -> int
  val inc : t -> t
  val dec : t -> t
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