open Core

(* Define a cyclic group of some size *)

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
end

module Make (G : sig
    val n : int
  end) : S = struct
  type t = int [@@deriving sexp]

  let n = if G.n > 0 then G.n else failwith "invalid size for cyclic group"

  let to_int x = x
  let rec of_int x = if x < 0 then Int.abs x |> of_int |> inverse else x mod n
  and inverse x = of_int (n - x)
  let ( + ) a b = (a + b) |> of_int 
  let ( * ) a b = (a * b) |> of_int
  let equal = Int.equal
  let compare = Int.compare
  let int_equal = equal
  let int_compare = compare
  let all = List.init n ~f:of_int
  let inc x = Int.(+) 1 x |> of_int
  let dec x = Int.(+) (-1) x |> of_int

end

module Z2 = Make (
  struct
    let n = 2
  end
)

module Z3 = Make (
  struct
    let n = 3 
  end
)

module Z4 = Make (
  struct
    let n = 4
  end
)