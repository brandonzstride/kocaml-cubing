(* Define a cyclic group of some size *)

module type Group = sig
  val operation : [> `Multiplication | `Addition]
  val n : int
end

module type S = sig
  type t 
  val ( * ) : t -> t -> t
  val int_equal : t -> int -> bool
  val int_compare : t -> int -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val of_int : int -> t
  val to_int : t -> int
  val n : int
  val all : t list
end

module Make (G : Group) : S = struct
  type t = int 

  let n = if G.n > 0 then G.n else failwith "invalid size for cyclic group"

  let op = match G.operation with `Multiplication -> ( * ) | `Addition -> ( + )

  let ( * ) a b = (op a b) mod n
  let equal = Core.Int.equal
  let compare = Core.Int.compare
  let int_equal = equal
  let int_compare = compare
  let of_int x = x mod n
  let to_int x = x
  let all = Core.List.init n ~f:Core.Fn.id

end

module Z2 = Make (
  struct
    let operation = `Addition
    let n = 2
  end
)

module Z3 = Make (
  struct
    let operation = `Addition
    let n = 3 
  end
)

module Z4 = Make (
  struct
    let operation = `Addition
    let n = 4
  end
)