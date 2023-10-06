(* This is a permutation of a cube. In reality, it can be represented as a move *)

type t = Move.t

val identity : t
val is_identity : t -> bool
val perform_move : t -> Move.t -> t (* Move is performed after permutation *) 
val of_move_list : Move.t list -> t (* earlier moves get performed first *)
val to_corners_list : t -> Cubie.With_orientation.Corner.t list
val to_edges_list : t -> Cubie.With_orientation.Edge.t list
val to_ud_edges_list : t -> Cubie.With_orientation.Edge.t list
val to_ud_slice_edges_list : t -> Cubie.With_orientation.Edge.t list
val pp : t -> string