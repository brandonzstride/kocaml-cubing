(* This is a permutation of a cube. In reality, it can be represented as a move *)

type t = Move.t

val to_corners_list : t -> Cubie.Corner.t list
val to_edges_list : t -> Cubie.Edge.t list
val to_ud_edges_list : t -> Cubie.Edge.t list
val to_ud_slice_edges_list : t -> Cubie.Edge.t list