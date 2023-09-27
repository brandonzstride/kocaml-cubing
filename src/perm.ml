open Core

type t = Move.t

let identity = Fn.id

let is_identity (p : t) : bool =
  List.for_all Cubie.all ~f:(fun c -> Cubie.compare c (p c) = 0)

let perform_move (p : t) (m : Move.t) =
  Move.(m * p) (* Does p first and then m *)

let of_move_list (ls : Move.t list) : t =
  List.fold ls ~init:identity ~f:perform_move

let to_corners_list (p : t) : Cubie.Corner.t list =
  let open List.Let_syntax in
  let%map x = Cubie.Corner.all in
  match p (Cubie.Corner x) with
  | Corner c -> c
  | _ -> failwith "ERROR: Corner maps to edge in permutation"

let map_edges (p : t) (ls : Cubie.Edge.t list) : Cubie.Edge.t list =
  let open List.Let_syntax in
  let%map x = ls in
  match p (Cubie.Edge x) with
  | Edge e -> e
  | _ -> failwith "ERROR: Edge maps to corner in permutation"

let to_edges_list (p : t) : Cubie.Edge.t list =
  map_edges p Cubie.Edge.all

let to_ud_edges_list (p : t) : Cubie.Edge.t list =
  map_edges p Cubie.Edge.all_ud_edges

let to_ud_slice_edges_list (p : t) : Cubie.Edge.t list =
  map_edges p Cubie.Edge.all_ud_slice_edges

let pp (p : t) =
  let pp c = Cubie.pp c ^ " -> " ^ Cubie.pp (p c) in
  Cubie.all
  |> List.map ~f:pp
  |> List.fold ~init:"" ~f:(fun accum s -> accum ^ "\n" ^ s)