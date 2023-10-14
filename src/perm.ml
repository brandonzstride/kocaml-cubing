open Core

type t = Move.t

let identity = Move.id

let is_identity (p : t) : bool =
  List.for_all Cubie.all ~f:(fun cubie ->
    0 = Cubie.With_orientation.compare (Cubie.With_orientation.of_cubie cubie) (p cubie)
    )
  
let perform_move (p : t) (m : Move.t) =
  Move.(p * m) (* Does p first and then m *)

let perform_fixed_move (p : t) (m : Move.Fixed.Super.t) : t =
  m |> Move.Fixed.G.to_move |> perform_move p

let perform_fixed_move_list (p : t) (ls : Move.Fixed.Super.t list) : t =
  ls
  |> List.fold ~init:p ~f:(fun p m -> m |> Move.Fixed.G.to_move |> perform_move p)

let of_move_list (ls : Move.t list) : t =
  List.fold ls ~init:identity ~f:perform_move

let to_corners_list (p : t) : Cubie.With_orientation.Corner.t list =
  let open List.Let_syntax in
  let%map x = Cubie.Corner.all in
  match p (Cubie.Corner x) with
  | Corner c -> c
  | _ -> failwith "ERROR: Corner maps to edge in permutation"

let map_edges (p : t) (ls : Cubie.Edge.t list) : Cubie.With_orientation.Edge.t list =
  let open List.Let_syntax in
  let%map x = ls in
  match p (Cubie.Edge x) with
  | Edge e -> e
  | _ -> failwith "ERROR: Edge maps to corner in permutation"

let to_edges_list (p : t) : Cubie.With_orientation.Edge.t list =
  map_edges p Cubie.Edge.all

let to_ud_edges_list (p : t) : Cubie.With_orientation.Edge.t list =
  map_edges p Cubie.Edge.all_ud_edges

let to_ud_slice_edges_list (p : t) : Cubie.With_orientation.Edge.t list =
  map_edges p Cubie.Edge.all_ud_slice_edges

let pp (p : t) =
  let pp c = Cubie.pp c ^ " -> " ^ Cubie.With_orientation.pp (p c) in
  Cubie.all
  |> List.map ~f:pp
  |> List.fold ~init:"" ~f:(fun accum s -> accum ^ "\n" ^ s)