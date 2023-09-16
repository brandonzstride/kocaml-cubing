open Core
open OUnit2

[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-26"]

(*
  For this test, I generated a random sequence of moves, applied them to my physical
  cube, and recorded the final permutation in a "is replaced by" way.
  This test runs that move sequence and compares the resulting permutations and orientations
  in the corners, UD slice, and UD faces separately.
  Since one mistake when making the moves really blows up in your face, I assume that if
  this test passes, then the moves are all working properly.
*)
let test_move_sequence _ = 
  let open Cubie in
  let open Move.Faceturn in
  let open Move.Fixed_move in
  let corner_list = List.map ~f:(fun (a, b) -> Corner.{ c = a ; o = Modular_int.Z3.of_int b}) in
  let edge_list   = List.map ~f:(fun (a, b) ->   Edge.{ e = a ; o = Modular_int.Z2.of_int b}) in
  let p = [(B, 2); (D, 3); (B, 1); (L, 2); (B, 2); (D, 1); (U, 3); (F, 3); (B, 2);
           (U, 2); (D, 2); (R, 2); (B, 1); (R, 3); (L, 1); (F, 3); (R, 3); (B, 1);
           (D, 3); (F, 2); (L, 1); (R, 1); (F, 1); (U, 2); (B, 2)]
    |> List.map ~f:(fun (a,b) -> { faceturn = a ; count = Modular_int.Z4.of_int b} |> Move.Fixed_move.to_move)
    |> Perm.of_move_list
  in
  assert_equal (Perm.to_corners_list p)        (corner_list [(URF, 2); (DLF, 0); (DFR, 1); (UBR, 0); (ULB, 1); (DBL, 1); (UFL, 0); (DRB, 1)]);
  assert_equal (Perm.to_ud_slice_edges_list p) (edge_list   [(UR, 0); (BL, 1); (BR, 1); (UF, 1)]);
  assert_equal (Perm.to_ud_edges_list p)       (edge_list   [(UL, 1); (DL, 0); (UB, 1); (DF, 1); (FL, 0); (DR, 0); (DB, 1); (FR, 1)])

(* Iterates through all possible coordinates and asserts that each is uniquely calculated *)
let test_coordinate_inverses (module Raw_coord : Coordinate.Raw) =
  let verify_inverse x =
    assert_equal (Raw_coord.to_rank x) (x |> Raw_coord.invert |> Raw_coord.calculate |> Raw_coord.to_rank)
  in
  let rec loop = function
  | None -> ()
  | Some x -> verify_inverse x; loop @@ Raw_coord.next x
  in
  loop (Some Raw_coord.zero)

let test_perm_coord_inverses _ =
  let open Coordinate.Phase2 in
  test_coordinate_inverses (module Edge_perm);
  test_coordinate_inverses (module Corner_perm_raw);
  test_coordinate_inverses (module UD_slice_perm)

let test_ori_coord_inverses _ =
  let open Coordinate.Phase1 in
  test_coordinate_inverses (module Twist);
  test_coordinate_inverses (module Flip)
  
(* this is for saving memoized results in the test folder *)
module M (F : sig val filename : string end) : Coordinate.Memoization =
  struct
    let is_already_saved = false
    let move_save_location = "./lookup_tables/coordinates/move/" ^ F.filename
    let sym_save_location = "./lookup_tables/coordinates/sym/" ^ F.filename
  end

module M_twist  = M (struct let filename = "twist_test.sexp" end)
module M_edge_perm = M (struct let filename = "edge_perm_test.sexp" end)

let test_memoized_coordinates_inverses _ =
  let open Coordinate.Phase1 in
  let open Coordinate.Phase2 in
  test_coordinate_inverses (module Coordinate.Memoize_raw (Twist) (M_twist))
  (* this is commented out because it takes a long time. I should see about speeding it up *) 
  (* test_coordinate_inverses (module Coordinate.Memoize_raw (Edge_perm) (M_edge_perm)) *)
;; 

print_endline (Core_unix.getcwd ())
;;

let cube_tests = "cube tests" >: test_list [
  "move sequence" >:: test_move_sequence;
  "perm coord inverses" >:: test_perm_coord_inverses;
  "ori coord inverses" >:: test_ori_coord_inverses;
  "memoized coord inverses" >:: test_memoized_coordinates_inverses;
]

let series = "series" >::: [
  cube_tests;
]

let () =
  run_test_tt_main series