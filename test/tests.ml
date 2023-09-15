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

(*
  I will try a few different permutations and manually calculate the twist coordinate.
  Then make sure that invert cancels with calculate. If I check that calculate is the
  right inverse of invert, do I need to check the other way?
*)
let test_twist _ =
  ()

let cube_tests = "cube tests" >: test_list [
  "move sequence" >:: test_move_sequence;
  "twist" >:: test_twist;
]

let series = "series" >::: [
  cube_tests;
]

let () =
  run_test_tt_main series