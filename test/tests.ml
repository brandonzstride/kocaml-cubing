open Core
open OUnit2

(*
  -----
  SETUP   
  -----
*)

(* module Config =
  struct
    module M =
      struct
        type t =
          { module_name : string } [@@deriving sexp]
      end

    type t =
      { test_directory : string 
      ; modules : M.t list } [@@deriving sexp]

    let save x = Sexp.save (x.test_directory ^ "confix.sexp") (sexp_of_t x)
  end

let config =
  let open Config in
  { test_directory = "C:/Users/brand/Documents/kocaml-cubing/test/"
  ; modules = [] }

Config.save config *)


(*
  ----------
  TEST MOVES   
  ----------
*)

let perm_of_edge_list ls =
  let x = 
  ls
  |> List.map ~f:(fun (e, o) -> Cubie.Edge Cubie.Edge.{ e ; o = Modular_int.Z2.of_int o })
  |> List.zip_exn Cubie.Edge.all
  in
  function
  | Cubie.Corner _ as c -> c
  | Cubie.Edge e -> List.find_exn x ~f:(fun (a, _) -> Cubie.Edge.compare e a = 0) |> Tuple2.get2

let perm_of_corner_list ls =
  let x = 
  ls
  |> List.map ~f:(fun (c, o) -> Cubie.Corner Cubie.Corner.{ c ; o = Modular_int.Z3.of_int o })
  |> List.zip_exn Cubie.Corner.all
  in
  function
  | Cubie.Edge _ as e -> e
  | Cubie.Corner c -> List.find_exn x ~f:(fun (a, _) -> Cubie.Corner.compare c a = 0) |> Tuple2.get2

(*
  For this test, I generated a random sequence of moves, applied them to my physical
  cube, and recorded the final permutation in a "is replaced by" way.
  This test runs that move sequence and compares the resulting permutations and orientations
  in the corners, UD slice, and UD faces separately.
  Since one mistake when making the moves really blows up in your face, I assume that if
  this test passes, then the moves are all working properly.
*)
let test_move_sequence = "move sequence" >:: (fun _ ->
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
  )


(*
  ----------------
  TEST COORDINATES   
  ----------------

  There are a few ways that coordinates need to be tested.

  I should start with some known permutations and hand-calculate the coordinates
  I expect them to have. This will be time-consuming, but it's necessary to assert
  that I wrote the coordinates correctly.
  
  I need to assert
  that every coordinate has to_perm and of_perm as inverses.
    i.e. x |> to_perm |> of_perm => x
  
  I also need to check that if I apply a sequence of moves to some perm, and
  I apply the same sequence to its coordinate and then convert back to perm, 
  that the resulting perms are equivalent. I can start with a random coord
  to do this because I know that coords appropriately generate permutations 
  from the tests above.

  Further, I need to assert that memoization works; it should not affect behavior
  at all. I should be able to assert that any function behaves exactly the same as
  it did before it was memoized.
*)

let test_coord_equal x p (module T : Coordinate.T) _ =
  assert_equal x (p |> T.of_perm |> T.to_rank)

let test_corner_coords = 
  let p = perm_of_corner_list [(DFR, 2); (UFL, 0); (ULB, 1); (URF, 2); (DRB, 2); (DLF, 0); (DBL, 0); (UBR, 2)] in
  "test corner_coords" >::: [
    "twist id" >:: test_coord_equal 0 Fn.id (module Coordinate.Twist.Raw);
    "test twist" >:: test_coord_equal 1611 p (module Coordinate.Twist.Raw);
    "perm id" >:: test_coord_equal 0 Fn.id (module Coordinate.Corner_perm.Raw);
    "test perm" >:: test_coord_equal 21021 p (module Coordinate.Corner_perm.Raw);
  ]

let test_phase1_edge_coords =
  let p = perm_of_edge_list [(UL, 1); (UF, 1); (DF, 0); (FL, 0); (DR, 0); (BL, 1);
                             (DL, 1); (UR, 0); (BR, 1); (DB, 0); (UB, 0); (FR, 1)] in
  "test phase1 edge coords" >::: [
    "flip id" >:: test_coord_equal 0 Fn.id (module Coordinate.Flip.Raw);
    "test flip" >:: test_coord_equal 1588 p (module Coordinate.Flip.Raw);
    "ud slice id" >:: test_coord_equal 0 Fn.id (module Coordinate.UD_slice.Raw);
    "test ud slice" >:: test_coord_equal 95 p (module Coordinate.UD_slice.Raw);
    "flip ud slice id" >:: test_coord_equal 0 Fn.id (module Coordinate.Flip_UD_slice.Raw);
    "test flip ud slice id" >:: test_coord_equal 196148 p (module Coordinate.Flip_UD_slice.Raw);
  ]

(* I will naively assume for now that since the corner permutation works,
   and since the phase2 edge coordinates are permutations using the same
   framework, then the phase2 edge coordinates work as well. *)

let test_counts (module T : Coordinate.T) _ =
  let rec loop i = function
  | None -> i
  | Some x -> loop (i + 1) (T.next x)
  in
  assert_equal (loop 0 (Some T.zero)) T.n

let test_counts = "raw coord counts" >::: [
  "twist"         >:: test_counts (module Coordinate.Twist.Raw);
  "flip"          >:: test_counts (module Coordinate.Flip.Raw);
  "UD slice"      >:: test_counts (module Coordinate.UD_slice.Raw);
  "flip UD slice" >:: test_counts (module Coordinate.Flip_UD_slice.Raw);
  "edge perm"     >:: test_counts (module Coordinate.Edge_perm.Raw);
  "corner perm"   >:: test_counts (module Coordinate.Corner_perm.Raw);
  "UD slice perm" >:: test_counts (module Coordinate.UD_slice_perm.Raw);
]

let test_inverses (module T : Coordinate.T) _ =
  (* T has compare, so assert_equal will work *)
  let verify_inverse x =
    assert_equal x (x |> T.to_perm |> T.of_perm)
  in
  let rec loop = function
  | None -> ()
  | Some x -> verify_inverse x; loop @@ T.next x
  in
  loop (Some T.zero)


let test_raw_perm_coord_inverses = "raw perm coord inverses" >::: [
    "edge" >:: test_inverses (module Coordinate.Edge_perm.Raw);
    "corner" >:: test_inverses (module Coordinate.Corner_perm.Raw);
    "UD slice" >:: test_inverses (module Coordinate.UD_slice_perm.Raw);
  ]

let test_raw_ori_coord_inverses = "raw ori coord inverses" >::: [
    "twist" >:: test_inverses (module Coordinate.Twist.Raw);
    "flip" >:: test_inverses (module Coordinate.Flip.Raw);
  ]

module M (F : sig val coord_name : string end) : Coordinate.Memo_params =
  struct
    let status = `Needs_computation
    let (^/) a b = a ^ "/" ^ b
    let path = "C:/Users/brand/Documents/kocaml-cubing/test/lookup_tables/coordinates"
    let move_filepath = path ^/ "move" ^/ F.coord_name ^ "_move_table.sexp"
    let symmetry_filepath = path ^/ "sym" ^/ F.coord_name ^ "_sym_table.sexp"
  end

module M_twist = M (struct let coord_name = "twist" end)
module M_edge_perm = M (struct let coord_name = "edge_perm" end)

module Twist_memo = Coordinate.Twist.Make_memoized_coordinate (M_twist)
module Edge_perm_memo = Coordinate.Edge_perm.Make_memoized_coordinate (M_edge_perm)

let random_fixed_move _ =
  Move.Fixed_move.n
  |> Random.int 
  |> List.nth_exn Move.Fixed_move.all

let random_fixed_move_list _ =
  List.init 40 ~f:random_fixed_move
  

let test_coord_move_sequence = 
  let p = (* random starting permutation *)
    random_fixed_move_list ()
    |> List.map ~f:Move.Fixed_move.to_move
    |> Perm.of_move_list
  in
  let move_list = random_fixed_move_list () in
  let p' = 
    move_list
    |> List.fold ~init:p ~f:(fun p m -> m |> Move.Fixed_move.to_move |> Perm.perform_move p)
  in
  let test_module (module T : Coordinate.T) _ =
    let x' = List.fold move_list ~init:(T.of_perm p) ~f:T.perform_fixed_move in
    assert_equal (T.of_perm p') x' (* cannot compare perms because some cubies are not relevant to coord *)
  in
  "coord move sequences" >::: [
    "twist memo" >:: test_module (module Twist_memo);
    "edge perm memo" >:: test_module (module Edge_perm_memo);
    "twist" >:: test_module (module Coordinate.Twist.Raw);
    "edge perm" >:: test_module (module Coordinate.Edge_perm.Raw);
    "corner perm" >:: test_module (module Coordinate.Corner_perm.Raw);
    (* "ud slice perm" >:: test_module (module Coordinate.UD_slice_perm.Raw); *)
    (* "ud slice" >:: test_module (module Coordinate.UD_slice.Raw); *)
    (* "flip ud slice" >:: test_module (module Coordinate.Flip_UD_slice.Raw); *)
]

(*   
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
  test_coordinate_inverses (module Coordinate.Memoize_raw (Twist) (M_twist));
  test_coordinate_inverses (module Coordinate.Memoize_raw (Edge_perm) (M_edge_perm))
;; 

print_endline (Core_unix.getcwd ())
;; *)

let cube_tests = "cube tests" >::: [
  test_corner_coords;
  test_phase1_edge_coords;
  test_counts;
  test_move_sequence;
  test_raw_perm_coord_inverses;
  test_raw_ori_coord_inverses;
  test_coord_move_sequence;
]

let series = "series" >::: [
  cube_tests;
]

let () =
  run_test_tt_main series