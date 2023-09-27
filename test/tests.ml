open Core
open OUnit2

[@@@ocaml.warning "-26"]
[@@@ocaml.warning "-32"]

(*
  -----
  SETUP   
  -----
*)

(*
  -----------
  COORDINATES   
  -----------

  Most of my tests here will be about coordinates. I need to be sure that
  they are calculated and inverted correctly, and that they work well under
  moves. However, cubes have to be formed a certain way for them to work.
  I may get errors resulting from logical impossibilities if the input cube
  is malformed or not in the correct phase.

  E.g. the UD slice perm coord is not defined on a cube that is not in G1.

  I therefore split up the coordinates into Phase1 and Phase2 here. I could
  do this in coordinates, but I prefer to avoid that nesting. I may change
  my mind on this later, and it will be a simple change.
*)

module C = Coordinate

module Ref_module =
  struct
    type t =
      { m : (module C.T) ref
      ; name : string }
  end

(* I use reference cells to hold modules. *)
let raw_phase1 = let open Ref_module in [
  { m = ref (module C.Twist.Raw : C.T)         ; name = "twist raw"         };
  { m = ref (module C.Flip.Raw : C.T)          ; name = "flip raw"          };
  { m = ref (module C.UD_slice.Raw : C.T)      ; name = "ud slice raw"      };
  { m = ref (module C.Flip_UD_slice.Raw : C.T) ; name = "flip ud slice raw" };
]

let raw_phase2 = let open Ref_module in [
  { m = ref (module C.Corner_perm.Raw : C.T)   ; name = "corner perm raw"   };
  { m = ref (module C.Edge_perm.Raw : C.T)     ; name = "edge perm raw"     };
  { m = ref (module C.UD_slice_perm.Raw : C.T) ; name = "ud slice perm raw" };
]

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

let make_test test_fun test_name (m : Ref_module.t) =
  (m.name ^ " " ^ test_name) >:: test_fun (m.m.contents)

let test_all tests ls_name (ls : Ref_module.t list) =
  ls_name >::: (List.map2_exn tests ls ~f:(fun test m -> test m))

let test_all1 test ls_name ls =
  test_all (List.init (List.length ls) ~f:(Fn.const test)) ls_name ls

let test_coord_equal x p (module T : Coordinate.T) _ =
  assert_equal x (p |> T.of_perm |> T.to_rank)

let test_coord_of_perm =
  let p_c = perm_of_corner_list [(DFR, 2); (UFL, 0); (ULB, 1); (URF, 2); (DRB, 2); (DLF, 0); (DBL, 0); (UBR, 2)] in
  let arb_test x p = make_test (test_coord_equal x p) "arbitrary_coord" in
  let id_test = make_test (test_coord_equal 0 Fn.id) "id" in
  let p_e = perm_of_edge_list [(UL, 1); (UF, 1); (DF, 0); (FL, 0); (DR, 0); (BL, 1);
                               (DL, 1); (UR, 0); (BR, 1); (DB, 0); (UB, 0); (FR, 1)] in
  "test coord of perm" >::: [
    test_all [arb_test 1611 p_c; arb_test 1588 p_e; arb_test 95 p_e; arb_test 196148 p_e] "raw phase 1" raw_phase1;
    test_all [arb_test 21021 p_c] "raw phase 2" [List.hd_exn raw_phase2];
    test_all1 id_test "raw phase 1" raw_phase1;
    id_test (List.hd_exn raw_phase2);
  ]

(* I will naively assume for now that since the corner permutation works,
   and since the phase2 edge coordinates are permutations using the same
   framework, then the phase2 edge coordinates work as well. *)

let test_on_all f name =
  name >::: [
    test_all1 f "raw phase1" raw_phase1;
    test_all1 f "raw_phase2" raw_phase2;
  ]

(* Make sure each only has n total coordinates *)
let test_counts = 
  let f (module T : C.T) _ =
    let rec loop i = function
    | None -> i
    | Some x -> loop (i + 1) (T.next x)
    in
    assert_equal (loop 0 (Some T.zero)) T.n
  in
  test_on_all (make_test f "raw coord counts") "raw coord counts"

(* Make sure that ranks are less than n *)
let test_ranks =
  let f (module T : C.T) _ =
    let rec loop = function
    | None -> ()
    | Some x -> assert_equal true (T.to_rank x < T.n); loop (T.next x)
    in
    loop (Some T.zero)
  in
  test_on_all (make_test f "coord less than n") "coord less than n"

(* Make sure that to_perm is the right inverse of of_perm *)
let test_inverses =
  let f (module T : C.T) _ =
    let verify_inverse x =
      assert_equal x (x |> T.to_perm |> T.of_perm)
    in
    let rec loop = function
    | None -> ()
    | Some x -> verify_inverse x; loop @@ T.next x
    in
    loop (Some T.zero)
  in
  test_on_all (make_test f "verify inverse") "verify_inverse"

(* module M (F : sig val coord_name : string end) : Coordinate.Memo_params =
  struct
    let status = `Needs_computation
    let (^/) a b = a ^ "/" ^ b
    let path = "/mnt/c/Users/brand/Documents/kocaml-cubing/test/lookup_tabels/coordinates"
    let move_filepath = path ^/ "move" ^/ F.coord_name ^ "_move_table.sexp"
    let symmetry_filepath = path ^/ "sym" ^/ F.coord_name ^ "_sym_table.sexp"
  end *)

(* module M_twist = M (struct let coord_name = "twist" end)
module M_edge_perm = M (struct let coord_name = "edge_perm" end)

module Twist_memo = Coordinate.Twist.Make_memoized_coordinate (M_twist)
module Edge_perm_memo = Coordinate.Edge_perm.Make_memoized_coordinate (M_edge_perm) *)

let random_fixed_move _ =
  Move.Fixed_move.n
  |> Random.int 
  |> List.nth_exn Move.Fixed_move.all

let random_fixed_move_list _ =
  List.init 40 ~f:random_fixed_move
  
(* flip seems to not be equal on F, B, F3, B3 *)
(* Note that these are the moves the don't preserve orientation *)
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
    (* "twist memo" >:: test_module (module Twist_memo); *)
    (* "edge perm memo" >:: test_module (module Edge_perm_memo); *)
    (* "twist" >:: test_module (module Coordinate.Twist.Raw); *)
    (* "edge perm" >:: test_module (module Coordinate.Edge_perm.Raw); *)
    "corner perm" >:: test_module (module Coordinate.Corner_perm.Raw);
    "flip" >:: test_module (module Coordinate.Flip.Raw);
    (* "ud slice perm" >:: test_module (module Coordinate.UD_slice_perm.Raw); *)
    (* "ud slice" >:: test_module (module Coordinate.UD_slice.Raw); *)
    (* "flip ud slice" >:: test_module (module Coordinate.Flip_UD_slice.Raw); *)
]

let cube_tests = "cube tests" >::: [
  test_coord_of_perm;
  test_counts;
  test_ranks;
  test_inverses;
  test_move_sequence;
  test_coord_move_sequence;
]

let series = "series" >::: [
  cube_tests;
]

let () =
  run_test_tt_main series