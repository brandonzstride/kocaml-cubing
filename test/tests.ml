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

(* These are raw coordinates -- no memoization or symmetry classes *)
module C = Coordinate.Exposed_for_testing

module Ref_module =
  struct
    type t =
      { m : (module Coordinate.S) ref
      ; name : string }
  end

(* I use reference cells to hold modules. *)
let raw_phase1 = let open Ref_module in [
  { m = ref (module C.Twist         : Coordinate.S) ; name = "twist raw"         };
  { m = ref (module C.Flip          : Coordinate.S) ; name = "flip raw"          };
  { m = ref (module C.UD_slice      : Coordinate.S) ; name = "ud slice raw"      };
  { m = ref (module C.Flip_UD_slice : Coordinate.S) ; name = "flip ud slice raw" };
]

let raw_phase2 = let open Ref_module in [
  { m = ref (module C.Corner_perm   : Coordinate.S) ; name = "corner perm raw"   };
  { m = ref (module C.Edge_perm     : Coordinate.S) ; name = "edge perm raw"     };
  { m = ref (module C.UD_slice_perm : Coordinate.S) ; name = "ud slice perm raw" };
]

(*
  ----------
  TEST MOVES   
  ----------
*)

let perm_of_edge_list ls =
  let open Cubie in
  let x = 
  ls
  |> List.map ~f:(fun (e, o) -> With_orientation.Edge With_orientation.Edge.{ e ; o = Modular_int.Z2.of_int o })
  |> List.zip_exn Cubie.Edge.all
  in
  function
  | Corner _ as c -> Move.id c
  | Edge e -> List.find_exn x ~f:(fun (a, _) -> Edge.compare e a = 0) |> Tuple2.get2

let perm_of_corner_list ls =
  let open Cubie in
  let x = 
  ls
  |> List.map ~f:(fun (c, o) -> With_orientation.Corner With_orientation.Corner.{ c ; o = Modular_int.Z3.of_int o })
  |> List.zip_exn Cubie.Corner.all
  in
  function
  | Edge _ as e -> Move.id e
  | Corner c -> List.find_exn x ~f:(fun (a, _) -> Corner.compare c a = 0) |> Tuple2.get2

(*
  For this test, I generated a random sequence of moves, applied them to my physical
  cube, and recorded the final permutation in a "is replaced by" way.
  This test runs that move sequence and compares the resulting permutations and orientations
  in the corners, UD slice, and UD faces separately.
  Since one mistake when making the moves really blows up in your face, I assume that if
  this test passes, then the moves are all working properly.
*)
(* let test_move_sequence = "move sequence" >:: (fun _ ->
  let open Cubie in
  let open Move.All_fixed_move in
  let open Move.All_fixed_move.Faceturn in
  let corner_list = List.map ~f:(fun (a, b) -> With_orientation.Corner.{ c = a ; o = Modular_int.Z3.of_int b}) in
  let edge_list   = List.map ~f:(fun (a, b) ->   With_orientation.Edge.{ e = a ; o = Modular_int.Z2.of_int b}) in
  let p = [(B, 2); (D, 3); (B, 1); (L, 2); (B, 2); (D, 1); (U, 3); (F, 3); (B, 2);
           (U, 2); (D, 2); (R, 2); (B, 1); (R, 3); (L, 1); (F, 3); (R, 3); (B, 1);
           (D, 3); (F, 2); (L, 1); (R, 1); (F, 1); (U, 2); (B, 2)]
    |> List.map ~f:(fun (a, b) -> of_faceturn_and_count a b |> to_move)
    |> Perm.of_move_list
  in
  assert_equal (Perm.to_corners_list p)        (corner_list [(URF, 2); (DLF, 0); (DFR, 1); (UBR, 0); (ULB, 1); (DBL, 1); (UFL, 0); (DRB, 1)]);
  assert_equal (Perm.to_ud_slice_edges_list p) (edge_list   [(UR, 0); (BL, 1); (BR, 1); (UF, 1)]);
  assert_equal (Perm.to_ud_edges_list p)       (edge_list   [(UL, 1); (DL, 0); (UB, 1); (DF, 1); (FL, 0); (DR, 0); (DB, 1); (FR, 1)])
  ) *)


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

let test_coord_equal x p (module M : Coordinate.S) _ =
  assert_equal x (p |> M.of_perm |> M.to_rank)

let test_coord_of_perm =
  let p_c = perm_of_corner_list [(DFR, 2); (UFL, 0); (ULB, 1); (URF, 2); (DRB, 2); (DLF, 0); (DBL, 0); (UBR, 2)] in
  let arb_test x p = make_test (test_coord_equal x p) "arbitrary_coord" in
  let id_test = make_test (test_coord_equal 0 Move.id) "id" in
  let p_e = perm_of_edge_list [(UL, 1); (UF, 1); (DF, 0); (FL, 0); (DR, 0); (BL, 1);
                               (DL, 1); (UR, 0); (BR, 1); (DB, 0); (UB, 0); (FR, 1)] in
  "test coord of perm" >::: [
    test_all [arb_test 1611 p_c; arb_test 1588 p_e; arb_test 95 p_e; arb_test 196148 p_e] "raw phase 1" raw_phase1;
    test_all [arb_test 21021 p_c] "raw phase 2" [List.hd_exn raw_phase2]; (* assume that if corner works, then the others do too *)
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
  let f (module M : Coordinate.S) _ =
    let rec loop i = function
    | None -> i
    | Some x -> loop (i + 1) (M.next x)
    in
    assert_equal (loop 0 (Some M.zero)) M.n
  in
  test_on_all (make_test f "raw coord counts") "raw coord counts"

(* Make sure that ranks are less than n *)
let test_ranks =
  let f (module M : Coordinate.S) _ =
    let rec loop = function
    | None -> ()
    | Some x -> assert_equal true (M.to_rank x < M.n); loop (M.next x)
    in
    loop (Some M.zero)
  in
  test_on_all (make_test f "coord less than n") "coord less than n"

(* Make sure that to_perm is the right inverse of of_perm *)
let test_inverses =
  let f (module M : Coordinate.S) _ =
    let verify_inverse x =
      assert_equal x (x |> M.to_perm |> M.of_perm)
    in
    let rec loop = function
    | None -> ()
    | Some x -> verify_inverse x; loop @@ M.next x
    in
    loop (Some M.zero)
  in
  test_on_all (make_test f "verify inverse") "verify_inverse"

(**
  Tests that coordinates are well-defined under move sequences.
  i.e. checks that from a random starting permutation, moves on coordinates
  are identical to moves on the cube.
  i.e. the coordinate of the cube after all the moves is the same as the
  resulting coordinate after moves only done on coordinate.
*)

let test_coord_move_sequence n_trials n_moves move_list_generator (module M : Coordinate.S) _ =
  let test_module _ =
    let p = move_list_generator n_moves |> Perm.perform_fixed_move_list Perm.identity in (* random starting permutation *)
    let move_list = move_list_generator n_moves in (* moves to apply to permutation *)
    let p' = Perm.perform_fixed_move_list p move_list in (* resulting perm from the moves *actually applied to the cube* *)
    let x' = List.fold move_list ~init:(M.of_perm p) ~f:(fun x m -> m |> M.Fixed_move.of_super_t |> M.perform_fixed_move x) in (* resulting coord of the moves *only applied to the coordinate* *)
    assert_equal (M.of_perm p' |> M.to_rank) (x' |> M.to_rank) (* cannot compare perms because some cubies are not relevant to coord *)
    (* ^ note that this means only equivalence classes are compared for symmetry coordinates, which is what we want.
       This is because sometimes two sym coords represent the same cube. Overall, this passes almost every time when we compare
       exact coords, but this is my quick patch on the case where cubes are identical but sym coords are not. *)
  in
  Fn.apply_n_times ~n:n_trials test_module ()

(*
  This comment is out of date for current scale of tests.
  Use move sequences of 40 moves, 100 times

  If there is one move that fails on a coordinate, then (roughly, because it
  could be canceled by an adjacent move) there is a ((17/18)^40)^10 ~=~ 10^-100
  chance of that move not getting hit. I'll take my chances and assume that this
  test is sufficient.     
*)
let n_trials = 500
let n_moves = 40
let test_coord_move_sequence_phase1 = test_coord_move_sequence n_trials n_moves Move.Fixed.G.random_list
let test_coord_move_sequence_phase2 = test_coord_move_sequence n_trials n_moves (fun n -> Move.Fixed.G1.random_list n |> List.map ~f:Move.Fixed.G1.to_super_t)

let test_raw_phase1_coord_move_sequence =
  "raw phase1 coord move sequences" >::: [
    "twist"         >:: test_coord_move_sequence_phase1 (module C.Twist);
    "flip"          >:: test_coord_move_sequence_phase1 (module C.Flip);
    "ud slice"      >:: test_coord_move_sequence_phase1 (module C.UD_slice);
    "flip ud slice" >:: test_coord_move_sequence_phase1 (module C.Flip_UD_slice);
  ]

let test_raw_phase2_coord_move_sequence =
  "raw phase2 coord move sequences" >::: [
    "edge perm"     >:: test_coord_move_sequence_phase2 (module C.Edge_perm);
    "ud slice perm" >:: test_coord_move_sequence_phase2 (module C.UD_slice_perm);
    "corner perm"   >:: test_coord_move_sequence_phase2 (module C.Corner_perm);
  ]

module P : Coordinate.Params =
  struct
    (* let status = `Compute *) (* do not save *)
    (* back out of test, default, _build, then into src ... *)
    (* It takes about half a second to load all of the coordinates into memory on my machine *)
    let status = `Is_saved_at_directory "../../../src/coordinates/" (* requiries that `dune exec -- src/setupml` has been run *)
  end

module Twist = Coordinate.Twist (P)
module Edge_perm = Coordinate.Edge_perm (P)
module UD_slice_perm = Coordinate.UD_slice_perm (P)

let test_memoized_phase1_coord_move_sequence =
  "memoized phase1 coord move sequences" >::: [
    "twist" >:: test_coord_move_sequence_phase1 (module Twist);
  ]

let test_memoized_phase2_coord_move_sequence =
  "memoized phase2 coord move sequences" >::: [
    "edge perm"     >:: test_coord_move_sequence_phase2 (module Edge_perm);
    "ud slice perm" >:: test_coord_move_sequence_phase2 (module UD_slice_perm);
  ]

(*
  ---------------
  TEST SYMMETRIES   
  ---------------

  I need to verify that symmetries work as expected before I use them for symmetry
  coordinates.

  I'll test by making sure that a few "random" moves are appropriately converted by
  symmetries. I say "random" because I'll just choose some that feel representative.

  Then I'll perform a symmetry on a cube, do all the moves, undo the symmetry, and
  make sure it's consistent with the cube under the same moves without the symmetry.
  Specifically, here is the way this will work:
    Take a random permutation p, some random move m, and a random symmetry s.
    We apply the symmetry to the perm, and then the move to that:
      s * p * s^-1 * m =: p'
    The other way is to take the original perm and apply a symmetry to the move:
      p * s^-1 * m * s =: p''
    We should see that s^-1 * p' * s = p''.
  This test is just some mathematical identities that should cancel, but the code
  doesn't recognize that it cancels, so we let it run it all through and check at the
  end that it worked.
  
  We can expand this to move lists:
      s * p * s^-1 * m_1 * ... * m_n =: p'
    and
      p * s^-1 * m_1 * s * s^-1 * m_2 * s * ... * s^-1 * m_n * s =: p''
    where again
      s^-1 * p' * s = p''
  
*)

(* This commented out because fixed move generators are no longer exposed *)
(* let test_move_symmetries =
  let sf2 = Symmetry.of_rank 4 in
  let su4 = Symmetry.of_rank 1 in
  let max = Symmetry.of_rank 7 in (* S_F2 * S_U4^3 *)
  let compare_sym_move (m : (Move.All_fixed_move.Faceturn.t * int)) (s : Symmetry.t) (m' : (Move.Faceturn.t * int)) _ : unit =
    let m = Fn.uncurry Move.All_fixed_move.of_faceturn_and_count m in
    let m' = Fn.uncurry Move.All_fixed_move.of_faceturn_and_count m' in
    assert_equal (Symmetry.on_fixed_move s m) m'
  in
  let open Move.All_fixed_move.Faceturn in
  "Symmetry move equivalence tests" >::: [
    "S_F2 U"  >:: compare_sym_move (U, 1) sf2 (D, 1);
    "S_F2 R3" >:: compare_sym_move (R, 3) sf2 (L, 3);
    "S_F2 B"  >:: compare_sym_move (B, 1) sf2 (B, 1);
    "S_U4 R"  >:: compare_sym_move (R, 1) su4 (B, 1);
    "S_U4 U"  >:: compare_sym_move (U, 1) su4 (U, 1);
    "S_U4 L2" >:: compare_sym_move (L, 2) su4 (F, 2);
    "max R"   >:: compare_sym_move (R, 1) max (F, 1);
    "max U"   >:: compare_sym_move (U, 1) max (D, 1);
    "max B3"  >:: compare_sym_move (B, 3) max (L, 3);
  ] *)

let test_sym_moves_on_perm =
  (* Try this on random perms with random moves and random symmetries *)
  let run_trial (p : Perm.t) (ls : Move.Fixed.G.t list) (s : Symmetry.t) : unit =
    let s' = Symmetry.inverse s in
    let p' =
      p
      |> Symmetry.on_perm s
      |> Fn.flip Perm.perform_fixed_move_list ls
      |> Symmetry.on_perm s'
      in
    ls
    |> List.map ~f:(Symmetry.on_fixed_move s')
    |> Perm.perform_fixed_move_list p
    |> Move.equal p'
    |> assert_equal true
  in
  "Fixed moves under symmetry on perm" >:: 
    begin fun _ ->
    List.iter
      (List.init 1000 ~f:(Fn.const ()))
      ~f:(fun _ ->
        let p = Move.Fixed.G.random_list 40 |> Perm.perform_fixed_move_list Perm.identity in (* 40 moves to generate random perm *)
        let ls = Move.Fixed.G.random_list 40 in (* test move sequences of 40 moves to be applied to perm *)
        let s = Symmetry.Exposed_for_testing.random () in
        run_trial p ls s 
      )
    end

  
(*
  -------------------------
  TEST SYMMETRY COORDINATES
  -------------------------

  Now that symmetries are all tested and appear to work, it's time to test symmetry coordinates.
  We need to assert that move sequences on a regular cube are consistent with move sequences
  on a symmetry coordinate.

  We can do this in the same way as we tested raw coordinates. See the tests above for explanation.
*)

(*
  The following functor call is commented out because it is very slow.
  To compute and memoize the entire symmetry coordinate...
  * There were 127166 symmetry classes, all found in 13 seconds
  * The move table was of size 2,288,988, computed in 462 seconds
    * UPDATE: when only generators are memoized, this has 1/3 the size,
      and it takes only 173 seconds

  The test passed, so it will not be run again until there is a major change.

  The associated test is `test_sym_phase1_coord_move_sequence`, and as such
  it is commented out.
*)
module Flip_UD_slice = Coordinate.Flip_UD_slice (P)
(* Note that this takes 200 seconds in utop but less than 20 seconds in tests *)
module Corner_perm = Coordinate.Corner_perm (P)

let test_sym_phase1_coord_move_sequence =
  "sym phase1 coord move sequences" >::: [
    "flip ud slice" >:: test_coord_move_sequence_phase1 (module Flip_UD_slice)
  ]

let test_sym_phase2_coord_move_sequence =
  "sym phase2 coord move sequences" >::: [
    "corner perm" >:: test_coord_move_sequence_phase2 (module Corner_perm);
  ]

(*
  ----------
  TEST CUBES   
  ----------

  I need to test the Phase1 and Phase2 full representations of the cubes.
  There are a few things I'm going for:
  * The solved state is correctly recognized as the goal state
  * I can reach the goal state from some easily solved preset cubes (contained within the next item)
  * Moves should work as if they're on the physical permutation
  * Symmetric cubes should have the same rank
*)

(*
  TODO: dont' use `sanitize`. Instead use `get_identical_cubes`.   
*)
let test_cube_move_sequence n_trials n_moves move_list_generator (module M : Cube.S) _ =
  let test_module _ =
    let p = (* random starting permutation *)
      move_list_generator n_moves
      |> Perm.perform_fixed_move_list Perm.identity
    in
    let move_list = move_list_generator n_moves in (* moves to apply to permutation *)
    let p' = Perm.perform_fixed_move_list p move_list in (* resulting perm from the moves *actually applied to the cube* *)
    let sanitize x = (* sanitize a cube by forcing it to use same symmetry as of_perm *)
      x
      |> M.Exposed_for_testing.to_perm
      |> M.of_perm
      |> Result.ok_or_failwith
      |> M.to_rank
    in
    let x' = (* resulting coord of the moves *only applied to the cube* *)
      List.fold
        move_list
        ~init:(
          p
          |> M.of_perm
          |> Result.ok_or_failwith
        )
        ~f:(fun x m ->
          m
          |> M.Fixed_move.of_super_t
          |> M.perform_fixed_move x
        ) in 
    assert_equal (M.of_perm p' |> Result.ok_or_failwith |> M.to_rank) (sanitize x') (* cannot compare perms because some cubies are not relevant to coord *)
  in
  Fn.apply_n_times ~n:n_trials test_module ()

let n_trials = 500
let n_moves = 40
let test_cube_move_sequence_phase1 = test_cube_move_sequence n_trials n_moves Move.Fixed.G.random_list
let test_cube_move_sequence_phase2 = test_cube_move_sequence n_trials n_moves (fun n -> Move.Fixed.G1.random_list n |> List.map ~f:Move.Fixed.G1.to_super_t)

(*
  These sometimes pass, sometimes fail without "sanitation".
  
  Phase1 seems to pass much more often. I can run thousands of trials with it passing.
  * 10**4 trials and 40 moves gives 99.81% success rate

  I seem to be only able to run tens of trials in Phase2 and still have it pass.
  * 10**4 trials and 40 moves gives 97.19% success rate

  This is because sometimes one cube can be represented by multiple
  symmetry coordinates, which would yield different ranks.
  * This is relevant when getting the symmetry coordinate *from* a perm because it uses
    the smallest symmetry that converts to the representative
  * A symmetry coordinate during regular usage does not always yield the smallest symmetry
    that converts to the representative.

  So I sanitize the cube by forcing it to a permutation and then back from a permutation.
  This always gives the "smallest symmetry". When I use 10**4 trials now, there should be
  about a 5 in a billion chance it doesn't throw an error if these actually aren't working.
  I don't run this regularly, but I have run it before, and it didn't fail.
  ( 0.9981 ^ 10000 ~=~ 5 * 10^-9)

  This is just a patch on the issue that I don't handle cubes that are represented
  by more than one symmetry coordinate.
*)
let test_cube_move_sequence =
  "cube move sequences" >::: [
    "phase1" >:: test_cube_move_sequence_phase1 (module Cube.Phase1);
    "phase2" >:: test_cube_move_sequence_phase2 (module Cube.Phase2);
  ]

let cube_tests = "cube tests" >::: [
  test_coord_of_perm;
  (* The next three tests might be commented out because they are exhaustive and slow. Total, they take over 10 seconds *)
  test_counts;
  test_ranks;
  test_inverses;
  (* test_move_sequence; *)
  test_raw_phase1_coord_move_sequence;
  test_raw_phase2_coord_move_sequence;
  test_memoized_phase1_coord_move_sequence;
  test_memoized_phase2_coord_move_sequence;
  (* test_move_symmetries; *)
  test_sym_moves_on_perm;
  test_sym_phase1_coord_move_sequence;
  test_sym_phase2_coord_move_sequence;
  test_cube_move_sequence;
]

let series = "series" >::: [
  cube_tests;
]

let () =
  run_test_tt_main series