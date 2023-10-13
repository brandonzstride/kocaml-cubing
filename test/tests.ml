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
let test_move_sequence = "move sequence" >:: (fun _ ->
  let open Cubie in
  let open Move.Faceturn in
  let open Move.Fixed_move in
  let corner_list = List.map ~f:(fun (a, b) -> With_orientation.Corner.{ c = a ; o = Modular_int.Z3.of_int b}) in
  let edge_list   = List.map ~f:(fun (a, b) ->   With_orientation.Edge.{ e = a ; o = Modular_int.Z2.of_int b}) in
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

(**
  Tests that coordinates are well-defined under move sequences.
  i.e. checks that from a random starting permutation, moves on coordinates
  are identical to moves on the cube.
  i.e. the coordinate of the cube after all the moves is the same as the
  resulting coordinate after moves only done on coordinate.
*)

let test_coord_move_sequence n_trials n_moves move_list_generator (module T : Coordinate.T) _ =
  let test_module _ =
    let p = move_list_generator n_moves |> Perm.perform_fixed_move_list Perm.identity in (* random starting permutation *)
    let move_list = move_list_generator n_moves in (* moves to apply to permutation *)
    let p' = Perm.perform_fixed_move_list p move_list in (* resulting perm from the moves *actually applied to the cube* *)
    let x' = List.fold move_list ~init:(T.of_perm p) ~f:T.perform_fixed_move in (* resulting coord of the moves *only applied to the coordinate* *)
    assert_equal (T.of_perm p') x' (* cannot compare perms because some cubies are not relevant to coord *)
  in
  Fn.apply_n_times ~n:n_trials test_module ()

(*
  Use move sequences of 40 moves, 10 times

  If there is one move that fails on a coordinate, then (roughly, because it
  could be canceled by an adjacent move) there is a ((17/18)^40)^10 ~=~ 10^-10
  chance of that move not getting hit. I'll take my chances and assume that this
  test is sufficient.     
*)
let test_coord_move_sequence_phase1 = test_coord_move_sequence 10 40 Move.Fixed_move.random_list
let test_coord_move_sequence_phase2 = test_coord_move_sequence 10 40 Move.Fixed_move.random_g1_list

let test_raw_phase1_coord_move_sequence =
  "raw phase1 coord move sequences" >::: [
    "twist"         >:: test_coord_move_sequence_phase1 (module C.Twist.Raw);
    "flip"          >:: test_coord_move_sequence_phase1 (module C.Flip.Raw);
    "ud slice"      >:: test_coord_move_sequence_phase1 (module C.UD_slice.Raw);
    "flip ud slice" >:: test_coord_move_sequence_phase1 (module C.Flip_UD_slice.Raw);
  ]

let test_raw_phase2_coord_move_sequence =
  "raw phase2 coord move sequences" >::: [
    "edge perm"     >:: test_coord_move_sequence_phase2 (module C.Edge_perm.Raw);
    "ud slice perm" >:: test_coord_move_sequence_phase2 (module C.UD_slice_perm.Raw);
    "corner perm"   >:: test_coord_move_sequence_phase2 (module C.Corner_perm.Raw);
  ]

(* module M (F : sig val coord_name : string end) : Coordinate.Memo_params =
  struct
    let status = `Needs_computation
    let (^/) a b = a ^ "/" ^ b
    let path = "/mnt/c/Users/brand/Documents/kocaml-cubing/test/lookup_tabels/coordinates"
    let move_filepath = path ^/ "move" ^/ F.coord_name ^ "_move_table.sexp"
    let symmetry_filepath = path ^/ "sym" ^/ F.coord_name ^ "_sym_table.sexp"
  end

module M_twist = M (struct let coord_name = "twist" end)
module M_edge_perm = M (struct let coord_name = "edge_perm" end)

module Twist_memo = Coordinate.Twist.Make_memoized_coordinate (M_twist)
(* Edge_perm_memo probably fails because it tries to memoize on all moves, not just g1 generators *)
module Edge_perm_memo = Coordinate.Edge_perm.Make_memoized_coordinate (M_edge_perm) *)

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
  e.g.
    permutation p, move sequence m_1, ..., m_n, symmetry s.
    s * p * s^-1   |->   perform moves m_1, ..., m_n   |->   fun p' -> s^-1 * p' * s
    This should be exactly
    p * s^-1 * m_1 * ... * m_n * s
    And it should have the same result as applying all moves under inverse symmetry
    applied to just p.


  I want to be able to apply a sequence of moves to a cube until that cube is symmetric
  to the solved cube. Then I am told which symmetry converts it to the solved cube.
  Then apply that symmetry to all the moves so that I have a new sequence of moves
  that takes the original cube directly to the solved state.
  e.g.
    The original perm was p, and a move sequence m_1, ..., m_n is applied, and then
    I know a symmetry s takes the result to the solved state.
    i.e.
      s * (p * m_1 * ... * m_n) * s^-1 = solved
    where zero means it is solved.
    We want
      p * m'_1 * ... * m'_n = solved
    
    s^-1 * s * p * m_1 * ... * m_n * s^-1 * s = s^-1 * solved * s

  So actually I'm misunderstanding the problem. We have a cube that is symmetry to the
  smallest cube in its symmetry class.

  All cubes in a symmetry class have the same distance from the goal state. We can only
  see the moves that affect the representative, which are not the same as the moves
  that we're actually dealing with and wanting to apply to the perm at hand.

  So s * p * s^-1 is the representative. And we know that applying move m takes it closer
  to the goal. Which move do we actually apply to the underlying cube?
  s * p * s^-1 * m = p'
  p * s^-1 * m * s = s^-1 * p' * s
  So I need to make sure that when I apply m to a cube under a symmetry, that is the same
  as applying the inverse symmetry to the move and applying inverse symmetry to result.

  What's going to happen with symmetry coordinates is that the user only knows which
  symmetry class a cube is in and if that's the solved class. Then the symmetry is
  extracted from that solved class.

*)

let test_move_symmetries =
  let sf2 = Symmetry.of_rank 4 in
  let su4 = Symmetry.of_rank 1 in
  let max = Symmetry.of_rank 7 in (* S_F2 * S_U4^3 *)
  let compare_sym_move (m : (Move.Faceturn.t * int)) (s : Symmetry.t) (m' : (Move.Faceturn.t * int)) _ : unit =
    let m = Move.Fixed_move.{ faceturn = Tuple2.get1 m ; count = Modular_int.Z4.of_int (Tuple2.get2 m) } in
    let m' = Move.Fixed_move.{ faceturn = Tuple2.get1 m' ; count = Modular_int.Z4.of_int (Tuple2.get2 m') } in
    assert_equal (Symmetry.on_fixed_move s m) m'
  in
  let open Move.Faceturn in
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
  ]

let test_sym_moves_on_perm =
  (* Try this on random perms with random moves and random symmetries *)
  let run_trial (p : Perm.t) (m : Move.Fixed_move.t) (s : Symmetry.t) : unit =
    let p' = (* s^-1 * s * p * s^-1 * m * s *)
      p
      |> Symmetry.on_perm s
      |> Fn.flip Perm.perform_fixed_move m
      |> Symmetry.on_perm (Symmetry.inverse s)
      in
    s
    |> Symmetry.inverse
    |> Fn.flip Symmetry.on_fixed_move m
    |> Perm.perform_fixed_move p (* p * s^-1 * m * s *)
    |> Move.equal p'
    |> assert_equal true
  in
  "Fixed moves under symmetry on perm" >:: 
    begin fun _ ->
    List.iter
      (List.init 1000 ~f:(Fn.const ()))
      ~f:(fun _ ->
        let p = Move.Fixed_move.random_list 40 |> Perm.perform_fixed_move_list Perm.identity in (* 40 moves to generate random perm *)
        let m = Move.Fixed_move.random_list 1 |> List.hd_exn in
        let s = Symmetry.random () in
        run_trial p m s 
      )
    end


module S : Coordinate.Sym_memo_params =
  struct
    let status = `Needs_computation
    (* Current implementation doesn't save, so no filepaths needed *)
    let move_filepath = ""
    let class_to_rep_filepath = ""
    let rep_to_class_filepath = ""
  end

(* module Flip_UD_slice_sym = Coordinate.Flip_UD_slice.Make_symmetry_coordinate (S) *)
module Corner_perm_sym = Coordinate.Corner_perm.Make_symmetry_coordinate (S)

(* let test_sym_phase1_coord_move_sequence =
  "sym phase1 coord move sequences" >::: [
    "flip ud slice" >:: test_coord_move_sequence_phase1 (module Flip_UD_slice_sym)
  ] *)

let test_sym_phase2_coord_move_sequence =
  "sym phase2 coord move sequences" >::: [
    "corner perm" >:: test_coord_move_sequence_phase2 (module Corner_perm_sym);
  ]

let cube_tests = "cube tests" >::: [
  test_coord_of_perm;
  (* The next three tests might be commented out because they are exhaustive and slow *)
  test_counts;
  test_ranks;
  test_inverses;
  test_move_sequence;
  test_raw_phase1_coord_move_sequence;
  test_raw_phase2_coord_move_sequence;
  test_move_symmetries;
  test_sym_moves_on_perm;
  (* test_sym_phase1_coord_move_sequence; *)
  test_sym_phase2_coord_move_sequence;
]

let series = "series" >::: [
  cube_tests;
]

let () =
  run_test_tt_main series