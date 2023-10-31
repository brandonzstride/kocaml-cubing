
(*
  File: setup.ml
  Purpose: perform necessary calculations before solving cubes.
  Compiles to: executable
  Status: not started.

  Detailed description:
    The user needs to have precomputed coordinate behaviors and
    pruning tables before running the solving algorithm, or else
    the solver will be extremely slow.

    This is an executable that performs all setup so that afterwards,
    the solver is ready to run.

  Other considerations:
    The solver executable should load all necessary tables into
    memory before accepting a cube as input. I'm placing this note
    here until I make a solver executable.
*)

(*
  NOTICE
    This is just some rough code to precompute the coordinates without
    any official config. I am doing this to speed up tests.

    Use command
    ```
    dune exec -- src/setup.exe
    ```
    It takes about two minutes and ten seconds for all the coordinates on my machine.
    Flip_UD_slice takes about two minutes, and the rest of the time are the other coordinates.
*)
module P : Coordinate.Params =
  struct
    let status = `Compute_and_save_at_directory "./src/coordinates/"
  end

let () = 
  (* Compute and save all coordinates *)
  Printf.printf "Starting setup...\n";
  let t0 = Caml_unix.gettimeofday () in
  let module Twist = Coordinate.Twist (P) in
  let module Flip_UD_slice = Coordinate.Flip_UD_slice (P) in

  let module Edge_perm = Coordinate.Edge_perm (P) in
  let module Corner_perm = Coordinate.Corner_perm (P) in
  let module UD_slice_perm = Coordinate.UD_slice_perm (P) in
  Printf.printf "Finished setup in %fs\n" (Caml_unix.gettimeofday () -. t0);
  print_endline "done";