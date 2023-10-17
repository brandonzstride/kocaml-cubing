
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

module P : Coordinate.Params =
  struct
    let status = `Compute_and_save_at_directory "./src/coordinates/"
  end

(* Compute and save all coordinates *)
module Twist = Coordinate.Twist (P)
module Flip_UD_slice = Coordinate.Flip_UD_slice (P)

module Edge_perm = Coordinate.Edge_perm  (P)
module Corner_perm = Coordinate.Corner_perm (P)
module UD_slice_perm = Coordinate.UD_slice_perm (P)