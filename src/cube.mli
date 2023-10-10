(*
  File: cube.mli
  Module purpose: efficiently represent a whole cube.
  Status: not started.

  Detailed description:
    This module will represent a cube in some phase of the solving algorithm,
    and it supports moves and symmetries on the cube. Such actions will
    be efficient and will use memoized versions of the coordinates.

    The cube can be in Phase 1 or Phase 2 of the solving algorithm. This module
    helps the user recognize which phase it is in and if the cube has reached
    the goal state.

  Other considerations:
    Might support functionality without memoization to compare speed.
*)