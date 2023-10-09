# kocaml-cubing
An OCaml implementation of the Kociemba Two-Phase algorithm.

## Notices

This project is a work in progress. It is far from complete at the moment. Please do not expect it to do anything meaningful for the time being. Refer to "current status" below to see what works.

This project is entirely my interpretation of Herbert Kociemba's description on [kociemba.org](http://kociemba.org/cube.htm). I do not refer to any code that implements the Two-Phase algorithm.

Build with OCaml 5.0.0. Uses Core, ppx_jane, and OUnit2.

## Overview

Here is a quick walk through the project. The goal of this section is to help a spontaneous viewer of this repo understand what this project is. I will give a very high level overview of the algorithm and then point you to a starting place to view the code.

The Kociemba Two-Phase algorithm is a method to find a solution to a Rubik's cube in 30 moves or less. It represents a cube by *coordinates*. A coordinate is an integer that uniquely describes some aspect of the cube. For example, the Twist coordinate describes how each corner of the cube is oriented, but not where it is positioned. One coordinate for each aspect of the cube lets us represent a cube entirely by a few integers. The goal is to find a short sequence of moves that brings each coordinate to the solved state. There are two phases of the algorithm, and each phase tries to bring a different set of coordinates to the solved state, where the second phase does not disrupt the solution to the first.

Because the space of coordinates is large, we choose to reduce the size of the space via symmetries of the cube. A *symmetry coordinate* is a description of the coordinate after it's been reduced by symmetries.

Once the cube is fully represented by coordinates, we can perform a graph search for the goal state of each phase--the goal state of phase 2 is the solved state of the cube. We use A* for this graph search where the heuristic is a precomputed path length to solve some of the coordinates in that phase.

That's the 20,000 foot picture: represent the cube with some integers and search a graph for the solution. To take a peek into the code, I suggest starting with the `.mli` files `cubie.mli`, `move.mli` and `coordinate.mli`. Then take a peek at `coordinate.ml` for some of my more significant work.

## Module overview

In this section I briefly describe each module. For more detail, view the `.mli` files and the comments in the code. These are listed in alphabetical order. They are given a status of "not started", "in progress", or "complete". Of course, nothing is *truly* 100% complete until the project solves a cube, but it is complete to my current knowledge and satisfaction.

* `Coordinate`. This holds all the coordinates that could describe a cube. These coordinates can be memoized for faster computations (so that some more "physical" and computationally expensive representation of the cube can be discarded) and converted to symmetry coordinates. IN PROGRESS--HIGHLY COMPLETE.

* `Cube`. This will represent a cube state as a set of coordinates and will be the type for each node in the graph search. NOT STARTED.

* `Cubie`. These are the "little cubes" on the Rubik's cube. Cubies are corner pieces and edge pieces, and they are labeled with the faces of the cube they touch. For example, the `URF` cubie is the corner on the up, right, and front faces of the cube. A cubie has an orientation with respect to the solved state. COMPLETE.

* `Lookup_table`. These are data structures that allow fast lookup of ranked types. They use arrays, but the mutation is behind-the-scenes, and the table is only mutated upon creation. These are used to quickly look up how a coordinate is affected by a move, or similar. COMPLETE.

* `Modular_int`. This holds the "Z mod n" groups. The Rubik's cube group relies on these groups for turns of the faces and for cubie orientations. COMPLETE.

* `Move`. This describes some move on the cube. These might be turns of the faces, compositions of faceturns, or just some arbitrary function that maps cubies to cubies. A move will use the "is replaced by" notation: if a move is called `f`, then `f(URF) = (UFL, 1)` means that the URF position of the cubie will get replaced by the cubie that was in the UFL position, and it will gain orientation 1. COMPLETE.

* `Perm_sgn`. This can be ignored for now. It is some code that will help me determine if a cube is well-formed because only even permutations of the cubies are achievable by turns of the faces. COMPLETE.

* `Perm`. This is some "permutation" of the cube. It can be described exactly as a move, but it is meant to model some unsolved state of the cube. It is described by the "move" that takes the solved state to that permutation. The goal of this whole project is to find the inverse to a permutation as a composition of faceturns. COMPLETE.

* `Pruning_table`. These will hold the A* heuristic for the graph search. It is a table because the heuristic is some integer for each coordinate value, which is not predictable and must be calculated and stored. NOT STARTED.

* `Setup`. This will help the user perform setup calculations to compute the coordinates and pruning tables. NOT STARTED.

* `Symmetry`. This module describes the symmetries of the cube under rotations and reflections. Only symmetries that preserve the UD slice are considered (those that don't take the middle horizontal slice of the cube to some other slice). IN PROGRESS--HIGHLY COMPLETE (needs corner orientations).

## Status

TODO urgent:
* Test moves on symmetry coordinates to discover the inconsistency (they fail somehow).
* Fix reflection symmetry's behavior on corner orientation because it is needed for graph search to goal.
  * Or (for a quick spot-fix) remove the reflection symmetry; this only doubles the number of symmetry classes, which is not completely game-breaking.

TODO eventually:
* Model a cube in `Cube` module and let moves and symmetries act on it.
* Implement A*.
* Implement pruning tables.
  * Must consider how one cube can be represented by two symmetry coordinates.
* Implement repeated search for worse phase1 result and better phase2 result.
* Create setup executable to calculate tables.
* Use config for table locations and setup state.
* And probably lots more.

Current status:
* 8 Oct 2023 -- symmetries work without orientations. Symmetry coordinates failing moves.
* 6 Oct 2023 -- all raw coordinates work completely under move sequences starting from random permutations. However, the reflection symmetry is broken.
* 26 Sep 2023 -- orientation coordinates fail under moves. I may refactor moves to not consider initial orientation, but I don't yet see how this solves the issue.
* pre 26 Sep -- no updates on status. Project started in early September.