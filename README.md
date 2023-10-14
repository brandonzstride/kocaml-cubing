# kocaml-cubing
An OCaml implementation of the Kociemba Two-Phase algorithm.

## Notices

This project is a work in progress. There is meaningful functionality, but please do not expect it to solve a cube yet. Refer to the **Status** section below to see what works.

**This project is entirely my interpretation of Herbert Kociemba's explanation on [kociemba.org](http://kociemba.org/cube.htm)**. I do not refer to any code that implements the Two-Phase algorithm. I give credit for the ideas behind the algorithm to Herbert Kociemba. Implementation details are all my own, and I will note my algorithmic additions in this README.

Written in `OCaml 5.0.0`. Depends on `Core`, `ppx_jane`, and `OUnit2`.

This project adheres to functional standards and uses no mutation **except when contained under the hood of a module and can appear to be completely functional**. I use `Core` for stronger types than `Base` or `Batteries`. Exceptions are used for logical impossibilities and are only thrown if there is a mistake in the code logic; they are never intentionally caught as an implementation choice.

## Overview

Here is a quick walk through the project. The goal of this section is to help a spontaneous viewer of the repo understand what this project is. I will give a very high level overview of the algorithm and then point you to a starting place to view the code.

The Kociemba Two-Phase algorithm finds a solution to a Rubik's cube in 30 moves or less. It represents a cube by *coordinates*. A coordinate is an integer that uniquely describes some aspect of the cube. For example, the Twist coordinate describes how each corner of the cube is oriented, but not where it is positioned. One coordinate for each aspect of the cube lets us represent a cube entirely by a few integers. The goal is to find a short sequence of moves that brings each coordinate to the solved state. There are two phases of the algorithm, and each phase tries to bring a different set of coordinates to the solved state, where the second phase does not disrupt the solution to the first.

Because the space of coordinates is large, we choose to reduce the size of the space via symmetries of the cube. Two cubes are symmetric if one is a rotation of the other, so they are equally far from the goal state. A *symmetry coordinate* is a description of the coordinate after it's been reduced by symmetries.

Once the cube is fully represented by coordinates, we can perform a graph search for the goal state of each phase--the goal state of phase 2 is the solved state of the cube. We use A* for this graph search where the heuristic is a precomputed path length to solve some of the coordinates in that phase.

That's the 20,000 foot picture: represent the cube with some integers and search a graph for the solution. It sounds simple when put like that.

To take a peek into the code, I suggest starting with the following files: `cubie.mli`, `move.mli` and `coordinate.mli`. Note the module descriptions at the top of the files, and then look at the signatures. Afterwards, look at `coordinate.ml` for some of my more significant work.

## Algorithmic contributions

In all parts of the implementation, I had to think hard about how something is done. However, it was all in the effort of implementing exactly the algorithm Kociemba describes. There is only one "improvement" I make to the algorithm itself:
* Instead of memoizing all fixed moves on a cube, I only memoize the generators for those moves. For the group G, this cuts down on move table size by a factor of three, but it slightly increases computation time (which appears to be negligible).
  * The same cannot be done on symmetry coordinates because generators under symmetries are not always other generators. Please see my comment in `coordinate.ml` for this.

## Status

**Summary**:

Cubes are fully represented by coordinates and are reduced by symmetry classes. This means the foundation has been layed to begin working on the search part of the algorithm.

---

**Tested functionality**:
* Turns of the cube faces are consistent with the physical cube.
  * They compose for arbitrary moves sequences.
* Rotational symmetries are physically consistent.
  * They act on permutations of the cube and on cube faceturns.
* Coordinates are appropriately calculated and are invertible.
  * Coordinates cannot escape their scope.
  * Arbitrary sequences of moves on coordinates are consistent with the same moves on the physical cube.
  * Moves on the coordinates are restricted only to those allowed in the relevant phase.
* Memoized coordinates are calculated and are consistent with non-memoized coordinates.
  * Only move generators are memoized to save space.
* Symmetry coordinates work for move sequences.
  * The equivalence class always comes out right, but the code may struggle when one cube can be represented by two symmetry coordinates.

---

**Untested functionality**:
* The following tests are commented out because they are not adjusted to work with my recent refactor, **but other functionality depends on them and passes.**
  * Symmetries on moves.
  * Move sequences on a permutation.

---

**TODO urgent**:
* Finish `Cube` module and test it.

---

**TODO eventually**:
* Functionality:
  * Implement A*.
  * Implement pruning tables.
    * Must consider how one cube can be represented by two symmetry coordinates.
  * Implement repeated search for worse phase1 result and better phase2 result.
  * Figure out why move tables can't be saved during testing.
  * Create setup executable to calculate tables.
  * Use config for table locations and setup state.
  * Reintroduce reflection symmetry with working orientations.
  * Memoize symmetries on the coordinates before computing symmetry coordinate move tables
    * Will this really increase efficiency? This table will be about as large as the symmetry coordinate move table anyways...
* Code improvements:
  * Use quickcheck instead of my random selections.
  * See about passing a `Move.Fixed_move.S` around coordinates much less. It's excessive, but I haven't yet found a way around it, even using functors.
* User experience:
  * Front end?
  * opam file and "how to run" instructions.
* And probably lots more in all categories...

---

**Status updates**:
* 13 Oct 2023 -- coordinates are limited to only their appropriate moves. All symmetry and memoized coordinates are tested and working (still without reflection). Only generators are memoized.
* 12 Oct 2023 -- symmetry coordinates work but without reflection symmetry.
* 08 Oct 2023 -- symmetries work without orientations. Symmetry coordinates failing moves.
* 06 Oct 2023 -- all raw coordinates work completely under move sequences starting from random permutations. However, the reflection symmetry is broken.
* 26 Sep 2023 -- orientation coordinates fail under moves. I may refactor moves to not consider initial orientation, but I don't yet see how this solves the issue.
* 09 Sep 2023 -- no consistent updates on status before 26 Sep 2023. First commit 09 Sep 2023.