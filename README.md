# kocaml-cubing
An OCaml implementation of the Kociemba Two-Phase algorithm.

## Notices

This project is a work in progress. There is meaningful functionality, but please do not expect it to solve a cube yet. Refer to the **Status** section below to see what works.

**This project is entirely my interpretation of Herbert Kociemba's explanation on [kociemba.org](http://kociemba.org/cube.htm)**. I do not refer to any code that implements the Two-Phase algorithm. I give credit for the ideas behind the algorithm to Herbert Kociemba. Implementation details are all my own, and I note my algorithmic additions in this README.

Written in `OCaml 5.0.0`. Depends on `Core`, `ppx_jane`, `OUnit2`, and `Psq` (priority search queue).

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
* Instead of memoizing all fixed moves on a cube, I only memoize the generators for those moves. For the group G, this cuts down on move table size by a factor of three, but it slightly increases computation time (which appears to be near negligible).
  * The same is done on symmetry coordinates because I don't use the reflection symmetry. Without the reflection symmetry, all move generators are other generators when acted on by a symmetry, so it is safe
  to only memoize the generators.

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
* Cubes are represented as a combination of coordinates and can be acted on by moves.

---

**Untested functionality**:
* The following tests are commented out because they are not adjusted to work with my recent refactor, **but other functionality greatly depends on them and passes.**
  * Symmetries on moves.
  * Move sequences on a permutation.

---

**TODO urgent**:
* All "Code improvement" TODOs are the most urgent because I value my code quality, and this project has no deadline.
* Create pruning tables.

---

**TODO eventually**:
* Functionality:
  * Implement A*.
  * Implement repeated search for worse phase1 result and better phase2 result.
  * Create setup executable to calculate tables.
  * Use config for table locations and setup state.
  * Reintroduce reflection symmetry with working orientations.
* Code improvements:
  * Use `quickcheck` instead of my random selections.
  * See about passing a `Move.Fixed_move.S` around coordinates much less. It's excessive, but I haven't yet found a way around it.
  * Consider `drom` for project structure?
* User experience:
  * Front end?
  * opam file and "how to run" instructions.
* And probably lots more in all categories...

---

**Status updates**:
* 31 Oct 2023 -- symmetry coordinates use memoized raw coordinates underneath for significantly faster computation of symmetries on the raw coordinate.
* 17 Oct 2023 -- only the necessary coordinates are exposed, and they are all memoized and saved.
* 13 Oct 2023 -- coordinates are limited to only their appropriate moves. All symmetry and memoized coordinates are tested and working (still without reflection). Only generators are memoized.
* 12 Oct 2023 -- symmetry coordinates work but without reflection symmetry.
* 08 Oct 2023 -- symmetries work without orientations. Symmetry coordinates failing moves.
* 06 Oct 2023 -- all raw coordinates work completely under move sequences starting from random permutations. However, the reflection symmetry is broken.
* 26 Sep 2023 -- orientation coordinates fail under moves. I may refactor moves to not consider initial orientation, but I don't yet see how this solves the issue.
* 09 Sep 2023 -- no consistent updates on status before 26 Sep 2023. First commit 09 Sep 2023.


## Proposal

This section is to propose the continuation of this project as an independent study project for Spring 2024. Please review the above to understand the background of the project. 

**Goal:** The goal is to have a working Rubik's cube solver following Kociemba's Two-phase algorithm.

**Purpose:** The purpose is to learn to design efficient systems in OCaml by solving a computationally hard problem.

### Justification ###

Here are the reasons this project is sufficient for independent study:


**Complexity**

The problem is algorithmically and mathematically complex. It requires me to create a well-designed system, practicing good software development. It is overall an exercise in creating a working, complex project.

**Scale**

While I've finish some of the logic already, there is still quite a ways to go, and there is virtually no end to efficiency improvements I can make. This project will certainly not be too small for the semester.

Some efficiency improvements include
* Use bytes to represent "distance to goal" with only an int mod 3, thus compressing storage used "per cube" from 8 bytes to a few bits
* Use on-disk data storage and compare to in-memory storage of pruning tables
* Parallelize creation of coordinates
* Add more layers to the abstraction to use literal ints as a final layer instead of dynamically allocated data (e.g. in Cube)

Also, I have never made a front end, and this might be a good chance to learn how to do that.

**Novelty**

Nobody yet appears to have implemented this algorithm in OCaml.

I've searched GitHub thoroughly for some mention of "Kociembia" and the OCaml lanuage, and there is one project that suggests the idea but never follows through.

**"Open-source" contributions**

The algorithm is not well documented (in my opinion), so it provides an opportunity to contribute to documentation. It also is an opportunity to learn how to document a large project, from tutorial to explanation to reference, etc.

**Advanced language features**

This project presents an opportunity to learn some of OCaml's more complex language features that aren't tackled in any class here.
1. I can use parallelism in several places:
    * Search graph for better solution in parallel
    * Do precomputations in parallel
    * Query for heuristic in pruning table in parallel
2. I can learn to write a preprocessing extension (ppx)
    * Maybe to handle the many invariant tests by inlining quickcheck tests
    * Maybe to make the excessive functors and/or nested modules nicer

### Implementation plan ###

Here is an initial proposal of steps I'd like to complete, with no stretch goals (yet):
1. Get a working first pass solver (est. end of Feb)
2. Iterate for more optimal solution
3. Use on-disk storage and compare performance
4. Improve documentation/explanation of the algorithm
