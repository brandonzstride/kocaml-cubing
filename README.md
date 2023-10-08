# kocaml-cubing
An OCaml implementation of the Kociemba Two-Phase algorithm.

IMPORTANT: This project is a work in progress. It is far from complete at the moment. Please do not expect it to do anything meaningful for the time being. Refer to "current status" below to see what works.

This project is entirely my interpretation of Herbert Kociemba's explanation on [kociemba.org](http://kociemba.org/cube.htm). I do not refer to any code that implements the Two-Phase algorithm.

TODO urgent:
* Test moves on symmetry coordinates to discover the inconsistency (they fail somehow).
* Fix reflection symmetry's behavior on corner orientation because it is needed for graph search to goal.
  * Or (for a quick spot-fix) remove the reflection symmetry; this only doubles the number of symmetry classes, which is not completely game-breaking.

TODO eventually:
* Implement moves on a cube as a whole--combining all coordinates to completely represent the cube. (make `Cube` module?)
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