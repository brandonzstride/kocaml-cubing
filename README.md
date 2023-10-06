# kocaml-cubing
An OCaml implementation of the Kociemba Two-Phase algorithm.

IMPORTANT: This project is a work in progress. It is far from complete at the moment. Please do not expect it to do anything meaningful for the time being.

TODO:
* Fix coordinate behavior under moves.
* Verify symmetry coordinates are consistent.
* Implement A*.
* Implement pruning tables.
* Implement repeated search for worse phase1 result and better phase2 result.
* Create setup executable to calculate tables.
* Use config for table locations and setup state.
* And probably lots more.

move_refactor branch:
* Need to make it so that a move only takes a cubie and returns cubie with an orientation.

Current status:
* 26 Sep 2023 -- orientation coordinates fail under moves. I may refactor moves to not consider initial orientation, but I don't yet see how this solves the issue.