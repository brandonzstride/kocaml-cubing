# kocaml-cubing
An OCaml implementation of the Kociemba Two-Phase algorithm.

IMPORTANT: This project is a work in progress. It is far from complete at the moment. Please do not expect it to do anything meaningful for the time being.

This project is entirely my interpretation of Herbert Kociemba's explanation on [kociemba.org](http://kociemba.org/cube.htm). I do not refer to any other code that implements the Two-Phase algorithm.

TODO:
* Fix reflection symmetry's behavior on corner orientation.
* Verify symmetry coordinates are consistent.
* Implement A*.
* Implement pruning tables.
* Implement repeated search for worse phase1 result and better phase2 result.
* Create setup executable to calculate tables.
* Use config for table locations and setup state.
* And probably lots more.

Current status:
* 6 Oct 2023 -- all raw coordinates work completely under move sequences starting from random permutations. However, the reflection symmetry is broken.
* 26 Sep 2023 -- orientation coordinates fail under moves. I may refactor moves to not consider initial orientation, but I don't yet see how this solves the issue.