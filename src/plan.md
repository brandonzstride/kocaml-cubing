Here I describe the process and what we need.

Assuming everything we need has be precalculated, the process is this:
1. Search to get the cube to G1
2. Apply the moves to the cube to calculate the coordinates needed for phase2
3. Search to get the cube to solved.

That sounds so simple. But how does this happen?
1. The cube's coordinates are calculated
  a. Do I calculate those only needed for phase 2, or should I do those later?
2. The spot in the pruning table is found, and depth is gotten
3. All 18 moves are applied, and we look at the lowest depth for the pruning table in these spots. Effectively this is a DFS?
4. Goal is found, and the moves needed are applied to the other parts of the cube to determine starting state for phase 2.
5. The spot in the pruning table is found, and we apply moves searching for goal, but also keeping track of UD slice coordinate.
6. Goal might be found, but the cube might not be solved because of the UD slice coordinate. At this point, we check if it's solved, and if not, then we back out and try other paths until one works.


What we need to make this work.

Moves
* We need move representations on the cubie level. We use this to calculate how coordinates are moved.
* We might need move multiplication.

(Raw) Coordinate definitions
* For phase 1
  * Corner orientation (raw) aka UD twist
  * Edge orientation (raw) aka UD flip
  * UD slice coordinate (raw)
  * Combine the above two to get Flip UD Slice (raw)
  * Use this to get sym coord for Flip UD Slice
* For phase 2
  * Corner permutation (raw and sym)
  * 8 edge permutation (raw)
  * UD Slice permutation (raw)

Coordinate inversions
* We need to map some coordinate to a representative cube
* This is so that we can apply a move to the cube and calculate the resulting coordinate for the move table

Symmetries
* We need to define how symmetries affect the cube
* Then show how conjugates affect the cube (?)
* Need multiplications of symmetries : int -> int -> int
  * Might do this by applying two symmetries to a solved cube and seeing how it compares to other symmetries
* Need multiplication of symmetry with move : int -> move -> move
* Then how symmetries affect coordinates
  * I think just UD Twist and 8 Edge perm
  * I still neeed to think about how symmetries affect other coordinates like UD slice and if we need that.
  * Further, how do all these sym coords and stuff work with the moves that we're actually applying? Do moves need to be converted?

Tables
* These will be functions with tables under the hood
* UDTwistTable : int -> move -> int
* FlipUDSliceTable : int -> move -> int
  * This is a sym table and it depends on...
  * SymMove : (sym_index : int) -> move -> move
  * SymMult : int -> int -> int
  * RepresentativeMoveTable : int -> move -> int
    * This depends on...
    * Getting representative raw coordinate, inverting to cube, moving, calculting coordinate, finding symmetry class, getting sym coord from class
* CornerPermTable : int -> move -> int
  * This is a sym table, so it needs a representative move table like above, but SymMove and SymMult can be reused.
* EdgePermTable : int -> move -> int
  * Only 8 edges
* (?maybe) UDSliceSortedTable : int -> move -> int
  * Is permutation of UD slice edges.
* Pruning tables
  * Are populated by applying moves to coordinate tuples from goal state and filling in depth
  * These are the heuristic in A*

Cube representation
* I like having a function that is a map from a position to what is actually there.
* But this is identical to a composition of moves
* Potentially we call this a Perm
* In phase 1 can use tuples to represent cube
* similarly in phase 2 but with different tuples

Solver
* Should have a solve function
* This will do the logic of combining the steps to reach a solved cube
* Hopefully we will represent the search somewhere else.

Setup
* I need a setup executable that creates all the tables and saves them.
* I will likely save them as sexp, each in their own file.