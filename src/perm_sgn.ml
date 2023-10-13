(*
  File: perm_sgn.ml
  Module purpose: permutation sign for checking if a cube is well-formed.
  Status: incomplete.

  Detailed description:
    This module can be ignored for now.

    This file holds some code I intend to use for verifying if a cube
    is well-formed. Only even permutations of the cubies are achievable
    by turns of the faces, so we can verify well-formedness by checking
    the sign of the permutation.

  Other considerations:
    Maybe make this a "cube-checker" module that checks all properties
    for well-formedness.
    * permutation sign
    * colors make real cube
    * cubie orientations sum to zero mod n

  Dependencies:
    None
*)

let sgn ls ~compare =
  let rec num_inversions = function
  | [] -> 0
  | hd :: tl -> List.count tl ~f:(fun x -> compare x hd < 0) + num_inversions tl
  in
  match num_inversions ls mod 2 with
  | 0 -> `Even
  | _ -> `Odd