
(*
To check if a cube is well formed, just send perm to list of corners and edges,
  and make sure both lists are odd or both lists are even.
Must also check orientations. This can be done similarly to coordinates.   
*)

let sgn ls ~compare =
  let rec num_inversions = function
  | [] -> 0
  | hd :: tl -> List.count tl ~f:(fun x -> compare x hd < 0) + num_inversions tl
  in
  match num_inversions ls mod 2 with
  | 0 -> `Even
  | _ -> `Odd