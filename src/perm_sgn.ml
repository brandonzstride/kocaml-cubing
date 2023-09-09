
let sgn ls ~compare =
  let rec num_inversions = function
  | [] -> 0
  | hd :: tl -> List.count tl ~f:(fun x -> compare x hd < 0) + num_inversions tl
  in
  match num_inversions ls mod 2 with
  | 0 -> `Even
  | _ -> `Odd

(* The cosets will be all elements with that coordinate value. Since C0 = all with corner coordinate zero,
  then a single g in G gives a coset C0*g that has corner coordinate the same as g. So C0*g is all elements
  in G with corner coordinate the same as g.   
*)

(* Note that I can map a color set to a solved corner, so then I can just do "is replace by" easily to get
   the permutation it's currently in. Then check that sgn is even.
   
   Also can easily check orientations check out by knowing which facelet is reference and checking distance.
   I might have to assign some metric. 

   e.g.

   let distance cubie1 cubie2 =
    let distance_corner c1 c2 =
      match Cubie.Corner.reference_color c1, Cubie.Corner.reference_color c2 with
      | actually this won't work because L face on UFL cubie is 2, but L face on ULB cubie is 1. Not too bad to hard code though. 
    match cubie1, cubie2 with
    | Corner c1, Corner c2 -> distance_corner c1 c2
    match Cubie.reference_color cubie1, Cubie.reference_color cubie2 with
    | 
   *)