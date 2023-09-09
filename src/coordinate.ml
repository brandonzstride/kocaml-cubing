(* Because we'll probably create move tables, I'll want to make a setup
   executable that runs all the coordinates and saves the move tables.
   
   Efficiency shouldn't matter *that* much here because these only need to be
   calculated once.

   For these lookup tables, should use Array behind the scenes because that
   will be faster than a map. For a table of size 10^6, it's about 5 times
   faster to do an array than a map, which is not that bad. I did this by
   generating 10^6 random integers mapping to a random int in 0..10^6 and
   queried for 1000 random ones. It appears 10 times faster with caching by
   doing the same integer a ton of times, but when the query jumps around a lot
   (because it's random), the map catches up (however it's still ~5 times slower).

   I need to be able to invert coordinates.
*)

(* Note that "flipUDSlice" is the UDslice coordinate multiplied by edge orientation, I think *)
(* By some of the code, it appears that UDSlice coord = flipUDslice / 2048, and
   edge ori coord = flipUDslice mod 2048
    Why isn't this explained anywhere?   
  So say flipudslice = udclice*2048 + edgeori
  This quite checks out because the edge orientations are in 2^11 = 2048.
*)


module Phase1 = struct
  
  (* Perm.t might just be Move.t. However we hope to find the moves that reverse a permutation,
     and a permutation might just be some unsolved cube state that we're not yet sure how to solve
    
     Might better off call this Cube.t. Maybe I want Cube to represent something else. Not sure yet.

     Constraints: returned int in [0, 2186]
  *)
  (* issue here is that p probably expects Corner of Corner_facelet.t | Edge of Edge_facelet.t *)
  (* although this is much prefered to avoid duplication and having to change things twice. *)
  let orientation_coordinate (p : Perm.t) ~(base : int) ~(o_to_int : Modular_int.S.t -> int) ~all =
    let rec go acc = function
    | [] | [_] -> acc
    | hd :: tl -> go (acc * base + o_to_int (p hd).o) tl
    in
    go 0 all
  
  (* other solution is this: *)
  (* aka corner "twist" aka "UD twist" *)
  let corner_orientation_coordinate (p: Perm.t) : int =
    (* use acc to be tail recursive. Hopefully it's faster even though stack will be small *)
    let rec go acc = function
    | [] | [_] -> acc (* this is to ignore last *)
    | c :: tl -> go (acc * 3 + (p c).o) tl (* maybe will need Z3.to_int because of (p c).o type as modular int *)
    in
    go 0 Corner_facelet.all

  (* although this is similar to corner orientation, I prefer to not abstract it because will need to pass Z2 or Z3 to_int which just gets long *)
  (* aka edge "flip" *)
  let edge_orientation_coordinate (p : Perm.t) : int =
    let rec go acc = function
    | [] | [_] -> acc
    | e :: tl -> go (acc * 2 + (p e).o) tl
    in
    go 0 Edge_facelet.all

  (*
    all_rev is reversed list of all possible cubies. max is length of that list.
  *)
  let permutation_coordinate (p : Perm.t) ~(compare: 'a -> 'a -> int) ~(all_rev: 'a list) ~(max : int): int =
    let count_inversions c tl =
      let pc = (p c).c in
      List.count tl ~f:(fun x -> compare (p x).c pc > 0)
    in
    let rec go acc i = function
    | [] | [_] -> acc
    | c :: tl -> go (acc * i + count_inversions c tl) (i - 1) tl
    in
    go [] max all_rev

  let corner_permutation_coordinate (p : Perm.t) : int =
    permutation_coordinate p
      ~compare:Corner_facelet.compare
      ~all_rev:Corner_facelet.all_rev
      ~max:Corner_facelet.max

  let edge_permutation_coordinate (p : Perm.t) : int =
    permutation_coordinate p
      ~compare:Edge_facelet.compare
      ~all_rev:Edge_facelet.all_rev
      ~max:Edge_facelet.max

  (* I need choose to do this. Nowhere seems to have choose, so I need to write it myself. *)
  let ud_slice_coordinate (p : Perm.t) : int =
    0
end

module Phase2 = struct

  let ud_slice_coordinate (p : Perm.t) : int = 
    Phase1.permutation_coordinate p
      ~compare:Edge_facelet.compare
      ~all_rev:Edge.[BR; BL; FL; FR] (* consider not hardcoding this so much *)
      ~max:4

  let corner_orientation_coordinate = Phase1.corner_orientation_coordinate

  let edge_permutation_coordinate (p : Perm.t) : int =
    Phase1.permutation_coordinate p
      ~compare:Edge_facelet.compare
      ~all_rev:Edge.[DB; DL; DF; DR; UB; UL; UF; UR]
      ~max:8

end
