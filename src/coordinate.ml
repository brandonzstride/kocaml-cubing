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

open Core

[@@@ocaml.warning "-27"] (* unused variable declarations *)

module type Raw =
  sig
    type t
    val invert : t -> Perm.t
    val calculate : Perm.t -> t
    val perform_fixed_move : t -> Move.Fixed_move.t -> t
    val perform_symmetry : t -> Symmetry.t -> t
  end

module type Sym = Raw

module type Is_memoized =
  sig
    val is_memoized : bool
  end

module type S_raw =
  sig
    module Make (_ : Is_memoized) : Raw
  end

module type S_sym =
  sig
    module Make (_ : Is_memoized) : Sym
  end

let memoized_dir= "./lookup_tables/coordinates/"
(* Will use Sexp.save filepath sexp_instance to save*)

let rec sum_of_digits ~base = function
  | 0 -> 0
  | x -> (x mod base) + sum_of_digits ~base (x / base)

module Phase1 = struct

  module Twist : S_raw =
    struct
      module Make (M : Is_memoized) : Raw =
        struct
          (*
            There are 8 corners, each with 3 possible orientations, but the last
            corner's orientation can be calculated by the other 7 if the cube is
            well-formed. Therefore, there are 3^7 possible orientations of all 8
            corners. We represent a corner orientation by a unique integer in
            0..2186 (3^7 - 1 2186)
          *)
          module T =
            struct
              type t = int
              let to_rank = fun x -> x
              let n = 2187
            end

          include T

          (* Stores the results of coordinate -> move -> coordinate *)
          module Twist_move_table = Lookup_table.Make2D (T) (Move.Fixed_move) (Int)
          (* Stores the results of coordinate -> symmetry -> coordinate *)
          module Twist_symmetry_table = Lookup_table.Make2D (T) (Symmetry) (Int)

          let move_table =
            match M.is_memoized with
            | false -> None
            | true  -> Some (Twist_move_table.from_file (memoized_dir ^ "twist_move.sexp")) (* consider a global or config file here*)

          let symmetry_table =
            match M.is_memoized with
            | false -> None
            | true  -> Some (Twist_symmetry_table.from_file (memoized_dir ^ "twist_symmetry.sexp"))
          
          (*
            To invert the corner twists into a cube, we simply twist a few
            corners on a solved cube until they meet the desired orientation.
            Here, we iterate over all the corners that are directly in the
            coordinate, and then we assume the cube is well-formed and derive
            the last orientation because it must make the sum of all
            orientations 0 mod 3. We leave facelets the same; we only twist.
          *)
          let invert (x : t) : Perm.t =
            let open Cubie in
            let rec go x' = function
            | [] -> begin function
              | Corner { c ; o = _ } -> Corner { c ; o = Modular_int.Z3.of_int (sum_of_digits x ~base:3) |> Modular_int.Z3.inverse} 
              | e -> e 
              end
            | hd :: tl -> begin function
              | Corner { c ; o = _ } when Cubie.Corner_facelet.compare hd c = 0 -> Corner { c ; o = Modular_int.Z3.of_int x' }
              | Corner _ as c -> (go (x' / 3) tl) c
              | e -> e (* let edges be untouched *)
              end
            in
            Cubie.Corner_facelet.all_rev
            |> List.tl_exn (* ignore least significant corner *)
            |> go x
            
          
          let calculate (p : Perm.t) : t =
            let open Cubie.Corner in (* how come we need to open Cubie.Corner to access record field o? *)
            let rec go acc = function
            | [] | [_] -> acc (* ignores least signficant corner *)
            | c :: tl -> go (acc * 3 + Modular_int.Z3.to_int c.o) tl
            in
            go 0 (Perm.to_corners_list p)
          
          let perform_fixed_move : t -> Move.Fixed_move.t -> t =
            match move_table with
            | Some table -> fun x m -> Twist_move_table.lookup table x m
            | None -> failwith "unimplemented"
          
          let perform_symmetry : t -> Symmetry.t -> t =
            match symmetry_table with
            | Some table -> fun x s -> Twist_symmetry_table.lookup table x s
            | None -> fun x s -> invert x |> Symmetry.conjugate s |> calculate
        end
    end

  module Flip : S_raw =
    struct
      module Make (M : Is_memoized) : Raw =
        struct
          type t = int (* in 0..2047 *)
          
          let invert (x : t) : Perm.t =
            failwith "unimplemented"
          
          let calculate (p : Perm.t) : t =
            failwith "unimplemented"
          
          let perform_fixed_move (x : t) (m : Move.Fixed_move.t) : t =
            failwith "unimplemented"

          let perform_symmetry = failwith "unimplemented"
        end
    end

  module UD_slice : S_raw =
    struct
      module Make (M : Is_memoized) : Raw =
        struct
          type t = int (* in 0..(nCr 12 4) *)
          
          let invert (x : t) : Perm.t =
            failwith "unimplemented"
          
          let calculate (p : Perm.t) : t =
            failwith "unimplemented"
          
          let perform_fixed_move (x : t) (m : Move.Fixed_move.t) : t =
            failwith "unimplemented"

          let perform_symmetry = failwith "unimplemented"
        end
    end
    
  module Flip_UD_slice_raw : S_raw =
    struct
      module Make (M : Is_memoized) : Raw =
        struct
          type t = int (* in 0..2186*(nCr 12 4) *)
          
          let invert (x : t) : Perm.t =
            failwith "unimplemented"
          
          let calculate (p : Perm.t) : t =
            failwith "unimplemented"
          
          let perform_fixed_move (x : t) (m : Move.Fixed_move.t) : t =
            failwith "unimplemented"

          let perform_symmetry = failwith "unimplemented"
        end
    end

  module Flip_UD_slice : S_sym =
    struct
      module Make (M : Is_memoized) : Sym =
        struct
          type t = int (* in 0..2186*(nCr 12 4) *)
          
          let invert (x : t) : Perm.t =
            failwith "unimplemented"
          
          let calculate (p : Perm.t) : t =
            failwith "unimplemented"
          
          let perform_fixed_move (x : t) (m : Move.Fixed_move.t) : t =
            failwith "unimplemented"

          let perform_symmetry = failwith "unimplemented"
        end
    end
  
  
  (* Perm.t might just be Move.t. However we hope to find the moves that reverse a permutation,
     and a permutation might just be some unsolved cube state that we're not yet sure how to solve
    
     Might better off call this Cube.t. Maybe I want Cube to represent something else. Not sure yet.

     Constraints: returned int in [0, 2186]
  *)
  (* issue here is that p probably expects Corner of Corner_facelet.t | Edge of Edge_facelet.t *)
  (* although this is much prefered to avoid duplication and having to change things twice. *)
  (* let orientation_coordinate (p : Perm.t) ~(base : int) ~(o_to_int : Modular_int.S.t -> int) ~all =
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
    go 0 Edge_facelet.all *)

  (*
    all_rev is reversed list of all possible cubies. max is length of that list.
  *)
  (* Issue now is that perm acts on Cubie.t, so I need to handle getting that out of just list of facelets *)
  (* let permutation_coordinate (p : Perm.t) ~(compare: 'a -> 'a -> int) ~(all_rev: 'a list) ~(max : int): int =
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
      ~max:Edge_facelet.max *)

end

module Phase2 = struct

  (* let ud_slice_coordinate (p : Perm.t) : int = 
    Phase1.permutation_coordinate p
      ~compare:Edge_facelet.compare
      ~all_rev:Edge.[BR; BL; FL; FR] (* consider not hardcoding this so much *)
      ~max:4

  (* let corner_orientation_coordinate = Phase1.corner_orientation_coordinate *)

  let edge_permutation_coordinate (p : Perm.t) : int =
    Phase1.permutation_coordinate p
      ~compare:Edge_facelet.compare
      ~all_rev:Edge.[DB; DL; DF; DR; UB; UL; UF; UR]
      ~max:8 *)

  module Edge_perm : S_raw =
    struct
      module Make (M : Is_memoized) : Raw =
        struct
          type t = int (* in 0..8!-1 *)
          
          let invert (x : t) : Perm.t =
            failwith "unimplemented"
          
          let calculate (p : Perm.t) : t =
            failwith "unimplemented"
          
          let perform_fixed_move (x : t) (m : Move.Fixed_move.t) : t =
            failwith "unimplemented"

          let perform_symmetry = failwith "unimplemented"
        end
    end
  
    
  module Corner_perm_raw : S_raw =
    struct
      module Make (M : Is_memoized) : Raw =
        struct
          type t = int (* in 0..8!-1 *)
          
          let invert (x : t) : Perm.t =
            failwith "unimplemented"
          
          let calculate (p : Perm.t) : t =
            failwith "unimplemented"
          
          let perform_fixed_move (x : t) (m : Move.Fixed_move.t) : t =
            failwith "unimplemented"

          let perform_symmetry = failwith "unimplemented"
        end
    end

  module Corner_perm : S_sym = 
    struct
      module Make (M : Is_memoized) : Sym =
        struct
          type t = int (* in 0..8!-1 *)
          
          let invert (x : t) : Perm.t =
            failwith "unimplemented"
          
          let calculate (p : Perm.t) : t =
            failwith "unimplemented"
          
          let perform_fixed_move (x : t) (m : Move.Fixed_move.t) : t =
            failwith "unimplemented"

          let perform_symmetry = failwith "unimplemented"
        end
    end

  module UD_slice_perm : S_raw =
    struct
      module Make (M : Is_memoized) : Raw =
        struct
          type t = int (* in 0..23 *)
          
          let invert (x : t) : Perm.t =
            failwith "unimplemented"
          
          let calculate (p : Perm.t) : t =
            failwith "unimplemented"
          
          let perform_fixed_move (x : t) (m : Move.Fixed_move.t) : t =
            failwith "unimplemented"

          let perform_symmetry = failwith "unimplemented"
        end
    end

end
