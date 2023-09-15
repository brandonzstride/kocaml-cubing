
open Core

[@@@ocaml.warning "-27"] (* unused variable declarations *)

module type Raw =
  sig
    type t
    val invert : t -> Perm.t
    val calculate : Perm.t -> t
    val perform_fixed_move : t -> Move.Fixed_move.t -> t
    val perform_symmetry : t -> Symmetry.t -> t
    val next : t -> t option
    val to_rank : t -> int
    val zero : t
    val n : int 
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

let ncr n r = failwith "unimplemented"

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
            0..2186 (3^7 - 1 = 2186)
          *)
          module T =
            struct
              type t = int
              let to_rank = fun x -> x
              let n = 2187
            end

          include T

          let zero = 0
          let next x = if x = n - 1 then None else Some (x + 1)

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
            let rec go x = function
            | [] -> failwith "impossible if cube permutation is well-formed"
            | hd :: tl -> begin function
              | Corner { c ; o = _ } when Cubie.Corner_facelet.compare hd c = 0 -> Corner {c ; o = Modular_int.Z3.of_int x }
              | Corner _ as c -> (go (x / 3) tl) c
              | e -> e (* let edges be untouched because they don't matter for corner orientation coordinate *)
              end
            in
            let final_orientation = (* deduce orientation of the final cubie *)
              x
              |> sum_of_digits ~base:3
              |> Modular_int.Z3.of_int
              |> Modular_int.Z3.inverse
              |> Modular_int.Z3.to_int
            in
            go (x * 3 + final_orientation) Cubie.Corner_facelet.all_rev
          
          (*
            Iterate over all corners (except for the last, least significant one),
            and make each one its own digit in a base 3 number.   
          *)
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
            | None -> fun x m ->
              let m = Move.Fixed_move.to_move m in
              let p = invert x in
              Perm.perform_move p m |> calculate
            
          let perform_symmetry : t -> Symmetry.t -> t =
            match symmetry_table with
            | Some table -> fun x s -> Twist_symmetry_table.lookup table x s
            | None -> fun x s -> invert x |> Symmetry.on_perm s |> calculate
        end
    end

  module Flip : S_raw =
    struct
      module Make (M : Is_memoized) : Raw =
        struct
          (*
            There are 12 edges to flip. Each has orientation 0 or 1. If the
            cube is well-formed, then the orientation of the last one can be
            determined by the orientation of the other 11, but the other 11 are
            independent of each other.
            So represent a cube's edge orientations as a number in 0..(2^11 - 1).
            Each edge takes up one bit in this number.
          *)
          module T =
            struct
              type t = int
              let to_rank = fun x -> x
              let n = 2048
            end

          include T

          let zero = 0
          let next x = if x = n - 1 then None else Some (x + 1)

          (* TODO: see which lookup tables are needed (do we need symmetry table?) *)
          
          (*
            Like inverting the corner orientation, construct a function that
            twists corners according to each bit. See `Twist.invert` for the
            analogous function on corners.
          *)
          let invert (x : t) : Perm.t =
            let open Cubie in
            let rec go x = function
            | [] -> failwith "impossible if cube permutation is well-formed"
            | hd :: tl -> begin function
              | Edge { e ; o = _ } when Cubie.Edge_facelet.compare hd e = 0 -> Edge {e ; o = Modular_int.Z2.of_int x }
              | Edge _ as e -> (go (x / 2) tl) e
              | c -> c (* don't touch corners *)
              end
            in
            let final_orientation = (* deduce orientation of the final edge *)
              x
              |> sum_of_digits ~base:2
              |> Modular_int.Z2.of_int
              |> Modular_int.Z2.inverse
              |> Modular_int.Z2.to_int
            in
            go (x * 2 + final_orientation) Cubie.Edge_facelet.all_rev
            
          (*
            Iterate over all edges (except for the last, least significant one),
            and make each one its own digit in a binary number.
          *)
          let calculate (p : Perm.t) : t =
            let open Cubie.Edge in
            let rec go acc = function
            | [] | [_] -> acc (* ignores least signficant edge *)
            | e :: tl -> go (acc * 2 + Modular_int.Z2.to_int e.o) tl
            in
            go 0 (Perm.to_edges_list p)
          
          let perform_fixed_move (x : t) (m : Move.Fixed_move.t) : t =
            failwith "unimplemented"

          let perform_symmetry (x : t) (s : Symmetry.t) : t =
            failwith "unimplemented"
        end
    end

  module UD_slice : S_raw =
    struct
      module Make (M : Is_memoized) : Raw =
        struct
          (*
            There are 12 total edges, and there are 4 whose positions we care about.
            This leaves 12 choose 4 possible cases. We map each case to a number in
            0..(nCr 12 4 - 1).   
          *)
          module T =
            struct
              type t = int
              let to_rank = fun x -> x
              let n = ncr 12 4
            end

          include T

          let zero = 0
          let next x = if x = n - 1 then None else Some (x + 1)
          
          let invert (x : t) : Perm.t =
            failwith "unimplemented"
          
          let calculate (p : Perm.t) : t =
            failwith "unimplemented"
          
          let perform_fixed_move (x : t) (m : Move.Fixed_move.t) : t =
            failwith "unimplemented"

          let perform_symmetry (x : t) (s : Symmetry.t) : t =
            failwith "unimplemented"
        end
    end
    
  (*
    Flip_UD_slice is the UD_slice coordinate combined with edge orientation.
    We can represent as a tuple or as a multiplied number.
    Kociemba does this:
      Flip_UD_slice = UD_slice * 2048 + Flip
      => Flip = Flip_UD_slice mod 2048
         UD_slice = Flip_UD_Slice / 2048
  *)
  module Flip_UD_slice_raw : S_raw =
    struct
      module Make (M : Is_memoized) : Raw =
        struct
          module F = Flip.Make(M)
          module U = UD_slice.Make(M)
          (*
            Flip UD Slice is a combination of the Flip coordinate and
            UD Slice coordinates. We choose to represent it as a record.
          *)
          module T =
            struct
              type t = { ud_slice : U.t ; flip : F.t }
              let to_rank { ud_slice ; flip } = F.n * U.to_rank ud_slice + F.to_rank flip
              let n = F.n * U.n
            end

          include T

          let zero = { ud_slice = U.zero ; flip = F.zero }
          let next { ud_slice ; flip } =
            match U.next ud_slice, F.next flip with
            | None, None -> None
            | _ , Some x -> Some { ud_slice ; flip = x }
            | Some x, None -> Some { ud_slice = x ; flip = F.zero }
          
          let invert (x : t) : Perm.t =
            Move.(F.invert x.flip * U.invert x.ud_slice)
          
          let calculate (p : Perm.t) : t =
            { ud_slice = U.calculate p ; flip = F.calculate p }
          
          (* Need to consider if it's much faster to let this have a lookup table vs
             letting Flip and UD Slice have their own lookup tables.
            Further, these might only have a lookup table for the representatives, which
            could maybe be stored in the sym module.
          *)
          let perform_fixed_move (x : t) (m : Move.Fixed_move.t) : t =
            failwith "unimplemented"

          let perform_symmetry (x : t) (s : Symmetry.t) : t =
            failwith "unimplemented"
        end
    end

  module Flip_UD_slice : S_sym =
    struct
      module Make (M : Is_memoized) : Sym =
        struct
          (*
            A symmetry coordinate has a representative of the equivalence class
            and the symmetry that is applied to that representative to get the
            desired cube.
            Thus, the coordinate is a representative index and a symmetry.
            There are 64430 equivalence classes of the Flip US Slice coordinate.
            We only save results for the representative, so we state there are only
            as many coordinates as there are equivalence classes.
          *)
          module T =
            struct
              type t = { rep : int ; sym : Symmetry.t }
              let to_rank = fun x -> x.rep
              let n = 64430
            end

          include T

          let zero = { rep = 0; sym = Symmetry.zero } 
          let next { rep ; sym } =
            match Symmetry.next sym with
            | None when rep = n - 1 -> None
            | None -> Some { rep = rep + 1 ; sym = Symmetry.zero }
            | Some x -> Some { rep ; sym = x }
          
          let invert (x : t) : Perm.t =
            failwith "unimplemented"
          
          let calculate (p : Perm.t) : t =
            failwith "unimplemented"
          
          let perform_fixed_move (x : t) (m : Move.Fixed_move.t) : t =
            failwith "unimplemented"

          let perform_symmetry (x : t) (s : Symmetry.t) : t =
            failwith "unimplemented"
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
          (*
            The Phase 2 edge perm coordinate only considers the edges in the
            UD faces. Hence, there are 8 edges and 8! possible permutations.
            So t is int in 0..(8! - 1)
          *)
          module T =
            struct
              type t = int 
              let to_rank = fun x -> x
              let n = 40320
            end

          include T

          let zero = 0
          let next x = if x = n - 1 then None else Some (x + 1)

          (*
            To compute the permutation coordinate, we consider all cubies included
            in the permutation, and for each one we count how far it is out of order
            and apply a weight that is factorially proportionate.
          *)
          (* let permutation_coordinate (p : Perm.t) ~(compare: 'a -> 'a -> int) ~(all_rev: 'a list) ~(max : int): int =
            let count_inversions c tl =
              let pc = (p c).c in
              List.count tl ~f:(fun x -> compare (p x).c pc > 0)
            in
            let rec go acc i = function
            | [] | [_] -> acc
            | c :: tl -> go (acc * i + count_inversions c tl) (i - 1) tl
            in
            go [] max all_rev *)

          
          let invert (x : t) : Perm.t =
            failwith "unimplemented"
          
          let calculate (p : Perm.t) : t =
            failwith "unimplemented"
          
          let perform_fixed_move (x : t) (m : Move.Fixed_move.t) : t =
            failwith "unimplemented"

          let perform_symmetry (x : t) (s : Symmetry.t) : t =
            failwith "unimplemented"
        end
    end
  
    
  module Corner_perm_raw : S_raw =
    struct
      module Make (M : Is_memoized) : Raw =
        struct
          (*
            Just like edge perm, there are 8 cubies. See edge perm details.   
          *)
          module T =
            struct
              type t = int 
              let to_rank = fun x -> x
              let n = 40320
            end

          include T

          let zero = 0
          let next x = if x = n - 1 then None else Some (x + 1)
          
          let invert (x : t) : Perm.t =
            failwith "unimplemented"
          
          let calculate (p : Perm.t) : t =
            failwith "unimplemented"
          
          let perform_fixed_move (x : t) (m : Move.Fixed_move.t) : t =
            failwith "unimplemented"

          let perform_symmetry (x : t) (s : Symmetry.t) : t =
            failwith "unimplemented"
        end
    end

  module Corner_perm : S_sym = 
    struct
      module Make (M : Is_memoized) : Sym =
        struct
          (* This is not thought through yet *)
          module T =
            struct
              type t = int 
              let to_rank = fun x -> x
              let n = 40320
            end

          include T

          let zero = 0
          let next x = if x = n - 1 then None else Some (x + 1)
          
          let invert (x : t) : Perm.t =
            failwith "unimplemented"
          
          let calculate (p : Perm.t) : t =
            failwith "unimplemented"
          
          let perform_fixed_move (x : t) (m : Move.Fixed_move.t) : t =
            failwith "unimplemented"

          let perform_symmetry (x : t) (s : Symmetry.t) : t =
            failwith "unimplemented"
        end
    end

  module UD_slice_perm : S_raw =
    struct
      module Make (M : Is_memoized) : Raw =
        struct
          (* four cubies to permute *)
          module T =
            struct
              type t = int 
              let to_rank = fun x -> x
              let n = 24
            end

          include T

          let zero = 0
          let next x = if x = n - 1 then None else Some (x + 1)
          
          let invert (x : t) : Perm.t =
            failwith "unimplemented"
          
          let calculate (p : Perm.t) : t =
            failwith "unimplemented"
          
          let perform_fixed_move (x : t) (m : Move.Fixed_move.t) : t =
            failwith "unimplemented"

          let perform_symmetry (x : t) (s : Symmetry.t) : t =
            failwith "unimplemented"
        end
    end

end
