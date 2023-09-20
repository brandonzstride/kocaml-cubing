
open Core

[@@@ocaml.warning "-27"]

module type Raw =
  sig
    type t [@@deriving sexp, compare]
    (* find a representative permutation for the coordinate *)
    val to_perm : t -> Perm.t
    (* calculate the coordinate for a given permutation *)
    val of_perm : Perm.t -> t
    (* calculate the resulting coordinate after applying one of the regular, fixed moves *)
    val perform_fixed_move : t -> Move.Fixed_move.t -> t
    (* Cube P with coordinate x and symmetry S. Gets coordinate of S * P * S^-1 *)
    val perform_symmetry : t -> Symmetry.S.t -> t
    (* Find the next largest coordinate. This is useful for creating lookup tables. *)
    val next : t -> t option
    (* Rank the coordinates as integers *)
    val to_rank : t -> int
    (* Get the coordinate of the given rank.
       Ideally this isn't used much, and `next` is used instead. *)
    val of_rank : int -> t
    (* The smallest coordinate *)
    val zero : t
    (* How many coordinates there are *)
    val n : int
    val all : unit -> t list
  end

module type Sym =
  sig
    type t [@@deriving sexp, compare]
    val to_perm : t -> Perm.t
    val of_perm : Perm.t -> t
    val perform_fixed_move : t -> Move.Fixed_move.t -> t
    val perform_symmetry : t -> Symmetry.S.t -> t
    (* Get the representative of the next class, not just the next coordinate *)
    val next_representative : t -> t option
    (* Gets the rank of the eq class of the coordinate *)
    (* Equivalent to getting some representative raw coordinate for the class *)
    (* I need to think about how I change this now that rank is slow if not memoized. *)
    val to_eq_class_rank : t -> int
    (* Gets which symmetry converts the coordinate to the representative *)
    (* i.e. if the perm is P, then it is converted to representative by S * P * S^-1 *)
    val get_symmetry : t -> Symmetry.S.t
    (* Gets the first representative *)
    val zero_representative : t
    (* Number of equivalence classes i.e. number of representatives *)
    val n : int
    val all : unit -> t list
  end

module Int_coord (N : sig val n : int end) =
  struct
    type t = int [@@deriving sexp, compare]
    let to_rank = Fn.id
    let of_rank = Fn.id
    let n = N.n
    let zero = 0
    let next x = if x = n - 1 then None else Some (x + 1)
    let all () = List.init n ~f:Fn.id
  end


module Sym_of_raw (R : Raw) : Sym =
  struct

    module Rset = Set.Make (R)

    (* is a set of all representative raw coordinates *)
    let raw_reps = 
      let get_sym_class x =
        Symmetry.S.all
        |> List.map ~f:(fun s -> R.perform_symmetry x s)
        |> Rset.of_list
      in
      let rec loop reps is_done = function
      | None -> assert (Set.length is_done = R.n); reps
      | Some x when Set.mem is_done x -> loop reps is_done (R.next x)
      | Some x ->
        let sym_class = get_sym_class x in (* nonempty because includes x *)
        let rep = Set.min_elt_exn sym_class in
        let is_done = Set.fold sym_class ~init:is_done ~f:Set.add in
        loop (Set.add reps rep) is_done (R.next x)
      in loop Rset.empty Rset.empty (Some R.zero)

    (*
      Externally, it will appear as if there are only as many coordinates
      as there are classes. Internally, there are a multiple of
      Symmetry.S.n more.
    *)
    module I = Int_coord (struct let n = Set.length raw_reps end)
    include I

    let to_eq_class_rank x = x / Symmetry.S.n
    let get_symmetry_rank x = x mod Symmetry.S.n (* private *)
    let get_symmetry x = x |> get_symmetry_rank |> Symmetry.S.of_rank
    
    let next_representative x = to_eq_class_rank x |> I.next
    let zero_representative = I.zero

    module Class_to_rep_table = Lookup_table.Make1D
      (struct
        type t = int
        let to_rank = Fn.id
      end) 
      (R)

    (* might just use Set.nth, but I would have to consider speed before making choice *)
    let class_to_rep_table = Class_to_rep_table.create (Set.to_list raw_reps)

    module Rmap = Map.Make (R)

    (* Takes raw representative coordinate and maps to symmetry class index *)
    let rep_to_class_map =
      raw_reps
      |> Set.to_list
      |> List.foldi ~init:Rmap.empty ~f:(fun i accum r -> Map.add_exn accum ~key:r ~data:i)

    let to_raw_rep (x : t) : R.t =
      x
      |> to_eq_class_rank
      |> Class_to_rep_table.lookup class_to_rep_table

    let to_raw (x : t) : R.t =
      x
      |> get_symmetry (* the symmetry that converts the raw coord to the rep raw coord *)
      |> Symmetry.S.inverse (* now the symmetry that converts rep to coord *)
      |> R.perform_symmetry (to_raw_rep x)

    let of_symmetry_and_raw (s : Symmetry.S.t) (x : R.t) : t =
      let rep_rank =
        let raw_rep = R.perform_symmetry x s in
        match Map.find rep_to_class_map raw_rep with (* finds class rank of rep coord *)
        | Some i -> i
        | None -> failwith "invalid representative coordinate" (* logically impossible *)
      in
      Symmetry.S.n * rep_rank + Symmetry.S.to_rank s

    (* x could be as large as n * Symmetry.S.n *)
    let to_perm (x : t) : Perm.t =
      x
      |> to_raw
      |> R.to_perm

    let of_perm (p : Perm.t) : t =
      let raw = R.of_perm p in
      let f s = R.perform_symmetry raw s |> Set.mem raw_reps in
      match List.find Symmetry.S.all ~f:f with
      | Some s -> of_symmetry_and_raw s raw
      | None -> failwith "no equivalence class found for given permutation" (* logically impossible *)

    let perform_fixed_move (x : t) (m : Move.Fixed_move.t) : t =
      (*
        Process:
        1. Convert to raw representative coordinate
        2. Convert move to equivalent move on rep
        3. Apply move to rep
        4. Convert resulting raw to sym
        5. Handle symmetry? See notes
      *)
      (* placeholder *)
      let _ = x in let _ = m in zero_representative

    let perform_symmetry _ = failwith "unimplemented"


  end

module type Memoization =
  sig
    val is_already_saved : bool
    val move_save_location : string (* absolute string filepath *)
    val sym_save_location : string (* absolute string filepath *) 
  end


let rec sum_of_digits ~base = function
  | 0 -> 0
  | x -> (x mod base) + sum_of_digits ~base (x / base)

let rec fac = function
  | 0 -> 1
  | n -> n * fac (n - 1)

(* assume that n! won't exceed int max and 0 <= r <= n *)
let ncr n r =
  fac n / (fac r * fac (n - r))


module Memoize_raw (R : Raw) (M : Memoization) =
  struct
    (*
      The type in a memoized coordinate will be int. This integer
      is exactly the rank of the corresponding coordinate in the
      non-memoized version.
    *)
    module I = Int_coord (struct let n = R.n end)
    include I

    (* Since I.t has sexp, we can say return type is I *)
    module Move_table = Lookup_table.Make2D (I) (Move.Fixed_move) (I)
    module Symmetry_table = Lookup_table.Make2D (I) (Symmetry.S) (I)

    let move_table =
      if M.is_already_saved then Move_table.from_file M.move_save_location else
      let tbl = Move_table.create' (R.all ()) Move.Fixed_move.all ~f:(fun x m -> R.perform_fixed_move x m |> R.to_rank) in
      (* Move_table.to_file tbl M.move_save_location; *)
      tbl

    let sym_table =
      if M.is_already_saved then Symmetry_table.from_file M.sym_save_location else
      let tbl = Symmetry_table.create' (R.all ()) Symmetry.S.all ~f:(fun x s -> R.perform_symmetry x s |> R.to_rank) in
      (* Symmetry_table.to_file tbl M.sym_save_location; *)
      tbl

    (* these don't get called much, and memoizing would take too much space *)
    let to_perm x = x |> R.of_rank |> R.to_perm
    let of_perm p = p |> R.of_perm |> R.to_rank

    let perform_fixed_move = Move_table.lookup     move_table
    let perform_symmetry   = Symmetry_table.lookup sym_table
  end

(* Placeholder definition *)
module Memoize_sym (S : Sym) (_ : Memoization) =
  struct
    include S
  end

module type Raw_base =
  sig
    type t [@@deriving sexp, compare]
    val to_rank : t -> int
    val of_rank : int -> t
    val n : int
    val zero : t
    val next : t -> t option
    val to_perm : t -> Perm.t
    val of_perm : Perm.t -> t
    val all : unit -> t list
  end

(* Performs fixed moves and symmetries once everything else is provided from Raw *)
module Raw_of_calculate_invert (R : Raw_base) : Raw with type t := R.t =
  struct
    include R

    let perform_fixed_move (x : t) (m : Move.Fixed_move.t) : t =
      let m = Move.Fixed_move.to_move m in
      let p = R.to_perm x in
      Perm.perform_move p m |> R.of_perm
      
    let perform_symmetry (x : t) (s : Symmetry.S.t) : t = 
      x
      |> R.to_perm
      |> Symmetry.S.on_perm s
      |> R.of_perm
  end

module Phase1 = 
  struct
    (* Orientation of corners *)
    module Twist : Raw =
      struct
        module T : Raw_base =
          struct
            (*
              There are 8 corners, each with 3 possible orientations, but the last
              corner's orientation can be calculated by the other 7 if the cube is
              well-formed. Therefore, there are 3^7 possible orientations of all 8
              corners. We represent a corner orientation by a unique integer in
              0..2186 (3^7 - 1 = 2186)
            *)
            module I = Int_coord (struct let n = Int.(3 ** 7) end)
            include I

            let corner_facelet_all_rev = List.rev Cubie.Corner_facelet.all
            
            (*
              To invert the corner twists into a cube, we simply twist a few
              corners on a solved cube until they meet the desired orientation.
              Here, we iterate over all the corners that are directly in the
              coordinate, and then we assume the cube is well-formed and derive
              the last orientation because it must make the sum of all
              orientations 0 mod 3. We leave facelets the same; we only twist.
            *)
            let to_perm (x : t) : Perm.t =
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
              go (x * 3 + final_orientation) corner_facelet_all_rev
            
            (*
              Iterate over all corners (except for the last, least significant one),
              and make each one its own digit in a base 3 number.   
            *)
            let of_perm (p : Perm.t) : t =
              let open Cubie.Corner in (* how come we need to open Cubie.Corner to access record field o? *)
              let rec go acc = function
              | [] | [_] -> acc (* ignores least signficant corner *)
              | c :: tl -> go (acc * 3 + Modular_int.Z3.to_int c.o) tl
              in
              go 0 (Perm.to_corners_list p)
          end (* end T *)
        
        include T

        (* compute fixed move and symmetry functions *)
        module R = Raw_of_calculate_invert (T)
        
        include R

      end (* end Twist *)

    (* Orientation of edges *)
    module Flip : Raw =
      struct
        module T : Raw_base =
          struct
            (*
              There are 12 edges to flip. Each has orientation 0 or 1. If the
              cube is well-formed, then the orientation of the last one can be
              determined by the orientation of the other 11, but the other 11 are
              independent of each other.
              So represent a cube's edge orientations as a number in 0..(2^11 - 1).
              Each edge takes up one bit in this number.
            *)
            module I = Int_coord (struct let n = Int.(2 ** 11) end)
            include I

            let edge_facelet_all_rev = List.rev Cubie.Edge_facelet.all
            
            (*
              Like inverting the corner orientation, construct a function that
              twists corners according to each bit. See `Twist.invert` for the
              analogous function on corners.
            *)
            let to_perm (x : t) : Perm.t =
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
              go (x * 2 + final_orientation) edge_facelet_all_rev
              
            (*
              Iterate over all edges (except for the last, least significant one),
              and make each one its own digit in a binary number.
            *)
            let of_perm (p : Perm.t) : t =
              let open Cubie.Edge in
              let rec go acc = function
              | [] | [_] -> acc (* ignores least signficant edge *)
              | e :: tl -> go (acc * 2 + Modular_int.Z2.to_int e.o) tl
              in
              go 0 (Perm.to_edges_list p)
          end (* end T *)

        include T

        (* compute fixed move and symmetry functions *)
        module R = Raw_of_calculate_invert (T)

        include R

      end (* end Flip *)

    module UD_slice : Raw =
      struct
        module T : Raw_base = 
          struct
            (*
              There are 12 total edges, and there are 4 whose positions we care about.
              This leaves 12 choose 4 possible cases. We map each case to a number in
              0..(nCr 12 4 - 1).   
            *)
            module I = Int_coord (struct let n = ncr 12 4 end)
            include I
          
            (*
              Now I have to invert. Start at the largest edges.
              If it's filled, then at most the coordinate is
              nCr 10 2 + nCr 9 2 + ... + nCr 3 2 = 164
              and if it's empty, then at least the coordinate is
              nCr 11 3 = 165
              So at any edge i with 3-k edges found so far, we just
              check if the coordinate is at least nCr i k, and if
              it is, then consider it empty and decrease i.
              Otherwise, fill with some ud_slice edge, decrease k, and
              subtract off nCr i k from the coordinate before continuing.
            *)
            let to_perm (x : t) : Perm.t =
              let open Cubie in
              let is_filled x i k = x < ncr i k in
              let ud_slice = List.map Edge.all_ud_slice_edges ~f:(fun e -> Edge e) in
              let ud_edge  = List.map Edge.all_ud_edges ~f:(fun e -> Edge e) in
              let rec go x i k = function
              | _ when k < 0 -> fun x -> x (* all ud slice found, just leave the rest in place *)
              | [] -> fun x -> x (* logically impossible *)
              | hd :: tl when is_filled x i k -> begin function
                | Edge e when Edge.compare hd e = 0 -> List.nth_exn ud_slice k
                | Corner _ as c -> c
                | e -> e |> go x (i - 1) (k - 1) tl
                end
              | hd :: tl -> begin function (* this space is not filled with ud_slice edge *) 
                | Edge e when Edge.compare hd e = 0 -> List.nth_exn ud_edge (i - k - 1) (* fill with non-ud-slice edge *)
                | Corner _ as c -> c
                | e -> e |> go (x - ncr i k) (i - 1) k tl
                end
              in
              go x 11 3 (List.rev Edge.all)

            
            (*
              Label the spots 0-11. Four are filled with UD slice edges.
              An unfilled slot i takes on value nCr i k where
              k = 3 - #filled spots to right of i.
              Ignore if k is negative.
              Sum these to get the coordinate.
            *)
            let of_perm (p : Perm.t) : t =
              let is_filled e = Cubie.Edge e |> p |> Cubie.is_ud_slice in
              let rec go i k = function
              | _ when k < 0 -> 0
              | [] -> 0
              | hd :: tl when is_filled hd -> go (i - 1) (k - 1) tl
              | hd :: tl -> ncr i k + go (i - 1) k tl
              in
              Cubie.Edge.all
              |> List.rev
              |> go 11 3
          end (* end T *)

        include T

        (* compute fixed move and symmetry functions *)
        module R = Raw_of_calculate_invert (T)
        
        include R

      end (* end UD_slice *)

    module Flip_UD_slice_raw : Raw =
      struct
        module T : Raw_base = 
          struct
            (*
              Flip UD Slice is a combination of the Flip coordinate and
              UD Slice coordinates. We choose to represent it as a record.
            *)
            type t = { ud_slice : UD_slice.t ; flip : Flip.t } [@@deriving compare] (* TODO: test that this is same as to_rank and Int.compare *)
            let to_rank { ud_slice ; flip } = Flip.n * UD_slice.to_rank ud_slice + Flip.to_rank flip
            let of_rank i = { ud_slice = UD_slice.of_rank (i / Flip.n) ; flip = Flip.of_rank (i mod Flip.n) }
          
            (* use the rank for sexp representation *)
            let t_of_sexp s = s |> Int.t_of_sexp |> of_rank
            let sexp_of_t x = x |> to_rank |> Int.sexp_of_t

            let n = Flip.n * UD_slice.n

            let zero = { ud_slice = UD_slice.zero ; flip = Flip.zero }
            let next { ud_slice ; flip } =
              match UD_slice.next ud_slice, Flip.next flip with
              | None, None -> None
              | _ , Some x -> Some { ud_slice ; flip = x }
              | Some x, None -> Some { ud_slice = x ; flip = Flip.zero }

            let all () = 
              let rec loop = function
              | None -> []
              | Some x -> x :: loop (next x)
              in
              loop (Some zero)
            
            let to_perm (x : t) : Perm.t =
              Move.(Flip.to_perm x.flip * UD_slice.to_perm x.ud_slice)
            
            let of_perm (p : Perm.t) : t =
              { ud_slice = UD_slice.of_perm p ; flip = Flip.of_perm p }
          end (* end T *)

        include T

        module R = Raw_of_calculate_invert (T)

        include R
            
      end (* end Flip_UD_slice_raw *)

    module Flip_UD_slice = Sym_of_raw (Flip_UD_slice_raw)

  end (* end Phase1 *)


module Phase2 =
  struct

    (*
      Functor for a permutation coordinate on some cubies.   
    *)
    module Perm_coord (C : sig val all : Cubie.t list end): Raw =
      struct
        module T : Raw_base =
          struct
            let k = List.length C.all
            let all_rev = List.rev C.all

            module I = Int_coord (struct let n = fac k end)
            include I

            let cubie_compare a b =
              let open Cubie in
              match a, b with
              | Corner { c=c1 ; o=_}, Corner { c=c2 ; o=_} -> Corner_facelet.compare c1 c2
              | Edge   { e=e1 ; o=_}, Edge   { e=e2 ; o=_} -> Edge_facelet.compare e1 e2
              | _ -> failwith "cannot compare Corner with Edge"

            (*
              A permutation coordinate is a unique number for each permutation
              of some cubies. It is calculated by counting how "out of order"
              a cubie is in the permutation. We ignore the least significant cubie
              because it's position is determined by the rest.
              definition:
                coord = sum_{i=1}^{k} i! * count(p x > p c_i for all x < c_i)
              So for some cubie c_i, the weight that is put on that term is
                i! * count(p x  > p c_i for all x < c_i)
              i.e. for all cubies that are smaller than c_i, count the number of
              cubies that are replaced by a larger cubie than c_i is replaced by,
              and multiply by i!.
            *)
            let of_perm (p : Perm.t) : t =
              (* count the number of cubies to the left that are mapped to a larger value *)
              let count_inversions c ls =
                let pc = (p c) in
                List.count ls ~f:(fun x -> cubie_compare (p x) pc > 0)
              in
              let rec go acc i = function
              | [] | [_] -> acc (* ignore least significant cubie *)
              | hd :: tl -> go (acc * i + count_inversions hd tl) (i - 1) tl
              in
              go 0 k all_rev

            (*
              See the description above of how the coordinate is calculated. Here is
              how we might invert it:
              We will use the example of all corners.
              The i'th cubie (where URF is 0th, UFL is 1st, etc.) can have at most i
              cubies to the left that are replaced by something greater.
              Consider the most weighted cubie, DRB, which is replaced by YYY. If its
              inversion count (see `calculate`) is y, then there are y cubies to the left
              greater YYY. Since all the cubies are to the left, YYY must have rank k-y
              where k is the greatest rank of any cubie. For corners, k=7.
              Now we move on to the next cubie, DBL, replaced by XXX and count x. So
              it has rank x out of all cubies except YYY.
              Note that sum_{i=1}^{n-1}(i * i!) = n! - 1, so for term of i!, can divide by
              i! to get exactly the rank that concerns us.
              Then for next step, subract off this rank * i!, and repeat.
            *)
            let to_perm (x : t) : Perm.t =
              let rm x = List.filter ~f:(fun a -> cubie_compare x a <> 0) in
              (* 
                Here matching on list of cubies we're mapping, and ls is all possible cubies
                that can be mapped to with the "is replaced by" notation.
              *)
              let rec go x i possible_mappings = function
              | [] -> Fn.id (* leave all remaining cubies in place *)
              | hd :: tl -> let this_cubie = List.nth_exn possible_mappings (x / fac i) in
                begin function
                | c when cubie_compare hd c = 0 -> this_cubie
                | c -> c |> go (x mod fac i) (i - 1) (rm this_cubie possible_mappings) tl
                end
              in go x (k - 1) all_rev all_rev

          end (* end T *)
        
        include T
        module R = Raw_of_calculate_invert (T)
        include R
      end (* end Perm_coord *)

    module Edge_perm = Perm_coord (struct
      let all =
        Cubie.Edge.all_ud_edges
        |> List.map ~f:(fun x -> Cubie.Edge x)
      end)

    module Corner_perm_raw = Perm_coord (struct
      let all =
        Cubie.Corner.all
        |> List.map ~f:(fun x -> Cubie.Corner x)
      end)

    module UD_slice_perm = Perm_coord (struct
      let all =
        Cubie.Edge.all_ud_slice_edges
        |> List.map ~f:(fun x -> Cubie.Edge x)
      end)

    
    module Corner_perm = Sym_of_raw (Corner_perm_raw)

  end (* end Phase2 *)