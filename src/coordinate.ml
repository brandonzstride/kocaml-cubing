
open Core

[@@@ocaml.warning "-27"]

module type Raw =
  sig
    type t
    (* find a representative permutation for the coordinate *)
    val invert : t -> Perm.t
    (* calculate the coordinate for a given permutation *)
    val calculate : Perm.t -> t
    (* calculate the resulting coordinate after applying one of the regular, fixed moves *)
    val perform_fixed_move : t -> Move.Fixed_move.t -> t
    (* Cube P with coordinate x and symmetry S. Gets coordinate of S * P * S^-1 *)
    val perform_symmetry : t -> Symmetry.S.t -> t
    (* Find the next largest coordinate. This is useful for creating lookup tables. *)
    val next : t -> t option
    (* Rank the coordinates as integers *)
    val to_rank : t -> int
    (* The smallest coordinate *)
    val zero : t
    (* How many coordinates there are *)
    val n : int
  end

module type Sym =
  sig
    type t
    val invert : t -> Perm.t
    val calculate : Perm.t -> t
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
  end


module Sym_of_raw (R : Raw) : Sym =
  struct
    (* Placeholder for now *)
    type t = R.t
    let invert = R.invert
    let calculate = R.calculate
    let perform_fixed_move = R.perform_fixed_move
    let perform_symmetry = R.perform_symmetry
    let next_representative = R.next
    let to_eq_class_rank = R.to_rank
    let get_symmetry _ = Symmetry.S.zero
    let zero_representative = R.zero
    let n = R.n
  end

module type Memoization =
  sig
    val is_already_saved : bool
    val save_location : string (* absolute string filepath *)
  end

(* Placeholder definitions *)
module Memoize_raw (R : Raw) (_ : Memoization) =
  struct
    include R
  end
module Memoize_sym (S : Sym) (_ : Memoization) =
  struct
    include S
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

module Int_coord (N : sig val n : int end) =
  struct
    type t = int
    let to_rank = Fn.id
    let n = N.n
    let zero = 0
    let next x = if x = n - 1 then None else Some (x + 1)
  end

module type Raw_base =
  sig
    type t
    val to_rank : t -> int
    val n : int
    val zero : t
    val next : t -> t option
    val invert : t -> Perm.t
    val calculate : Perm.t -> t
  end

(* Performs fixed moves and symmetries once everything else is provided from Raw *)
module Raw_of_calculate_invert (R : Raw_base) : Raw with type t := R.t =
  struct
    include R

    let perform_fixed_move (x : t) (m : Move.Fixed_move.t) : t=
      let m = Move.Fixed_move.to_move m in
      let p = invert x in
      Perm.perform_move p m |> calculate
      
    let perform_symmetry (x : t) (s : Symmetry.S.t) : t = 
      x
      |> invert
      |> Symmetry.S.on_perm s
      |> calculate
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
              go (x * 3 + final_orientation) corner_facelet_all_rev
            
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
              go (x * 2 + final_orientation) edge_facelet_all_rev
              
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
          
            let invert (x : t) : Perm.t =
              failwith "unimplemented"
            
            let calculate (p : Perm.t) : t =
              failwith "unimplemented"
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
            type t = { ud_slice : UD_slice.t ; flip : Flip.t }
            let to_rank { ud_slice ; flip } = Flip.n * UD_slice.to_rank ud_slice + Flip.to_rank flip
            let n = Flip.n * UD_slice.n

            let zero = { ud_slice = UD_slice.zero ; flip = Flip.zero }
            let next { ud_slice ; flip } =
              match UD_slice.next ud_slice, Flip.next flip with
              | None, None -> None
              | _ , Some x -> Some { ud_slice ; flip = x }
              | Some x, None -> Some { ud_slice = x ; flip = Flip.zero }
            
            let invert (x : t) : Perm.t =
              Move.(Flip.invert x.flip * UD_slice.invert x.ud_slice)
            
            let calculate (p : Perm.t) : t =
              { ud_slice = UD_slice.calculate p ; flip = Flip.calculate p }
          end (* end T *)

        include T

        module R = Raw_of_calculate_invert (T)

        include R
            
      end (* end Flip_UD_slice_raw *)

    module Flip_UD_slice = Sym_of_raw (Flip_UD_slice_raw)

  end (* end Phase1 *)


module Phase2 =
  struct

    (* some of my utop tests show that perm coords are not working. *)

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
            let calculate (p : Perm.t) : t =
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
            let invert (x : t) : Perm.t =
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