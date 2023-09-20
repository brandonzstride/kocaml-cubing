
open Core

(* 
  Here are some common functions we need to compute to and from unique
  integers for these coordinates.
*)
let rec sum_of_digits ~base = function
  | 0 -> 0
  | x -> (x mod base) + sum_of_digits ~base (x / base)

let rec fac = function
  | 0 -> 1
  | n -> n * fac (n - 1)

(* assume that n! won't exceed int max and 0 <= r <= n *)
let ncr n r =
  fac n / (fac r * fac (n - r))


module Make (Sym : Symmetry.S) =
  struct
    module type Coordinate =
      sig
        type t [@@deriving sexp, compare]
        val zero : t
        val n : int
        val next : t -> t option
        val to_rank : t -> int
        val of_rank : int -> t
        val to_perm : t -> Perm.t
        val of_perm : Perm.t -> t
        val perform_fixed_move : t -> Move.Fixed_move.t -> t
        val perform_symmetry : t -> Sym.t -> t
        val all : unit -> t list
      end

    module type Sym_coordinate =
      sig
        include Coordinate
        val get_symmetry : t -> Sym.t
      end
    
    module type Sym_base =
      sig
        module Raw : Coordinate
        type t
        val of_raw : Raw.t -> t
        val get_rep : t -> Raw.t
        val get_sym : t -> Sym.t
        val perform_fixed_move : t -> Move.Fixed_move.t -> t
        val perform_symmetry : t -> Sym.t -> t
        val all : unit -> t list (* gets all reprsentatives of eq classes *)
      end
    
    module Sym_base_of_raw (R : Coordinate) =
      struct
        module Raw = R
        type t =
          { rep : R.t     (* the representative raw coordinate of the symmetry class *)
          ; sym : Sym.t } (* the symmetry that converts the raw coord to the rep *)

        let get_rep x = x.rep
        let get_sym x = x.sym
        
        let get_sym_class x =
          List.map Sym.all ~f:(fun s -> R.perform_symmetry x s)

        let argmin compare = 
          function
          | [] -> None
          | a :: ls -> begin
            let rec find m mi i = function
            | [] -> Some (m, mi)
            | hd :: tl when compare hd m < 0 -> find hd i (i + 1) tl
            | _  :: tl -> find m mi (i + 1) tl
            in
            find a 0 1 ls
          end

        let of_raw (r : R.t) : t =
          let rep, sym_i = 
            r
            |> get_sym_class
            |> argmin R.compare
            |> Option.value_exn
          in
          { rep ; sym = Sym.of_rank sym_i }
        
        (*
          Suppose x <=> perm P. We want P * m.
          Since x has symmetry S to rep raw coord R, we know
          S * P * S^-1 = R.
          Then P * m = S^-1 * R * S * m
            = S^-1 * R * (S * m * S^-1) * S
            = S^-1 * (R * m') * S
          Suppose R * m' = R', some raw coordinate. R' has rep
          raw coord R'' with symmetry S', i.e.
            S' * R' * S'^-1 = R''
            <=> R' = S'^-1 * R'' * S'
            <=> R * m' = S'^-1 * R'' * S'
          Then
            P * m = S^-1 * (S'^-1 * R'' * S') * S
              = (S' * S)^-1 * R'' * (S' * S)
          So the resulting raw coord is R'' and the resulting
          symmetry is S' * S.
        *)
        let perform_fixed_move (x : t) (m : Move.Fixed_move.t) : t =
          let m' = Sym.on_move x.sym m in
          let y = R.perform_fixed_move x.rep m' |> of_raw in
          { rep = y.rep ; sym = Sym.mult y.sym x.sym }
          
        let perform_symmetry (x : t) (s : Sym.t) : t =
          { x with sym = Sym.mult x.sym s }

        (* very computationally expensive to get all reps *)
        let all () =
          let module Rset = Set.Make (R) in
          let get_sym_class x = get_sym_class x |> Rset.of_list
          in
          let rec loop reps is_done = function
          | None -> assert (Set.length is_done = R.n); reps
          | Some x when Set.mem is_done x -> loop reps is_done (R.next x)
          | Some x ->
            let sym_class = get_sym_class x in (* nonempty because includes x *)
            let rep = Set.min_elt_exn sym_class in
            let is_done = Set.fold sym_class ~init:is_done ~f:Set.add in
            loop (Set.add reps rep) is_done (R.next x)
          in
          (Some R.zero)
          |> loop Rset.empty Rset.empty
          |> Set.to_list
          |> List.map ~f:(fun rep -> { rep ; sym = Sym.zero })
      end

    (*
      A coordinate that has an integer type can be defined entirely
      from a max value `n` and conversions functions to/from Perm.t.
      This module type is the argument to Make_int_coord below.
    *)
    module type Int_coord =
      sig
        val n : int
        (* `of_perm` only valid on positive inputs less than `n` *)
        val of_perm : Perm.t -> int
        (* `to_perm` only valid on positive inputs less than `n` *)
        val to_perm : int -> Perm.t
      end

    (*
      Creates a coordinate given a max value `n` and functions
      to convert to and from permutations.   
    *)
    module Make_int_coord (I : Int_coord) : Coordinate =
      struct
        include I
        type t = int [@@deriving sexp, compare]
        let to_rank = Fn.id
        let of_rank = Fn.id
        let zero = 0
        let next x = if x = n - 1 then None else Some (x + 1)
        let all () = List.init n ~f:Fn.id
        let perform_fixed_move (x : t) (m : Move.Fixed_move.t) : t =
          m
          |> Move.Fixed_move.to_move
          |> Perm.perform_move (I.to_perm x)
          |> I.of_perm
        let perform_symmetry (x : t) (s : Sym.t) : t = 
          x
          |> I.to_perm
          |> Sym.on_perm s
          |> I.of_perm
      end

   module Phase1 =
    struct
      (*
        The Twist coordinate describes the orientation of the eight corners.
        
        There are eight corners, each with three possible orientations, but
        the last corner's orientation can be calculated by the other seven if
        the cube is well-formed. Therefore, there are 3^7 possible orientations
        of all eight corners.

        We represent a corner orientation by a unique integer in 0..(3^7 - 1).
        We treat it as a base-3 number in these calculations where each corner
        gets its own digit in the number.
      *)
      module Twist = Make_int_coord (
        struct
          let n = Int.(3 ** 7)

          (*
            Iterate over all corners (except for the last, least significant one),
            and give each one its own digit in a base 3 number.
          *)
          let of_perm (p : Perm.t) : int =
            let open Cubie.Corner in
            let rec go acc = function
            | [] | [_] -> acc (* ignores least signficant corner *)
            | c :: tl -> go (acc * 3 + Modular_int.Z3.to_int c.o) tl
            in
            go 0 (Perm.to_corners_list p)

          (*
            To invert the corner twists into a cube, we can leave all cubies in
            place and give orientations to the corners. We can modulo by 3 to get
            the least significant digit in the base 3 number, and apply that twist
            to the corner.
          *)
          let to_perm (x : int) : Perm.t =
            let open Cubie in
            let rec go x = function
            | [] -> failwith "impossible if cube permutation is well-formed"
            | hd :: tl -> begin function
              | Corner c when Cubie.Corner_facelet.compare hd c.c = 0 -> Corner {c = c.c ; o = Modular_int.Z3.of_int x }
              | c -> (go (x / 3) tl) c
              end
            in
            let new_coord = (* deduce orientation of the final cubie and add to coord *)
              x
              |> sum_of_digits ~base:3
              |> Modular_int.Z3.of_int
              |> Modular_int.Z3.inverse
              |> Modular_int.Z3.to_int
              |> (+) (x * 3)
            in
            function (* the return type is a function. Capture edges first and pipe corners through *)
            | Edge   _ as e -> e (* let edges be untouched *)
            | Corner _ as c -> go new_coord (List.rev Cubie.Corner_facelet.all) c 
      
        end
      ) 

      (*
        Flip describes the orientation of the edges. It is identical to Twist
        but for edges instead. Refer to the explanations of `Twist` above to
        understand this coordinate.
        
        Therefore, it is an integer in 0..(2^11 - 1) and is treated as a base-2
        number.
      *)
      module Flip = Make_int_coord (
        struct
          let n = Int.(2 ** 11)

          let of_perm (p : Perm.t) : int =
            let open Cubie.Edge in
            let rec go acc = function
            | [] | [_] -> acc (* ignores least signficant edge *)
            | e :: tl -> go (acc * 2 + Modular_int.Z2.to_int e.o) tl
            in
            go 0 (Perm.to_edges_list p)

          let to_perm (x : int) : Perm.t =
            let open Cubie in
            let rec go x = function
            | [] -> failwith "impossible if cube permutation is well-formed"
            | hd :: tl -> begin function
              | Edge e when Cubie.Edge_facelet.compare hd e.e = 0 -> Edge {e = e.e ; o = Modular_int.Z2.of_int x }
              | e -> (go (x / 2) tl) e
              end
            in
            let new_coord = (* deduce orientation of the final cubie and add to coord *)
              x
              |> sum_of_digits ~base:2
              |> Modular_int.Z2.of_int
              |> Modular_int.Z2.inverse
              |> Modular_int.Z2.to_int
              |> (+) (x * 2)
            in
            function (* the return type is a function. Capture corners first and pipe edges through *)
            | Corner _ as c -> c (* let corners be untouched *)
            | Edge   _ as e -> go new_coord (List.rev Cubie.Edge_facelet.all) e 
        end
      ) 

      (*
        The UD_slice coordinates describes the positions of the edge cubies
        that, in the solved state, exist in the slice between the U and D
        faces--the UD slice. There are four edges in the UD slice, and there
        are twelve edges total, so there are 12 choose 4 possible configurations
        if we ignore the relative order of the edges.

        We map each case to a number in 0..(nCr 12 4 - 1)
      *)
      module UD_slice = Make_int_coord (
        struct
          let n = ncr 12 4
          
          (*
            Label the edge spots 0..11. Four are filled with UD slice edges.
            Sum the values in each spot. The values are calculated as follows.
            Consider if a spot is filled with a UD slice edge:
              * If yes, then it does not contribute to the sum.
              * If no, then it takes on value nCr i k where i is the index
                in 0..11 and k is 3 - #filled spots to the right of i. If k
                is negative, then it takes on value 0 instead.
            This assigns a unique integer to each case. It becomes more clear how
            this is unique when we invert the coordinate in `to_perm`.
          *)
          let of_perm (p : Perm.t) : int =
            let is_filled e = Cubie.Edge e |> p |> Cubie.is_ud_slice in
            let rec go i k = function
            | _ when k < 0 -> 0
            | [] -> 0
            | hd :: tl when is_filled hd -> go (i - 1) (k - 1) tl
            | _ :: tl -> ncr i k + go (i - 1) k tl
            in
            Cubie.Edge.all
            |> List.rev
            |> go 11 3
          
          (*
            This will be definition by example.
            Consider the largest edge (spot 11).
            If this spot is filled, then at most the coordinate is
              nCr 10 2 + nCr 9 2 + ... + nCr 3 2 = 164
            If it is empty, then the coordinate is at least
              nCr 11 3 = 165
            We see that at any edge i with 3-k UD slice edges found
            so far, we can check if the coordinate is at least nCr i k.
              * If yes, then the spot is not filled with a UD slice edge.
              * If no, then the spot is filled with some UD slice edge,
                and we decrease k and subtract nCr i k off the coordinate
                before continuing.
          *)
          let to_perm (x : int) : Perm.t =
            let open Cubie in
            let is_filled x i k = x < ncr i k in
            let ud_slice = List.map Edge.all_ud_slice_edges ~f:(fun e -> Edge e) in
            let ud_edge  = List.map Edge.all_ud_edges ~f:(fun e -> Edge e) in
            let rec go x i k = function
            | _ when k < 0 -> fun x -> x (* all ud slice found, just leave the rest in place *)
            | [] -> fun x -> x (* logically impossible *)
            | hd :: tl when is_filled x i k -> begin function
              | Edge e when Edge.compare hd e = 0 -> List.nth_exn ud_slice k
              | e -> go x (i - 1) (k - 1) tl e
              end
            | hd :: tl -> begin function (* this space is not filled with ud_slice edge *) 
              | Edge e when Edge.compare hd e = 0 -> List.nth_exn ud_edge (i - k - 1) (* fill with non-ud-slice edge *)
              | e -> go (x - ncr i k) (i - 1) k tl e
              end
            in
            function (* return type is function *)
            | Corner _ as c -> c (* leave corners untouched *)
            | Edge   _ as e -> go x 11 3 (List.rev Edge.all) e

        end
      ) 

      (*
        The Flip UD Slice coordinate is a combinate of the Flip and
        UD Slice coordinates.
        
        It is naturally represented as a tuple or record, and this is
        mapped one-to-one with an integer to make an Int_coord like the
        rest of the coordinates.

        The integer representation is the rank of the record.
      *)
      module Flip_UD_slice_raw = Make_int_coord (
        struct
          (*
            module T will be our natural representation of the coordinate,
            and we use it to create the `to_perm` and `of_perm` functions
            for integers.
          *)
          module T =
            struct
              type t =
                { ud_slice : UD_slice.t
                ; flip     : Flip.t }
              
              let to_rank { ud_slice ; flip } =
                Flip.n * UD_slice.to_rank ud_slice + Flip.to_rank flip

              let of_rank i =
                { ud_slice = UD_slice.of_rank (i / Flip.n)
                ; flip     = Flip.of_rank (i mod Flip.n) }
            end

          let n = UD_slice.n * Flip.n
          
          (*
            `Flip.to_perm` preserves permutation but changes orientation,
            and `UD_slice.to_perm` preserves orientation but changes
            permutation, so we can combine them by simply composing via
            Move multiplication.   
          *)
          let to_perm (x : int) : Perm.t =
            let y = T.of_rank x in
            Move.(Flip.to_perm y.flip * UD_slice.to_perm y.ud_slice)

          let of_perm (p : Perm.t) : int =
            T.{ ud_slice = UD_slice.of_perm p ; flip = Flip.of_perm p }
            |> T.to_rank
        end
      ) 

      module Flip_UD_slice = Sym_base_of_raw (Flip_UD_slice_raw)

    end (* end Phase1 *)

  module Phase2 =
    struct

      (*
        The Perm_coord module is a functor for a permutation coordinate on
        some cubies.
        
        A permutation coordinate on n cubies is an integer in 0..(n!-1) because
        there are n! ways to permute the cubies. It is calculated by counting
        how "out of order" a cubie is in the permutation. We ignore the least
        significant cubie because its position is determined by the rest.

        Here is the definition of the permutation coordinate:
        We take a sum of values for each cubie.
        For some cubie c_i, the value of the i'th term is
          i! * count(p x > p c_i for all x < c_i)
        i.e. for all cubies that are smaller than c_i, count the number of
        cubies that are replace by a larger cubie than c_i is replaced by, and
        weight it by i!.

        Notice that at most, the "count" in the term is i, and
          sum_{i=1}^{n-1} (i * i!) = n! - 1
        So this properly gives a number in 0..(n!-1).

        Assumptions:
          C.all is entirely edges or entirely corners.
      *)
      module Perm_coord (C : sig val all : Cubie.t list end) = Make_int_coord (
        struct
          let k = List.length C.all
          let all_rev = List.rev C.all

          let n = fac k

          (* We need a way to determine order in the permutation *)
          let cubie_compare a b =
            let open Cubie in
            match a, b with
            | Corner { c=c1 ; o=_}, Corner { c=c2 ; o=_} -> Corner_facelet.compare c1 c2
            | Edge   { e=e1 ; o=_}, Edge   { e=e2 ; o=_} -> Edge_facelet.compare e1 e2
            | _ -> failwith "cannot compare Corner with Edge"

          let of_perm (p : Perm.t) : int =
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
            TODO: clean up this comment
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
          let to_perm (x : int) : Perm.t =
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
        end
      )

      module Edge_perm = Perm_coord (
        struct
          let all =
            Cubie.Edge.all_ud_edges
            |> List.map ~f:(fun x -> Cubie.Edge x)
        end
      )

      module Corner_perm_raw = Perm_coord (
        struct
          let all =
            Cubie.Corner.all
            |> List.map ~f:(fun x -> Cubie.Corner x)
        end
      )

      module UD_slice_perm = Perm_coord (
        struct
          let all =
            Cubie.Edge.all_ud_slice_edges
            |> List.map ~f:(fun x -> Cubie.Edge x)
        end
      ) 

      module Corner_perm = Sym_base_of_raw (Corner_perm_raw)

    end (* end Phase2 *)

  module type Memo_params =
    sig
      val status : [> `Is_already_saved | `Computation_needed]
      val move_save_location : string 
      val symmetry_save_location : string 
    end

  module type Sym_memo_params =
    sig
      include Memo_params (* symmetry_save_location not actually needed *)
      val class_to_rep_location : string
      val rep_to_class_location : string
    end


  module Make_memoized (R : Coordinate) (M : Memo_params) : Coordinate =
    struct
      (*
        The type in a memoized coordinate will be int. This integer
        is exactly the rank of the corresponding coordinate in the
        non-memoized version.
      *)
      module I =
        struct
          type t = int [@@deriving sexp, compare]
          let to_rank = Fn.id
          let n = R.n 
        end
      include I

      let of_rank = Fn.id
      let zero = 0
      let next x = if x = n - 1 then None else Some (x + 1)
      let all () = List.init n ~f:Fn.id

      (* Since I.t has sexp, we can say return type is I *)
      module Move_table = Lookup_table.Make2D (I) (Move.Fixed_move) (I)
      module Symmetry_table = Lookup_table.Make2D (I) (Sym) (I)

      let move_table =
        match M.status with
        | `Is_already_saved -> Move_table.from_file M.move_save_location
        | `Computation_needed ->
          let tbl = Move_table.create' (R.all ()) Move.Fixed_move.all ~f:(fun x m -> R.perform_fixed_move x m |> R.to_rank) in
          (* Move_table.to_file tbl M.move_save_location; *)
          tbl

      let sym_table =
        match M.status with
        | `Is_already_saved -> Symmetry_table.from_file M.symmetry_save_location
        | `Computation_needed ->
          let tbl = Symmetry_table.create' (R.all ()) Sym.all ~f:(fun x s -> R.perform_symmetry x s |> R.to_rank) in
          (* Symmetry_table.to_file tbl M.sym_save_location; *)
          tbl

      (* these don't get called much, and memoizing would take too much space *)
      let to_perm x = x |> R.of_rank |> R.to_perm
      let of_perm p = p |> R.of_perm |> R.to_rank

      let perform_fixed_move = Move_table.lookup     move_table
      let perform_symmetry   = Symmetry_table.lookup sym_table
    end

    module Make_memoized_sym (S : Sym_base) (M : Sym_memo_params) =
      struct
        
        module Raw_table =
          Lookup_table.Make1D
            (struct type t = int let to_rank = Fn.id end)
            (S.Raw)
        
        module Raw_map = Map.Make (S.Raw)

        (* Need to get representative coordinates--save location *)
        let class_to_rep_table : Raw_table.t =
          match M.status with
          | `Is_already_saved -> Raw_table.from_file M.class_to_rep_location
          | `Computation_needed ->
            S.all ()
            |> List.map ~f:S.get_rep
            |> List.sort ~compare:S.Raw.compare
            |> Raw_table.create

        (*
          Externally, it should appear as if there are only as many symmetry
          coordinates as there are equivalence classes.
          Internally, this will be larger by a multiple of Sym.n
        *)
        module I =
          struct
            type t = int [@@deriving sexp, compare]
            let to_rank x = x / Sym.n (* gets rank of representative *)
            let n = Raw_table.get_n class_to_rep_table
          end

        include I
        
        (* A table is not compatible because it's not easy to get rep rank *)
        let rep_to_class_map : int Raw_map.t =
          match M.status with
          | `Is_already_saved -> M.rep_to_class_location |> Sexp.load_sexp |> Raw_map.t_of_sexp Int.t_of_sexp
          | `Computation_needed ->
              let f = Raw_table.lookup class_to_rep_table in
              let rec go i map =
                if i = n then map
                else Map.add_exn map ~key:(f i) ~data:i |> go (i + 1)
              in
              go 0 Raw_map.empty

        let of_rank i = i * Sym.n (* gets representative sym coordinate from rank *)
        let zero = 0
        let next x = let y = to_rank x in if y = n - 1 then None else Some (y + 1)
        let all () = List.init n ~f:of_rank (* gets only representatives sym coords *)

        let get_rep_raw_coord (x : t) : S.Raw.t =
          Raw_table.lookup class_to_rep_table (to_rank x)

        let get_symmetry (x : t) : Sym.t =
          x mod Sym.n |> Sym.of_rank

        let of_base (x : S.t) : t =
          let class_index = Map.find_exn rep_to_class_map (S.get_rep x) in
          let sym_rank = Sym.to_rank (S.get_sym x) in
          class_index * Sym.n + sym_rank

        module Move_table = Lookup_table.Make2D (I) (Move.Fixed_move) (I)

        let move_table =
          match M.status with
          | `Is_already_saved -> Move_table.from_file M.move_save_location
          | `Computation_needed ->
            let f x m =
              x
              |> get_rep_raw_coord
              |> S.of_raw
              |> Fn.flip S.perform_fixed_move m
              |> of_base
            in
            let tbl = Move_table.create' (all ()) Move.Fixed_move.all ~f in
            (* Move_table.to_file tbl M.move_save_location; *)
            tbl

        (*
          This repeats some logic from Sym_base.
        *)
        let perform_fixed_move (x : t) (m : Move.Fixed_move.t) : t =
          let s1 = get_symmetry x in
          let m' = Sym.on_move s1 m in
          let y = Move_table.lookup move_table x m' in (* resulting sym coord in rep sym coord for x *)
          let s2 = get_symmetry y in  
          (y |> to_rank |> of_rank) + (Sym.mult s2 s1 |> Sym.to_rank)

        let perform_symmetry (x : t) (s : Sym.t) : t =
          (* convert to rep coord first by applying x's symmetry, then do s *)
          (* this code is really similar to above. Can I logically connect? *)
          (x |> to_rank |> of_rank) + (Sym.mult s (get_symmetry x) |> Sym.to_rank)

        (*
          x <=> P   
          R = S * P * S^-1
          P = S^-1 * R * S
        *)
        let to_perm (x : t) : Perm.t =
          let s = 
            x
            |> get_symmetry
            |> Sym.inverse
          in
          x
          |> get_rep_raw_coord
          |> S.Raw.to_perm
          |> Sym.on_perm s

        let of_perm (p : Perm.t) : t =
          p
          |> S.Raw.of_perm
          |> S.of_raw
          |> of_base

      end

  end