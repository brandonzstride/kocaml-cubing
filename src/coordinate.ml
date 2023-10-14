(*
  File: coordinate.ml   

  How to approach this file:
    Note a few things:
    * The necessary module types are first copied from coordinate.mli.
    * Some functors are defined that will help create the modules to
      eventually satisfy the mli.
    * The coordinates are described simply as conversions to and from
      permutations, and the above functors fill in the rest.

    Memoized coordinates:
    Before any coordinates are defined, I can make a functor to memoize
    a module of type T. This is trivial and requires only creating lookup
    tables.

    Symmetry coordinates:
    I create a Sym_base module to act as an intermediate layer between
    a raw coordinate and a symmetry coordinate. It helps ease the transition
    between the two by providing partial functionality and a similar representation
    to the full symmetry coordinate. Then, I use this to appropriately implement
    the Make_symmetry_coordinate functor.

    Raw coordinates:
    Read the comments above each coordinate for its definition. Another comment
    above each function attempts to explain the process because the code might
    be difficult to jump into due to the nature of the definitions.
*)


open Core

exception LogicallyImpossible of string

module type T =
  sig
    module Fixed_move : Move.Fixed.S
    type t [@@deriving sexp, compare]
    val zero : t
    val n : int
    val next : t -> t option
    val to_rank : t -> int
    val of_rank : int -> t
    val to_perm : t -> Perm.t
    val of_perm : Perm.t -> t
    val perform_fixed_move : t -> Fixed_move.t -> t
    val perform_symmetry : t -> Symmetry.t -> t
    val get_symmetry : t -> Symmetry.t
    val all : unit -> t list
  end

module type Memo_params =
  sig
    val status : [> `Is_saved | `Needs_computation ]
    val move_filepath : string option
    val symmetry_filepath : string option
  end

module type Sym_memo_params =
  sig
    val status : [> `Is_saved | `Needs_computation ]
    val move_filepath : string option
    val class_to_rep_filepath : string option
    val rep_to_class_filepath : string option
  end

module type Coordinate =
  sig
    module Fixed_move : Move.Fixed.S
    module type T = T with module Fixed_move = Fixed_move

    module Raw : T
    module Make_memoized_coordinate (_ : Memo_params) : T
    module Make_symmetry_coordinate (_ : Sym_memo_params) : T
  end

module type Phase1Coordinate = Coordinate with module Fixed_move = Move.Fixed.G
module type Phase2Coordinate = Coordinate with module Fixed_move = Move.Fixed.G1

(* begin implementation *)

(* 
  Here are some common functions we need to compute to and from unique
  integers for these coordinates.
*)
let rec sum_of_digits ~base = function
  | 0 -> 0
  | x -> (x mod base) + sum_of_digits ~base (x / base)

let rec fac = function
  | 0 -> 1
  | n when n < 0 -> failwith "factorial not defined on negative numbers"
  | n -> n * fac (n - 1)

(* assume that n! won't exceed int max and 0 <= r <= n *)
let ncr n r =
  fac n / (fac r * fac (n - r))


(*
  Here is a functor to memoize a given coordinate.

  Create lookup tables to save the results of moves and symmetries
  on each possible coordinate.

  It's assumed that the coordinate is not so extremely large that
  these lookup tables are unfeasable.
*)
module Make_memoized_coordinate
    (Raw : T)
    (M : Memo_params)
    : T with module Fixed_move = Raw.Fixed_move =
  struct
    module Fixed_move = Raw.Fixed_move
    (*
      The type in a memoized coordinate will be int. This integer
      is exactly the rank of the corresponding coordinate in the
      non-memoized version.
    *)
    module I =
      struct
        type t = int [@@deriving sexp, compare]
        let to_rank = Fn.id
        let n = Raw.n
      end
    include I

    let of_rank = Fn.id
    let zero = 0
    let next x = if x = n - 1 then None else Some (x + 1)
    let all () = List.init n ~f:Fn.id

    (* Since I.t has sexp, we can say return type is I *)
    module Move_table = Lookup_table.Make2D (I) (Fixed_move.Generator) (I)
    module Symmetry_table = Lookup_table.Make2D (I) (Symmetry) (I)

    let move_table =
      match M.status with
      | `Is_saved -> begin
        match M.move_filepath with
        | Some s -> Move_table.from_file s
        | None -> failwith "filepath expected but not given for memoized move table"
        end
      | `Needs_computation ->
        let time = Caml_unix.gettimeofday () in
        (* Use generator with count 1 to make a fixed_move *)
        let f = fun x gen -> Raw.perform_fixed_move (Raw.of_rank x) (Fixed_move.of_gen gen) |> Raw.to_rank in
        let tbl = Move_table.create (all ()) Fixed_move.Generator.all ~f:f in
        (match M.move_filepath with None -> () | Some s -> Move_table.to_file tbl s);
        Printf.printf "Computed move table of size %d in time %fs\n" (Raw.n * List.length Fixed_move.Generator.all) (Caml_unix.gettimeofday() -. time);
        tbl

    let sym_table =
      match M.status with
      | `Is_saved -> begin 
        match M.symmetry_filepath with
        | Some s -> Symmetry_table.from_file s
        | None -> failwith "filepath expected but not given for memoized symmetry table"
        end
      | `Needs_computation ->
        let time = Caml_unix.gettimeofday () in
        let f = fun x s -> Raw.perform_symmetry (Raw.of_rank x) s |> Raw.to_rank in
        let tbl = Symmetry_table.create (all ()) Symmetry.all ~f:f in
        (match M.symmetry_filepath with None -> () | Some s -> Symmetry_table.to_file tbl s);
        Printf.printf "Computed symmetry table of size %d in time %fs\n" (Raw.n * Symmetry.n) (Caml_unix.gettimeofday() -. time);
        tbl

    (* these don't get called much, and memoizing would take too much space *)
    let to_perm x = x |> Raw.of_rank |> Raw.to_perm
    let of_perm p = p |> Raw.of_perm |> Raw.to_rank

    let perform_fixed_move (x : t) (m : Fixed_move.t) : t =
      let gen, count = Fixed_move.to_generator_and_count m in
      Fn.apply_n_times ~n:count (fun x -> Move_table.lookup move_table x gen) x
      
    let perform_symmetry = Symmetry_table.lookup sym_table

    let get_symmetry _ = Symmetry.id

  end

(*
  It's helpful to me to have an intermediate step between a raw coordinate
  and a symmetry coordinate. I let this be a Sym_base module. It can behave
  like a symmetry coordinate but without any of the expensive operations.   
*)
module type Sym_base =
  sig
    module Raw : T
    type t
    val of_raw : Raw.t -> t
    val get_rep : t -> Raw.t
    val get_sym : t -> Symmetry.t
    val perform_fixed_move : t -> Raw.Fixed_move.t -> t
    val perform_symmetry : t -> Symmetry.t -> t
    val all : unit -> t list (* gets all reprsentatives of eq classes *)
  end

module Sym_base_of_raw (T : T) : Sym_base with module Raw = T =
  struct
    module Raw = T
    (*
      In the Sym_base, we have an intermediate symmetry coordinate.
      It is not a full-blown symmetry coordinate as Kociemba describes.
      The representative is kept as a raw coordinate, and the symmetry
      is not yet stored as rank.
    *)
    type t =
      { rep : Raw.t     (* the representative raw coordinate of the symmetry class *)
      ; sym : Symmetry.t } (* the symmetry that converts the raw coord to the rep *)

    let get_rep x = x.rep
    let get_sym x = x.sym
    
    let get_sym_class x =
      List.map Symmetry.all ~f:(fun s -> Raw.perform_symmetry x s)

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

    let of_raw (r : Raw.t) : t =
      let rep, sym_i = 
        r
        |> get_sym_class
        |> argmin Raw.compare (* find smallest raw coordinate in class and its symmetry index *)
        |> Option.value_exn
      in
      { rep ; sym = Symmetry.of_rank sym_i }
    
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
    let perform_fixed_move (x : t) (m : Raw.Fixed_move.t) : t =
      let m' =
        m
        |> Raw.Fixed_move.to_super_t 
        |> Symmetry.on_fixed_move x.sym (* symmetries can act on this type *)
        |> Raw.Fixed_move.of_super_t (* convert back to a fixed move that Raw knows how to operate on *)
      in
      let y = Raw.perform_fixed_move x.rep m' |> of_raw in
      { rep = y.rep ; sym = Symmetry.mult y.sym x.sym }
      
    (*
      To perform a symmetry s, we need to consider the new symmetry
      it will take to transform to the representative.
      Currently, 
        R = S * P * S^-1
      and we want to apply s to P and find the symmetry needed to take
      s * P * s^-1 to the representative R.
      We have
        (S * s^-1) * s * P * s^-1 * (S * s^-1)^-1 = R
      So the new symmetry of this coordinate is
        S * s^-1
    *)
    let perform_symmetry (x : t) (s : Symmetry.t) : t =
      { x with sym = Symmetry.mult x.sym (Symmetry.inverse s) }

    (* very computationally expensive to get all reps *)
    let all () =
      let module Rset = Set.Make (Raw) in
      let get_sym_class x = get_sym_class x |> Rset.of_list
      in
      let rec loop reps is_done = function
      | None -> assert (Set.length is_done = Raw.n); reps
      | Some x when Set.mem is_done x -> loop reps is_done (Raw.next x)
      | Some x ->
        let sym_class = get_sym_class x in (* nonempty because includes x *)
        let rep = Set.min_elt_exn sym_class in
        let is_done = Set.fold sym_class ~init:is_done ~f:Set.add in
        loop (Set.add reps rep) is_done (Raw.next x)
      in
      (Some Raw.zero)
      |> loop Rset.empty Rset.empty
      |> Set.to_list
      |> List.map ~f:(fun rep -> { rep ; sym = Symmetry.id })
  end

(*
  Now I'll use the Sym_base to create a full-blown symmetry coordinate.
  This will be expensive to compute because it needs to call Sym_base.all.
*)
module Make_symmetry_coordinate
    (S : Sym_base)
    (M : Sym_memo_params)
    : T with module Fixed_move = S.Raw.Fixed_move =
  struct

    module Fixed_move = S.Raw.Fixed_move
    
    module Raw_table =
      Lookup_table.Make1D
        (struct type t = int let to_rank = Fn.id end)
        (S.Raw)
    
    module Raw_map = Map.Make (S.Raw)

    (*
      Maps a symmetry class index to a representative raw coordinate.   
    *)
    let class_to_rep_table : Raw_table.t =
      match M.status with
      | `Is_saved -> begin
        match M.class_to_rep_filepath with
        | Some s -> Raw_table.from_file s
        | None -> failwith "filepath expected but not given for symmetry class to rep table"
        end
      | `Needs_computation ->
        let time = Caml_unix.gettimeofday () in
        let tbl =
          S.all ()
          |> List.map ~f:S.get_rep
          |> List.sort ~compare:S.Raw.compare
          |> Raw_table.of_list
        in
        (match M.class_to_rep_filepath with None -> () | Some s -> Raw_table.to_file tbl s);
        Printf.printf "Computed class to rep table of size %d in time %fs\n" (Raw_table.get_n tbl) (Caml_unix.gettimeofday() -. time);
        tbl

    (*
      Externally, it should appear as if there are only as many symmetry
      coordinates as there are equivalence classes.
      Internally, this will be larger by a multiple of Sym.n
    *)
    module I =
      struct
        type t = int [@@deriving sexp, compare]
        let to_rank x = x / Symmetry.n (* gets rank of representative *)
        let n = Raw_table.get_n class_to_rep_table
      end

    include I
    
    (*
      Maps raw representative to index of equivalence class. A lookup table
      is not compatible because we need the rank of the representative among
      the other representatives, which would be slow to compute for each rep.
    *)
    let rep_to_class_map : int Raw_map.t =
      match M.status with
      | `Is_saved -> begin
        match M.rep_to_class_filepath with
        | Some s -> s |> Sexp.load_sexp |> Raw_map.t_of_sexp Int.t_of_sexp
        | None -> failwith "filepath expected but not given for symmetry rep to class map"
        end
      | `Needs_computation ->
        let time = Caml_unix.gettimeofday () in
        let map =
          let f = Raw_table.lookup class_to_rep_table in
          let rec go i map =
            if i = n then map
            else Map.add_exn map ~key:(f i) ~data:i |> go (i + 1)
          in
          go 0 Raw_map.empty
        in
        (match M.rep_to_class_filepath with None -> () | Some s -> Sexp.save s (Raw_map.sexp_of_t Int.sexp_of_t map));
        Printf.printf "Computed rep to class map of size %d in time %fs\n" I.n (Caml_unix.gettimeofday() -. time);
        map

    let of_rank = Int.( * ) Symmetry.n (* gets representative sym coordinate from rank *)
    let zero = 0
    let next x = let y = to_rank x in if y = n - 1 then None else Some (y + 1)
    let all () = List.init n ~f:of_rank (* gets only representatives sym coords *)

    (* let is_rep x = x mod Symmetry.n = 0 *)
    let get_rep x = x |> to_rank |> of_rank

    let get_rep_raw_coord (x : t) : S.Raw.t =
      Raw_table.lookup class_to_rep_table (to_rank x)

    let get_symmetry (x : t) : Symmetry.t =
      x mod Symmetry.n |> Symmetry.of_rank

    let of_base (x : S.t) : t =
      let class_index = Map.find_exn rep_to_class_map (S.get_rep x) in
      let sym_rank = Symmetry.to_rank (S.get_sym x) in
      class_index * Symmetry.n + sym_rank

    (*
      Unfortunately, it's not enough to memoize the result of a move generator
      because the result might have some symmetry wrt to the representative, so
      to apply a generator n times, we would need to then apply a new symmetry
      to the generator in order to finish applying it (n - 1) times.
      However, this new move is not necessarily another generator, so we
      could theoretically enter an infinite loop, unless I can prove that the math
      requires quick termination.
      Even if the loop is not infinite, it could be long, and it feels safer to just
      memoize the result of all moves to ensure quick termination at the cost of
      extra space.
      Without the reflection, it does seem that all generators under symmetries are
      new generators, so this would yield a nice improvement in space requirement,
      but since I plan to someday use reflections, it's not smart to only memoize
      the generators.
    *)
    module Move_table = Lookup_table.Make2D (I) (Fixed_move) (I)

    (* This would just be generator, which sym_base can handle just fine
       if converted to a fixed move  *)
    let move_table =
      match M.status with
      | `Is_saved -> begin 
        match M.move_filepath with
        | Some s -> Move_table.from_file s
        | None -> failwith "filepath expected but not given for symmetry move table"
        end
      | `Needs_computation ->
        let time = Caml_unix.gettimeofday () in
        let f x m =
          x
          |> get_rep_raw_coord
          |> S.of_raw (* sym_base will help us operate on a raw representative of the class *)
          |> Fn.flip S.perform_fixed_move m 
          |> of_base (* convert back to full symmetry coordinate from sym_base *)
        in
        let tbl = Move_table.create (all ()) Fixed_move.all ~f in
        (match M.move_filepath with None -> () | Some s -> Move_table.to_file tbl s);
        Printf.printf "Computed move table of size %d in time %fs\n" (I.n * Fixed_move.n) (Caml_unix.gettimeofday() -. time);
        tbl

    (*
      This repeats some logic from Sym_base.
    *)
    let perform_fixed_move (x : t) (m : Fixed_move.t) : t =
      let s1 = get_symmetry x in
      let m' =
        m
        |> Fixed_move.to_super_t
        |> Symmetry.on_fixed_move s1
        |> Fixed_move.of_super_t (* is logically save to convert back *)
      in
      let y = Move_table.lookup move_table x m' in (* resulting sym coord in rep sym coord for x *)
      let s2 = get_symmetry y in  
      (get_rep y) + (Symmetry.mult s2 s1 |> Symmetry.to_rank)

    let perform_symmetry (x : t) (s : Symmetry.t) : t =
      (* convert to rep coord first by applying x's symmetry, then do s *)
      (* this code is really similar to above. Can I logically connect? *)
      (get_rep x) + (Symmetry.mult s (get_symmetry x) |> Symmetry.to_rank)

    (*
      x <=> P   
      R = S * P * S^-1
      P = S^-1 * R * S
    *)
    let to_perm (x : t) : Perm.t =
      let s = 
        x
        |> get_symmetry
        |> Symmetry.inverse
      in
      x
      |> get_rep_raw_coord
      |> S.Raw.to_perm
      |> Symmetry.on_perm s

    let of_perm (p : Perm.t) : t =
      p
      |> S.Raw.of_perm
      |> S.of_raw
      |> of_base

  end

(*
  A coordinate that has an integer type can be defined entirely
  from a max value `n` and conversions functions to/from Perm.t.
  This module type is the argument to Make below.
*)
module type Int_coord_raw =
  sig
    val n : int
    (* `of_perm` only valid on positive inputs less than `n` *)
    val of_perm : Perm.t -> int
    (* `to_perm` only valid on positive inputs less than `n` *)
    val to_perm : int -> Perm.t
  end

module Make
    (FM : Move.Fixed.S)
    (I : Int_coord_raw)
    : Coordinate with module Fixed_move = FM =
  struct
    module Fixed_move = FM
    module type T = T with module Fixed_move = Fixed_move

    (* All raw coordinate behavior is determined by of_perm and to_perm *)
    module Raw : T =
      struct
        module Fixed_move = FM
        include I
        type t = int [@@deriving sexp, compare]
        let to_rank = Fn.id
        let of_rank x = assert (x < n); x
        let zero = 0
        let next x = if x = n - 1 then None else Some (x + 1)
        let all () = List.init n ~f:Fn.id
        let perform_fixed_move (x : t) (m : Fixed_move.t) : t =
          m
          |> Fixed_move.to_move
          |> Perm.perform_move (I.to_perm x)
          |> I.of_perm
        let perform_symmetry (x : t) (s : Symmetry.t) : t = 
          x
          |> I.to_perm
          |> Symmetry.on_perm s
          |> I.of_perm
        let get_symmetry _  = Symmetry.id
      end

    module Make_memoized_coordinate = functor (M : Memo_params) -> (Make_memoized_coordinate (Raw) (M) : T)
    module S = Sym_base_of_raw (Raw)
    module Make_symmetry_coordinate = functor (M : Sym_memo_params) -> (Make_symmetry_coordinate (S) (M) : T)
  end

module Make_Phase1 : functor (_ : Int_coord_raw) -> Phase1Coordinate = Make (Move.Fixed.G)
module Make_Phase2 : functor (_ : Int_coord_raw) -> Phase2Coordinate = Make (Move.Fixed.G1)

(*
  --------------------
  DEFINING COORDINATES   
  --------------------

  I have prepped a lot of behavior above. What has been enabled is this:
  For any coordinate, I need only define its specialized to_perm and of_perm
  behavior as well as its size. I can plug this into an Int_coord_raw and
  make the entire remainder of the coordinate using this behavior.

  Some coordinates might be more easily described with a record instead of
  an int, but I can just map this record to an int afterwards.
*)

(*
  -------------------
  PHASE 1 COORDINATES   
  -------------------
*)

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
module Twist = Make_Phase1 (
  struct
    let n = Int.(3 ** 7)

    (* save the reversed list so it does not need to be reversed on every to_perm call *)
    let corner_cubies_rev = List.rev Cubie.Corner.all

    (*
      Iterate over all corners (except for the last, least significant one),
      and give each one its own digit in a base 3 number.
    *)
    let of_perm (p : Perm.t) : int =
      let open Cubie.With_orientation.Corner in
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
      | [] -> raise (LogicallyImpossible "cube not well-formed in Twist to_perm")
      | hd :: tl -> begin function
        | Corner c when Cubie.Corner.compare hd c = 0 -> Cubie.With_orientation.Corner { c = c; o = Modular_int.Z3.of_int x }
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
      | Edge e -> Cubie.With_orientation.Edge { e ; o = Modular_int.Z2.zero } (* let edges be untouched *)
      | Corner _ as c -> go new_coord corner_cubies_rev c 

  end
) 

(*
  Flip describes the orientation of the edges. It is identical to Twist
  but for edges instead. Refer to the explanations of `Twist` above to
  understand this coordinate.
  
  Therefore, it is an integer in 0..(2^11 - 1) and is treated as a base-2
  number.
*)
module Flip = Make_Phase1 (
  struct
    let n = Int.(2 ** 11)

    (* save the reversed list so it does not need to be reversed on every to_perm call *)
    let edge_cubies_rev = List.rev Cubie.Edge.all

    let of_perm (p : Perm.t) : int =
      let open Cubie.With_orientation.Edge in
      let rec go acc = function
      | [] | [_] -> acc (* ignores least signficant edge *)
      | e :: tl -> go (acc * 2 + Modular_int.Z2.to_int e.o) tl
      in
      go 0 (Perm.to_edges_list p)

    let to_perm (x : int) : Perm.t =
      let open Cubie in
      let rec go x = function
      | [] -> raise (LogicallyImpossible "cube not well-formed in Flip to_perm")
      | hd :: tl -> begin function
        | Edge e when Cubie.Edge.compare hd e = 0 -> Cubie.With_orientation.Edge { e = e ; o = Modular_int.Z2.of_int x }
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
      | Corner c -> Cubie.With_orientation.Corner { c ; o = Modular_int.Z3.zero } (* let corners be untouched *)
      | Edge   _ as e -> go new_coord edge_cubies_rev e
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
module UD_slice = Make_Phase1 (
  struct
    let n = ncr 12 4

    (* save the reversed list so it does not need to be reversed on every to_perm call *)
    let edge_cubies_rev = List.rev Cubie.Edge.all
    
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
      let is_filled e = Cubie.Edge e |> p |> Cubie.With_orientation.is_ud_slice in
      let rec go i k = function
      | _ when k < 0 -> 0
      | [] -> 0
      | hd :: tl when is_filled hd -> go (i - 1) (k - 1) tl
      | _ :: tl -> ncr i k + go (i - 1) k tl
      in
      go 11 3 edge_cubies_rev
    
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
      let rec go x i k = function
      | _ when k < 0 -> fun x -> x (* all ud slice found, just leave the rest in place *)
      | [] -> raise (LogicallyImpossible "cube not well-formed in UD_slice to_perm")
      | hd :: tl when is_filled x i k -> begin function
        | e when Edge.compare hd e = 0 -> List.nth_exn Edge.all_ud_slice_edges k
        | e -> go x (i - 1) (k - 1) tl e
        end
      | hd :: tl -> begin function (* this space is not filled with ud_slice edge *) 
        | e when Edge.compare hd e = 0 -> List.nth_exn Edge.all_ud_edges (i - k - 1) (* fill with non-ud-slice edge *)
        | e -> go (x - ncr i k) (i - 1) k tl e
        end
      in
      function (* return type is function *)
      | Corner c -> Cubie.With_orientation.Corner { c ; o = Modular_int.Z3.zero } (* leave corners untouched *)
      | Edge   e -> Cubie.With_orientation.Edge { e = go x 11 3 edge_cubies_rev e ; o = Modular_int.Z2.zero }

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
module Flip_UD_slice = Make_Phase1 (
  struct
    (*
      module T will be our natural representation of the coordinate,
      and we use it to create the `to_perm` and `of_perm` functions
      for integers.
    *)
    module T =
      struct
        type t =
          { ud_slice : UD_slice.Raw.t
          ; flip     : Flip.Raw.t }
        
        let to_rank { ud_slice ; flip } =
          Flip.Raw.n * UD_slice.Raw.to_rank ud_slice + Flip.Raw.to_rank flip

        let of_rank i =
          { ud_slice = UD_slice.Raw.of_rank (i / Flip.Raw.n)
          ; flip     = Flip.Raw.of_rank (i mod Flip.Raw.n) }
      end

    let n = UD_slice.Raw.n * Flip.Raw.n
    
    (*
      `Flip.to_perm` preserves permutation but changes orientation,
      and `UD_slice.to_perm` preserves orientation but changes
      permutation, so we can combine them by simply composing via
      Move multiplication.   
    *)
    let to_perm (x : int) : Perm.t =
      let y = T.of_rank x in
      Move.(UD_slice.Raw.to_perm y.ud_slice * Flip.Raw.to_perm y.flip)

    let of_perm (p : Perm.t) : int =
      T.{ ud_slice = UD_slice.Raw.of_perm p ; flip = Flip.Raw.of_perm p }
      |> T.to_rank
  end
) 

(*
  -------------------
  PHASE 2 COORDINATES   
  -------------------
*)

(*
  All phase 2 coordinates describe permutations. Therefore, they can
  all use the same basic behavior. I define a functor to help with this.

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
module Perm_coord (C : sig val all : Cubie.t list end) = Make_Phase2 (
  struct
    let k = List.length C.all
    let all_rev = List.rev C.all

    let n = fac k

    (* We need a way to determine order in the permutation *)
    (* Compares cubies with orientation, but ignores orientation *)
    let cubie_compare a b =
      let open Cubie.With_orientation in
      match a, b with
      | Corner { c=c1 ; o=_}, Corner { c=c2 ; o=_} -> Cubie.Corner.compare c1 c2
      | Edge   { e=e1 ; o=_}, Edge   { e=e2 ; o=_} -> Cubie.Edge.compare e1 e2
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
      let rm x = List.filter ~f:(fun a -> Cubie.compare x a <> 0) in
      (* 
        Here matching on list of cubies we're mapping, and ls is all possible cubies
        that can be mapped to with the "is replaced by" notation.
      *)
      let rec go x i possible_mappings = function
      | [] -> Cubie.With_orientation.of_cubie (* leave all remaining cubies in place *)
      | hd :: tl -> let this_mapping = List.nth_exn possible_mappings (x / fac i) in
        begin function
        | c when Cubie.compare hd c = 0 -> Cubie.With_orientation.of_cubie this_mapping
        | c -> c |> go (x mod fac i) (i - 1) (rm this_mapping possible_mappings) tl
        end
      in
      go x (k - 1) all_rev all_rev
  end
)


module Edge_perm = Perm_coord (
  struct
    let all =
      Cubie.Edge.all_ud_edges
      |> List.map ~f:(fun x -> Cubie.Edge x)
  end
)

module Corner_perm = Perm_coord (
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