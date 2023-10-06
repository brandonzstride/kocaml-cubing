(*
  All `failwith` calls in the file should be logically impossible
  if the code is correct.   
*)


open Core

module Generator =
  struct
    (*
      These symmetries generate all possible cube symmetries that
      preserve the UD slice.
      
      S_F2  : 180 degree turn around axis through F and B faces. Order 2.
      S_U4  : 90 degree clockwise turn around axis through U and D faces. Order 4.
      S_LR2 : reflection across the plane between the L and R faces. Order 2.
    *)
    type t = S_F2 | S_U4 | S_LR2

    (*
      The change in orientation of the cubies can depend on the initial orientation.
      For example, the reflection symmetry sends corner orientations to their mod 3
      inverse.

      To handle this, I make a new "move" that doesn't return a cubie with an orientation
      but rather return a cubie with a function on the orientation.

      This move can be compared and composed with regular moves, maybe...
    *)

    module Cubie_with_function =
      struct
        module Corner =
          struct
            type t = { c : Cubie.Corner.t ; f : Modular_int.Z3.t -> Modular_int.Z3.t }
          end
        module Edge =
          struct
            type t = { e : Cubie.Edge.t ; f : Modular_int.Z2.t -> Modular_int.Z2.t }
          end

        type t =
          | Corner of Corner.t
          | Edge of Edge.t

        let edge_exn = function
          | Edge e -> e
          | _ -> failwith "cannot get edge from Cubie_with_function.Corner constructor"

        let corner_exn = function
          | Corner c -> c
          | _ -> failwith "cannot get corner from Cubie_with_function.Edge constructor"
      end

    (* module Orientation_move =
      struct
        type t = Cubie.With_orientation.t -> Cubie.With_orientation.t

        let id = Fn.id

        let of_move move = fun x -> Cubie.With_orientation.of_cubie x |>  *)

    (**
      This is the module that behaves a lot like `Move`, but instead of letting
      orientations get passed along and added up, it lets us declared exactly
      how the orientation will be affected during the move.
      This is to be used only internally during the creation of the `Symmetry`
      module (this file).    
    *)
    module Function_move =
      struct
        type t = Cubie.t -> Cubie_with_function.t

        let id = function
          | Cubie.Corner c -> Cubie_with_function.Corner { c ; f = Modular_int.Z3.(+) Modular_int.Z3.zero } 
          | Cubie.Edge   e -> Cubie_with_function.Edge   { e ; f = Modular_int.Z2.(+) Modular_int.Z2.zero } 

        let of_move move =
          fun x ->
            match move x with
            | Cubie.With_orientation.Edge { e ; o } -> Cubie_with_function.Edge { e ; f = Modular_int.Z2.(+) o }
            | Cubie.With_orientation.Corner { c; o } -> Cubie_with_function.Corner { c ; f = Modular_int.Z3.(+) o }

        let ( * ) a b =
          function
          | Cubie.Edge _ as x ->
            let bx = b x |> Cubie_with_function.edge_exn in
            let abx = a (Cubie.Edge bx.e) |> Cubie_with_function.edge_exn in
            Cubie_with_function.Edge { e = abx.e ; f = fun o -> bx.f |> abx.f }
          | Cubie.Corner _ as x ->
            let bx = b x |> Cubie_with_function.corner_exn in
            let abx = a (Cubie.Corner bx.c) |> Cubie_with_function.corner_exn in
            Cubie_with_function.Corner { c = abx.c ; f = fun o -> bx.f |> abx.f }

        let equal (m1 : t) (m2 : t) : bool =
          let z2_function_equal f1 f2 = List.for_all Modular_int.Z2.all ~f:(fun x -> Modular_int.Z2.equal (f1 x) (f2 x)) in
          let z3_function_equal f1 f2 = List.for_all Modular_int.Z3.all ~f:(fun x -> Modular_int.Z3.equal (f1 x) (f2 x)) in
          let open Cubie_with_function in
          List.for_all Cubie.all ~f:(fun x ->
            match (m1 x), (m2 x) with
            | Corner { c = c1 ; f = f1 }, Corner { c = c2 ; f = f2 } -> Cubie.Corner.compare c1 c2 = 0 && z3_function_equal f1 f2
            | Edge { e = e1 ; f = f2 }, Edge { e = e2 ; f = f2 } -> Cubie.Edge.compare e1 e2 = 0 && z2_function_equal f1 f2
            | _ -> failwith "cannot compare corner to edge"
            )

        (* Apply a function_move to a move, where the function_move is done first *)
        (* Since the function_move is done first, the initial orientation is unknown, and it needs to stay a function_move *)
        let on_move_left (m : t) (m' : Move.t) : Function_move.t =
          m * (of_move m')

        (* Apply a function_move to a move, where the function_move is done second *)
        let on_move_right (m' : Move.t) (m : t) : Move.t
          function
          | Cubie.Edge _ as x ->
            let mx = m' x |> Cubie.With_orientation.edge_exn in
            let mmx = m (Cubie.Edge mx.e) |> Cubie_with_function.edge_exn in
            Cubie.With_orientation.Edge { e = mmx.e ; o = mmx.f mx.o }
          | Cubie.Corner _ as x ->
            let mx = m' x |> Cubie.With_orientation.corner_exn in
            let mmx = m (Cubie.Corner mx.c) |> Cubie_with_function.corner_exn in
            Cubie.With_orientation.Corner { c = mmx.c ; o = mmx.f mx.o }

      end

    let to_function_move : t -> Fuction_move.t =
    let open Cubie in
    let open Cubie.Edge in
    let open Cubie.Corner in
    let open Modular_int in
    function
    | S_F2 -> begin function
      | Edge e ->
        let e' = begin match e with
        | UR -> DL | UF -> DF | UL -> DR | UB -> DB
        | DR -> UL | DF -> UF | DL -> UR | DB -> UB
        | FR -> FL | FL -> FR | BL -> BR | BR -> BL end
        in Cubie_with_function.Edge { e = e' ; f = Fn.id }
      | Corner c ->
        let c' = begin match c with
        | URF -> DLF | UFL -> DFR | ULB -> DRB | UBR -> DBL
        | DFR -> UFL | DLF -> URF | DBL -> UBR | DRB -> ULB end
        in Cubie_with_function.Corner { c = c' ; f = Fn.id }
      end
    | S_U4 -> begin function
      | Edge e ->
        let e', o = begin match e with
        | UR -> UB, 0 | UF -> UR, 0 | UL -> UF, 0 | UB -> UL, 0
        | DR -> DB, 0 | DF -> DR, 0 | DL -> DF, 0 | DB -> DL, 0
        | FR -> BR, 1 | FL -> FR, 1 | BL -> FL, 1 | BR -> BL, 1 end
        in Cubie_with_function.Edge { e = e' ; f = Z2.(+) (Z2.of_int o) }
      | Corner c ->
        let c' = begin match c with
        | URF -> UBR | UFL -> URF | ULB -> UFL | UBR -> ULB
        | DFR -> DRB | DLF -> DFR | DBL -> DLF | DRB -> DBL end
        in Cubie_with_function.Corner { c = c' ; f = Fn.id }
      end
    | S_LR2 -> begin function
      | Edge e ->
        let e' = begin match e with
        | UR -> UL | UL -> UR | FR -> FL | FL -> FR
        | BR -> BL | BL -> BR | DR -> DL | DL -> DR | _ -> e end
        in Cubie_with_function.Edge { e = e' ; f = Fn.id }
      | Corner c -> 
        let c' = begin match c with
        | URF -> UFL | UFL -> URF | UBR -> ULB | ULB -> UBR
        | DFR -> DLF | DLF -> DFR | DBL -> DRB | DRB -> DBL end
        in Cubie_with_function.Corner { c = c' ; f = Modular_int.Z3.inverse }
      end
  end

module Multiples =
  struct
    (*
      Some power of a generator. counts are constrained by module S below.   
    *)
    type t = { gen : Generator.t ; count : int }

    let to_function_move =
      let open Generator in
      let open Move in
      let aux gen =
        let m = Generator.to_function_move gen in
        function
        | 0 -> Function_move.id
        | 1 -> m 
        | 2 -> m * m
        | _ -> m * m * m (* logically must be 3 by pattern match below *)
      in
    function
    | { gen = S_F2  ; count } when count < 2 -> aux S_F2  count
    | { gen = S_U4  ; count } when count < 4 -> aux S_U4  count
    | { gen = S_LR2 ; count } when count < 2 -> aux S_LR2 count
    | _ -> failwith "bad symmetry multiple"
  end

module S =
  struct
    (* s = 8x_2 + 2x_3 + x_4 *)
    (* m = S_F2^x_2 * S_U4^x_3 * S_LR2^x_4 *)
    (*
      A symmetry is some element in the group generated by the
      Generator class above. We can constrain the exponents on the 
      generators to only their group order so that we don't get deeply
      nested functions when converting to Move.t.
    *)
    type t =
      { x_2 : Modular_int.Z2.t
      ; x_3 : Modular_int.Z4.t
      ; x_4 : Modular_int.Z2.t } [@@deriving enumerate]

    let to_rank { x_2 ; x_3 ; x_4 } =
      Modular_int.(Z2.to_int x_2 * 8 + Z4.to_int x_3 * 2 + Z2.to_int x_4)
    
    let n = List.length all (* should be 16 *)

    let of_rank x =
      Modular_int.(
        { x_2 = Z2.of_int (x / 8)
        ; x_3 = Z4.of_int ((x mod 8) / 2)
        ; x_4 = Z2.of_int x }
      )
    
    (* let next x =
      let x' = to_rank x + 1 in
      if x' = n then None else Some (of_rank x') *)

    let to_function_move (s : t) : Move.t =
      let open Move in
      Multiples.(
          to_move { gen = S_F2  ; count = Modular_int.Z2.to_int s.x_2 }
        * to_move { gen = S_U4  ; count = Modular_int.Z4.to_int s.x_3 }
        * to_move { gen = S_LR2 ; count = Modular_int.Z2.to_int s.x_4 }
      )

    let mult (s1 : t) (s2 : t) : t =
      (* Symmetries don't commute, so I'll have to convert to moves and compare moves *)
      let m = Function_move.(to_function_move s1 * to_function_move s2) in
      List.find all ~f:(fun a -> Funtion_move.equal (to_function_move a) m)
      |> function
        | Some s -> s
        | None -> failwith "could not find equivalent symmetry for multiplication"

    let inverse (s : t) : t =
      let m = to_functon_move s in
      (** Assume that right inverses are sufficient, and don't need left inverse *)
      List.find all ~f:(fun a -> Function_move.(equal (m * to_function_move a) Function_move.id))
      |> function
        | Some s -> s
        | None -> failwith "could not find inverse symmetry"

    (*
      The problem here is that we need some initial permutation 

      To perform a symmetry on a permutation is to first apply
      the symmetry, then the permutation, and then the symmetry inverse.

      However, if a permutation is applied in conjunction with this,
      we can't assume that the orientation coming in will be zero.

      Note that the reflection is always applied last if it exists,
      and it is applied exactly once.

      s = 8x_2 + 2x_3 + x_4
      m = S_F2^x_2 * S_U4^x_3 * S_LR2^x_4

      So to see how a symmetry acts on a move, I can first find how S_LR2 acts
      *only* on the permutation, and from there apply the rest as moves.

      How does it act on a permutation? Well, if I can convert a permutation
      to a function move (which to me seems possible, but my tests before the)
      move refactor seem to suggest is not possible
    *)

    let on_perm (s : t) (p : Perm.t) : Perm.t =
      Move.(to_move s * p * to_move (inverse s))

    let on_move (s : t) (m : Move.Fixed_move.t) : Move.Fixed_move.t = 
      let open Move in
      let m' = to_move s * Fixed_move.to_move m * to_move (inverse s) in
      List.find Fixed_move.all ~f:(fun a -> equal (Fixed_move.to_move a) m')
      |> function
        | Some m -> m
        | None -> m (*failwith "could not find equivalent move under symmetry"*) (* TODO: fix *)

  end

(*
  The symmetry module is small enough that I can precompute everything
  before the program starts. Since there are only sixteen symmetries,
  this will be very fast.   
*)

(*
  The integer representing the symmetry will be exactly the rank in
  the module S above.   
*)
module I =
  struct
    type t = int [@@deriving sexp]
    let n = S.n
    let to_rank = Fn.id
  end

include I

let of_rank = Fn.id
let next x = if x = n - 1 then None else Some (x + 1)
let zero = 0
let all = List.map S.all ~f:S.to_rank

module Sym_mult_table = Lookup_table.Make2D (I) (I) (I)

let sym_mult_table =
  Sym_mult_table.create all all ~f:(fun x1 x2 -> S.mult (S.of_rank x1) (S.of_rank x2) |> S.to_rank)

let mult = Sym_mult_table.lookup sym_mult_table

module Sym_inverse_table = Lookup_table.Make1D (I) (I)

let inverse =
  all
  |> Sym_inverse_table.create ~f:(fun x -> x |> S.of_rank |> S.inverse |> S.to_rank)
  |> Sym_inverse_table.lookup

(* no memoization possible because there are too many possible permutations *)
let on_perm x =
  x |> S.of_rank |> S.on_perm

module Sym_move_table = Lookup_table.Make2D (I) (Move.Fixed_move) (Move.Fixed_move)

let on_move =
  Sym_move_table.create all Move.Fixed_move.all ~f:(fun x m -> let s = S.of_rank x in S.on_move s m)
  |> Sym_move_table.lookup


