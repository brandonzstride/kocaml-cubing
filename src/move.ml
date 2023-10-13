open Core

module T =
  struct
    type t = Cubie.t -> Cubie.With_orientation.t
  end

include T

let id = Cubie.With_orientation.of_cubie

let equal (m1 : t) (m2 : t) : bool =
  List.for_all Cubie.all ~f:(fun x -> Cubie.With_orientation.compare (m1 x) (m2 x) = 0)

let equal_without_orientation (m1 : t) (m2 : t) : bool =
  List.for_all Cubie.all ~f:(fun x ->
    (m1 x, m2 x)
    |> Tuple2.map ~f:Cubie.With_orientation.to_cubie
    |> Tuple2.uncurry Cubie.compare
    |> ( = ) 0
  )


(** Compose a and b by first applying a, then applying b *)
let ( * ) a b = 
  function
  | Cubie.Edge _ as x ->
    (** Safe to use edge_exn if the move is well-formed and maps edges to edges *)
    let bx = b x |> Cubie.With_orientation.edge_exn in
    let abx = a (Cubie.Edge bx.e) |> Cubie.With_orientation.edge_exn in
    Cubie.With_orientation.Edge Cubie.With_orientation.Edge.{ e = abx.e ; o = Modular_int.Z2.(bx.o + abx.o) }
  | Cubie.Corner _ as x ->
    (** Safe to use corner_exn if the move is well-formed and maps corners to corners *)
    let bx = b x |> Cubie.With_orientation.corner_exn in
    let abx = a (Cubie.Corner bx.c) |> Cubie.With_orientation.corner_exn in
    Cubie.With_orientation.Corner Cubie.With_orientation.Corner.{ c = abx.c ; o = Modular_int.Z3.(bx.o + abx.o) }

module Faceturn =
  struct
    type t = U | R | F | D | B | L [@@deriving enumerate, variants, sexp, compare]

    (* While it's typically more readable to give each match case its own line
        in this specific situation, it seems better to group them on a line *)
    let to_move = 
      let open Cubie in
      let open Cubie.Edge in
      let open Cubie.Corner in
      let open Modular_int in
      function
      | U -> begin function
        (* we use the "is replaced by" format, so when U is turned cw, UR goes to UF and keeps the same orientation *)
        | Edge e ->
          let e' = begin match e with
          | UR -> UB | UF -> UR | UL -> UF | UB -> UL | _ -> e end
          in Cubie.With_orientation.Edge { e = e' ; o = Z2.zero } 
        (* we use the "is replaced by" format, so when U is turned cw, UBR goes to URF and keeps the same orientation *)
        | Corner c -> 
          let c' = begin match c with
          | URF -> UBR | UFL -> URF | ULB -> UFL | UBR -> ULB | _ -> c end
          in Cubie.With_orientation.Corner { c = c' ; o = Z3.zero }
        end
      | R -> begin function
        | Edge e ->
          let e' = begin match e with
          | UR -> FR | BR -> UR | DR -> BR | FR -> DR | _ -> e end
          in Cubie.With_orientation.Edge { e = e' ; o = Z2.zero } 
        | Corner c -> 
          let c', o = begin match c with
          | URF -> DFR, 2 | UBR -> URF, 1 | DRB -> UBR, 2 | DFR -> DRB, 1 | _ -> c, 0 end
          in Cubie.With_orientation.Corner { c = c' ; o = Z3.of_int o }
        end
      | F -> begin function
        | Edge e -> 
          let e', o = begin match e with
          | UF -> FL, 1 | FR -> UF, 1 | DF -> FR, 1 | FL -> DF, 1 | _ -> e, 0 end
          in Cubie.With_orientation.Edge { e = e' ; o = Z2.of_int o }
        | Corner c ->
          let c', o = begin match c with
          | URF -> UFL, 1 | UFL -> DLF, 2 | DLF -> DFR, 1 | DFR -> URF, 2 | _ -> c, 0 end
          in Cubie.With_orientation.Corner { c = c' ; o = Z3.of_int o }
        end
      | D -> begin function
        | Edge e ->
          let e' = begin match e with
          | DR -> DF | DF -> DL | DL -> DB | DB -> DR | _ -> e end
          in Cubie.With_orientation.Edge { e = e' ; o = Z2.zero }
        | Corner c ->
          let c' = begin match c with
          | DFR -> DLF | DLF -> DBL | DBL -> DRB | DRB -> DFR | _ -> c end
          in Cubie.With_orientation.Corner { c = c' ; o = Z3.zero }
        end
      | B -> begin function
        | Edge e -> 
          let e', o = begin match e with
          | UB -> BR, 1 | BL -> UB, 1 | DB -> BL, 1 | BR -> DB, 1 | _ -> e, 0 end
          in Cubie.With_orientation.Edge { e = e' ; o = Z2.of_int o }
        | Corner c ->
          let c', o = begin match c with
          | ULB -> UBR, 1 | UBR -> DRB, 2 | DRB -> DBL, 1 | DBL -> ULB, 2 | _ -> c, 0 end
          in Cubie.With_orientation.Corner { c = c' ; o = Z3.of_int o }
        end
      | L -> begin function
        | Edge e ->
          let e' = begin match e with
          | UL -> BL | FL -> UL | DL -> FL | BL -> DL | _ -> e end
          in Cubie.With_orientation.Edge { e = e' ; o = Z2.zero }
        | Corner c ->
          let c', o = begin match c with
          | UFL -> ULB, 1 | ULB -> DBL, 2 | DBL -> DLF, 1 | DLF -> UFL, 2 | _ -> c, 0 end
          in Cubie.With_orientation.Corner { c = c' ; o = Z3.of_int o }
        end
  end

module G1_faceturn =
  struct
    type t = U | D | R2 | F2 | B2 | L2 [@@deriving enumerate, variants, sexp, compare]

    let to_faceturn : t -> Faceturn.t = function
    | U -> Faceturn.U
    | D -> Faceturn.D
    | R2 -> Faceturn.R
    | F2 -> Faceturn.F
    | B2 -> Faceturn.B
    | L2 -> Faceturn.L

    let to_move x = 
      let y = to_faceturn x in
      match x with
      | U | D -> Faceturn.to_move y
      | _ -> Faceturn.to_move y * Faceturn.to_move y
  end

module All_fixed_move_T =
  struct
    type t = { faceturn : Faceturn.t ; count : Modular_int.Z4.t }
  end

module type Fixed_move =
  sig
    module Faceturn :
      sig
        type t
      end
    type t [@@deriving sexp, compare]
    val of_faceturn_and_count : Faceturn.t -> int -> t
    val to_faceturn_and_count : t -> (Faceturn.t * int)
    val all : t list (* all non-identity moves *)
    val n : int (* number of non-identity moves *)
    val to_rank : t -> int (* not defined on identity moves *)
    val to_move : t -> T.t

    (* allow conversions to and from every possible fixed move *)
    val to_all_fixed_move : t -> All_fixed_move_T.t
    val of_all_fixed_move : All_fixed_move_T.t -> t

    val random_list : int -> t list
  end


module All_fixed_move : Fixed_move with type t = All_fixed_move_T.t and module Faceturn = Faceturn =
  struct
    module Faceturn = Faceturn

    type t = All_fixed_move_T.t = { faceturn : Faceturn.t ; count : Modular_int.Z4.t } [@@deriving enumerate, sexp, compare]

    let of_faceturn_and_count (faceturn : Faceturn.t) (count : int) : t =
      { faceturn ; count = Modular_int.Z4.of_int count }

    let to_faceturn_and_count (x : t) : (Faceturn.t * int) =
      x.faceturn, Modular_int.Z4.to_int x.count

    let to_rank x = 
      assert (Modular_int.Z4.to_int x.count <> 0);
      Int.(Faceturn.Variants.to_rank x.faceturn * 3 + Modular_int.Z4.to_int x.count - 1)

    let all = 
      let cmp a b = Int.compare (to_rank a) (to_rank b) in
      all
      |> List.filter ~f:(fun x -> Modular_int.Z4.compare x.count Modular_int.Z4.zero <> 0)
      |> List.sort ~compare:cmp

    let n = List.length all

    let to_move { faceturn ; count } =
      let m = Faceturn.to_move faceturn in
      match Modular_int.Z4.to_int count with
      | 0 -> id (* we do allow count to be zero here. *)
      | 1 -> m
      | 2 -> m * m
      | _ -> m * m * m

    let random_from_list ls _ =
      ls
      |> List.length
      |> Random.int 
      |> List.nth_exn ls

    let random_list n =
      List.init n ~f:(random_from_list all)

    let to_all_fixed_move = Fn.id
    let of_all_fixed_move = Fn.id
    
  end

module G1_fixed_move : Fixed_move =
  struct
    module Faceturn = G1_faceturn

    module Single =
      struct
        type t = { faceturn : Faceturn.t ; count : Modular_int.Z4.t} [@@deriving enumerate, sexp, compare]

        let all = List.filter all ~f:(fun x ->
          match x.faceturn with
          | U | D -> Modular_int.Z4.to_int x.count <> 0
          | _ -> false
          )
      end

    module Double =
      struct
        type t = { faceturn : Faceturn.t ; count : Modular_int.Z2.t} [@@deriving enumerate, sexp, compare]

        let all = List.filter all ~f:(fun x ->
          match x.faceturn with
          | U | D -> false
          | _ -> Modular_int.Z2.to_int x.count <> 0
          )
      end

    type t =
      | Single of Single.t
      | Double of Double.t
      [@@deriving enumerate, sexp, compare]

    let of_faceturn_and_count (faceturn : Faceturn.t) (count : int) : t =
      match faceturn with
      | U | D -> Single Single.{ faceturn ; count = Modular_int.Z4.of_int count }
      | _ -> Double Double.{ faceturn ; count = Modular_int.Z2.of_int count }

    let to_faceturn_and_count : t -> (Faceturn.t * int) =
      function
      | Single x -> x.faceturn, Modular_int.Z4.to_int x.count
      | Double x -> x.faceturn, Modular_int.Z2.to_int x.count

    let to_rank : t -> int = function
      | Single x -> begin
        match x.faceturn with
        | U -> Modular_int.Z4.to_int x.count - 1
        | D -> Modular_int.Z4.to_int x.count + 2 (* offset by 3 because of U, then must subtract 1 from count *)
        | _ -> failwith "logically impossible"
        end
      | Double x -> Int.(6 + Faceturn.Variants.to_rank x.faceturn) 

    let all = 
      let is_nonzero_count = function
        | Single x -> Modular_int.Z4.compare x.count Modular_int.Z4.zero <> 0
        | Double x -> Modular_int.Z2.compare x.count Modular_int.Z2.zero <> 0
      in
      let cmp a b = Int.compare (to_rank a) (to_rank b) in
      all (* defined from enumerate *)
      |> List.filter ~f:is_nonzero_count
      |> List.sort ~compare:cmp

    let n = List.length all

    let to_all_fixed_move (x : t) : All_fixed_move.t =
      let fturn, count =
        match x with
        | Single y -> Faceturn.to_faceturn y.faceturn, Modular_int.Z4.to_int y.count
        | Double y -> Faceturn.to_faceturn y.faceturn, Int.(Modular_int.Z2.to_int y.count * 2)
      in
      All_fixed_move.of_faceturn_and_count fturn count

    let to_move x =
      x
      |> to_all_fixed_move
      |> All_fixed_move.to_move

    let random_from_list ls _ =
      ls
      |> List.length
      |> Random.int 
      |> List.nth_exn ls

    let random_list n =
      List.init n ~f:(random_from_list all)
    
    let of_all_fixed_move (x : All_fixed_move.t) : t =
      let open Faceturn in
      let fturn, count =
        match All_fixed_move.to_faceturn_and_count x with
        | All_fixed_move.Faceturn.U, i -> U, i
        | All_fixed_move.Faceturn.D, i -> D, i
        | All_fixed_move.Faceturn.R, i -> R2, i / 2
        | All_fixed_move.Faceturn.F, i -> F2, i / 2
        | All_fixed_move.Faceturn.B, i -> B2, i / 2
        | All_fixed_move.Faceturn.L, i -> L2, i / 2
      in
      of_faceturn_and_count fturn count
  end