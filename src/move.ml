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

module type Generator =
  sig
    type t [@@deriving enumerate, sexp, compare]
    val to_move : t -> T.t
    val to_rank : t -> int
  end

module G_T =
  struct
    type t = U | R | F | D | B | L
  end

(* These moves generate the whole cube *)
module Generator : Generator with type t = G_T.t =
  struct
    type t = G_T.t = U | R | F | D | B | L [@@deriving enumerate, variants, sexp, compare]

    let to_rank = Variants.to_rank

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

module G1_T =
  struct
    type t = U | D | R2 | F2 | B2 | L2
  end

(* These generate the G1 subgroup of the cube *)
module G1_generator :
    sig
      include Generator
      val to_g_t : t -> Generator.t
    end with type t = G1_T.t =
  struct
    type t = G1_T.t = U | D | R2 | F2 | B2 | L2 [@@deriving enumerate, variants, sexp, compare]

    let to_rank = Variants.to_rank

    let to_g_t : t -> Generator.t = function
    | U -> G_T.U
    | D -> G_T.D
    | R2 -> G_T.R
    | F2 -> G_T.F
    | B2 -> G_T.B
    | L2 -> G_T.L

    let to_move x = 
      let y = to_g_t x in
      match x with
      | U | D -> Generator.to_move y
      | _ -> Generator.to_move y * Generator.to_move y
  end

module Fixed =
  struct
    (* This is the super type. All S.t can convert to and from it *)
    module Super =
      struct
        type t = { gen : Generator.t ; c : Modular_int.Z4.t }
      end

    module type S =
      sig
        module Generator : Generator
        type t [@@deriving sexp, compare]
        val of_gen : Generator.t -> t
        val of_generator_and_count : Generator.t -> int -> t
        val to_generator_and_count : t -> (Generator.t * int)
        val all : t list (* all non-identity moves *)
        val n : int (* number of non-identity moves *)
        val to_rank : t -> int (* not defined on identity moves *)
        val to_move : t -> T.t

        (* allow conversions to and from every possible fixed move *)
        (* This makes all seem like a subtype of All_fixed_move *)
        val to_super_t : t -> Super.t
        val of_super_t : Super.t -> t

        (* TODO: exchange this for quickcheck *)
        val random_list : int -> t list
      end

    (* Fixed moves on the whole Rubik's cube group G *)
    module G : S with type t = Super.t and type Generator.t = Generator.t =
      struct
        module Generator = Generator
        type t = Super.t = { gen : Generator.t ; c : Modular_int.Z4.t } [@@deriving enumerate, sexp, compare]

        let of_generator_and_count (gen : Generator.t) (count : int) : t =
          { gen ; c = Modular_int.Z4.of_int count }

        let of_gen (gen : Generator.t) : t =
          of_generator_and_count gen 1

        let to_generator_and_count (x : t) : (Generator.t * int) =
          x.gen, Modular_int.Z4.to_int x.c

        let to_rank x = 
          assert (Modular_int.Z4.to_int x.c <> 0);
          Int.(Generator.to_rank x.gen * 3 + Modular_int.Z4.to_int x.c - 1)

        let all = 
          let cmp a b = Int.compare (to_rank a) (to_rank b) in
          all
          |> List.filter ~f:(fun x -> Modular_int.Z4.compare x.c Modular_int.Z4.zero <> 0)
          |> List.sort ~compare:cmp

        let n = List.length all

        let to_move { gen ; c } =
          let m = Generator.to_move gen in
          match Modular_int.Z4.to_int c with
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

        let to_super_t = Fn.id
        let of_super_t = Fn.id
      end

    module G1 : S with type Generator.t = G1_generator.t =
      struct
        module Generator = G1_generator (* has same type as G1_T *)
        module Single =
          struct
            type t = { gen : Generator.t ; c : Modular_int.Z4.t} [@@deriving enumerate, sexp, compare]
    
            let all = List.filter all ~f:(fun x ->
              match x.gen with
              | G1_T.U | G1_T.D -> Modular_int.Z4.to_int x.c <> 0
              | _ -> false
              )
          end
    
        module Double =
          struct
            type t = { gen : Generator.t ; c : Modular_int.Z2.t} [@@deriving enumerate, sexp, compare]
    
            let all = List.filter all ~f:(fun x ->
              match x.gen with
              | G1_T.U | G1_T.D -> false
              | _ -> Modular_int.Z2.to_int x.c <> 0
              )
          end
    
        type t =
          | Single of Single.t
          | Double of Double.t
          [@@deriving enumerate, sexp, compare]
    
        let of_generator_and_count (gen : Generator.t) (count : int) : t =
          match gen with
          | G1_T.U | G1_T.D -> Single Single.{ gen ; c = Modular_int.Z4.of_int count }
          | _ -> Double Double.{ gen ; c = Modular_int.Z2.of_int count }

        let of_gen (gen : Generator.t) : t =
          of_generator_and_count gen 1
    
        let to_generator_and_count : t -> (Generator.t * int) =
          function
          | Single x -> x.gen, Modular_int.Z4.to_int x.c
          | Double x -> x.gen, Modular_int.Z2.to_int x.c
    
        let to_rank : t -> int = function
          | Single x -> begin
            match x.gen with
            | G1_T.U -> Modular_int.Z4.to_int x.c - 1
            | G1_T.D -> Modular_int.Z4.to_int x.c + 2 (* offset by 3 because of U, then must subtract 1 from count *)
            | _ -> failwith "logically impossible"
            end
          | Double x -> Int.(4 + Generator.to_rank x.gen)  (* offset by 6 for U,D ranks, then minus 2 for their variant rank *)
    
        let all = 
          let is_nonzero_count = function
            | Single x -> Modular_int.Z4.compare x.c Modular_int.Z4.zero <> 0
            | Double x -> Modular_int.Z2.compare x.c Modular_int.Z2.zero <> 0
          in
          let cmp a b = Int.compare (to_rank a) (to_rank b) in
          all (* defined from enumerate *)
          |> List.filter ~f:is_nonzero_count
          |> List.sort ~compare:cmp
    
        let n = List.length all
    
        let to_super_t (x : t) : Super.t =
          let gen, count =
            match x with
            | Single y -> Generator.to_g_t y.gen, Modular_int.Z4.to_int y.c
            | Double y -> Generator.to_g_t y.gen, Int.(Modular_int.Z2.to_int y.c * 2)
          in
          G.of_generator_and_count gen count
    
        let to_move x =
          x
          |> to_super_t
          |> G.to_move
    
        let random_from_list ls _ =
          ls
          |> List.length
          |> Random.int 
          |> List.nth_exn ls
    
        let random_list n =
          List.init n ~f:(random_from_list all)
        
        let of_super_t (x : Super.t) : t =
          let open G1_T in
          let gen, count =
            match G.to_generator_and_count x with
            | G_T.U, i -> U, i
            | G_T.D, i -> D, i
            | G_T.R, i -> R2, i / 2
            | G_T.F, i -> F2, i / 2
            | G_T.B, i -> B2, i / 2
            | G_T.L, i -> L2, i / 2
          in
          of_generator_and_count gen count
      end
  end