(*
  All `failwith` calls in the file should be logically impossible
  if the code is correct.   
*)


open Core

[@@@ocaml.warning "-27"] (* unused variable declarations *)

module Generator =
  struct
    type t = S_F2 | S_U4 | S_LR2

    let to_move =
    let open Cubie in
    let open Cubie.Edge_facelet in
    let open Cubie.Corner_facelet in function
    | S_F2 -> begin function
      | Edge { e ; o } ->
        let e' = begin match e with
        | UR -> DL | UF -> DF | UL -> DR | UB -> DB
        | DR -> UL | DF -> UF | DL -> UR | DB -> UB
        | FR -> FL | FL -> FR | BL -> BR | BR -> BL end
        in Edge { e = e' ; o }
      | Corner { c ; o } ->
        let c' = begin match c with
        | URF -> DLF | UFL -> DFR | ULB -> DRB | UBR -> DBL
        | DFR -> UFL | DLF -> URF | DBL -> UBR | DRB -> ULB end
        in Corner { c = c' ; o }
      end
    | S_U4 -> begin function
      | Edge { e ; o } ->
        let e', d_o = begin match e with
        | UR -> UB, 0 | UF -> UR, 0 | UL -> UF, 0 | UB -> UL, 0
        | DR -> DB, 0 | DF -> DR, 0 | DL -> DF, 0 | DB -> DL, 0
        | FR -> BR, 1 | FL -> FR, 1 | BL -> FL, 1 | BR -> BL, 1 end
        in Edge { e = e' ; o = Modular_int.Z2.(o + of_int d_o) }
      | Corner { c ; o } ->
        let c' = begin match c with
        | URF -> UBR | UFL -> URF | ULB -> UFL | UBR -> ULB
        | DFR -> DRB | DLF -> DFR | DBL -> DLF | DRB -> DBL end
        in Corner { c = c' ; o }
      end
    | S_LR2 -> begin function
      | Edge { e ; o } ->
        let e' = begin match e with
        | UR -> UL | UL -> UR | FR -> FL | FL -> FR
        | BR -> BL | BL -> BR | DR -> DL | DL -> DR | _ -> e end
        in Edge { e = e' ; o }
      | Corner { c ; o } -> 
        let c' = begin match c with
        | URF -> UFL | UFL -> URF | UBR -> ULB | ULB -> UBR
        | DFR -> DLF | DLF -> DFR | DBL -> DRB | DRB -> DBL end
        in Corner { c = c' ; o }
      end
  end

module Multiples =
  struct
    type t = { gen : Generator.t ; count : int }

    let to_move =
      let open Generator in
      let open Move in
      let aux gen =
        let m = Generator.to_move gen in
        function
        | 0 -> fun x -> x
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

module type S = 
  sig
    type t
    val mult : t -> t -> t
    val inverse : t -> t
    val on_perm : t -> Perm.t -> Perm.t
    val on_move : t -> Move.Fixed_move.t -> Move.Fixed_move.t
    val to_rank : t -> int
    val n : int
    val next : t -> t option
    val zero : t
  end

module S : S =
  struct
    (* s = 8x_2 + 2x_3 + x_4 *)
    (* m = S_F2^x_2 * S_U4^x_3 * S_LR2^x_4 *)
    type t =
      { x_2 : Modular_int.Z2.t
      ; x_3 : Modular_int.Z4.t
      ; x_4 : Modular_int.Z2.t } [@@deriving enumerate]

    let to_rank { x_2 ; x_3 ; x_4 } =
      Modular_int.(Z2.to_int x_2 * 8 + Z4.to_int x_3 * 2 + Z2.to_int x_4)

    let zero =
      Modular_int.(
        { x_2 = Z2.of_int 0
        ; x_3 = Z4.of_int 0
        ; x_4 = Z2.of_int 0 }
      )
    
    let n = List.length all (* should be 16 *)

    let of_rank x =
      Modular_int.(
        { x_2 = Z2.of_int (x / 8)
        ; x_3 = Z4.of_int ((x mod 8) / 2)
        ; x_4 = Z2.of_int x }
      )
    
    let next x =
      let x' = to_rank x + 1 in
      if x' = n then None else Some (of_rank x')

    let to_move (s : t) : Move.t =
      let open Move in
      Multiples.(
          to_move { gen = S_F2  ; count = Modular_int.Z2.to_int s.x_2 }
        * to_move { gen = S_U4  ; count = Modular_int.Z4.to_int s.x_3 }
        * to_move { gen = S_LR2 ; count = Modular_int.Z2.to_int s.x_4 }
      )

    let mult (s1 : t) (s2 : t) : t =
      (* Symmetries don't commute, so I'll have to convert to moves and compare moves *)
      let m = Move.(to_move s1 * to_move s2) in
      List.find all ~f:(fun a -> Move.equal (to_move a) m)
      |> function
        | Some s -> s
        | None -> failwith "could not find equivalent symmetry for multiplication"

    let inverse (s : t) : t =
      let m = to_move s in
      List.find all ~f:(fun a -> Move.(equal (m * to_move a) Fn.id))
      |> function
        | Some s -> s
        | None -> failwith "could not find inverse symmetry"

    let on_perm (s : t) (p : Perm.t) : Perm.t =
      Move.(to_move s * p * to_move (inverse s))

    let on_move (s : t) (m : Move.Fixed_move.t) : Move.Fixed_move.t = 
      let open Move in
      let m' = to_move s * Fixed_move.to_move m * to_move (inverse s) in
      List.find Fixed_move.all ~f:(fun a -> equal (Fixed_move.to_move a) m')
      |> function
        | Some m -> m
        | None -> failwith "could not find equivalent move under symmetry"

  end

module type Memoization =
  sig
    val is_already_saved : bool
    val save_location : string
  end

(* placeholder *)
module Memoize (S : S) (_ : Memoization) : S with type t := S.t = 
  struct
    include S
  end