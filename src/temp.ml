
    let to_move =
    let open Cubie in
    let open Modular_int in
    function
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
        in
        let f_o x =
          x
          |> Modular_int.Z3.to_int
          |> (function 0 -> 0 | 1 -> 2 | _ -> 1)
          |> Modular_int.Z3.of_int
        in Corner { c = c' ; o = f_o o }
      end