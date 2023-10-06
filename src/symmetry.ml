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


    let to_move : t -> Move.t =
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
        in With_orientation.Edge { e = e' ; o = Z2.zero }
      | Corner c ->
        let c' = begin match c with
        | URF -> DLF | UFL -> DFR | ULB -> DRB | UBR -> DBL
        | DFR -> UFL | DLF -> URF | DBL -> UBR | DRB -> ULB end
        in With_orientation.Corner { c = c' ; o = Z3.zero }
      end
    | S_U4 -> begin function
      | Edge e ->
        let e', o = begin match e with
        | UR -> UB, 0 | UF -> UR, 0 | UL -> UF, 0 | UB -> UL, 0
        | DR -> DB, 0 | DF -> DR, 0 | DL -> DF, 0 | DB -> DL, 0
        | FR -> BR, 1 | FL -> FR, 1 | BL -> FL, 1 | BR -> BL, 1 end
        in With_orientation.Edge { e = e' ; o = Z2.of_int o }
      | Corner c ->
        let c' = begin match c with
        | URF -> UBR | UFL -> URF | ULB -> UFL | UBR -> ULB
        | DFR -> DRB | DLF -> DFR | DBL -> DLF | DRB -> DBL end
        in With_orientation.Corner { c = c' ; o = Z3.zero }
      end
    | S_LR2 -> begin function
      | Edge e ->
        let e' = begin match e with
        | UR -> UL | UL -> UR | FR -> FL | FL -> FR
        | BR -> BL | BL -> BR | DR -> DL | DL -> DR | _ -> e end
        in With_orientation.Edge { e = e' ; o = Z2.zero }
      | Corner c -> 
        let c' = begin match c with
        | URF -> UFL | UFL -> URF | UBR -> ULB | ULB -> UBR
        | DFR -> DLF | DLF -> DFR | DBL -> DRB | DRB -> DBL end
        in With_orientation.Corner { c = c' ; o = Z3.zero } (* should apply inverse *)
      end
    
    (*
      I'd like to conjugate an arbitrary permutation with this function
      for S_LR2.
      This function is its own inverse, i.e. S_LR2^-1 = S_LR2   

      So basically, for a permutation p, I need to find
        S_LR2 * p * S_LR2

      The edge part is trivial because orientations are unaffected. I am
      only concerned with the corner part.
      
      This wouldn't be hard if I could decode p down to fixed moves and then
      use hardcoded fixed move conjugations. But to do that is to solve p, which
      is part of the problem at-hand.

      So I need to work with p as if it's only a function on cubies. 
      Let s := S_LR2
      
      s(c) = c', f(o) a function on the orientation
      p(c') = c'', do a difference in the orientation
      s(c'') = c''', f(o) a function on the orientation

      I can feed through easily to find exactly which cubie goes where. This
      is natural. But what about the orientation?

      Let's break it down.
      * This function f(o) is always the inverse function
      * If the original orientation of the cubie c''' (which is what in the
        end will replace c) is 0, then the final orientation of c is f(do).
      * If it's 1, then f(1) = 2, so final is f(2 + do)
        If do is 0, then final 1
        1 -> 0
        2 -> 2
      * If it's 2, then f(2) = 1, so final is f(1 + do)
        If do is 0, then final is 2
        1 -> 1
        2 -> 0
      * It's trivial that the total function on the orientation is f(f(o) + do)
        where o is the original orientation of c''', and do is the orientation
        from the permutation.
        do is completely arbitrary, so I don't think I can deduce anything from it
        to simplify this more.
        We see from the above deductions on do that we still depend on initial
        orientation.
      
      * Can I ever assume that initial orientation is zero?
        "Initial orientation" here means what?
        It's a tough concept because the usage of these symmetries is really just
        to conjugate a symmetry with a permutation and remeasure the coordinate.
        We assume that the permutation is not a move but is rather an exact
        definition of what exists in that cubie location and with what orientation.
        Past that, we only apply move sequences to a permutation.
        When this is done, however, the move sequence is applied "first" because of
        the "is replaced by" notation, so to determine how a permutation ends up after
        a move sequence, the permutation must ready to accept some non-zero orientation.
        But this doesn't quite feel right. I need to think about this.

      A symmetry coordinate needs to be converted to a perm by s * p * s^-1 where
      p is the perm of the underlying raw representative coordinate.
      When is this ever actually done? We most often just deal with fixed moves, and
      it's already known how fixed moves are done on symmetries, so it's trivial to get
      the resulting sym coord under a fixed move:
        Convert to fixed move under symmetry, apply to coordinate (which is memoized),
        and then convert back to sym coordinate.
      
      The perm will be needed when converting back to permutation after phase1, but not
      really because we could just do the moves on the original permutation to get there,
      which is very, very fast to do just for a short sequence of moves once.

      Then I'm not seeing when we ever really need to perform a symmetry on a permutation
      and *then* do something with that.
      => so it's safe to say that the original thing passed in has orientation zero, and 
      the result of `on_perm` won't ever be used in any multiplication. (did I consider
      multiplication in Flip UD slice?)

      For this reason, I can say that the final orientation is f(do) where do is the 
      `o` in the Cubie.With_orientation.Corner.t part of the conjugation.
      It will therefore be worth hardcoding the way that reflection behaves on a permutation
      separately from the other symmetries; the others will be done with simple moves.
    *)
  end

module Multiples =
  struct
    (*
      Some power of a generator. counts are constrained by module S below.   
    *)
    type t = { gen : Generator.t ; count : int }

    (* must be careful on S_LR2 because it's currently wrong *)
    let to_move =
      let open Generator in
      let open Move in
      let aux gen =
        let m = Generator.to_move gen in
        function
        | 0 -> Move.id
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

    (* must be careful on S_LR2 *)
    let to_move (s : t) : Move.t =
      let open Move in
      Multiples.(
          to_move { gen = S_F2  ; count = Modular_int.Z2.to_int s.x_2 }
        * to_move { gen = S_U4  ; count = Modular_int.Z4.to_int s.x_3 }
        * to_move { gen = S_LR2 ; count = Modular_int.Z2.to_int s.x_4 }
      )

    (* I think this is currently safe on S_LR2 because no symmetry only
       orients the cubies, so if orientation of to_move is wrong, then
      it's okay because it needs to only depend on the permutation *)
    let mult (s1 : t) (s2 : t) : t =
      (* Symmetries don't commute, so I'll have to convert to moves and compare moves *)
      let m = Move.(to_move s1 * to_move s2) in
      List.find all ~f:(fun a -> Move.equal (to_move a) m)
      |> function
        | Some s -> s
        | None -> failwith "could not find equivalent symmetry for multiplication"

    let inverse (s : t) : t =
      let m = to_move s in
      (** Assume that right inverses are sufficient, and don't need left inverse *)
      List.find all ~f:(fun a -> Move.(equal (m * to_move a) Move.id))
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
      to a function move (which to me seems possible, but my tests before the
      move refactor seem to suggest is not possible), then I use only the
      reflection...

      Here's what needs to happen:
      * I take in a cube permutation, which is definitely well-defined
      * And I just convert that to a new permutation, which I know should be
        well-defined.
      * This is done by converting the symmetry to a function on the cubies
        and composing as normal.
      * This goes wrong when I have a reflection that is not defined so normally
        because the function it generates changes orientations differently
        than anything else.
      * Potentially I can hard-code a function that is the reflection conjugating
        any move. This will still work smoothly with the other symmetries because
          ((non-reflection-parts) * (reflection-part))^-1
          = (reflection-part)^-1 * (non-reflection-parts)^-1
          = (reflection-part) * (non-reflection-parts)^-1
        and this is totally ok because I only ever conjugate and need to return
        a new permutation or a new move.
      * When I need to return a new fixed move, I know the hard-coded reflection
        move table, which gives a new fixed move, and then I apply the regular
        code on the non-reflection part of the symmetry
      * When I need to return a new permutation, then this is a little tougher.
        The reflection needs an initial orientation of the cubies to tell the final
        orientation. One reason this is difficult is that the user has no idea
        what will soon replace the cubie, so when the symmetry says that it replaces
        URF with UFL and inverses the orientation, the user needs to somehow tell the
        symmetry what was the original orientation of UFL. This makes it a bit of a
        two-step process. However, as the coder (and only user), I *do* know which
        orientation to find because I'm behind the scenes of the function.
        This is continued up by the to_move in Generator.
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


