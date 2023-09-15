[@@@ocaml.warning "-27"] (* unused variable declarations *)

(* I'll need to be somewhat careful because storing this as int can get out of hand *)
(* I will need to create a module that describes it as a move with a name better,
   and conver that to int
*)
type t = int (* is index 0..15 in symmetry list *)

let inverse (s : t) : t =
  failwith "unimplemented"

let on_perm (s : t) (p : Perm.t) : Perm.t =
  failwith "unimplemented"

let mult (s1 : t) (s2 : t) : t =
  failwith "unimplemented"

let on_move (s : t) (m : Move.Fixed_move.t) : Move.Fixed_move.t =
  failwith "unimplemented"

(*
  While there are 48 symmetries, only 16 preserve UD symmetry, which is necessary for the solving method.
*)
let n = 16 
(*
  Since we store as an integer, there's really nothing to convert to a rank   
*)
let to_rank = fun x -> x

let next x = if x = n - 1 then None else Some (x + 1)

let zero = 0