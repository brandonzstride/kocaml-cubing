[@@@ocaml.warning "-27"] (* unused variable declarations *)

(* I'll need to be somewhat careful because storing this as int can get out of hand *)
(* I will need to create a module that describes it as a move with a name better,
   and conver that to int
*)
type t = int (* is index 0..15 in symmetry list *)

let inverse = failwith "unimplemented"

let conjugate = failwith "unimplemented"

let mult = failwith "unimplemented"

let on_move = failwith "unimplemented"

(*
  While there are 48 symmetries, only 16 preserve UD symmetry, which is necessary for the solving method.
*)
let n = 16 
(*
  Since we store as an integer, there's really nothing to convert to a rank   
*)
let to_rank = fun x -> x