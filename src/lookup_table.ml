(*
  The lookup tables use arrays for quick access, so they use mutation,
  but this is all behind the scenes, so users of the lookup table can
  be purely functional.
*)


open Core

module type Key =
  sig
    type t
    val to_rank : t -> int
  end

module type Return_type =
  sig
    type t [@@deriving sexp]
  end

module Make1D (Key : Key) (R : Return_type) =
  struct
    type t = R.t array [@@deriving sexp]

    let from_file (filename : string) =
      filename
      |> Sexp.load_sexp
      |> t_of_sexp
    
    let to_file (table : t) (filename : string) : unit =
      table
      |> sexp_of_t
      |> Sexp.save filename

    let create (ls : 'a list) ~(f : 'a -> R.t) : t =
      Array.of_list_map ls ~f

    let of_list (ls : R.t list) =
      Array.of_list ls

    let lookup (table : t) (k : Key.t) =
      Array.nget table (Key.to_rank k)

    let get_n (table : t) : int =
      Array.length table
  end

module Make2D (Key1 : Key) (Key2 : Key) (R : Return_type) =
  struct
    type t = 
      { arr : R.t array
      ; n1  : int
      ; n2  : int } [@@deriving sexp]

    let from_file (filename : string) =
      filename
      |> Sexp.load_sexp
      |> t_of_sexp

    let to_file (table : t) (filename : string) : unit =
      table
      |> sexp_of_t
      |> Sexp.save filename

    let create ?(n1 : int option) ?(n2 : int option) (l1 : 'a list) (l2 : 'b list) ~(f : 'a -> 'b -> R.t) : t =
      let get_n ls n = match n with None -> List.length ls | Some x -> x in
      let n1 = get_n l1 n1 in let n2 = get_n l2 n2 in
      let x = f (List.hd_exn l1) (List.hd_exn l2) in (* default value for array *)
      let arr = Array.create x ~len:(n1 * n2) in
      List.iteri l1 ~f:(fun i1 -> fun a ->
        List.iteri l2 ~f:(fun i2 -> fun b ->
          Array.set arr (n2 * i1 + i2) (f a b)
        )
      );
      { arr ; n1 ; n2 }

    let lookup (table : t) (k1 : Key1.t) (k2 : Key2.t) =
      (Key1.to_rank k1) * table.n2 + Key2.to_rank k2
      |> Array.nget table.arr
    
  end