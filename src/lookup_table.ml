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

module type Key2D =
  sig
    include Key
    val n : int
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

    let create' (ls : 'a list) ~(f : 'a -> R.t) : t =
      Array.of_list_map ls ~f

    let create (ls : R.t list) =
      Array.of_list ls

    let lookup (table : t) (k : Key.t) =
      Array.nget table (Key.to_rank k)

    let get_n (table : t) : int =
      Array.length table
  end

module Make2D (Key1 : Key2D) (Key2 : Key2D) (R : Return_type) =
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

    let create' (l1 : 'a list) (l2 : 'b list) ~(f : 'a -> 'b -> R.t) : t =
      let x = f (List.hd_exn l1) (List.hd_exn l2) in (* default value for array *)
      let arr = Array.create x ~len:(Key1.n * Key2.n) in
      List.iteri l1 ~f:(fun i1 -> fun a ->
        List.iteri l2 ~f:(fun i2 -> fun b ->
          Array.set arr (Key2.n * i1 + i2) (f a b)
        )
      );
      arr

    let create (ls : R.t list list) =
      let lens = ls |> List.map ~f:List.length in
      let is_rectangular = let n = List.hd_exn lens in List.for_all lens ~f:((=) n) in
      let total_len = List.fold lens ~init:0 ~f:(+) in
      assert (total_len = (Key1.n * Key2.n));
      assert is_rectangular;
      ls
      |> List.join
      |> Array.of_list

    let lookup (table : t) (k1 : Key1.t) (k2 : Key2.t) =
      (Key1.to_rank k1) * Key2.n + Key2.to_rank k2
      |> Array.nget table
    
  end