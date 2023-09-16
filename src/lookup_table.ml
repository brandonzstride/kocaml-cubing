open Core

module type Key =
  sig
    type t
    val to_rank : t -> int
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

    let create (ls : R.t list) =
      assert (List.length ls = Key.n);
      Array.of_list ls

    let lookup (table : t) (k : Key.t) =
      Array.nget table (Key.to_rank k)
    
  end

module Make2D (Key1 : Key) (Key2 : Key) (R : Return_type) =
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