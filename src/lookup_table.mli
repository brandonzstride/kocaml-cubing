(*
  File: lookup_table.mli
  Module purpose: data structure for fast mappings on ranked types.
  Status: complete.

  Detailed description:
    A lookup table is a data structure that allows fast lookup of ranked types.
    They use arrays, but the mutation is behind-the-scenes and the table is
    only mutated upon creation. The user can treat the table as if it is
    immutable.

  Expected usage:
    * Map coordinate and move to a new coordinate
    * Map coordinate and symmetry to new coordinate
    * Map representative symmetry coordinate and move to new symmetry coordinate
    * Map two symmetries to a new symmetry
    * Store pruning tables.

    The domain must have a `to_rank` function that takes an element of the domain
    to a unique integer.

  Dependencies:
    None

*)

module type Key =
  sig
    type t
    val to_rank : t -> int
    (** [to_rank x] sends the key [x] to a unique integer such that there is
        no key [y] not equal to [x] where [to_rank x = to_rank y]. *)
  end

module type Return_type =
  sig
    type t [@@deriving sexp]
  end

module One_dim :
  sig
    module Make (Key : Key) (R : Return_type) :
      sig
        type t

        val from_file : string -> t
        (** [from_file filepath] loads the lookup table from the [filepath], supposing it
            was saved using [to_file] below. *)

        val to_file : t -> string -> unit
        (** [to_file tbl filepath] saves the [tbl] at the given [filepath] such that it is
            readable back into a table using [from_file] above. *)

        val of_list : R.t list -> t
        (** [of_list ls] creates a lookup table such that the i'th element in [ls] can be
            looked up with a key of rank i.*)

        val create : Key.t list -> f:(Key.t -> R.t) -> t
        (** [create ls ~f] creates a lookup key from a list of keys [ls], where the lists are
            sorted by rank. Each key is mapped to a return type using [f].
            
            Note: it is not asserted that the list is sorted by rank. It is up to the user to
            ensure that the list is sorted. *)

        val lookup : t -> Key.t -> R.t
        (** [lookup tbl key] uses the [tbl] to find the returned value of [key]. *)

        val get_size : t -> int
        (** [get_size tbl] gets the size (i.e. length) of the lookup table. *)
      end
  end

module Two_dim :
  sig
    module Make (Key1 : Key) (Key2 : Key) (R : Return_type) :
      sig
        type t

        val from_file : string -> t
        (** [from_file filepath] loads the lookup table from the [filepath], supposing it
            was saved using [to_file] below. *)

        val to_file : t -> string -> unit
        (** [to_file tbl filepath] saves the [tbl] at the given [filepath] such that it is
            readable back into a table using [from_file] above. *)

        (* Given lists *must* be sorted by rank. This is not asserted, and it is up to the user to assert *)
        val create : ?n1:int -> ?n2:int -> Key1.t list -> Key2.t list -> f:(Key1.t -> Key2.t -> R.t) -> t
        (** [create ?n1 ?n2 ls1 ls2 ~f] creates a lookup table from the lists of keys [ls1] and [ls2], where
            the lists of are length [n1] and [n2] respectively (which are optionally provided). The keys are
            mapped to the returned value using [f].
            
            Note: it is not asserted that the lists are sorted by rank. It is up to the user to
            ensure that the lists are sorted. *)

        val lookup : t -> Key1.t -> Key2.t -> R.t
        (** [lookup tbl key1 key2] looks up the value associated with both keys [key1] and [key2]
            in the given [tbl]. *)

        val get_size : t -> int
        (** [get_size tbl] gets the total length (i.e. number of elements) in the [tbl].
            
            Note: see [create] above. [get_size tbl] is then [n1 * n2]. *)
      end
  end