module type Elt_sig = sig
  type t

  val size : int

  val allocate : unit -> t

  val zero : t

  val eq : t -> t -> bool
end

module type Carray_sig = sig
  type t

  type elt

  val t : t Repr.t

  val empty : t

  (** [allocate len] creates a C array of size [len] initialized with zeros. *)
  val allocate : int -> t

  (** [init n f] returns a fresh C array of length [n], with element number [i]
      initialized to the result of [f i]. *)
  val init : int -> (int -> elt) -> t

  (** [degree p] returns the index of the last non-zero element of [p].
      Returns -1 if all elements of [p] are zero. *)
  val degree : t -> int

  (** [erase p n] fills with zeros the first [n] elements of [p]. *)
  val erase : t -> int -> unit

  (** [length c] returns the length of a C array [c] *)
  val length : t -> int

  (** [get c i] returns the [i]-th element of a C array [c] *)
  val get : t -> int -> elt

  (** [get_inplace c i res] copies the [i]-th element of a C array [c] in res *)
  val get_inplace : t -> int -> elt -> unit

  (** [iter_copy_elt f a] applies function [f] in turn to a **copy** of all
   the elements of [a].  It is equivalent to
   [f a.(0); f a.(1); ...; f a.(length a - 1); ()]. *)
  val iter_copy_elt : (elt -> unit) -> t -> unit

  (** Same as {!iter_copy_elt}, but the
   function is applied to the index of the element as first argument,
   and a **copy** of the element itself as second argument. *)
  val iteri_copy_elt : (int -> elt -> unit) -> t -> unit

  val set : t -> elt -> int -> unit

  (** [copy c] copies [len] elements from a C array [c] starting from position [offset] *)
  val copy : ?offset:int -> ?len:int -> t -> t

  (** [blit src src_off dst dst_off len] copies [len] elements from [src] to
      [dst] starting at the respective offsets. *)
  val blit : t -> src_off:int -> t -> dst_off:int -> len:int -> unit

  (** [equal a offset1 b offset2] returns true if the segments of [len] elements of
      [a] and [b] are equal starting from their respective offsets [offset1] (for [a])
      and [offset2] (for [b]). *)
  val equal : t -> offset1:int -> t -> offset2:int -> len:int -> bool

  (** [to_array c] converts a C array [c] to an OCaml array *)
  val to_array : ?offset:int -> ?len:int -> t -> elt array

  (** [of_array c] converts an OCaml array [c] to a C array *)
  val of_array : elt array -> t

  val of_bigstring : Bigstringaf.t -> t

  val to_bigstring : t -> Bigstringaf.t

  (** [eq a b] returns true if C arrays [a] and [b] are equals *)
  val eq : t -> t -> bool

  (** [sub a off len] extracts a sub-array of [a] *)
  val sub : t -> off:int -> len:int -> t

  (** [fold_left_map] is a combination of fold_left and map that threads an
    accumulator through calls to [f]. *)
  val fold_left_map : ('acc -> elt -> 'acc * elt) -> 'acc -> t -> 'acc * t
end

(** Note that an unsafe type casting is performed by this module.
    USE WITH CARE!
    The type of get and set changes with whatever Elt.t is given as input.
    This works because Elt also indicates the size in bytes which would be
    read/written and because Elt is assumed to be backed by a custom block
    (it is accessed with [Data_custom_val]). *)
module Make (Elt : Elt_sig) : Carray_sig with type elt = Elt.t = struct
  type t = Bigstringaf.t

  type elt = Elt.t

  module Stubs = struct
    (** [get elt p i size] copies the [i]-th element of a given array [p] in [elt],
        assuming elements of [size] bytes.
        - requires: [0 <= i < size p]
        - ensures: [elt = p[i]] *)
    external get : elt -> t -> int -> int -> unit
      = "caml_bls12_381_polynomial_internal_polynomial_carray_get_stubs"
      [@@noalloc]

    (** [set p elt i size] copies [elt] in the [i]-th element of [p],
        assuming elements of [size] bytes.
        - requires: [0 <= i < size p]
        - ensures: [elt = p[i]] *)
    external set : t -> elt -> int -> int -> unit
      = "caml_bls12_381_polynomial_internal_polynomial_carray_set_stubs"
      [@@noalloc]

    (** [memset_zero p n] writes [n] bytes of zeros in [p]
        - requires: [n <= size p] *)
    external memset_zero : t -> int -> unit
      = "caml_bls12_381_polynomial_internal_polynomial_memset_zero_stubs"
      [@@noalloc]
  end

  let t : t Repr.t =
    Repr.(
      map
        string
        (fun s -> Bigstringaf.of_string ~off:0 ~len:(String.length s) s)
        Bigstringaf.to_string)

  let length a = Bigstringaf.length a / Elt.size

  let empty = Bigstringaf.empty

  let allocate n =
    if n < 1 then raise @@ Invalid_argument "allocate: size should be >= 1" ;
    let size = Elt.size * n in
    let res = Bigstringaf.create size in
    Stubs.memset_zero res size ;
    res

  let init n f =
    if n < 1 then raise @@ Invalid_argument "init: size should be >= 1" ;
    let res = Bigstringaf.create (Elt.size * n) in
    for i = 0 to n - 1 do
      Stubs.set res (f i) i Elt.size
    done ;
    res

  let sub t ~off ~len = Bigstringaf.sub t ~off ~len:(len * Elt.size)

  let erase p n =
    if n < 0 || n > length p then
      raise @@ Invalid_argument "erase: invalid length" ;
    Stubs.memset_zero p (Elt.size * n)

  let get p i =
    if i < 0 || i >= length p then
      raise @@ Invalid_argument "get: index out of bounds" ;
    let res = Elt.allocate () in
    Stubs.get res p i Elt.size ;
    res

  let get_unsafe p i =
    let res = Elt.allocate () in
    Stubs.get res p i Elt.size ;
    res

  (* TODO: implement loop in C
     See https://gitlab.com/nomadic-labs/cryptography/privacy-team/-/issues/215 *)
  let degree a =
    let rec aux i =
      if i = -1 then -1
      else if Elt.eq (get a i) Elt.zero then aux (i - 1)
      else i
    in
    aux (length a - 1)

  let get_inplace p i res =
    if i < 0 || i >= length p then
      raise @@ Invalid_argument "get: index out of bounds" ;
    Stubs.get res p i Elt.size

  let iter_copy_elt f a =
    for i = 0 to length a - 1 do
      f (get_unsafe a i)
    done

  let iteri_copy_elt f a =
    for i = 0 to length a - 1 do
      f i (get_unsafe a i)
    done

  let set p fr i =
    if i < 0 || i >= length p then
      raise @@ Invalid_argument "set: index out of bounds" ;
    Stubs.set p fr i Elt.size

  let to_array ?(offset = 0) ?len p =
    let len = Option.value ~default:(length p - offset) len in
    if len < 0 || offset < 0 || length p - offset < len then
      raise
      @@ Invalid_argument
           (Format.sprintf
              "to_array: invalid len %d or offset %d for size %d"
              len
              offset
              (length p)) ;
    Array.init len (fun i -> get p (offset + i))

  let of_array caml_array =
    let n = Array.length caml_array in
    let res = allocate n in
    Array.iteri (fun i g -> set res g i) caml_array ;
    res

  let blit src ~src_off dst ~dst_off ~len =
    let src_off = src_off * Elt.size in
    let dst_off = dst_off * Elt.size in
    let len = len * Elt.size in
    Bigstringaf.blit src ~src_off dst ~dst_off ~len

  let copy ?(offset = 0) ?len p =
    let len = Option.value ~default:(length p - offset) len in
    if len < 0 || offset < 0 || length p - offset < len then
      raise
      @@ Invalid_argument
           (Format.sprintf
              "copy: invalid len %d or offset %d for size %d"
              len
              offset
              (length p)) ;
    let res = allocate len in
    blit p ~src_off:offset res ~dst_off:0 ~len ;
    res

  let equal a ~offset1 b ~offset2 ~len =
    Bigstringaf.memcmp a offset1 b offset2 (len * Elt.size) = 0

  let eq a b =
    let len_a = length a in
    let len_b = length b in
    if len_a <> len_b then false else equal a ~offset1:0 b ~offset2:0 ~len:len_a

  let of_bigstring b = b

  let to_bigstring b = b

  let fold_left_map f acc input_array =
    let len = length input_array in
    if len < 1 then raise @@ Invalid_argument "allocate: size should be >= 1"
    else
      let acc, elt = f acc (get_unsafe input_array 0) in
      let output_array = allocate len in
      set output_array elt 0 ;
      let acc = ref acc in
      for i = 1 to len - 1 do
        let acc', elt = f !acc (get_unsafe input_array i) in
        acc := acc' ;
        set output_array elt i
      done ;
      (!acc, output_array)
end
