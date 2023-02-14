module type Elt_sig = sig
  type t

  val size : int
  val allocate : unit -> t
end

module type Carray_sig = sig
  type t = Bigstringaf.t
  type elt

  val t : t Repr.t

  val allocate : int -> t
  (** [allocate len] creates a C array of size [len] initialized with zeros. *)

  val erase : t -> int -> unit
  (** [erase p n] fills with zeros the first [n] elements of [p]. *)

  val length : t -> int
  (** [length c] returns the length of a C array [c] *)

  val get : t -> int -> elt
  (** [get c i] returns the [i]-th element of a C array [c] *)

  val set : t -> elt -> int -> unit

  val copy : ?offset:int -> ?len:int -> t -> t
  (** [copy c] copies [len] elements from a C array [c] starting from position [offset] *)

  val blit : t -> src_off:int -> t -> dst_off:int -> len:int -> unit
  (** [blit src src_off dst dst_off len] copies [len] elements from [src] to
      [dst] starting at the respective offsets. *)

  val to_array : ?offset:int -> ?len:int -> t -> elt array
  (** [to_array c] converts a C array [c] to an OCaml array *)

  val of_array : elt array -> t
  (** [of_array c] converts an OCaml array [c] to a C array *)
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
    external get : elt -> t -> int -> int -> unit
      = "caml_polynomial_carray_get_stubs"
      [@@noalloc]
    (** [get elt p i size] copies the [i]-th element of a given array [p] in [elt],
        assuming elements of [size] bytes.
        - requires: [0 <= i < size p]
        - ensures: [elt = p[i]] *)

    external set : t -> elt -> int -> int -> unit
      = "caml_polynomial_carray_set_stubs"
      [@@noalloc]
    (** [set p elt i size] copies [elt] in the [i]-th element of [p],
        assuming elements of [size] bytes.
        - requires: [0 <= i < size p]
        - ensures: [elt = p[i]] *)

    external memset_zero : t -> int -> unit
      = "caml_polynomial_memset_zero_stubs"
      [@@noalloc]
    (** [memset_zero p n] writes [n] bytes of zeros in [p]
        - requires: [n <= size p] *)
  end

  let t : t Repr.t =
    Repr.(
      map string
        (fun s -> Bigstringaf.of_string ~off:0 ~len:(String.length s) s)
        Bigstringaf.to_string)

  let length a = Bigstringaf.length a / Elt.size

  let allocate n =
    if n < 1 then raise @@ Invalid_argument "allocate: size should be >= 1";
    let size = Elt.size * n in
    let res = Bigstringaf.create size in
    Stubs.memset_zero res size;
    res

  let erase p n =
    if n < 0 || n > length p then
      raise @@ Invalid_argument "erase: invalid length";
    Stubs.memset_zero p (Elt.size * n)

  let get p i =
    if i < 0 || i >= length p then
      raise @@ Invalid_argument "get: index out of bounds";
    let res = Elt.allocate () in
    Stubs.get res p i Elt.size;
    res

  let set p fr i =
    if i < 0 || i >= length p then
      raise @@ Invalid_argument "set: index out of bounds";
    Stubs.set p fr i Elt.size

  let to_array ?(offset = 0) ?len p =
    let len = Option.value ~default:(length p - offset) len in
    if len < 0 || offset < 0 || length p - offset < len then
      raise
      @@ Invalid_argument
           (Format.sprintf "to_array: invalid len %d or offset %d for size %d"
              len offset (length p));
    Array.init len (fun i -> get p (offset + i))

  let of_array caml_array =
    let n = Array.length caml_array in
    let res = allocate n in
    Array.iteri (fun i g -> set res g i) caml_array;
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
           (Format.sprintf "copy: invalid len %d or offset %d for size %d" len
              offset (length p));
    let res = allocate len in
    blit p ~src_off:offset res ~dst_off:0 ~len;
    res
end
