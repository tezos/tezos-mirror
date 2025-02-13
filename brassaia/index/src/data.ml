open! Import

exception Invalid_size of string
(** The exception raised when trying to encode a key or a value of size other
    than encoded_size *)

module type Key = sig
  type t [@@deriving repr]
  (** The type for keys. *)

  val equal : t -> t -> bool
  (** The equality function for keys. *)

  val hash : t -> int
  (** Note: Unevenly distributed hash functions may result in performance drops. *)

  val hash_size : int
  (** The number of bits necessary to encode the maximum output value of
      [hash]. [Hashtbl.hash] uses 30 bits.

      Overestimating the [hash_size] will result in performance drops;
      underestimation will result in undefined behavior. *)

  val encode : t -> string
  (** [encode] is an encoding function. The resultant encoded values must have
      size {!encoded_size}. *)

  val encoded_size : int
  (** [encoded_size] is the size of the result of {!encode}, expressed in number
      of bytes. *)

  val decode : string -> int -> t
  (** [decode s off] is the decoded form of the encoded value at the offset
      [off] of string [s]. Must satisfy [decode (encode t) 0 = t]. *)
end

module type Value = sig
  type t [@@deriving repr]

  val encode : t -> string
  val encoded_size : int
  val decode : string -> int -> t
end

module Entry = struct
  module type S = sig
    type key
    type value

    type t = private { key : key; key_hash : int; value : value }
    [@@deriving repr]

    val v : key -> value -> t
    val encoded_size : int
    val encoded_sizeL : int63
    val decode : string -> int -> t
    val decode_key : string -> int -> key * int
    val decode_value : string -> int -> value
    val encode : t -> (string -> unit) -> unit
    val encode' : key -> value -> (string -> unit) -> unit
    val compare : t -> t -> int
    (* Compare entries by their key hash. *)
  end

  module Make (K : Key) (V : Value) :
    S with type key := K.t and type value := V.t = struct
    type t = { key : K.t; key_hash : int; value : V.t } [@@deriving repr]

    let v key value = { key; key_hash = K.hash key; value }
    let encoded_size = K.encoded_size + V.encoded_size
    let encoded_sizeL = Int63.of_int encoded_size

    let decode string off =
      let key = K.decode string off in
      let value = V.decode string (off + K.encoded_size) in
      { key; key_hash = K.hash key; value }

    let decode_key string off =
      let key = K.decode string off in
      (key, K.hash key)

    let decode_value string off = V.decode string (off + K.encoded_size)

    let encode' key value f =
      let encoded_key = K.encode key in
      let encoded_value = V.encode value in
      if String.length encoded_key <> K.encoded_size then
        raise (Invalid_size encoded_key);
      if String.length encoded_value <> V.encoded_size then
        raise (Invalid_size encoded_value);
      f (encoded_key ^ encoded_value)

    let encode { key; value; _ } f = encode' key value f
    let compare a b = Int.compare a.key_hash b.key_hash
  end
end

module String_fixed (L : sig
  val length : int
end) : sig
  type t = string

  include Key with type t := string
  include Value with type t := string
end = struct
  type t = string [@@deriving repr]

  let hash = Hashtbl.hash
  let hash_size = 30
  let encode s = s
  let decode s off = String.sub s off L.length
  let encoded_size = L.length
  let equal = String.equal
end
