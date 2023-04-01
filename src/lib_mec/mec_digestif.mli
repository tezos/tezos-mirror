(**
   This is a fork from {{:https://github.com/mirage/digestif } Digestif },
   commit 891907675ead09b5e1c5de28c24246a0e411a546, adding personalisation to
   Blake2.

   When {{: https://github.com/mirage/digestif/issues/111 } this issue } is
   solved, this fork can be removed.
*)

type bigstring =
  ( char,
    Bigarray_compat.int8_unsigned_elt,
    Bigarray_compat.c_layout )
  Bigarray_compat.Array1.t

(** A general (inner) iterator. It applies the provided function to a collection
    of elements. For instance:

    - [let iter_k : 'a -> 'a iter = fun x f -> f x]
    - [let iter_pair : 'a * 'a -> 'a iter = fun (x, y) -> f x; f y]
    - [let iter_list : 'a list -> 'a iter = fun l f -> List.iter f l] *)
type 'a iter = ('a -> unit) -> unit

type 'a compare = 'a -> 'a -> int

type 'a equal = 'a -> 'a -> bool

type 'a pp = Format.formatter -> 'a -> unit

module type S = sig
  (** Size of hash results, in bytes. *)
  val digest_size : int

  type ctx

  type t

  (** An empty hash context. *)
  val empty : ctx

  (** Create a new hash state. *)
  val init : ?personalisation:Bytes.t -> unit -> ctx

  (** [feed_bytes msg t] adds informations in [msg] to [t]. [feed] is analogous
      to appending: [feed (feed t msg1) msg2 = feed t (append msg1 msg2)] *)
  val feed_bytes : ctx -> ?off:int -> ?len:int -> Bytes.t -> ctx

  (** Same as {!feed_bytes} but for {!String.t}. *)
  val feed_string : ctx -> ?off:int -> ?len:int -> String.t -> ctx

  (** Same as {!feed_bytes} but for {!bigstring}. *)
  val feed_bigstring : ctx -> ?off:int -> ?len:int -> bigstring -> ctx

  (** [feedi_bytes t iter = let r = ref t in iter (fun msg -> r := feed !r msg);
      !r] *)
  val feedi_bytes : ctx -> Bytes.t iter -> ctx

  (** Same as {!feed_bytes} but for {!String.t}. *)
  val feedi_string : ctx -> String.t iter -> ctx

  (** Same as {!feed_bytes} but for {!bigstring}. *)
  val feedi_bigstring : ctx -> bigstring iter -> ctx

  (** [get t] is the digest corresponding to [t]. *)
  val get : ctx -> t

  (** [digest_bytes msg] is the digest of [msg].

      [digest_bytes msg = get (feed_bytes empty msg)]. *)
  val digest_bytes : ?off:int -> ?len:int -> Bytes.t -> t

  (** Same as {!digest_bytes} but for a {!String.t}. *)
  val digest_string : ?off:int -> ?len:int -> String.t -> t

  (** Same as {!digest_bytes} but for a {!bigstring}. *)
  val digest_bigstring : ?off:int -> ?len:int -> bigstring -> t

  (** [digesti_bytes iter = feedi_bytes empty iter |> get]. *)
  val digesti_bytes : Bytes.t iter -> t

  (** Same as {!digesti_bytes} but for {!String.t}. *)
  val digesti_string : String.t iter -> t

  (** Same as {!digesti_bigstring} but for {!bigstring}. *)
  val digesti_bigstring : bigstring iter -> t

  (** Specialization of {!digesti_bytes} with a list of {!Bytes.t} (see
      {!iter}). *)
  val digestv_bytes : Bytes.t list -> t

  (** Same as {!digestv_bytes} but for {!String.t}. *)
  val digestv_string : String.t list -> t

  (** Same as {!digestv_bytes} but for {!bigstring}. *)
  val digestv_bigstring : bigstring list -> t

  (** [unsafe_compare] function returns [0] on equality and a negative/positive
      [int] depending on the difference (like {!String.compare}). This is
      usually OK, but this is not constant time, so in some cases it could leak
      some information. *)
  val unsafe_compare : t compare

  (** The equal (constant-time) function for {!t}. *)
  val equal : t equal

  (** Pretty-printer of {!t}. *)
  val pp : t pp

  (** [of_hex] tries to parse an hexadecimal representation of {!t}. [of_hex]
      raises an [invalid_argument] when input is malformed. We take only firsts
      {!digest_size} hexadecimal values and ignore rest of input. If it has not
      enough hexadecimal values, trailing values of the output hash are zero
      ([\x00]), *)
  val of_hex : string -> t

  (** [of_hex] tries to parse an hexadecimal representation of {!t}. [of_hex]
      returns [None] when input is malformed. We take only first {!digest_size}
      hexadecimal values and ignore rest of input. If it has not enough
      hexadecimal values, trailing values of the output hash are zero ([\x00]). *)
  val of_hex_opt : string -> t option

  (** [consistent_of_hex] tries to parse an hexadecimal representation of {!t}.
      [consistent_of_hex] raises an [invalid_argument] when input is malformed.
      However, instead {!of_hex}, [consistent_of_hex] expects exactly
      [{!digest_size} * 2] hexadecimal values (but continues to ignore
      whitespaces). *)
  val consistent_of_hex : string -> t

  (** [consistent_of_hex_opt] tries to parse an hexadecimal representation of
      {!t}. [consistent_of_hex] returns [None] when input is malformed. However,
      instead {!of_hex}, [consistent_of_hex] expects exactly
      [{!digest_size} * 2] hexadecimal values (but continues to ignore
      whitespaces). *)
  val consistent_of_hex_opt : string -> t option

  (** [to_hex] makes a hex-decimal representation of {!t}. *)
  val to_hex : t -> string

  (** [of_raw_string s] see [s] as a hash. Useful when reading serialized
      hashes. *)
  val of_raw_string : string -> t

  (** [of_raw_string_opt s] see [s] as a hash. Useful when reading serialized
      hashes. Returns [None] if [s] is not the {!digest_size} bytes long. *)
  val of_raw_string_opt : string -> t option

  (** [to_raw_string s] is [(s :> string)]. *)
  val to_raw_string : t -> string
end

module BLAKE2B : sig
  include S
end

module BLAKE2S : sig
  include S
end

module Make_BLAKE2B (D : sig
  val digest_size : int
end) : S

module Make_BLAKE2S (D : sig
  val digest_size : int
end) : S

type 'k hash = BLAKE2B : BLAKE2B.t hash | BLAKE2S : BLAKE2S.t hash

val blake2b : BLAKE2B.t hash

val blake2s : BLAKE2S.t hash

type 'kind t

val module_of : 'k hash -> (module S with type t = 'k)

val digest_bytes : 'k hash -> Bytes.t -> 'k t

val digest_string : 'k hash -> String.t -> 'k t

val digest_bigstring : 'k hash -> bigstring -> 'k t

val digesti_bytes : 'k hash -> Bytes.t iter -> 'k t

val digesti_string : 'k hash -> String.t iter -> 'k t

val digesti_bigstring : 'k hash -> bigstring iter -> 'k t

val pp : 'k hash -> 'k t pp

val equal : 'k hash -> 'k t equal

val unsafe_compare : 'k hash -> 'k t compare

val to_hex : 'k hash -> 'k t -> string

val of_hex : 'k hash -> string -> 'k t

val of_hex_opt : 'k hash -> string -> 'k t option

val consistent_of_hex : 'k hash -> string -> 'k t

val consistent_of_hex_opt : 'k hash -> string -> 'k t option

val of_raw_string : 'k hash -> string -> 'k t

val of_raw_string_opt : 'k hash -> string -> 'k t option

val to_raw_string : 'k hash -> 'k t -> string

val of_digest : (module S with type t = 'hash) -> 'hash -> 'hash t

val of_blake2b : BLAKE2B.t -> BLAKE2B.t t

val of_blake2s : BLAKE2S.t -> BLAKE2S.t t
