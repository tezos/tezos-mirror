(** Index

    [Index] is a scalable implementation of persistent indices in OCaml.

    [Index] provides the standard key-value interface: [find], [mem] and
    [replace]. It requires three IO instances:

    - A `log` IO containing all of the recently-added bindings; this is also
      kept in memory.

    - When the `log` IO is full, it is merged into the `index` IO. Search
      is done first in `log` then in `index`, which makes recently added
      bindings search faster.

    - A `lock` IO to ensure safe concurrent access.
*)

(** The input of [Make] for keys. *)
module type Key = sig
  type t
  (** The type for keys. *)

  val equal : t -> t -> bool
  (** The equality function for keys. *)

  val hash : t -> int
  (** Note: Unevenly distributed hash functions may result in performance
      drops. *)

  val hash_size : int
  (** The maximum number of bits used to encode hashes. `Hashtbl.hash` uses 30
      bits. *)

  val encode : t -> string
  (** [encode] is an encoding function. The resultant encoded values must have
      size {!encoded_size} bytes. *)

  val decode : string -> int -> t
  (** [decode] is a decoding function such that [decode (encode t) 0 = t]. *)

  val encoded_size : int
  (** [encoded_size] is the size of the encoded keys, expressed in number of
      bytes. *)

  val pp : t Fmt.t
  (** Formatter for keys *)
end

(** The input of [Make] for values. The same requirements as for [Key]
    apply. *)
module type Value = sig
  type t

  val encode : t -> string

  val decode : string -> int -> t

  val encoded_size : int

  val pp : t Fmt.t
end

module type IO = Io.S

exception RO_not_allowed
(** The exception raised when illegal operation is attempted on a read_only
    index. *)

(** Index module signature. *)
module type S = sig
  type t
  (** The type for indexes. *)

  type key
  (** The type for keys. *)

  type value
  (** The type for values. *)

  val v : ?fresh:bool -> ?readonly:bool -> log_size:int -> string -> t
  (** The constructor for indexes.
      @param fresh
      @param read_only whether read-only mode is enabled for this index.
      @param log_size  the maximum number of bindings in the `log` IO.
      *)

  val clear : t -> unit
  (** [clear t] clears [t] so that there are no more bindings in it. *)

  val find : t -> key -> value
  (** [find t k] is the binding of [k] in [t]. *)

  val mem : t -> key -> bool
  (** [mem t k] is [true] iff [k] is bound in [t]. *)

  exception Invalid_key_size of key

  exception Invalid_value_size of value
  (** The exceptions raised when trying to add a key or a value of different
      size than encoded_size *)

  val replace : t -> key -> value -> unit
  (** [replace t k v] binds [k] to [v] in [t], replacing any exising binding
      of [k]. *)

  val iter : (key -> value -> unit) -> t -> unit
  (** Iterates over the index bindings. Order is not specified.
      In case of recent replacements of existing values (after the last merge),
      this will hit both the new and old bindings. *)

  val flush : t -> unit
  (** Flushes all buffers to the disk. *)

  val close : t -> unit
  (** Closes the files and clears the caches of [t]. *)

  val force_merge : t -> key -> value -> unit
  (** [force_merge t k v] forces a merge for [t], where [k] and [v] are any key
      and value of [t]. *)
end

module Make (K : Key) (V : Value) (IO : IO) :
  S with type key = K.t and type value = V.t

(** These modules should not be used. They are exposed purely for testing purposes. *)
module Private : sig
  module Search : module type of Search

  module Io_array : module type of Io_array

  module Fan : module type of Fan
end
