(** Chunked byte vector *)
type t

(** [create length] creates a chunked byte vector that has capacity for [length]
    bytes. *)
val create : int64 -> t

(** [of_string str] creates a chunked byte vector from the given [str]. *)
val of_string : string -> t

(** [of_bytes bytes] creates a chunked byte vector from the given [bytes]. The
    underlying memory is effectively copied - further modifications to [bytes]
    are not reflected in the chunked byte vector. Use this over [of_string] when
    turning your [bytes] into a [string] would be potentially expensive. *)
val of_bytes : bytes -> t

(** [grow vector length_delta] increases the byte vector length by
    [length_delta]. *)
val grow : t -> int64 -> unit

(** [length vector] returns the length of [vector] in bytes. *)
val length : t -> int64

(** [load_byte vector offset] read the byte at [offset]. *)
val load_byte : t -> int64 -> int

(** [store_byte vector offset byte] set the byte at [offset] to [byte]. *)
val store_byte : t -> int64 -> int -> unit

(** [store_bytes vector offset bytes] set the bytes from [offset] to the given
    [bytes]. *)
val store_bytes : t -> int64 -> bytes -> unit

module Internal_for_tests : sig
  val page_size : int64

  val num_pages : int64 -> int64
end
