(** Chunked byte vector *)
type t

(** [create length] creates a chunked byte vector that has capacity for [length]
    bytes. *)
val create : int64 -> t

(** [of_string str] creates a chunked byte vector from the given [str]. *)
val of_string : string -> t

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
