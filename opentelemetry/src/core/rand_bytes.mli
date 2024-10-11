(** Generate random identifiers.

    We need random identifiers for trace IDs and span IDs. *)

val rand_bytes_16 : (unit -> bytes) ref
(** Generate 16 bytes of random data.
   The implementation can be swapped to use any random generator. *)

val rand_bytes_8 : (unit -> bytes) ref
(** Generate 16 bytes of random data.
   The implementation can be swapped to use any random generator. *)

val default_rand_bytes_8 : unit -> bytes
(** Default implementation using {!Random} *)

val default_rand_bytes_16 : unit -> bytes
(** Default implementation using {!Random} *)
