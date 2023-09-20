(** This module implements a generic tick model. These values were benchmarked
    from `MemoryCopy`, `MemoryFill` and `MemoryInit`. *)

type tick

(* Tick of cost `0`. *)
val zero : tick

(* Tick of cost `1`. *)
val one : tick

(** [of_int32 i] returns the tick representation of [i], or [None] if [i]
    is negative.
*)
val of_int32 : int32 -> tick option

(** [of_int32_exn i] returns the tick representation of [i].

    @raise Invalid_argument if [i] is negative.
*)
val of_int32_exn : int32 -> tick

(** [to_int32 t] returns the signed int32 representation of [t] or [None] if [t]
    overflows the signed int32 representation.
*)
val to_int32 : tick -> int32 option

(** [to_int32_exn t] returns the signed int32 representation of [t].

    @raise Invalid_argument if [t] overflows the signed int32 representation.
*)
val to_int32_exn : tick -> int32

(** [of_int64 i] returns the tick representation of [i], or [None] if [i]
    is negative.
*)
val of_int64 : int64 -> tick option

(** [of_int64_exn i] returns the tick representation of [i].

    @raise Invalid_argument if [i] is negative.
*)
val of_int64_exn : int64 -> tick

(** [to_int64 t] returns the signed int64 representation of [t] or [None] if [t]
    overflows the signed int64 representation.
*)
val to_int64 : tick -> int64 option

(** [to_int64_exn t] returns the signed int64 representation of [t].

    @raise Invalid_argument if [t] overflows the signed int64 representation.
*)
val to_int64_exn : tick -> int64

(* [of_z z] returns the tick representation of [z], or [None] if [z]
    is negative. *)
val of_z : Z.t -> tick option

(* [to_z t] returns the Z representation of a tick counter [t]. *)
val to_z : tick -> Z.t

(** [t1 + t2] returns the sum of [t1] and [t2] *)
val ( + ) : tick -> tick -> tick

(** [t1 * t2] returns the product of [t1] and [t2] *)
val ( * ) : tick -> tick -> tick

(** [nop] consumes zero tick. This is used specifically for
    `write_debug`. *)
val nop : tick

(** [ticks_per_byte_copied] is the consumption for a byte read in the memory and
    written in the memory in the context of reading a chunk of bytes. *)
val ticks_per_byte_copied : tick

(** [ticks_per_byte_read] is the consumption for a byte read from the memory in
    the context of reading a chunk of bytes. *)
val ticks_per_byte_read : tick

(** [ticks_per_byte_written] is the consumption for a byte written in the
    memory in the context of writing a chunk of bytes. *)
val ticks_per_byte_written : tick
