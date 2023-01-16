(** This module implements a generic tick model. These values were benchmarked
    from `MemoryCopy`, `MemoryFill` and `MemoryInit`. *)

type tick = Z.t

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
