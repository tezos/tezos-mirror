type tick = Z.t

let ( + ) = Z.add

let ( * ) = Z.mul

let nop = Z.zero

(* This value has been benchmarked from MemoryCopy. *)
let ticks_per_byte_copied = Z.of_int 48

(* This value has been benchmarked from MemoryFill and MemoryInit. *)
let ticks_per_byte_read = Z.of_int 42

(* This value has been benchmarked from MemoryFill and MemoryInit. *)
let ticks_per_byte_written = Z.of_int 42
