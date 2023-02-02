open Proto_compat

type tick = Z.t

let zero = Z.zero

let one = Z.one

let of_int32 i = if Compare.Int32.(i < 0l) then None else Some (Z.of_int32 i)

let of_int32_exn i =
  match of_int32 i with
  | Some i -> i
  | None -> raise (Invalid_argument "Ticks cannot be negative")

let to_int32 t = try Some (Z.to_int32 t) with Z.Overflow -> None

let to_int32_exn t =
  match to_int32 t with
  | Some i -> i
  | None ->
      raise
        (Invalid_argument
           "Ticks cannot be contained in the signed int32 representation")

let of_int64 i = if Compare.Int64.(i < 0L) then None else Some (Z.of_int64 i)

let of_int64_exn i =
  match of_int64 i with
  | Some i -> i
  | None -> raise (Invalid_argument "Ticks cannot be negative")

let to_int64 t = try Some (Z.to_int64 t) with Z.Overflow -> None

let to_int64_exn t =
  match to_int64 t with
  | Some i -> i
  | None ->
      raise
        (Invalid_argument
           "Ticks cannot be contained in the signed int64 representation")

let to_z t = t

let of_z z = if Compare.Z.(z < Z.zero) then None else Some z

let ( + ) = Z.add

let ( * ) = Z.mul

let nop = Z.zero

(* This value has been benchmarked from MemoryCopy. *)
let ticks_per_byte_copied = Z.of_int 48

(* This value has been benchmarked from MemoryFill and MemoryInit. *)
let ticks_per_byte_read = Z.of_int 42

(* This value has been benchmarked from MemoryFill and MemoryInit. *)
let ticks_per_byte_written = Z.of_int 42
