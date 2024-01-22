open Bigarray
open Lib.Bigarray
open Types
open Values

type size = int32 (* number of pages *)

type address = int32

type offset = int32

type count = int32

(* Expose exception types *)
exception Type

exception SizeLimit

exception OutOfMemory

exception Bounds

exception SizeOverflow

(* Copied from [Memory] module *)
let page_size = 0x10000L (* 64 KiB *)

module Chunked = struct
  module Backend = Chunked_byte_vector

  type t = Backend.t

  let length_from_pages pages = Int64.(mul (of_int32 pages) page_size)

  let create pages = Backend.allocate (length_from_pages pages)

  let grow delta_pages pages =
    Backend.grow pages (length_from_pages delta_pages)

  let total_size = Backend.length

  let total_pages vector = Int64.(div (total_size vector) page_size |> to_int32)

  let load_byte = Backend.load_byte

  let store_byte = Backend.store_byte
end

let reraise = function
  | Chunked_byte_vector.Bounds | Lazy_vector.Bounds -> raise Bounds
  | Chunked_byte_vector.SizeOverflow | Lazy_vector.SizeOverflow ->
      raise SizeOverflow
  | exn -> Lwt.reraise exn

type memory = {mutable ty : memory_type; content : Chunked.t}

(* Copied from [Memory] module *)
type t = memory

let content {content; _} = content

let max_pages = 0x10000l

(* Copied from [Memory] module *)
let valid_limits {min; max} =
  match max with None -> true | Some m -> I32.le_u min m

(* Copied from [Memory] module *)
let _create n =
  if I32.gt_u n max_pages then raise SizeOverflow
  else
    try
      let size = Int64.(mul (of_int32 n) page_size) in
      let mem = Array1_64.create Int8_unsigned C_layout size in
      Array1.fill mem 0 ;
      mem
    with Out_of_memory -> raise OutOfMemory

(* Copied from [Memory] module *)
let alloc (MemoryType lim as ty) =
  if not (valid_limits lim) then raise Type ;
  {ty; content = Chunked.create lim.min}

let of_chunks (MemoryType lim as ty) content =
  if not (valid_limits lim) then raise Type ;
  {ty; content}

let bound mem = Chunked.total_size mem.content

let size mem = Chunked.total_pages mem.content

(* Copied from [Memory] module *)
let type_of mem = mem.ty

let grow mem delta =
  let (MemoryType lim) = mem.ty in
  assert (lim.min = size mem) ;
  let old_size = lim.min in
  let new_size = Int32.add old_size delta in
  if I32.gt_u old_size new_size || I32.gt_u new_size max_pages then
    raise SizeOverflow ;
  let lim' = {lim with min = new_size} in
  if not (valid_limits lim') then raise SizeLimit else mem.ty <- MemoryType lim' ;
  try Chunked.grow delta mem.content with exn -> reraise exn

let load_byte mem i =
  Lwt.catch (fun () -> Chunked.load_byte mem.content i) reraise

let store_byte mem i b =
  Lwt.catch (fun () -> Chunked.store_byte mem.content i b) reraise

(* Copied from [Memory] module *)
let load_bytes mem a n =
  let a = I64_convert.extend_i32_u a in
  let open Lwt.Syntax in
  let buf = Buffer.create n in
  let+ () =
    List.init n (fun i ->
        let+ c = load_byte mem Int64.(add a (of_int i)) in
        Buffer.add_char buf (Char.chr c))
    |> Lwt.join
  in
  Buffer.contents buf

(* Copied from [Memory] module *)
let store_bytes mem a bs =
  let a = I64_convert.extend_i32_u a in
  List.init (String.length bs) (fun i ->
      store_byte mem Int64.(add a (of_int i)) (Char.code bs.[i]))
  |> Lwt.join

let store_bytes_from_bytes mem address bs =
  let address = I64_convert.extend_i32_u address in
  List.init (Bytes.length bs) (fun offset ->
      let value = Char.code (Bytes.get bs offset) in
      store_byte mem Int64.(add address (of_int offset)) value)
  |> Lwt.join

(* Copied from [Memory] module *)
let effective_address a o =
  let ea = Int32.add a o in
  if I32.lt_u ea a then raise Bounds ;
  ea

(* Copied from [Memory] module *)
let loadn mem a o n =
  let open Lwt.Syntax in
  assert (n > 0 && n <= 8) ;
  let rec loop a n =
    if n = 0 then Lwt.return 0L
    else
      let* x0 = loop (Int64.add a 1L) (n - 1) in
      let x = Int64.shift_left x0 8 in
      let+ v = load_byte mem a in
      Int64.logor (Int64.of_int v) x
  in
  let a = I64_convert.extend_i32_u (effective_address a o) in
  loop a n

(* Copied from [Memory] module *)
let storen mem a o n x =
  let open Lwt.Syntax in
  assert (n > 0 && n <= 8) ;
  let rec loop a n x =
    if n > 0 then
      let* () = Int64.(loop (add a 1L) (n - 1) (Int64.shift_right x 8)) in
      store_byte mem a (Int64.to_int x land 0xff)
    else Lwt.return_unit
  in
  let a = I64_convert.extend_i32_u (effective_address a o) in
  loop a n x

(* Copied from [Memory] module *)
let load_num mem a o t =
  let open Lwt.Syntax in
  let+ n = loadn mem a o (Types.num_size t) in
  match t with
  | I32Type -> I32 (Int64.to_int32 n)
  | I64Type -> I64 n
  | F32Type -> F32 (F32.of_bits (Int64.to_int32 n))
  | F64Type -> F64 (F64.of_bits n)

(* Copied from [Memory] module *)
let store_num mem a o n =
  let store = storen mem a o (Types.num_size (Values.type_of_num n)) in
  match n with
  | I32 x -> store (Int64.of_int32 x)
  | I64 x -> store x
  | F32 x -> store (Int64.of_int32 (F32.to_bits x))
  | F64 x -> store (F64.to_bits x)

(* Copied from [Memory] module *)
let extend x n = function
  | ZX -> x
  | SX ->
      let sh = 64 - (8 * n) in
      Int64.(shift_right (shift_left x sh) sh)

(* Copied from [Memory] module *)
let load_num_packed sz ext mem a o t =
  let open Lwt.Syntax in
  assert (packed_size sz <= num_size t) ;
  let w = packed_size sz in
  let+ payload = loadn mem a o w in
  let x = extend payload w ext in
  match t with
  | I32Type -> I32 (Int64.to_int32 x)
  | I64Type -> I64 x
  | _ -> raise Type

(* Copied from [Memory] module *)
let store_num_packed sz mem a o n =
  assert (packed_size sz <= num_size (Values.type_of_num n)) ;
  let w = packed_size sz in
  let x =
    match n with I32 x -> Int64.of_int32 x | I64 x -> x | _ -> raise Type
  in
  storen mem a o w x

(* Copied from [Memory] module *)
let load_vec mem a o t =
  let open Lwt.Syntax in
  match t with
  | V128Type ->
      let+ bits = load_bytes mem (effective_address a o) (Types.vec_size t) in
      V128 (V128.of_bits bits)

(* Copied from [Memory] module *)
let store_vec mem a o n =
  match n with
  | V128 x -> store_bytes mem (effective_address a o) (V128.to_bits x)

(* Copied from [Memory] module *)
let load_vec_packed sz ext mem a o t =
  let open Lwt.Syntax in
  assert (packed_size sz < vec_size t) ;
  let+ x = loadn mem a o (packed_size sz) in
  let b = Bytes.make 16 '\x00' in
  Bytes.set_int64_le b 0 x ;
  let v = V128.of_bits (Bytes.to_string b) in
  let r =
    match (sz, ext) with
    | Pack64, ExtLane (Pack8x8, SX) -> V128.I16x8_convert.extend_low_s v
    | Pack64, ExtLane (Pack8x8, ZX) -> V128.I16x8_convert.extend_low_u v
    | Pack64, ExtLane (Pack16x4, SX) -> V128.I32x4_convert.extend_low_s v
    | Pack64, ExtLane (Pack16x4, ZX) -> V128.I32x4_convert.extend_low_u v
    | Pack64, ExtLane (Pack32x2, SX) -> V128.I64x2_convert.extend_low_s v
    | Pack64, ExtLane (Pack32x2, ZX) -> V128.I64x2_convert.extend_low_u v
    | _, ExtLane _ -> assert false
    | Pack8, ExtSplat -> V128.I8x16.splat (I8.of_int_s (Int64.to_int x))
    | Pack16, ExtSplat -> V128.I16x8.splat (I16.of_int_s (Int64.to_int x))
    | Pack32, ExtSplat -> V128.I32x4.splat (I32.of_int_s (Int64.to_int x))
    | Pack64, ExtSplat -> V128.I64x2.splat x
    | Pack32, ExtZero -> v
    | Pack64, ExtZero -> v
    | _, ExtZero -> assert false
  in
  V128 r
