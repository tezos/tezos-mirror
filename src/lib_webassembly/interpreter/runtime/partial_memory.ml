open Bigarray
open Lib.Bigarray
open Types
open Values

type size = int32  (* number of pages *)
type address = int64
type offset = int32
type count = int32

exception Type
exception Bounds
exception SizeOverflow
exception SizeLimit
exception OutOfMemory

(* Copied from [Memory] module *)
let page_size = 0x10000L (* 64 KiB *)

module SubPage = struct
  module Map = Lazy_map.Mutable.Make (Int64)

  type t = (int, int8_unsigned_elt, c_layout) Array1.t

  (** Number of bits in a sub-page address *)
  let num_bits = 12

  (** Size of a sub-page in bytes *)
  let size = Int64.shift_left 1L num_bits

  (** Number of sub-pages in a WebAssembly page *)
  let sub_pages_per_page = Int64.div page_size size

  (** [Int64.logand address_mask x] gives you the sub-page address of  *)
  let address_mask = Int64.sub size 1L

  let index_of_address address = Int64.shift_right address num_bits

  let mask_address address = Int64.logand address address_mask

  type map = t Map.t

  let create_sub_page () =
    let sub_page = Array1_64.create Int8_unsigned C_layout size in
    Array1.fill sub_page 0;
    sub_page

  let create_map pages =
    Map.create
      ~produce_value:(fun _ -> create_sub_page ())
      Int64.(mul (of_int32 pages) sub_pages_per_page)

  let grow delta_pages =
    Map.grow Int64.(mul (of_int32 delta_pages) sub_pages_per_page)

  let total_pages map =
    Int64.(div (Map.num_elements map) sub_pages_per_page |> to_int32)

  let total_size map = Int64.mul (Map.num_elements map) size

  let load_byte page_map address =
    try
      let sub_page = Map.get (index_of_address address) page_map in
      Array1_64.get sub_page (mask_address address)
    with
      Lazy_map.OutOfBounds -> raise Bounds

  let store_byte page_map address byte =
    try
      let sub_page = Map.get (index_of_address address) page_map in
      Array1_64.set sub_page (mask_address address) byte
    with
      Lazy_map.OutOfBounds -> raise Bounds
end

type memory = {mutable ty : memory_type; content : SubPage.map}

(* Copied from [Memory] module *)
type t = memory

let max_pages = 0x10000l

(* Copied from [Memory] module *)
let valid_limits {min; max} =
  match max with
  | None -> true
  | Some m -> I32.le_u min m

(* Copied from [Memory] module *)
let _create n =
  if I32.gt_u n max_pages then raise SizeOverflow else
  try
    let size = Int64.(mul (of_int32 n) page_size) in
    let mem = Array1_64.create Int8_unsigned C_layout size in
    Array1.fill mem 0;
    mem
  with Out_of_memory -> raise OutOfMemory

(* Copied from [Memory] module *)
let alloc (MemoryType lim as ty) =
  if not (valid_limits lim) then raise Type;
  {ty; content = SubPage.create_map lim.min}

let bound mem = SubPage.total_size mem.content

let size mem = SubPage.total_pages mem.content

(* Copied from [Memory] module *)
let type_of mem = mem.ty

let grow mem delta =
  let MemoryType lim = mem.ty in
  assert (lim.min = size mem);
  let old_size = lim.min in
  let new_size = Int32.add old_size delta in
  if I32.gt_u old_size new_size || I32.gt_u new_size max_pages then
    raise SizeOverflow;
  let lim' = {lim with min = new_size} in
  if not (valid_limits lim') then raise SizeLimit else
  mem.ty <- MemoryType lim';
  SubPage.grow delta mem.content

let load_byte mem = SubPage.load_byte mem.content

let store_byte mem = SubPage.store_byte mem.content

(* Copied from [Memory] module *)
let load_bytes mem a n =
  let buf = Buffer.create n in
  for i = 0 to n - 1 do
    Buffer.add_char buf (Char.chr (load_byte mem Int64.(add a (of_int i))))
  done;
  Buffer.contents buf

(* Copied from [Memory] module *)
let store_bytes mem a bs =
  for i = String.length bs - 1 downto 0 do
    store_byte mem Int64.(add a (of_int i)) (Char.code bs.[i])
  done

(* Copied from [Memory] module *)
let effective_address a o =
  let ea = Int64.(add a (of_int32 o)) in
  if I64.lt_u ea a then raise Bounds;
  ea

(* Copied from [Memory] module *)
let loadn mem a o n =
  assert (n > 0 && n <= 8);
  let rec loop a n =
    if n = 0 then 0L else begin
      let x = Int64.(shift_left (loop (add a 1L) (n - 1)) 8) in
      Int64.logor (Int64.of_int (load_byte mem a)) x
    end
  in loop (effective_address a o) n

(* Copied from [Memory] module *)
let storen mem a o n x =
  assert (n > 0 && n <= 8);
  let rec loop a n x =
    if n > 0 then begin
      Int64.(loop (add a 1L) (n - 1) (shift_right x 8));
      store_byte mem a (Int64.to_int x land 0xff)
    end
  in loop (effective_address a o) n x

(* Copied from [Memory] module *)
let load_num mem a o t =
  let n = loadn mem a o (Types.num_size t) in
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
  | SX -> let sh = 64 - 8 * n in Int64.(shift_right (shift_left x sh) sh)

(* Copied from [Memory] module *)
let load_num_packed sz ext mem a o t =
  assert (packed_size sz <= num_size t);
  let w = packed_size sz in
  let x = extend (loadn mem a o w) w ext in
  match t with
  | I32Type -> I32 (Int64.to_int32 x)
  | I64Type -> I64 x
  | _ -> raise Type

(* Copied from [Memory] module *)
let store_num_packed sz mem a o n =
  assert (packed_size sz <= num_size (Values.type_of_num n));
  let w = packed_size sz in
  let x =
    match n with
    | I32 x -> Int64.of_int32 x
    | I64 x -> x
    | _ -> raise Type
  in storen mem a o w x

(* Copied from [Memory] module *)
let load_vec mem a o t =
  match t with
  | V128Type ->
    V128 (V128.of_bits (load_bytes mem (effective_address a o) (Types.vec_size t)))

(* Copied from [Memory] module *)
let store_vec mem a o n =
  match n with
  | V128 x -> store_bytes mem (effective_address a o) (V128.to_bits x)

(* Copied from [Memory] module *)
let load_vec_packed sz ext mem a o t =
  assert (packed_size sz < vec_size t);
  let x = loadn mem a o (packed_size sz) in
  let b = Bytes.make 16 '\x00' in
  Bytes.set_int64_le b 0 x;
  let v = V128.of_bits (Bytes.to_string b) in
  let r =
    match sz, ext with
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
  in V128 r
