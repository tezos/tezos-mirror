(* Decoding stream *)

open Binary_exn
module Vector = Lazy_vector.LwtInt32Vector

type stream = {name : string; bytes : string; pos : int ref}

let make_stream ~name ~bytes = {name; bytes; pos = ref 0}

type block_state = {
  blocks : (int, Ast.instr array) Hashtbl.t;
  next_block : int ref;
}

let make_block_state () =
  {
    blocks =
      (let init = Hashtbl.create 10 in
       Hashtbl.add init 0 [||] ;
       init);
    next_block = ref 1;
  }

let alloc_block s es =
  let es = Array.of_list es in
  let b = !(s.next_block) in
  Hashtbl.add s.blocks b es ;
  incr s.next_block ;
  assert (!(s.next_block) >= 0) ;
  Ast.Block_label b

let lookup_block bs (Ast.Block_label b) =
  Array.to_list (Hashtbl.find bs.blocks b)

let empty_block = Ast.Block_label 0

let len s = String.length s.bytes

let pos s = !(s.pos)

let eos s = pos s = len s

let check n s = if pos s + n > len s then raise EOS

let skip n s =
  if n < 0 then raise EOS else check n s ;
  s.pos := !(s.pos) + n

let read s = Char.code s.bytes.[!(s.pos)]

let peek s = if eos s then None else Some (read s)

let get s =
  check 1 s ;
  let b = read s in
  skip 1 s ;
  b

let get_string n s =
  let i = pos s in
  skip n s ;
  String.sub s.bytes i n

(* Errors *)

exception Code = Decode_error.Error

let string_of_byte b = Printf.sprintf "%02x" b

let string_of_multi n = Printf.sprintf "%02lx" n

let position s pos = Source.{file = s.name; line = -1; column = pos}

let region s left right =
  Source.{left = position s left; right = position s right}

let error s pos msg = raise (Code (region s pos pos, msg))

let require b s pos msg = if not b then error s pos msg

let guard f s =
  try f s with EOS -> error s (len s) "unexpected end of section or function"

let get = guard get

let get_string n = guard (get_string n)

let skip n = guard (skip n)

let expect b s msg = require (guard get s = b) s (pos s - 1) msg

let illegal s pos b = error s pos ("illegal opcode " ^ string_of_byte b)

let illegal2 s pos b n =
  error s pos ("illegal opcode " ^ string_of_byte b ^ " " ^ string_of_multi n)

let at f s =
  let left = pos s in
  let x = f s in
  let right = pos s in
  Source.(x @@ region s left right)

(* Generic values *)

let u8 s = get s

let u16 s =
  let lo = u8 s in
  let hi = u8 s in
  (hi lsl 8) + lo

let u32 s =
  let lo = Int32.of_int (u16 s) in
  let hi = Int32.of_int (u16 s) in
  Int32.(add lo (shift_left hi 16))

let u64 s =
  let lo = I64_convert.extend_i32_u (u32 s) in
  let hi = I64_convert.extend_i32_u (u32 s) in
  Int64.(add lo (shift_left hi 32))

let rec vuN n s =
  require (n > 0) s (pos s) "integer representation too long" ;
  let b = u8 s in
  require (n >= 7 || b land 0x7f < 1 lsl n) s (pos s - 1) "integer too large" ;
  let x = Int64.of_int (b land 0x7f) in
  if b land 0x80 = 0 then x else Int64.(logor x (shift_left (vuN (n - 7) s) 7))

let rec vsN n s =
  require (n > 0) s (pos s) "integer representation too long" ;
  let b = u8 s in
  let mask = (-1 lsl (n - 1)) land 0x7f in
  require
    (n >= 7 || b land mask = 0 || b land mask = mask)
    s
    (pos s - 1)
    "integer too large" ;
  let x = Int64.of_int (b land 0x7f) in
  if b land 0x80 = 0 then
    if b land 0x40 = 0 then x else Int64.(logor x (logxor (-1L) 0x7fL))
  else Int64.(logor x (shift_left (vsN (n - 7) s) 7))

let vu1 s = Int64.to_int (vuN 1 s)

let vu32 s = Int64.to_int32 (vuN 32 s)

let vs7 s = Int64.to_int (vsN 7 s)

let vs32 s = Int64.to_int32 (vsN 32 s)

let vs33 s = I32_convert.wrap_i64 (vsN 33 s)

let vs64 s = vsN 64 s

let f32 s = F32.of_bits (u32 s)

let f64 s = F64.of_bits (u64 s)

let v128 s = V128.of_bits (get_string (Types.vec_size Types.V128Type) s)

let len32 s =
  let pos = pos s in
  let n = vu32 s in
  if I32.le_u n (Int32.of_int (len s - pos)) then Int32.to_int n
  else error s pos "length out of bounds"

let bool s = vu1 s = 1

let rec list f n s =
  if n = 0 then []
  else
    let x = f s in
    x :: list f (n - 1) s

let opt f b s = if b then Some (f s) else None

let vec f s =
  let n = len32 s in
  list f n s

let sized f s =
  let size = len32 s in
  let start = pos s in
  let x = f size s in
  require (pos s = start + size) s start "section size mismatch" ;
  x

(** Incremental chunked byte vector creation (from implicit input). *)
type byte_vector_kont =
  | VKStart  (** Initial step. *)
  | VKRead of Chunked_byte_vector.Lwt.t * int64 * int64
      (** Reading step, containing the current position in the string and the
      length, reading byte per byte. *)
  | VKStop of Chunked_byte_vector.Lwt.t  (** Final step, cannot reduce. *)

let byte_vector_step s =
  let open Lwt.Syntax in
  function
  | VKStart ->
      let len = len32 s |> Int64.of_int in
      let vector = Chunked_byte_vector.Lwt.create len in
      VKRead (vector, 0L, len) |> Lwt.return
  | VKRead (vector, index, len) when Int64.compare index len >= 0 ->
      VKStop vector |> Lwt.return
  | VKRead (vector, index, len) ->
      let c = get s in
      let+ () = Chunked_byte_vector.Lwt.store_byte vector index c in
      VKRead (vector, Int64.succ index, len)
  (* Final step, cannot reduce *)
  | VKStop vector -> assert false

(* Types *)

open Types

let num_type s =
  match vs7 s with
  | -0x01 -> I32Type
  | -0x02 -> I64Type
  | -0x03 -> F32Type
  | -0x04 -> F64Type
  | _ -> error s (pos s - 1) "malformed number type"

let vec_type s =
  match vs7 s with
  | -0x05 -> V128Type
  | _ -> error s (pos s - 1) "malformed vector type"

let ref_type s =
  match vs7 s with
  | -0x10 -> FuncRefType
  | -0x11 -> ExternRefType
  | _ -> error s (pos s - 1) "malformed reference type"

let value_type s =
  match peek s with
  | Some n when n >= -0x04 land 0x7f -> NumType (num_type s)
  | Some n when n >= -0x0f land 0x7f -> VecType (vec_type s)
  | _ -> RefType (ref_type s)

let limits vu s =
  let has_max = bool s in
  let min = vu s in
  let max = opt vu has_max s in
  {min; max}

let table_type s =
  let t = ref_type s in
  let lim = limits vu32 s in
  TableType (lim, t)

let memory_type s =
  let lim = limits vu32 s in
  MemoryType lim

let mutability s =
  match u8 s with
  | 0 -> Immutable
  | 1 -> Mutable
  | _ -> error s (pos s - 1) "malformed mutability"

let global_type s =
  let t = value_type s in
  let mut = mutability s in
  GlobalType (t, mut)

(* Decode instructions *)

open Ast
open Operators

let var s = vu32 s

let op s = u8 s

let end_ s = expect 0x0b s "END opcode expected"

let zero s = expect 0x00 s "zero byte expected"

let memop s =
  let align = vu32 s in
  require (I32.le_u align 32l) s (pos s - 1) "malformed memop flags" ;
  let offset = vu32 s in
  (Int32.to_int align, offset)

let block_type s =
  match peek s with
  | Some 0x40 ->
      skip 1 s ;
      ValBlockType None
  | Some b when b land 0xc0 = 0x40 -> ValBlockType (Some (value_type s))
  | _ -> VarBlockType (at vs33 s)

(** Instruction parsing continuations. *)
type instr_block_kont =
  | IKStop of instr list  (** Final step of a block parsing. *)
  | IKRev of instr list * instr list  (** Reversal of lists of instructions. *)
  | IKNext of instr list
      (** Tag parsing, containing the accumulation of already parsed values. *)
  | IKBlock of block_type * int  (** Block parsing step. *)
  | IKLoop of block_type * int  (** Loop parsing step. *)
  | IKIf1 of block_type * int  (** If parsing step. *)
  | IKIf2 of block_type * int * instr list  (** If .. else parsing step. *)

let instr s pos tag =
  match tag with
  (* These tags corresponds to resp. block, loop and if, and are now handled
     directly by the main step loop (see `IKBlock`, `IKLoop` and `IKIf1`. *)
  | 0x02 | 0x03 | 0x04 -> assert false
  | 0x00 -> unreachable
  | 0x01 -> nop
  | 0x05 -> error s pos "misplaced ELSE opcode"
  | (0x06 | 0x07 | 0x08 | 0x09 | 0x0a) as b -> illegal s pos b
  | 0x0b -> error s pos "misplaced END opcode"
  | 0x0c -> br (at var s)
  | 0x0d -> br_if (at var s)
  | 0x0e ->
      let xs = vec (at var) s in
      let x = at var s in
      br_table xs x
  | 0x0f -> return
  | 0x10 -> call (at var s)
  | 0x11 ->
      let y = at var s in
      let x = at var s in
      call_indirect x y
  | (0x12 | 0x13 | 0x14 | 0x15 | 0x16 | 0x17 | 0x18 | 0x19) as b ->
      illegal s pos b
  | 0x1a -> drop
  | 0x1b -> select None
  | 0x1c -> select (Some (vec value_type s))
  | (0x1d | 0x1e | 0x1f) as b -> illegal s pos b
  | 0x20 -> local_get (at var s)
  | 0x21 -> local_set (at var s)
  | 0x22 -> local_tee (at var s)
  | 0x23 -> global_get (at var s)
  | 0x24 -> global_set (at var s)
  | 0x25 -> table_get (at var s)
  | 0x26 -> table_set (at var s)
  | 0x27 as b -> illegal s pos b
  | 0x28 ->
      let a, o = memop s in
      i32_load a o
  | 0x29 ->
      let a, o = memop s in
      i64_load a o
  | 0x2a ->
      let a, o = memop s in
      f32_load a o
  | 0x2b ->
      let a, o = memop s in
      f64_load a o
  | 0x2c ->
      let a, o = memop s in
      i32_load8_s a o
  | 0x2d ->
      let a, o = memop s in
      i32_load8_u a o
  | 0x2e ->
      let a, o = memop s in
      i32_load16_s a o
  | 0x2f ->
      let a, o = memop s in
      i32_load16_u a o
  | 0x30 ->
      let a, o = memop s in
      i64_load8_s a o
  | 0x31 ->
      let a, o = memop s in
      i64_load8_u a o
  | 0x32 ->
      let a, o = memop s in
      i64_load16_s a o
  | 0x33 ->
      let a, o = memop s in
      i64_load16_u a o
  | 0x34 ->
      let a, o = memop s in
      i64_load32_s a o
  | 0x35 ->
      let a, o = memop s in
      i64_load32_u a o
  | 0x36 ->
      let a, o = memop s in
      i32_store a o
  | 0x37 ->
      let a, o = memop s in
      i64_store a o
  | 0x38 ->
      let a, o = memop s in
      f32_store a o
  | 0x39 ->
      let a, o = memop s in
      f64_store a o
  | 0x3a ->
      let a, o = memop s in
      i32_store8 a o
  | 0x3b ->
      let a, o = memop s in
      i32_store16 a o
  | 0x3c ->
      let a, o = memop s in
      i64_store8 a o
  | 0x3d ->
      let a, o = memop s in
      i64_store16 a o
  | 0x3e ->
      let a, o = memop s in
      i64_store32 a o
  | 0x3f ->
      zero s ;
      memory_size
  | 0x40 ->
      zero s ;
      memory_grow
  | 0x41 -> i32_const (at vs32 s)
  | 0x42 -> i64_const (at vs64 s)
  | 0x43 -> f32_const (at f32 s)
  | 0x44 -> f64_const (at f64 s)
  | 0x45 -> i32_eqz
  | 0x46 -> i32_eq
  | 0x47 -> i32_ne
  | 0x48 -> i32_lt_s
  | 0x49 -> i32_lt_u
  | 0x4a -> i32_gt_s
  | 0x4b -> i32_gt_u
  | 0x4c -> i32_le_s
  | 0x4d -> i32_le_u
  | 0x4e -> i32_ge_s
  | 0x4f -> i32_ge_u
  | 0x50 -> i64_eqz
  | 0x51 -> i64_eq
  | 0x52 -> i64_ne
  | 0x53 -> i64_lt_s
  | 0x54 -> i64_lt_u
  | 0x55 -> i64_gt_s
  | 0x56 -> i64_gt_u
  | 0x57 -> i64_le_s
  | 0x58 -> i64_le_u
  | 0x59 -> i64_ge_s
  | 0x5a -> i64_ge_u
  | 0x5b -> f32_eq
  | 0x5c -> f32_ne
  | 0x5d -> f32_lt
  | 0x5e -> f32_gt
  | 0x5f -> f32_le
  | 0x60 -> f32_ge
  | 0x61 -> f64_eq
  | 0x62 -> f64_ne
  | 0x63 -> f64_lt
  | 0x64 -> f64_gt
  | 0x65 -> f64_le
  | 0x66 -> f64_ge
  | 0x67 -> i32_clz
  | 0x68 -> i32_ctz
  | 0x69 -> i32_popcnt
  | 0x6a -> i32_add
  | 0x6b -> i32_sub
  | 0x6c -> i32_mul
  | 0x6d -> i32_div_s
  | 0x6e -> i32_div_u
  | 0x6f -> i32_rem_s
  | 0x70 -> i32_rem_u
  | 0x71 -> i32_and
  | 0x72 -> i32_or
  | 0x73 -> i32_xor
  | 0x74 -> i32_shl
  | 0x75 -> i32_shr_s
  | 0x76 -> i32_shr_u
  | 0x77 -> i32_rotl
  | 0x78 -> i32_rotr
  | 0x79 -> i64_clz
  | 0x7a -> i64_ctz
  | 0x7b -> i64_popcnt
  | 0x7c -> i64_add
  | 0x7d -> i64_sub
  | 0x7e -> i64_mul
  | 0x7f -> i64_div_s
  | 0x80 -> i64_div_u
  | 0x81 -> i64_rem_s
  | 0x82 -> i64_rem_u
  | 0x83 -> i64_and
  | 0x84 -> i64_or
  | 0x85 -> i64_xor
  | 0x86 -> i64_shl
  | 0x87 -> i64_shr_s
  | 0x88 -> i64_shr_u
  | 0x89 -> i64_rotl
  | 0x8a -> i64_rotr
  | 0x8b -> f32_abs
  | 0x8c -> f32_neg
  | 0x8d -> f32_ceil
  | 0x8e -> f32_floor
  | 0x8f -> f32_trunc
  | 0x90 -> f32_nearest
  | 0x91 -> f32_sqrt
  | 0x92 -> f32_add
  | 0x93 -> f32_sub
  | 0x94 -> f32_mul
  | 0x95 -> f32_div
  | 0x96 -> f32_min
  | 0x97 -> f32_max
  | 0x98 -> f32_copysign
  | 0x99 -> f64_abs
  | 0x9a -> f64_neg
  | 0x9b -> f64_ceil
  | 0x9c -> f64_floor
  | 0x9d -> f64_trunc
  | 0x9e -> f64_nearest
  | 0x9f -> f64_sqrt
  | 0xa0 -> f64_add
  | 0xa1 -> f64_sub
  | 0xa2 -> f64_mul
  | 0xa3 -> f64_div
  | 0xa4 -> f64_min
  | 0xa5 -> f64_max
  | 0xa6 -> f64_copysign
  | 0xa7 -> i32_wrap_i64
  | 0xa8 -> i32_trunc_f32_s
  | 0xa9 -> i32_trunc_f32_u
  | 0xaa -> i32_trunc_f64_s
  | 0xab -> i32_trunc_f64_u
  | 0xac -> i64_extend_i32_s
  | 0xad -> i64_extend_i32_u
  | 0xae -> i64_trunc_f32_s
  | 0xaf -> i64_trunc_f32_u
  | 0xb0 -> i64_trunc_f64_s
  | 0xb1 -> i64_trunc_f64_u
  | 0xb2 -> f32_convert_i32_s
  | 0xb3 -> f32_convert_i32_u
  | 0xb4 -> f32_convert_i64_s
  | 0xb5 -> f32_convert_i64_u
  | 0xb6 -> f32_demote_f64
  | 0xb7 -> f64_convert_i32_s
  | 0xb8 -> f64_convert_i32_u
  | 0xb9 -> f64_convert_i64_s
  | 0xba -> f64_convert_i64_u
  | 0xbb -> f64_promote_f32
  | 0xbc -> i32_reinterpret_f32
  | 0xbd -> i64_reinterpret_f64
  | 0xbe -> f32_reinterpret_i32
  | 0xbf -> f64_reinterpret_i64
  | 0xc0 -> i32_extend8_s
  | 0xc1 -> i32_extend16_s
  | 0xc2 -> i64_extend8_s
  | 0xc3 -> i64_extend16_s
  | 0xc4 -> i64_extend32_s
  | (0xc5 | 0xc6 | 0xc7 | 0xc8 | 0xc9 | 0xca | 0xcb | 0xcc | 0xcd | 0xce | 0xcf)
    as b ->
      illegal s pos b
  | 0xd0 -> ref_null (ref_type s)
  | 0xd1 -> ref_is_null
  | 0xd2 -> ref_func (at var s)
  | 0xfc as b -> (
      match vu32 s with
      | 0x00l -> i32_trunc_sat_f32_s
      | 0x01l -> i32_trunc_sat_f32_u
      | 0x02l -> i32_trunc_sat_f64_s
      | 0x03l -> i32_trunc_sat_f64_u
      | 0x04l -> i64_trunc_sat_f32_s
      | 0x05l -> i64_trunc_sat_f32_u
      | 0x06l -> i64_trunc_sat_f64_s
      | 0x07l -> i64_trunc_sat_f64_u
      | 0x08l ->
          let x = at var s in
          zero s ;
          memory_init x
      | 0x09l -> data_drop (at var s)
      | 0x0al ->
          zero s ;
          zero s ;
          memory_copy
      | 0x0bl ->
          zero s ;
          memory_fill
      | 0x0cl ->
          let y = at var s in
          let x = at var s in
          table_init x y
      | 0x0dl -> elem_drop (at var s)
      | 0x0el ->
          let x = at var s in
          let y = at var s in
          table_copy x y
      | 0x0fl -> table_grow (at var s)
      | 0x10l -> table_size (at var s)
      | 0x11l -> table_fill (at var s)
      | n -> illegal2 s pos b n)
  | 0xfd -> (
      match vu32 s with
      | 0x00l ->
          let a, o = memop s in
          v128_load a o
      | 0x01l ->
          let a, o = memop s in
          v128_load8x8_s a o
      | 0x02l ->
          let a, o = memop s in
          v128_load8x8_u a o
      | 0x03l ->
          let a, o = memop s in
          v128_load16x4_s a o
      | 0x04l ->
          let a, o = memop s in
          v128_load16x4_u a o
      | 0x05l ->
          let a, o = memop s in
          v128_load32x2_s a o
      | 0x06l ->
          let a, o = memop s in
          v128_load32x2_u a o
      | 0x07l ->
          let a, o = memop s in
          v128_load8_splat a o
      | 0x08l ->
          let a, o = memop s in
          v128_load16_splat a o
      | 0x09l ->
          let a, o = memop s in
          v128_load32_splat a o
      | 0x0al ->
          let a, o = memop s in
          v128_load64_splat a o
      | 0x0bl ->
          let a, o = memop s in
          v128_store a o
      | 0x0cl -> v128_const (at v128 s)
      | 0x0dl -> i8x16_shuffle (List.init 16 (fun x -> u8 s))
      | 0x0el -> i8x16_swizzle
      | 0x0fl -> i8x16_splat
      | 0x10l -> i16x8_splat
      | 0x11l -> i32x4_splat
      | 0x12l -> i64x2_splat
      | 0x13l -> f32x4_splat
      | 0x14l -> f64x2_splat
      | 0x15l ->
          let i = u8 s in
          i8x16_extract_lane_s i
      | 0x16l ->
          let i = u8 s in
          i8x16_extract_lane_u i
      | 0x17l ->
          let i = u8 s in
          i8x16_replace_lane i
      | 0x18l ->
          let i = u8 s in
          i16x8_extract_lane_s i
      | 0x19l ->
          let i = u8 s in
          i16x8_extract_lane_u i
      | 0x1al ->
          let i = u8 s in
          i16x8_replace_lane i
      | 0x1bl ->
          let i = u8 s in
          i32x4_extract_lane i
      | 0x1cl ->
          let i = u8 s in
          i32x4_replace_lane i
      | 0x1dl ->
          let i = u8 s in
          i64x2_extract_lane i
      | 0x1el ->
          let i = u8 s in
          i64x2_replace_lane i
      | 0x1fl ->
          let i = u8 s in
          f32x4_extract_lane i
      | 0x20l ->
          let i = u8 s in
          f32x4_replace_lane i
      | 0x21l ->
          let i = u8 s in
          f64x2_extract_lane i
      | 0x22l ->
          let i = u8 s in
          f64x2_replace_lane i
      | 0x23l -> i8x16_eq
      | 0x24l -> i8x16_ne
      | 0x25l -> i8x16_lt_s
      | 0x26l -> i8x16_lt_u
      | 0x27l -> i8x16_gt_s
      | 0x28l -> i8x16_gt_u
      | 0x29l -> i8x16_le_s
      | 0x2al -> i8x16_le_u
      | 0x2bl -> i8x16_ge_s
      | 0x2cl -> i8x16_ge_u
      | 0x2dl -> i16x8_eq
      | 0x2el -> i16x8_ne
      | 0x2fl -> i16x8_lt_s
      | 0x30l -> i16x8_lt_u
      | 0x31l -> i16x8_gt_s
      | 0x32l -> i16x8_gt_u
      | 0x33l -> i16x8_le_s
      | 0x34l -> i16x8_le_u
      | 0x35l -> i16x8_ge_s
      | 0x36l -> i16x8_ge_u
      | 0x37l -> i32x4_eq
      | 0x38l -> i32x4_ne
      | 0x39l -> i32x4_lt_s
      | 0x3al -> i32x4_lt_u
      | 0x3bl -> i32x4_gt_s
      | 0x3cl -> i32x4_gt_u
      | 0x3dl -> i32x4_le_s
      | 0x3el -> i32x4_le_u
      | 0x3fl -> i32x4_ge_s
      | 0x40l -> i32x4_ge_u
      | 0x41l -> f32x4_eq
      | 0x42l -> f32x4_ne
      | 0x43l -> f32x4_lt
      | 0x44l -> f32x4_gt
      | 0x45l -> f32x4_le
      | 0x46l -> f32x4_ge
      | 0x47l -> f64x2_eq
      | 0x48l -> f64x2_ne
      | 0x49l -> f64x2_lt
      | 0x4al -> f64x2_gt
      | 0x4bl -> f64x2_le
      | 0x4cl -> f64x2_ge
      | 0x4dl -> v128_not
      | 0x4el -> v128_and
      | 0x4fl -> v128_andnot
      | 0x50l -> v128_or
      | 0x51l -> v128_xor
      | 0x52l -> v128_bitselect
      | 0x53l -> v128_any_true
      | 0x54l ->
          let a, o = memop s in
          let lane = u8 s in
          v128_load8_lane a o lane
      | 0x55l ->
          let a, o = memop s in
          let lane = u8 s in
          v128_load16_lane a o lane
      | 0x56l ->
          let a, o = memop s in
          let lane = u8 s in
          v128_load32_lane a o lane
      | 0x57l ->
          let a, o = memop s in
          let lane = u8 s in
          v128_load64_lane a o lane
      | 0x58l ->
          let a, o = memop s in
          let lane = u8 s in
          v128_store8_lane a o lane
      | 0x59l ->
          let a, o = memop s in
          let lane = u8 s in
          v128_store16_lane a o lane
      | 0x5al ->
          let a, o = memop s in
          let lane = u8 s in
          v128_store32_lane a o lane
      | 0x5bl ->
          let a, o = memop s in
          let lane = u8 s in
          v128_store64_lane a o lane
      | 0x5cl ->
          let a, o = memop s in
          v128_load32_zero a o
      | 0x5dl ->
          let a, o = memop s in
          v128_load64_zero a o
      | 0x5el -> f32x4_demote_f64x2_zero
      | 0x5fl -> f64x2_promote_low_f32x4
      | 0x60l -> i8x16_abs
      | 0x61l -> i8x16_neg
      | 0x62l -> i8x16_popcnt
      | 0x63l -> i8x16_all_true
      | 0x64l -> i8x16_bitmask
      | 0x65l -> i8x16_narrow_i16x8_s
      | 0x66l -> i8x16_narrow_i16x8_u
      | 0x67l -> f32x4_ceil
      | 0x68l -> f32x4_floor
      | 0x69l -> f32x4_trunc
      | 0x6al -> f32x4_nearest
      | 0x6bl -> i8x16_shl
      | 0x6cl -> i8x16_shr_s
      | 0x6dl -> i8x16_shr_u
      | 0x6el -> i8x16_add
      | 0x6fl -> i8x16_add_sat_s
      | 0x70l -> i8x16_add_sat_u
      | 0x71l -> i8x16_sub
      | 0x72l -> i8x16_sub_sat_s
      | 0x73l -> i8x16_sub_sat_u
      | 0x74l -> f64x2_ceil
      | 0x75l -> f64x2_floor
      | 0x76l -> i8x16_min_s
      | 0x77l -> i8x16_min_u
      | 0x78l -> i8x16_max_s
      | 0x79l -> i8x16_max_u
      | 0x7al -> f64x2_trunc
      | 0x7bl -> i8x16_avgr_u
      | 0x7cl -> i16x8_extadd_pairwise_i8x16_s
      | 0x7dl -> i16x8_extadd_pairwise_i8x16_u
      | 0x7el -> i32x4_extadd_pairwise_i16x8_s
      | 0x7fl -> i32x4_extadd_pairwise_i16x8_u
      | 0x80l -> i16x8_abs
      | 0x81l -> i16x8_neg
      | 0x82l -> i16x8_q15mulr_sat_s
      | 0x83l -> i16x8_all_true
      | 0x84l -> i16x8_bitmask
      | 0x85l -> i16x8_narrow_i32x4_s
      | 0x86l -> i16x8_narrow_i32x4_u
      | 0x87l -> i16x8_extend_low_i8x16_s
      | 0x88l -> i16x8_extend_high_i8x16_s
      | 0x89l -> i16x8_extend_low_i8x16_u
      | 0x8al -> i16x8_extend_high_i8x16_u
      | 0x8bl -> i16x8_shl
      | 0x8cl -> i16x8_shr_s
      | 0x8dl -> i16x8_shr_u
      | 0x8el -> i16x8_add
      | 0x8fl -> i16x8_add_sat_s
      | 0x90l -> i16x8_add_sat_u
      | 0x91l -> i16x8_sub
      | 0x92l -> i16x8_sub_sat_s
      | 0x93l -> i16x8_sub_sat_u
      | 0x94l -> f64x2_nearest
      | 0x95l -> i16x8_mul
      | 0x96l -> i16x8_min_s
      | 0x97l -> i16x8_min_u
      | 0x98l -> i16x8_max_s
      | 0x99l -> i16x8_max_u
      | 0x9bl -> i16x8_avgr_u
      | 0x9cl -> i16x8_extmul_low_i8x16_s
      | 0x9dl -> i16x8_extmul_high_i8x16_s
      | 0x9el -> i16x8_extmul_low_i8x16_u
      | 0x9fl -> i16x8_extmul_high_i8x16_u
      | 0xa0l -> i32x4_abs
      | 0xa1l -> i32x4_neg
      | 0xa3l -> i32x4_all_true
      | 0xa4l -> i32x4_bitmask
      | 0xa7l -> i32x4_extend_low_i16x8_s
      | 0xa8l -> i32x4_extend_high_i16x8_s
      | 0xa9l -> i32x4_extend_low_i16x8_u
      | 0xaal -> i32x4_extend_high_i16x8_u
      | 0xabl -> i32x4_shl
      | 0xacl -> i32x4_shr_s
      | 0xadl -> i32x4_shr_u
      | 0xael -> i32x4_add
      | 0xb1l -> i32x4_sub
      | 0xb5l -> i32x4_mul
      | 0xb6l -> i32x4_min_s
      | 0xb7l -> i32x4_min_u
      | 0xb8l -> i32x4_max_s
      | 0xb9l -> i32x4_max_u
      | 0xbal -> i32x4_dot_i16x8_s
      | 0xbcl -> i32x4_extmul_low_i16x8_s
      | 0xbdl -> i32x4_extmul_high_i16x8_s
      | 0xbel -> i32x4_extmul_low_i16x8_u
      | 0xbfl -> i32x4_extmul_high_i16x8_u
      | 0xc0l -> i64x2_abs
      | 0xc1l -> i64x2_neg
      | 0xc3l -> i64x2_all_true
      | 0xc4l -> i64x2_bitmask
      | 0xc7l -> i64x2_extend_low_i32x4_s
      | 0xc8l -> i64x2_extend_high_i32x4_s
      | 0xc9l -> i64x2_extend_low_i32x4_u
      | 0xcal -> i64x2_extend_high_i32x4_u
      | 0xcbl -> i64x2_shl
      | 0xccl -> i64x2_shr_s
      | 0xcdl -> i64x2_shr_u
      | 0xcel -> i64x2_add
      | 0xd1l -> i64x2_sub
      | 0xd5l -> i64x2_mul
      | 0xd6l -> i64x2_eq
      | 0xd7l -> i64x2_ne
      | 0xd8l -> i64x2_lt_s
      | 0xd9l -> i64x2_gt_s
      | 0xdal -> i64x2_le_s
      | 0xdbl -> i64x2_ge_s
      | 0xdcl -> i64x2_extmul_low_i32x4_s
      | 0xddl -> i64x2_extmul_high_i32x4_s
      | 0xdel -> i64x2_extmul_low_i32x4_u
      | 0xdfl -> i64x2_extmul_high_i32x4_u
      | 0xe0l -> f32x4_abs
      | 0xe1l -> f32x4_neg
      | 0xe3l -> f32x4_sqrt
      | 0xe4l -> f32x4_add
      | 0xe5l -> f32x4_sub
      | 0xe6l -> f32x4_mul
      | 0xe7l -> f32x4_div
      | 0xe8l -> f32x4_min
      | 0xe9l -> f32x4_max
      | 0xeal -> f32x4_pmin
      | 0xebl -> f32x4_pmax
      | 0xecl -> f64x2_abs
      | 0xedl -> f64x2_neg
      | 0xefl -> f64x2_sqrt
      | 0xf0l -> f64x2_add
      | 0xf1l -> f64x2_sub
      | 0xf2l -> f64x2_mul
      | 0xf3l -> f64x2_div
      | 0xf4l -> f64x2_min
      | 0xf5l -> f64x2_max
      | 0xf6l -> f64x2_pmin
      | 0xf7l -> f64x2_pmax
      | 0xf8l -> i32x4_trunc_sat_f32x4_s
      | 0xf9l -> i32x4_trunc_sat_f32x4_u
      | 0xfal -> f32x4_convert_i32x4_s
      | 0xfbl -> f32x4_convert_i32x4_u
      | 0xfcl -> i32x4_trunc_sat_f64x2_s_zero
      | 0xfdl -> i32x4_trunc_sat_f64x2_u_zero
      | 0xfel -> f64x2_convert_low_i32x4_s
      | 0xffl -> f64x2_convert_low_i32x4_u
      | n -> illegal s pos (I32.to_int_u n))
  | b -> illegal s pos b

let instr_block_step s bs cont =
  match cont with
  | [IKStop res] -> invalid_arg "instr_block"
  | IKStop res :: IKBlock (bt, pos) :: IKNext es :: rest ->
      end_ s ;
      let e = Source.(block bt (alloc_block bs res) @@ region s pos pos) in
      IKNext (e :: es) :: rest
  | IKStop res :: IKLoop (bt, pos) :: IKNext es :: rest ->
      end_ s ;
      let e = Source.(loop bt (alloc_block bs res) @@ region s pos pos) in
      IKNext (e :: es) :: rest
  | IKStop res :: IKIf1 (bt, pos) :: IKNext es :: rest ->
      if peek s = Some 0x05 then (
        skip 1 s ;
        IKNext [] :: IKIf2 (bt, pos, res) :: IKNext es :: rest)
      else (
        end_ s ;
        let e =
          Source.(if_ bt (alloc_block bs res) empty_block @@ region s pos pos)
        in
        IKNext (e :: es) :: rest)
  | IKStop res2 :: IKIf2 (bt, pos, res1) :: IKNext es :: rest ->
      end_ s ;
      let e =
        Source.(
          if_ bt (alloc_block bs res1) (alloc_block bs res2) @@ region s pos pos)
      in
      IKNext (e :: es) :: rest
  | IKRev ([], es) :: ks -> IKStop es :: ks
  | IKRev (e :: rest, es) :: ks -> IKRev (rest, e :: es) :: ks
  | IKNext es :: ks -> (
      match peek s with
      | None | Some (0x05 | 0x0b) -> IKRev (es, []) :: ks
      | _ -> (
          let pos = pos s in
          let tag = op s in
          match tag with
          | 0x02 ->
              let bt = block_type s in
              IKNext [] :: IKBlock (bt, pos) :: IKNext es :: ks
          | 0x03 ->
              let bt = block_type s in
              IKNext [] :: IKLoop (bt, pos) :: IKNext es :: ks
          | 0x04 ->
              let bt = block_type s in
              IKNext [] :: IKIf1 (bt, pos) :: IKNext es :: ks
          | _ ->
              let e = instr s pos tag in
              let es = Source.(e @@ region s pos pos) :: es in
              IKNext es :: ks))
  (* Stop can only be followed a new block, or being the final state. *)
  | IKStop _ :: _ -> invalid_arg "instr_block"
  (* These continuations never reduce directly and are always preceded by an end
     of block or the accumulation of parsed instructions (`IKNext`). *)
  | IKBlock _ :: _ | IKLoop _ :: _ | IKIf1 _ :: _ | IKIf2 _ :: _ -> assert false
  (* The empty continuation cannot reduce. *)
  | [] -> assert false

(** Vector and size continuations *)

(** Vector accumulator, used in two steps: first accumulating the values, then
    reversing them and possibly mapping them, counting the number of values in
    the list. Continuation passing style transformation of {!List.map} also
    returning length. *)
type ('a, 'b) vec_map_kont =
  | Collect of int * 'a list
  | Rev of 'a list * 'b list * int

type 'a lazy_vec_kont = Lazy_vec of {offset : int32; vector : 'a Vector.t}

let is_end_of_vec (Lazy_vec {offset; vector}) =
  Vector.num_elements vector <= offset

let init_lazy_vec n = Lazy_vec {offset = 0l; vector = Vector.create n}

let lazy_vec_step v (Lazy_vec {offset; vector}) =
  Lazy_vec {offset = Int32.add offset 1l; vector = Vector.set offset v vector}

type pos = int

(** Size checking version of {!sized} for CPS-style parsing. *)
type size = {size : int; start : pos}

let size s =
  let size = len32 s in
  let start = pos s in
  {size; start}

let check_size {size; start} s =
  require (pos s = start + size) s start "section size mismatch"

type name_step =
  | NKStart  (** UTF8 name starting point. *)
  | NKParse of pos * int lazy_vec_kont * int  (** UTF8 char parsing. *)
  | NKStop of int Vector.t  (** UTF8 name final step.*)

let name_step s = function
  | NKStart ->
      let pos = pos s in
      let len = len32 s in
      NKParse (pos, init_lazy_vec 0l, len)
  | NKParse (pos, Lazy_vec {vector; _}, 0) -> NKStop vector
  | NKParse (pos, Lazy_vec lv, len) ->
      let d, offset =
        try Utf8.decode_step get s
        with Utf8 -> error s pos "malformed UTF-8 encoding"
      in
      let vec = Lazy_vec {lv with vector = Vector.grow 1l lv.vector} in
      NKParse (pos, lazy_vec_step d vec, len - offset)
  | NKStop l -> assert false (* final step, cannot reduce. *)

let name s =
  let rec step = function NKStop n -> n | k -> step (name_step s k) in
  step NKStart

(* Sections *)

(** Representation of a section tag. *)
type section_tag =
  [ `CodeSection
  | `CustomSection
  | `DataCountSection
  | `DataSection
  | `ElemSection
  | `ExportSection
  | `FuncSection
  | `GlobalSection
  | `ImportSection
  | `MemorySection
  | `StartSection
  | `TableSection
  | `TypeSection ]

let id s =
  let bo = peek s in
  Lib.Option.map
    (function
      | 0 -> `CustomSection
      | 1 -> `TypeSection
      | 2 -> `ImportSection
      | 3 -> `FuncSection
      | 4 -> `TableSection
      | 5 -> `MemorySection
      | 6 -> `GlobalSection
      | 7 -> `ExportSection
      | 8 -> `StartSection
      | 9 -> `ElemSection
      | 10 -> `CodeSection
      | 11 -> `DataSection
      | 12 -> `DataCountSection
      | _ -> error s (pos s) "malformed section id")
    bo

let section_with_size tag f default s =
  match id s with
  | Some tag' when tag' = tag ->
      ignore (u8 s) ;
      sized f s
  | _ -> default

let section tag f default s = section_with_size tag (fun _ -> f) default s

(* Type section *)

type func_type_kont =
  | FKStart
  | FKIns of value_type lazy_vec_kont
  | FKOut of value_type Vector.t * value_type lazy_vec_kont
  | FKStop of func_type

let func_type_step s = function
  | FKStart ->
      let tag = vs7 s in
      let len = len32 s in
      if tag = -0x20 then FKIns (init_lazy_vec (Int32.of_int len))
      else error s (pos s - 1) "malformed function type"
  | FKIns (Lazy_vec {vector = ins; _} as vec) when is_end_of_vec vec ->
      let len = len32 s in
      FKOut (ins, init_lazy_vec (Int32.of_int len))
  | FKIns ins ->
      let vt = value_type s in
      FKIns (lazy_vec_step vt ins)
  | FKOut (ins, (Lazy_vec {vector = out; _} as out_vec))
    when is_end_of_vec out_vec ->
      FKStop (FuncType (ins, out))
  | FKOut (ins, out_vec) ->
      let vt = value_type s in
      FKOut (ins, lazy_vec_step vt out_vec)
  | FKStop _ -> assert false (* cannot reduce *)

(* let _type_ s = at func_type s *)

(* Import section *)

let import_desc s =
  match u8 s with
  | 0x00 -> FuncImport (at var s)
  | 0x01 -> TableImport (table_type s)
  | 0x02 -> MemoryImport (memory_type s)
  | 0x03 -> GlobalImport (global_type s)
  | _ -> error s (pos s - 1) "malformed import kind"

type import_kont =
  | ImpKStart  (** Import parsing starting point. *)
  | ImpKModuleName of name_step
      (** Import module name parsing UTF8 char per char step. *)
  | ImpKItemName of Ast.name * name_step
      (** Import item name parsing UTF8 char per char step. *)
  | ImpKStop of import'  (** Import final step. *)

let import_step s = function
  | ImpKStart -> ImpKModuleName NKStart
  | ImpKModuleName (NKStop module_name) -> ImpKItemName (module_name, NKStart)
  | ImpKModuleName nk -> ImpKModuleName (name_step s nk)
  | ImpKItemName (module_name, NKStop item_name) ->
      let idesc = at import_desc s in
      ImpKStop {module_name; item_name; idesc}
  | ImpKItemName (module_name, nk) -> ImpKItemName (module_name, name_step s nk)
  | ImpKStop _ -> assert false (* Final step, cannot reduce *)

(* Table section *)

let table s =
  let ttype = table_type s in
  {ttype}

(* Memory section *)

let memory s =
  let mtype = memory_type s in
  {mtype}

(* Export section *)

let export_desc s =
  match u8 s with
  | 0x00 -> FuncExport (at var s)
  | 0x01 -> TableExport (at var s)
  | 0x02 -> MemoryExport (at var s)
  | 0x03 -> GlobalExport (at var s)
  | _ -> error s (pos s - 1) "malformed export kind"

type export_kont =
  | ExpKStart  (** Export parsing starting point. *)
  | ExpKName of name_step  (** Export name parsing UTF8 char per char step. *)
  | ExpKStop of export'  (** Export final step. *)

let export_step s = function
  | ExpKStart -> ExpKName NKStart
  | ExpKName (NKStop name) ->
      let edesc = at export_desc s in
      ExpKStop {name; edesc}
  | ExpKName nk -> ExpKName (name_step s nk)
  | ExpKStop _ -> assert false (* Final step, cannot reduce *)

(* Start section *)

let start s =
  let sfunc = at var s in
  {sfunc}

let start_section s = section `StartSection (opt (at start) true) None s

(* Code section *)

let local s =
  let n = vu32 s in
  let t = value_type s in
  (n, t)

(** Code section parsing. *)
type code_kont =
  | CKStart  (** Starting point of a function parsing. *)
  | CKLocalsParse of {
      left : pos;
      size : size;
      pos : pos;
      vec_kont : (int32 * value_type) lazy_vec_kont;
      locals_size : Int64.t;
    }  (** Parse a local value with its number of occurences. *)
  | CKLocalsAccumulate of {
      left : pos;
      size : size;
      pos : pos;
      type_vec : (int32 * value_type) lazy_vec_kont;
      curr_type : (int32 * value_type) option;
      vec_kont : value_type lazy_vec_kont;
    }  (** Accumulate local values. *)
  | CKBody of {
      left : pos;
      size : size;
      locals : value_type Vector.t;
      const_kont : instr_block_kont list;
    }  (** Parsing step of the body of a function. *)
  | CKStop of func  (** Final step of a parsed function, irreducible. *)

let at' left s x =
  let right = pos s in
  Source.(x @@ region s left right)

let code_step s bs =
  let open Lwt.Syntax in
  function
  | CKStart ->
      (* `at` left *)
      let left = pos s in
      let size = size s in
      let pos = pos s in
      (* `vec` size *)
      let n = len32 s in
      CKLocalsParse
        {
          left;
          size;
          pos;
          locals_size = 0L;
          vec_kont = init_lazy_vec (Int32.of_int n);
        }
      |> Lwt.return
  | CKLocalsParse
      {
        left;
        size;
        pos;
        vec_kont = Lazy_vec {vector = types; _} as vec_kont;
        locals_size;
      }
    when is_end_of_vec vec_kont ->
      require (I64.lt_u locals_size 0x1_0000_0000L) s pos "too many locals" ;
      let vec_kont = init_lazy_vec (Int64.to_int32 locals_size) in
      CKLocalsAccumulate
        {
          left;
          size;
          pos;
          vec_kont;
          type_vec = Lazy_vec {offset = 0l; vector = types};
          curr_type = None;
        }
      |> Lwt.return
  | CKLocalsParse {left; size; pos; vec_kont; locals_size} ->
      let local = local s in
      (* small enough to fit in a tick *)
      let locals_size =
        I64.add locals_size (I64_convert.extend_i32_u (fst local))
      in
      CKLocalsParse
        {left; size; pos; vec_kont = lazy_vec_step local vec_kont; locals_size}
      |> Lwt.return
  | CKLocalsAccumulate
      {left; size; pos; vec_kont = Lazy_vec {vector = locals; _} as vec_kont; _}
    when is_end_of_vec vec_kont ->
      CKBody {left; size; locals; const_kont = [IKNext []]} |> Lwt.return
  | CKLocalsAccumulate
      {
        left;
        size;
        pos;
        vec_kont;
        curr_type = None | Some (0l, _);
        type_vec = Lazy_vec {offset = n; vector = types};
      } ->
      let+ next_type = Vector.get n types in
      CKLocalsAccumulate
        {
          left;
          size;
          pos;
          vec_kont;
          curr_type = Some next_type;
          type_vec = Lazy_vec {offset = Int32.succ n; vector = types};
        }
  | CKLocalsAccumulate
      {left; size; pos; vec_kont; curr_type = Some (occurences, ty); type_vec}
    ->
      let remaining_occurences = Int32.pred occurences in
      CKLocalsAccumulate
        {
          left;
          size;
          pos;
          vec_kont = lazy_vec_step ty vec_kont;
          curr_type = Some (remaining_occurences, ty);
          type_vec;
        }
      |> Lwt.return
  | CKBody {left; size; locals; const_kont = [IKStop body]} ->
      end_ s ;
      check_size size s ;
      let func =
        at' left s
        @@ {
             locals;
             body = alloc_block bs body;
             ftype = Source.(-1l @@ Source.no_region);
           }
      in
      CKStop func |> Lwt.return
  | CKBody {left; size; locals; const_kont} ->
      CKBody {left; size; locals; const_kont = instr_block_step s bs const_kont}
      |> Lwt.return
  | CKStop _ -> assert false (* final step, cannot reduce *)

(* Element section *)

let elem_index bs s =
  let x = at var s in
  alloc_block bs [Source.(ref_func x @@ x.at)]

let elem_kind s =
  match u8 s with
  | 0x00 -> FuncRefType
  | _ -> error s (pos s - 1) "malformed element kind"

type index_kind = Indexed | Const

type elem_kont =
  | EKStart  (** Starting point of an element segment parsing. *)
  | EKMode of {
      left : pos;
      index : int32 Source.phrase;
      index_kind : index_kind;
      early_ref_type : ref_type option;
      offset_kont : pos * instr_block_kont list;
    }  (** Element segment mode parsing step. *)
  | EKInitIndexed of {
      mode : segment_mode;
      ref_type : ref_type;
      einit_vec : const lazy_vec_kont;
    }
      (** Element segment initialization code parsing step for referenced values. *)
  | EKInitConst of {
      mode : segment_mode;
      ref_type : ref_type;
      einit_vec : const lazy_vec_kont;
      einit_kont : pos * instr_block_kont list;
    }
      (** Element segment initialization code parsing step for constant values. *)
  | EKStop of elem_segment'  (** Final step of a segment parsing. *)

let ek_start s =
  let v = vu32 s in
  match v with
  | 0x00l ->
      (* active_zero *)
      let index = Source.(0l @@ Source.no_region) in
      let left = pos s in
      EKMode
        {
          left;
          index;
          index_kind = Indexed;
          early_ref_type = Some FuncRefType;
          offset_kont = (left, [IKNext []]);
        }
  | 0x01l ->
      (* passive *)
      let mode_pos = pos s in
      let ref_type = elem_kind s in
      let n = len32 s in
      let mode = Source.(Passive @@ region s mode_pos mode_pos) in
      EKInitIndexed {mode; ref_type; einit_vec = init_lazy_vec (Int32.of_int n)}
  | 0x02l ->
      (* active *)
      let left = pos s in
      let index = at var s in
      let left_offset = pos s in
      EKMode
        {
          left;
          index;
          index_kind = Indexed;
          early_ref_type = None;
          offset_kont = (left_offset, [IKNext []]);
        }
  | 0x03l ->
      (* declarative *)
      let mode_pos = pos s in
      let mode = Source.(Declarative @@ region s mode_pos mode_pos) in
      let ref_type = elem_kind s in
      let n = len32 s in
      EKInitIndexed {mode; ref_type; einit_vec = init_lazy_vec (Int32.of_int n)}
  | 0x04l ->
      (* active_zero *)
      let index = Source.(0l @@ Source.no_region) in
      let left = pos s in
      EKMode
        {
          left;
          index;
          index_kind = Const;
          early_ref_type = Some FuncRefType;
          offset_kont = (left, [IKNext []]);
        }
  | 0x05l ->
      (* passive *)
      let mode_pos = pos s in
      let mode = Source.(Passive @@ region s mode_pos mode_pos) in
      let ref_type = ref_type s in
      let n = len32 s in
      let left = pos s in
      EKInitConst
        {
          mode;
          ref_type;
          einit_vec = init_lazy_vec (Int32.of_int n);
          einit_kont = (left, [IKNext []]);
        }
  | 0x06l ->
      (* active *)
      let left = pos s in
      let index = at var s in
      let left_offset = pos s in
      EKMode
        {
          left;
          index;
          index_kind = Const;
          early_ref_type = None;
          offset_kont = (left_offset, [IKNext []]);
        }
  | 0x07l ->
      (* declarative *)
      let mode_pos = pos s in
      let mode = Source.(Declarative @@ region s mode_pos mode_pos) in
      let ref_type = ref_type s in
      let n = len32 s in
      let left = pos s in
      EKInitConst
        {
          mode;
          ref_type;
          einit_vec = init_lazy_vec (Int32.of_int n);
          einit_kont = (left, [IKNext []]);
        }
  | _ -> error s (pos s - 1) "malformed elements segment kind"

let elem_step s bs = function
  | EKStart -> ek_start s
  | EKMode
      {
        left;
        index;
        index_kind;
        early_ref_type;
        offset_kont = left_offset, [IKStop offset];
      } ->
      end_ s ;
      let right = pos s in
      let offset =
        Source.(alloc_block bs offset @@ region s left_offset right)
      in
      let mode = Source.(Active {index; offset} @@ region s left right) in
      let ref_type =
        match early_ref_type with
        | Some t -> t
        | None -> if index_kind = Indexed then elem_kind s else ref_type s
      in
      (* `vec` size *)
      let n = len32 s in
      if index_kind = Indexed then
        EKInitIndexed
          {mode; ref_type; einit_vec = init_lazy_vec (Int32.of_int n)}
      else
        let left = pos s in
        EKInitConst
          {
            mode;
            ref_type;
            einit_vec = init_lazy_vec (Int32.of_int n);
            einit_kont = (left, [IKNext []]);
          }
  | EKMode
      {left; index; index_kind; early_ref_type; offset_kont = left_offset, k} ->
      let k' = instr_block_step s bs k in
      EKMode
        {
          left;
          index;
          index_kind;
          early_ref_type;
          offset_kont = (left_offset, k');
        }
  (* End of initialization parsing *)
  | EKInitConst
      {mode; ref_type; einit_vec = Lazy_vec {vector = einit; _} as einit_vec; _}
  | EKInitIndexed
      {mode; ref_type; einit_vec = Lazy_vec {vector = einit; _} as einit_vec}
    when is_end_of_vec einit_vec ->
      EKStop {etype = ref_type; einit; emode = mode}
  (* Indexed *)
  | EKInitIndexed {mode; ref_type; einit_vec} ->
      let elem_index = at (elem_index bs) s in
      EKInitIndexed
        {mode; ref_type; einit_vec = lazy_vec_step elem_index einit_vec}
  (* Const *)
  | EKInitConst {mode; ref_type; einit_vec; einit_kont = left, [IKStop einit]}
    ->
      end_ s ;
      let right = pos s in
      let einit = Source.(alloc_block bs einit @@ region s left right) in
      EKInitConst
        {
          mode;
          ref_type;
          einit_vec = lazy_vec_step einit einit_vec;
          einit_kont = (right, [IKNext []]);
        }
  | EKInitConst {mode; ref_type; einit_vec; einit_kont = left, k} ->
      let k' = instr_block_step s bs k in
      EKInitConst {mode; ref_type; einit_vec; einit_kont = (left, k')}
  | EKStop _ -> assert false (* Final step, cannot reduce *)

(* Data section *)

type data_kont =
  | DKStart  (** Starting point of a data segment parsing. *)
  | DKMode of {
      left : pos;
      index : int32 Source.phrase;
      offset_kont : pos * instr_block_kont list;
    }  (** Data segment mode parsing step. *)
  | DKInit of {dmode : segment_mode; init_kont : byte_vector_kont}
  | DKStop of data_segment'  (** Final step of a data segment parsing. *)

let data_start s =
  match vu32 s with
  | 0x00l ->
      (* active_zero *)
      let index = Source.(0l @@ Source.no_region) in
      let left = pos s in
      DKMode {left; index; offset_kont = (left, [IKNext []])}
  | 0x01l ->
      (* passive *)
      let mode_pos = pos s in
      let dmode = Source.(Passive @@ region s mode_pos mode_pos) in
      DKInit {dmode; init_kont = VKStart}
  | 0x02l ->
      (* active *)
      let left = pos s in
      let index = at var s in
      let left_offset = pos s in
      DKMode {left; index; offset_kont = (left_offset, [IKNext []])}
  | _ -> error s (pos s - 1) "malformed data segment kind"

let data_step s bs =
  let open Lwt.Syntax in
  function
  | DKStart -> data_start s |> Lwt.return
  | DKMode {left; index; offset_kont = left_offset, [IKStop offset]} ->
      end_ s ;
      let right = pos s in
      let offset =
        Source.(alloc_block bs offset @@ region s left_offset right)
      in
      let dmode = Source.(Active {index; offset} @@ region s left right) in
      DKInit {dmode; init_kont = VKStart} |> Lwt.return
  | DKMode {left; index; offset_kont = left_offset, k} ->
      let k' = instr_block_step s bs k in
      DKMode {left; index; offset_kont = (left_offset, k')} |> Lwt.return
  | DKInit {dmode; init_kont = VKStop dinit} ->
      DKStop {dmode; dinit} |> Lwt.return
  | DKInit {dmode; init_kont} ->
      let+ init_kont = byte_vector_step s init_kont in
      DKInit {dmode; init_kont}
  | DKStop _ -> assert false (* final step, cannot reduce *)

(* DataCount section *)

let data_count s = Some (vu32 s)

let data_count_section s = section `DataCountSection data_count None s

(* Custom section *)

let custom size s =
  let start = pos s in
  let id = name s in
  let bs = get_string (size - (pos s - start)) s in
  Some (id, bs)

let custom_section s = section_with_size `CustomSection custom None s

let non_custom_section s =
  match id s with
  | None | Some `CustomSection -> None
  | _ ->
      skip 1 s ;
      sized skip s ;
      Some ()

(* Modules *)

let rec iterate f s = if f s <> None then iterate f s

let magic = 0x6d736100l

(** Sections representation. *)
type _ field_type =
  | TypeField : type_ field_type
  | ImportField : import field_type
  | FuncField : var field_type
  | TableField : table field_type
  | MemoryField : memory field_type
  | GlobalField : global field_type
  | ExportField : export field_type
  | StartField : start field_type
  | ElemField : elem_segment field_type
  | DataCountField : int32 field_type
  | CodeField : func field_type
  | DataField : data_segment field_type

(** Result of a section parsing, being either a single value or a vector. *)
type field =
  | VecField : 'a field_type * 'a Vector.t -> field
  | SingleField : 'a field_type * 'a option -> field

(** Module parsing steps *)
type module_kont =
  | MKStart  (** Initial state of a module parsing *)
  | MKSkipCustom : ('a field_type * section_tag) option -> module_kont
      (** Custom section which are skipped, with the next section to parse. *)
  | MKFieldStart : 'a field_type * section_tag -> module_kont
      (** Starting point of a section, handles parsing generic section header. *)
  | MKField : 'a field_type * size * 'a lazy_vec_kont -> module_kont
      (** Section currently parsed, accumulating each element from the underlying vector. *)
  | MKElaborateFunc :
      var Vector.t * func Vector.t * func lazy_vec_kont * bool
      -> module_kont
      (** Elaboration of functions from the code section with their declared type in
      the func section, and accumulating invariants conditions associated to
      functions. *)
  | MKBuild of func Vector.t option * bool
      (** Accumulating the parsed sections vectors into a module and checking
      invariants. *)
  | MKStop of module_'  (** Final step of the parsing, cannot reduce. *)
    (* TODO (https://gitlab.com/tezos/tezos/-/issues/3120): actually, should be module_ *)
  | MKTypes of func_type_kont * pos * size * type_ lazy_vec_kont
      (** Function types section parsing. *)
  | MKImport of import_kont * pos * size * import lazy_vec_kont
      (** Import section parsing. *)
  | MKExport of export_kont * pos * size * export lazy_vec_kont
      (** Export section parsing. *)
  | MKGlobal of
      global_type * int * instr_block_kont list * size * global lazy_vec_kont
      (** Globals section parsing, containing the starting position, the
      continuation of the current global block instruction, and the size of the
      section. *)
  | MKElem of elem_kont * int * size * elem_segment lazy_vec_kont
      (** Element segments section parsing, containing the current element parsing
      continuation, the starting position of the current element, the size of
      the section. *)
  | MKData of data_kont * int * size * data_segment lazy_vec_kont
      (** Data segments section parsing, containing the current data parsing
      continuation, the starting position of the current data, the size of the
      section. *)
  | MKCode of code_kont * int * size * func lazy_vec_kont
      (** Code section parsing, containing the current function parsing
      continuation, the starting position of the current function, the size of
      the section. *)

type decode_kont = {
  building_state : field Vector.t;  (** Accumulated parsed sections. *)
  module_kont : module_kont;
  stream : stream;
  block_state : block_state;
}

(* TODO: https://gitlab.com/tezos/tezos/-/issues/3366
   Check the size of the proof generated after calling `find_vec'`. *)
let rec find_vec' :
    type t. t field_type -> int32 -> _ Vector.t -> t Vector.t Lwt.t =
 fun ty index fields ->
  (* This function is called once the whole module has been parsed. As such
     each module section is available and it cannot fail. *)
  let open Lwt.Syntax in
  if Vector.num_elements fields <= index then invalid_arg "find_vec"
  else
    let* field = Vector.get index fields in
    match field with
    | SingleField _ -> find_vec' ty (Int32.add index 1l) fields
    | VecField (ty', v) -> (
        let v = Lwt.return v in
        match (ty, ty') with
        (* TODO (https://gitlab.com/tezos/tezos/-/issues/3120):
           factor this out with a Leibnitz equality witness *)
        | TypeField, TypeField -> v
        | ImportField, ImportField -> v
        | FuncField, FuncField -> v
        | TableField, TableField -> v
        | MemoryField, MemoryField -> v
        | GlobalField, GlobalField -> v
        | ExportField, ExportField -> v
        | StartField, StartField -> v
        | ElemField, ElemField -> v
        | DataCountField, DataCountField -> v
        | CodeField, CodeField -> v
        | DataField, DataField -> v
        | _ -> find_vec' ty (Int32.succ index) fields)

(* TODO: https://gitlab.com/tezos/tezos/-/issues/3366
   Check the size of the proof generated after calling `find_single'`. *)
let rec find_single' :
    type t. t field_type -> int32 -> _ Vector.t -> t option Lwt.t =
 fun ty index fields ->
  (* This function is called once the whole module has been parsed. As such
     each module section is available and it cannot fail. *)
  let open Lwt.Syntax in
  if Vector.num_elements fields <= index then invalid_arg "find_single"
  else
    let* field = Vector.get index fields in
    match field with
    | VecField _ -> find_single' ty (Int32.add index 1l) fields
    | SingleField (ty', v) -> (
        let v = Lwt.return v in
        match (ty, ty') with
        | TypeField, TypeField -> v
        | ImportField, ImportField -> v
        | FuncField, FuncField -> v
        | TableField, TableField -> v
        | MemoryField, MemoryField -> v
        | GlobalField, GlobalField -> v
        | ExportField, ExportField -> v
        | StartField, StartField -> v
        | ElemField, ElemField -> v
        | DataCountField, DataCountField -> v
        | CodeField, CodeField -> v
        | DataField, DataField -> v
        | _ -> find_single' ty (Int32.succ index) fields)

let find_vec ty fields = find_vec' ty 0l fields

let find_single ty fields = find_single' ty 0l fields

let vec_field ty (Lazy_vec {vector; _}) = VecField (ty, vector)

let module_step state =
  let open Lwt.Syntax in
  let next module_kont = Lwt.return {state with module_kont} in
  let next_with_field field module_kont =
    Lwt.return
      {
        state with
        building_state = Vector.cons field state.building_state;
        module_kont;
      }
  in
  let s = state.stream in
  let bs = state.block_state in
  match state.module_kont with
  | MKStart ->
      (* Module header *)
      let header = u32 s in
      require (header = magic) s 0 "magic header not detected" ;
      let version = u32 s in
      require (version = Encode.version) s 4 "unknown binary version" ;
      (* Module header *)
      next @@ MKSkipCustom (Some (TypeField, `TypeSection))
  | MKSkipCustom k -> (
      match id s with
      | Some `CustomSection ->
          (* section_with_size *)
          ignore (u8 s) ;
          (* sized *)
          let l = len32 s in
          (* custom *)
          let start = pos s in
          let _id = name s in
          let _bs = get_string (l - (pos s - start)) s in
          next @@ MKSkipCustom k
      | _ -> (
          match k with
          | None ->
              let* func_types = find_vec FuncField state.building_state in
              let* func_bodies = find_vec CodeField state.building_state in
              if Vector.(num_elements func_types <> num_elements func_bodies)
              then next @@ MKBuild (None, true)
              else
                next
                @@ MKElaborateFunc
                     ( func_types,
                       func_bodies,
                       init_lazy_vec (Vector.num_elements func_types),
                       true )
          | Some (ty, tag) -> next @@ MKFieldStart (ty, tag)))
  | MKFieldStart (DataCountField, `DataCountSection) ->
      let v = data_count_section s in
      next_with_field
        (SingleField (DataCountField, v))
        (MKSkipCustom (Some (CodeField, `CodeSection)))
  | MKFieldStart (StartField, `StartSection) ->
      let v = start_section s in
      next_with_field
        (SingleField (StartField, v))
        (MKSkipCustom (Some (ElemField, `ElemSection)))
  (* Parsing of fields vector. *)
  | MKFieldStart (ty, tag) -> (
      match id s with
      | Some t when t = tag ->
          ignore (u8 s) ;
          let size = size s in
          (* length of `vec` *)
          let l = len32 s in
          next @@ MKField (ty, size, init_lazy_vec (Int32.of_int l))
      | _ ->
          let size = {size = 0; start = pos s} in
          next @@ MKField (ty, size, init_lazy_vec 0l)
          (* Transitions steps from the end of a section to the next one.

             The values accumulated from the section are accumulated into the building
             state..*))
  (* TODO (https://gitlab.com/tezos/tezos/-/issues/3120): maybe we can factor-out these similarly shaped module section transitions *)
  | MKField (TypeField, size, vec) when is_end_of_vec vec ->
      check_size size s ;
      next_with_field
        (vec_field TypeField vec)
        (MKSkipCustom (Some (ImportField, `ImportSection)))
  | MKField (ImportField, size, vec) when is_end_of_vec vec ->
      check_size size s ;
      next_with_field
        (vec_field ImportField vec)
        (MKSkipCustom (Some (FuncField, `FuncSection)))
  | MKField (FuncField, size, vec) when is_end_of_vec vec ->
      check_size size s ;
      next_with_field
        (vec_field FuncField vec)
        (MKSkipCustom (Some (TableField, `TableSection)))
  | MKField (TableField, size, vec) when is_end_of_vec vec ->
      check_size size s ;
      next_with_field
        (vec_field TableField vec)
        (MKSkipCustom (Some (MemoryField, `MemorySection)))
  | MKField (MemoryField, size, vec) when is_end_of_vec vec ->
      check_size size s ;
      next_with_field
        (vec_field MemoryField vec)
        (MKSkipCustom (Some (GlobalField, `GlobalSection)))
  | MKField (GlobalField, size, vec) when is_end_of_vec vec ->
      check_size size s ;
      next_with_field
        (vec_field GlobalField vec)
        (MKSkipCustom (Some (ExportField, `ExportSection)))
  | MKField (ExportField, size, vec) when is_end_of_vec vec ->
      check_size size s ;
      next_with_field
        (vec_field ExportField vec)
        (MKSkipCustom (Some (StartField, `StartSection)))
  | MKField (ElemField, size, vec) when is_end_of_vec vec ->
      check_size size s ;
      next_with_field
        (vec_field ElemField vec)
        (MKSkipCustom (Some (DataCountField, `DataCountSection)))
  | MKField (CodeField, size, vec) when is_end_of_vec vec ->
      check_size size s ;
      next_with_field
        (vec_field CodeField vec)
        (MKSkipCustom (Some (DataField, `DataSection)))
  | MKField (DataField, size, vec) when is_end_of_vec vec ->
      check_size size s ;
      next_with_field
        (vec_field DataField vec)
        (* All sections are parsed, time to build the module *)
        (MKSkipCustom None)
  | MKField (ty, size, vec) -> (
      match ty with
      | TypeField -> next @@ MKTypes (FKStart, pos s, size, vec)
      | ImportField -> next @@ MKImport (ImpKStart, pos s, size, vec)
      | FuncField ->
          let f = at var s in
          (* small enough to fit in a tick *)
          next @@ MKField (ty, size, lazy_vec_step f vec)
      | TableField ->
          let f = at table s in
          (* small enough to fit in a tick *)
          next @@ MKField (ty, size, lazy_vec_step f vec)
      | MemoryField ->
          let f = at memory s in
          (* small enough to fit in a tick *)
          next @@ MKField (ty, size, lazy_vec_step f vec)
      | GlobalField ->
          let gtype = global_type s in
          next @@ MKGlobal (gtype, pos s, [IKNext []], size, vec)
      | ExportField -> next @@ MKExport (ExpKStart, pos s, size, vec)
      | StartField ->
          (* not a vector *)
          assert false
      | ElemField -> next @@ MKElem (EKStart, pos s, size, vec)
      | DataCountField ->
          (* not a vector *)
          assert false
      | CodeField -> next @@ MKCode (CKStart, pos s, size, vec)
      | DataField ->
          next @@ MKData (DKStart, pos s, size, vec)
          (* These sections have a distinct step mechanism. *))
  | MKTypes (FKStop func_type, left, size, vec) ->
      let f = Source.(func_type @@ region s left (pos s)) in
      next @@ MKField (TypeField, size, lazy_vec_step f vec)
  | MKTypes (k, pos, size, curr_vec) ->
      next @@ MKTypes (func_type_step s k, pos, size, curr_vec)
  | MKImport (ImpKStop import, left, size, vec) ->
      let f = Source.(import @@ region s left (pos s)) in
      next @@ MKField (ImportField, size, lazy_vec_step f vec)
  | MKImport (k, pos, size, curr_vec) ->
      next @@ MKImport (import_step s k, pos, size, curr_vec)
  | MKExport (ExpKStop import, left, size, vec) ->
      let f = Source.(import @@ region s left (pos s)) in
      next @@ MKField (ExportField, size, lazy_vec_step f vec)
  | MKExport (k, pos, size, curr_vec) ->
      next @@ MKExport (export_step s k, pos, size, curr_vec)
  | MKGlobal (gtype, left, [IKStop res], size, vec) ->
      end_ s ;
      let ginit = Source.(alloc_block bs res @@ region s left (pos s)) in
      let f = Source.({gtype; ginit} @@ region s left (pos s)) in
      next @@ MKField (GlobalField, size, lazy_vec_step f vec)
  | MKGlobal (ty, pos, k, size, curr_vec) ->
      next @@ MKGlobal (ty, pos, instr_block_step s bs k, size, curr_vec)
  | MKElem (EKStop elem, left, size, vec) ->
      let elem = Source.(elem @@ region s left (pos s)) in
      next @@ MKField (ElemField, size, lazy_vec_step elem vec)
  | MKElem (elem_kont, pos, size, curr_vec) ->
      next @@ MKElem (elem_step s bs elem_kont, pos, size, curr_vec)
  | MKData (DKStop data, left, size, vec) ->
      let data = Source.(data @@ region s left (pos s)) in
      next @@ MKField (DataField, size, lazy_vec_step data vec)
  | MKData (data_kont, pos, size, curr_vec) ->
      let* data_kont = data_step s bs data_kont in
      next @@ MKData (data_kont, pos, size, curr_vec)
  | MKCode (CKStop func, left, size, vec) ->
      next @@ MKField (CodeField, size, lazy_vec_step func vec)
  | MKCode (code_kont, pos, size, curr_vec) ->
      let* code_kont = code_step s bs code_kont in
      next @@ MKCode (code_kont, pos, size, curr_vec)
  | MKElaborateFunc
      (ft, fb, (Lazy_vec {vector = func_types; _} as vec), no_datas_in_func)
    when is_end_of_vec vec ->
      next @@ MKBuild (Some func_types, no_datas_in_func)
  | MKElaborateFunc (fts, fbs, (Lazy_vec {offset; _} as vec), no_datas_in_func)
    ->
      let* ft = Vector.get offset fts in
      let* fb = Vector.get offset fbs in
      let fb' = Source.({fb.it with ftype = ft} @@ fb.at) in
      next
      @@ MKElaborateFunc
           ( fts,
             fbs,
             lazy_vec_step fb' vec,
             (* TODO: https://gitlab.com/tezos/tezos/-/issues/3387

                `Free` shouldn't be part of the PVM.*)
             no_datas_in_func
             && Free.((func (lookup_block bs) fb').datas = Set.empty) )
  | MKBuild (funcs, no_datas_in_func) ->
      let fields = state.building_state in
      let* types = find_vec TypeField fields in
      let* data_count = find_single DataCountField fields in
      let* datas = find_vec DataField fields in
      let* elems = find_vec ElemField fields in
      let* start = find_single StartField fields in
      let* tables = find_vec TableField fields in
      let* memories = find_vec MemoryField fields in
      let* globals = find_vec GlobalField fields in
      let* imports = find_vec ImportField fields in
      let* exports = find_vec ExportField fields in
      ignore types ;
      require (pos s = len s) s (len s) "unexpected content after last section" ;
      require
        (data_count = None || data_count = Some (Vector.num_elements datas))
        s
        (len s)
        "data count and data section have inconsistent lengths" ;
      let funcs =
        match funcs with
        | None ->
            error
              s
              (len s)
              "function and code section have inconsistent lengths"
        | Some l -> l
      in
      require
        (data_count <> None || no_datas_in_func)
        s
        (len s)
        "data count section required" ;
      {
        state with
        building_state = Vector.create 0l;
        (* At this point, there shouldn't be any new fields added, we can safely
           reset the building state. *)
        module_kont =
          MKStop
            {
              types;
              tables;
              memories;
              globals;
              funcs;
              imports;
              exports;
              elems;
              datas;
              start;
              blocks =
                Array.init !(bs.next_block) (fun i -> Hashtbl.find bs.blocks i);
            };
      }
      |> Lwt.return
  | MKStop _ (* Stop cannot reduce. *) -> assert false

let module_ stream =
  let open Lwt.Syntax in
  let rec loop = function
    | {module_kont = MKStop m; _} -> Lwt.return m
    | k ->
        let* next_state = module_step k in
        loop next_state
  in
  loop
    {
      building_state = Vector.create 0l;
      module_kont = MKStart;
      stream;
      block_state = make_block_state ();
    }

let decode ~name ~bytes =
  let open Lwt.Syntax in
  let s = make_stream ~name ~bytes in
  let left = pos s in
  let+ m = module_ s in
  let right = pos s in
  Source.(m @@ region s left right)

let all_custom tag s =
  let header = u32 s in
  require (header = magic) s 0 "magic header not detected" ;
  let version = u32 s in
  require (version = Encode.version) s 4 "unknown binary version" ;
  let rec collect () =
    iterate non_custom_section s ;
    match custom_section s with
    | None -> []
    | Some (n, s) when n = tag -> s :: collect ()
    | Some _ -> collect ()
  in
  collect ()

let decode_custom tag ~name ~bytes = all_custom tag (make_stream ~name ~bytes)
