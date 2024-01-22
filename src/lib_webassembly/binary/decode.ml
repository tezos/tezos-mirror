(* Decoding stream *)

open Binary_exn
module Vector = Lazy_vector.Int32Vector

let lwt_ignore p =
  let open Lwt.Syntax in
  let* _ = p in
  Lwt.return_unit

type stream = {name : string; bytes : Chunked_byte_vector.t; mutable pos : int}

let make_stream ~name ~bytes = {name; bytes; pos = 0}

let empty_block = Ast.Block_label 0l

let len64 s = Chunked_byte_vector.length s.bytes

let len s = Int64.to_int @@ len64 s

let pos s = s.pos

let eos s = pos s = len s

let check n s = if pos s + n > len s then raise EOS

let skip n s =
  if n < 0 then raise EOS else check n s ;
  s.pos <- s.pos + n

let read s = Chunked_byte_vector.load_byte s.bytes (Int64.of_int s.pos)

let peek s =
  let open Lwt.Syntax in
  if eos s then Lwt.return_none
  else
    let+ x = read s in
    Some x

let get s =
  let open Lwt.Syntax in
  check 1 s ;
  let+ b = read s in
  skip 1 s ;
  b

let get_string n s =
  let open Lwt.Syntax in
  let rec ( -- ) x y () =
    Lwt.return @@ if x = y then Lwt_seq.Nil else Lwt_seq.Cons (x, x + 1 -- y)
  in
  if n < 0 then raise EOS else check n s ;
  let buffer = Bytes.make n '\000' in
  let+ () =
    Lwt_seq.iter_s
      (fun i ->
        let+ b = get s in
        Bytes.set buffer i (Char.chr b))
      (0 -- n)
  in
  Bytes.to_string buffer

(* Errors *)

exception Code = Decode_error.Error

type state =
  | Byte_vector_step
  | Instr_step
  | Instr_block_step
  | Block_step
  | Name_step
  | Func_type_step
  | Import_step
  | Export_step
  | Code_step
  | Elem_step
  | Data_step
  | Module_step

exception Step_error of state

let string_of_byte b = Format.sprintf "%02x" b

let string_of_multi n = Format.sprintf "%02lx" n

let position_ file pos = Source.{file; line = -1; column = pos}

let region_ file left right =
  Source.{left = position_ file left; right = position_ file right}

let region s = region_ s.name

let error s pos msg = raise (Code (region s pos pos, msg))

let require b s pos msg = if not b then error s pos msg

let guard f s =
  try f s with EOS -> error s (len s) "unexpected end of section or function"

let guard_float ~allow_floats f s =
  if allow_floats then f s
  else
    Floating_point.error
      (region s (pos s) (pos s))
      "float instructions are forbidden"

let get = guard get

let get_string n = guard (get_string n)

let skip n = guard (skip n)

let expect b s msg =
  let open Lwt.Syntax in
  let+ x = guard get s in
  require (x = b) s (pos s - 1) msg

let illegal s pos b = error s pos ("illegal opcode " ^ string_of_byte b)

let illegal2 s pos b n =
  error s pos ("illegal opcode " ^ string_of_byte b ^ " " ^ string_of_multi n)

let at f s =
  let open Lwt.Syntax in
  let left = pos s in
  let+ x = f s in
  let right = pos s in
  Source.(x @@ region s left right)

let at_s f s =
  let open Lwt.Syntax in
  let left = pos s in
  let+ x = f s in
  let right = pos s in
  Source.(x @@ region s left right)

(* Generic values *)

let u8 s = get s

let u16 s =
  let open Lwt.Syntax in
  let* lo = u8 s in
  let+ hi = u8 s in
  (hi lsl 8) + lo

let u32 s =
  let open Lwt.Syntax in
  let* lo = u16 s in
  let lo = Int32.of_int lo in
  let+ hi = u16 s in
  let hi = Int32.of_int hi in
  Int32.(add lo (shift_left hi 16))

let u64 s =
  let open Lwt.Syntax in
  let* lo = u32 s in
  let lo = I64_convert.extend_i32_u lo in
  let+ hi = u32 s in
  let hi = I64_convert.extend_i32_u hi in
  Int64.(add lo (shift_left hi 32))

let rec vuN n s =
  let open Lwt.Syntax in
  require (n > 0) s (pos s) "integer representation too long" ;
  let* b = u8 s in
  require (n >= 7 || b land 0x7f < 1 lsl n) s (pos s - 1) "integer too large" ;
  let x = Int64.of_int (b land 0x7f) in
  if b land 0x80 = 0 then Lwt.return x
  else
    let+ v = vuN (n - 7) s in
    Int64.(logor x (shift_left v 7))

let rec vsN n s =
  let open Lwt.Syntax in
  require (n > 0) s (pos s) "integer representation too long" ;
  let* b = u8 s in
  let mask = (-1 lsl (n - 1)) land 0x7f in
  require
    (n >= 7 || b land mask = 0 || b land mask = mask)
    s
    (pos s - 1)
    "integer too large" ;
  let x = Int64.of_int (b land 0x7f) in
  if b land 0x80 = 0 then
    if b land 0x40 = 0 then Lwt.return x
    else Lwt.return Int64.(logor x (logxor (-1L) 0x7fL))
  else
    let+ v = vsN (n - 7) s in
    Int64.(logor x (shift_left v 7))

let vu1 s =
  let open Lwt.Syntax in
  let+ x = vuN 1 s in
  Int64.to_int x

let vu32 s =
  let open Lwt.Syntax in
  let+ x = vuN 32 s in
  Int64.to_int32 x

let vs7 s =
  let open Lwt.Syntax in
  let+ x = vsN 7 s in
  Int64.to_int x

let vs32 s =
  let open Lwt.Syntax in
  let+ x = vsN 32 s in
  Int64.to_int32 x

let vs33 s =
  let open Lwt.Syntax in
  let+ x = vsN 33 s in
  I32_convert.wrap_i64 x

let vs64 s = vsN 64 s

let f32 s =
  let open Lwt.Syntax in
  let+ x = u32 s in
  F32.of_bits x

let f64 s =
  let open Lwt.Syntax in
  let+ x = u64 s in
  F64.of_bits x

let v128 s =
  let open Lwt.Syntax in
  let+ x = get_string (Types.vec_size Types.V128Type) s in
  V128.of_bits x

let len32 s =
  let open Lwt.Syntax in
  let pos = pos s in
  let+ n = vu32 s in
  if I32.le_u n (Int32.of_int (len s - pos)) then Int32.to_int n
  else error s pos "length out of bounds"

let bool s =
  let open Lwt.Syntax in
  let+ x = vu1 s in
  x = 1

let rec list f n s =
  let open Lwt.Syntax in
  if n = 0 then Lwt.return_nil
  else
    let* x = f s in
    let+ rst = list f (n - 1) s in
    x :: rst

let opt f b s =
  let open Lwt.Syntax in
  if b then
    let+ x = f s in
    Some x
  else Lwt.return_none

let vec f s =
  let open Lwt.Syntax in
  let* n = len32 s in
  list f n s

let sized f s =
  let open Lwt.Syntax in
  let* size = len32 s in
  let start = pos s in
  let+ x = f size s in
  require (pos s = start + size) s start "section size mismatch" ;
  x

(** Incremental chunked byte vector creation (from implicit input). *)
type byte_vector_kont =
  | VKStart  (** Initial step. *)
  | VKRead of Ast.data_label * int64 * int64
      (** Reading step, containing the current position in the string and the
      length, reading byte per byte. *)
  | VKStop of Ast.data_label  (** Final step, cannot reduce. *)

let byte_vector_step vecs s =
  let open Lwt.Syntax in
  function
  | VKStart ->
      let* len = len32 s in
      let len = Int64.of_int len in
      let vector = Ast.alloc_data vecs len in
      VKRead (vector, 0L, len) |> Lwt.return
  | VKRead (vector, index, len) when Int64.compare index len >= 0 ->
      VKStop vector |> Lwt.return
  | VKRead (vector, index, len) ->
      let* c = get s in
      let+ () = Ast.add_to_data vecs vector index c in
      VKRead (vector, Int64.succ index, len)
  (* Final step, cannot reduce *)
  | VKStop _ -> raise (Step_error Byte_vector_step)

(* Types *)

open Types

let num_type ~allow_floats s =
  let open Lwt.Syntax in
  let+ x = vs7 s in
  match x with
  | -0x01 -> I32Type
  | -0x02 -> I64Type
  | -0x03 -> guard_float ~allow_floats (fun _ -> F32Type) s
  | -0x04 -> guard_float ~allow_floats (fun _ -> F64Type) s
  | _ -> error s (pos s - 1) "malformed number type"

let vec_type s =
  let open Lwt.Syntax in
  let+ x = vs7 s in
  match x with
  | -0x05 -> V128Type
  | _ -> error s (pos s - 1) "malformed vector type"

let ref_type s =
  let open Lwt.Syntax in
  let+ x = vs7 s in
  match x with
  | -0x10 -> FuncRefType
  | -0x11 -> ExternRefType
  | _ -> error s (pos s - 1) "malformed reference type"

let value_type ~allow_floats s =
  let open Lwt.Syntax in
  let* x = peek s in
  match x with
  | Some n when n >= -0x04 land 0x7f ->
      let+ x = num_type ~allow_floats s in
      NumType x
  | Some n when n >= -0x0f land 0x7f ->
      let+ x = vec_type s in
      VecType x
  | _ ->
      let+ x = ref_type s in
      RefType x

let limits vu s =
  let open Lwt.Syntax in
  let* has_max = bool s in
  let* min = vu s in
  let+ max = opt vu has_max s in
  {min; max}

let table_type s =
  let open Lwt.Syntax in
  let* t = ref_type s in
  let+ lim = limits vu32 s in
  TableType (lim, t)

let memory_type s =
  let open Lwt.Syntax in
  let+ lim = limits vu32 s in
  MemoryType lim

let mutability s =
  let open Lwt.Syntax in
  let+ x = u8 s in
  match x with
  | 0 -> Immutable
  | 1 -> Mutable
  | _ -> error s (pos s - 1) "malformed mutability"

let global_type ~allow_floats s =
  let open Lwt.Syntax in
  let* t = value_type ~allow_floats s in
  let+ mut = mutability s in
  GlobalType (t, mut)

(* Decode instructions *)

open Ast
open Operators

let var s = vu32 s

let op s = u8 s

let end_ s = expect 0x0b s "END opcode expected"

let zero s = expect 0x00 s "zero byte expected"

let memop s =
  let open Lwt.Syntax in
  let* align = vu32 s in
  require (I32.le_u align 32l) s (pos s - 1) "malformed memop flags" ;
  let+ offset = vu32 s in
  (Int32.to_int align, offset)

let block_type ~allow_floats s =
  let open Lwt.Syntax in
  let* x = peek s in
  match x with
  | Some 0x40 ->
      skip 1 s ;
      Lwt.return @@ ValBlockType None
  | Some b when b land 0xc0 = 0x40 ->
      let+ x = value_type ~allow_floats s in
      ValBlockType (Some x)
  | _ ->
      let+ x = at vs33 s in
      VarBlockType x

type 'a lazy_stack = LazyStack of {length : int32; vector : 'a Vector.t}

let empty_stack () = LazyStack {length = 0l; vector = Vector.empty ()}

let push_stack v (LazyStack {length; vector}) =
  let vector, length =
    if length >= Vector.num_elements vector then Vector.append v vector
    else (Vector.set length v vector, length)
  in
  LazyStack {length = Int32.succ length; vector}

let push_rev_values values stack =
  List.fold_left (fun stack v -> push_stack v stack) stack (List.rev values)

let pop_stack (LazyStack {length; vector}) =
  let open Lwt.Syntax in
  if length = 0l then Lwt.return_none
  else
    let length = Int32.pred length in
    let+ value = Vector.get length vector in
    Some (value, LazyStack {length; vector})

let pop_at_most n stack =
  let open Lwt.Syntax in
  let rec fold acc n stack =
    if n = 0 then Lwt.return (acc, stack)
    else
      let* value = pop_stack stack in
      match value with
      | None -> Lwt.return (acc, stack)
      | Some (v, stack) -> (fold [@ocaml.tailcall]) (v :: acc) (n - 1) stack
  in
  let+ values, stack = fold [] n stack in
  (List.rev values, stack)

(** Instruction parsing continuations. *)
type instr_block_kont =
  | IKStop of block_label  (** Final step of a block parsing. *)
  | IKNext of block_label
      (** Tag parsing, containing the accumulation of already parsed values. *)
  | IKBlock of block_type * int  (** Block parsing step. *)
  | IKLoop of block_type * int  (** Loop parsing step. *)
  | IKIf1 of block_type * int  (** If parsing step. *)
  | IKIf2 of block_type * int * block_label  (** If .. else parsing step. *)

let instr ~allow_floats s pos tag =
  let open Lwt.Syntax in
  let return_float_f f = guard_float ~allow_floats f s in
  let return_float v = guard_float ~allow_floats (fun _ -> Lwt.return v) s in
  match tag with
  (* These tags corresponds to resp. block, loop and if, and are now handled
     directly by the main step loop (see `IKBlock`, `IKLoop` and `IKIf1`. *)
  | 0x02 | 0x03 | 0x04 -> raise (Step_error Instr_step)
  | 0x00 -> Lwt.return unreachable
  | 0x01 -> Lwt.return nop
  | 0x05 -> error s pos "misplaced ELSE opcode"
  | (0x06 | 0x07 | 0x08 | 0x09 | 0x0a) as b -> illegal s pos b
  | 0x0b -> error s pos "misplaced END opcode"
  | 0x0c ->
      let+ x = at var s in
      br x
  | 0x0d ->
      let+ x = at var s in
      br_if x
  | 0x0e ->
      let* xs = vec (at var) s in
      let+ x = at var s in
      br_table xs x
  | 0x0f -> Lwt.return return
  | 0x10 ->
      let+ x = at var s in
      call x
  | 0x11 ->
      let* y = at var s in
      let+ x = at var s in
      call_indirect x y
  | (0x12 | 0x13 | 0x14 | 0x15 | 0x16 | 0x17 | 0x18 | 0x19) as b ->
      illegal s pos b
  | 0x1a -> Lwt.return drop
  | 0x1b -> Lwt.return @@ select None
  | 0x1c ->
      let+ x = vec (value_type ~allow_floats) s in
      select (Some x)
  | (0x1d | 0x1e | 0x1f) as b -> illegal s pos b
  | 0x20 ->
      let+ x = at var s in
      local_get x
  | 0x21 ->
      let+ x = at var s in
      local_set x
  | 0x22 ->
      let+ x = at var s in
      local_tee x
  | 0x23 ->
      let+ x = at var s in
      global_get x
  | 0x24 ->
      let+ x = at var s in
      global_set x
  | 0x25 ->
      let+ x = at var s in
      table_get x
  | 0x26 ->
      let+ x = at var s in
      table_set x
  | 0x27 as b -> illegal s pos b
  | 0x28 ->
      let+ a, o = memop s in
      i32_load a o
  | 0x29 ->
      let+ a, o = memop s in
      i64_load a o
  | 0x2a ->
      return_float_f @@ fun s ->
      let+ a, o = memop s in
      f32_load a o
  | 0x2b ->
      return_float_f @@ fun s ->
      let+ a, o = memop s in
      f64_load a o
  | 0x2c ->
      let+ a, o = memop s in
      i32_load8_s a o
  | 0x2d ->
      let+ a, o = memop s in
      i32_load8_u a o
  | 0x2e ->
      let+ a, o = memop s in
      i32_load16_s a o
  | 0x2f ->
      let+ a, o = memop s in
      i32_load16_u a o
  | 0x30 ->
      let+ a, o = memop s in
      i64_load8_s a o
  | 0x31 ->
      let+ a, o = memop s in
      i64_load8_u a o
  | 0x32 ->
      let+ a, o = memop s in
      i64_load16_s a o
  | 0x33 ->
      let+ a, o = memop s in
      i64_load16_u a o
  | 0x34 ->
      let+ a, o = memop s in
      i64_load32_s a o
  | 0x35 ->
      let+ a, o = memop s in
      i64_load32_u a o
  | 0x36 ->
      let+ a, o = memop s in
      i32_store a o
  | 0x37 ->
      let+ a, o = memop s in
      i64_store a o
  | 0x38 ->
      return_float_f @@ fun s ->
      let+ a, o = memop s in
      f32_store a o
  | 0x39 ->
      return_float_f @@ fun s ->
      let+ a, o = memop s in
      f64_store a o
  | 0x3a ->
      let+ a, o = memop s in
      i32_store8 a o
  | 0x3b ->
      let+ a, o = memop s in
      i32_store16 a o
  | 0x3c ->
      let+ a, o = memop s in
      i64_store8 a o
  | 0x3d ->
      let+ a, o = memop s in
      i64_store16 a o
  | 0x3e ->
      let+ a, o = memop s in
      i64_store32 a o
  | 0x3f ->
      let+ () = zero s in
      memory_size
  | 0x40 ->
      let+ () = zero s in
      memory_grow
  | 0x41 ->
      let+ x = at vs32 s in
      i32_const x
  | 0x42 ->
      let+ x = at vs64 s in
      i64_const x
  | 0x43 ->
      return_float_f @@ fun s ->
      let+ x = at f32 s in
      f32_const x
  | 0x44 ->
      return_float_f @@ fun s ->
      let+ x = at f64 s in
      f64_const x
  | 0x45 -> Lwt.return i32_eqz
  | 0x46 -> Lwt.return i32_eq
  | 0x47 -> Lwt.return i32_ne
  | 0x48 -> Lwt.return i32_lt_s
  | 0x49 -> Lwt.return i32_lt_u
  | 0x4a -> Lwt.return i32_gt_s
  | 0x4b -> Lwt.return i32_gt_u
  | 0x4c -> Lwt.return i32_le_s
  | 0x4d -> Lwt.return i32_le_u
  | 0x4e -> Lwt.return i32_ge_s
  | 0x4f -> Lwt.return i32_ge_u
  | 0x50 -> Lwt.return i64_eqz
  | 0x51 -> Lwt.return i64_eq
  | 0x52 -> Lwt.return i64_ne
  | 0x53 -> Lwt.return i64_lt_s
  | 0x54 -> Lwt.return i64_lt_u
  | 0x55 -> Lwt.return i64_gt_s
  | 0x56 -> Lwt.return i64_gt_u
  | 0x57 -> Lwt.return i64_le_s
  | 0x58 -> Lwt.return i64_le_u
  | 0x59 -> Lwt.return i64_ge_s
  | 0x5a -> Lwt.return i64_ge_u
  | 0x5b -> return_float f32_eq
  | 0x5c -> return_float f32_ne
  | 0x5d -> return_float f32_lt
  | 0x5e -> return_float f32_gt
  | 0x5f -> return_float f32_le
  | 0x60 -> return_float f32_ge
  | 0x61 -> return_float f64_eq
  | 0x62 -> return_float f64_ne
  | 0x63 -> return_float f64_lt
  | 0x64 -> return_float f64_gt
  | 0x65 -> return_float f64_le
  | 0x66 -> return_float f64_ge
  | 0x67 -> Lwt.return i32_clz
  | 0x68 -> Lwt.return i32_ctz
  | 0x69 -> Lwt.return i32_popcnt
  | 0x6a -> Lwt.return i32_add
  | 0x6b -> Lwt.return i32_sub
  | 0x6c -> Lwt.return i32_mul
  | 0x6d -> Lwt.return i32_div_s
  | 0x6e -> Lwt.return i32_div_u
  | 0x6f -> Lwt.return i32_rem_s
  | 0x70 -> Lwt.return i32_rem_u
  | 0x71 -> Lwt.return i32_and
  | 0x72 -> Lwt.return i32_or
  | 0x73 -> Lwt.return i32_xor
  | 0x74 -> Lwt.return i32_shl
  | 0x75 -> Lwt.return i32_shr_s
  | 0x76 -> Lwt.return i32_shr_u
  | 0x77 -> Lwt.return i32_rotl
  | 0x78 -> Lwt.return i32_rotr
  | 0x79 -> Lwt.return i64_clz
  | 0x7a -> Lwt.return i64_ctz
  | 0x7b -> Lwt.return i64_popcnt
  | 0x7c -> Lwt.return i64_add
  | 0x7d -> Lwt.return i64_sub
  | 0x7e -> Lwt.return i64_mul
  | 0x7f -> Lwt.return i64_div_s
  | 0x80 -> Lwt.return i64_div_u
  | 0x81 -> Lwt.return i64_rem_s
  | 0x82 -> Lwt.return i64_rem_u
  | 0x83 -> Lwt.return i64_and
  | 0x84 -> Lwt.return i64_or
  | 0x85 -> Lwt.return i64_xor
  | 0x86 -> Lwt.return i64_shl
  | 0x87 -> Lwt.return i64_shr_s
  | 0x88 -> Lwt.return i64_shr_u
  | 0x89 -> Lwt.return i64_rotl
  | 0x8a -> Lwt.return i64_rotr
  | 0x8b -> return_float f32_abs
  | 0x8c -> return_float f32_neg
  | 0x8d -> return_float f32_ceil
  | 0x8e -> return_float f32_floor
  | 0x8f -> return_float f32_trunc
  | 0x90 -> return_float f32_nearest
  | 0x91 -> return_float f32_sqrt
  | 0x92 -> return_float f32_add
  | 0x93 -> return_float f32_sub
  | 0x94 -> return_float f32_mul
  | 0x95 -> return_float f32_div
  | 0x96 -> return_float f32_min
  | 0x97 -> return_float f32_max
  | 0x98 -> return_float f32_copysign
  | 0x99 -> return_float f64_abs
  | 0x9a -> return_float f64_neg
  | 0x9b -> return_float f64_ceil
  | 0x9c -> return_float f64_floor
  | 0x9d -> return_float f64_trunc
  | 0x9e -> return_float f64_nearest
  | 0x9f -> return_float f64_sqrt
  | 0xa0 -> return_float f64_add
  | 0xa1 -> return_float f64_sub
  | 0xa2 -> return_float f64_mul
  | 0xa3 -> return_float f64_div
  | 0xa4 -> return_float f64_min
  | 0xa5 -> return_float f64_max
  | 0xa6 -> return_float f64_copysign
  | 0xa7 -> Lwt.return i32_wrap_i64
  | 0xa8 -> Lwt.return i32_trunc_f32_s
  | 0xa9 -> Lwt.return i32_trunc_f32_u
  | 0xaa -> Lwt.return i32_trunc_f64_s
  | 0xab -> Lwt.return i32_trunc_f64_u
  | 0xac -> Lwt.return i64_extend_i32_s
  | 0xad -> Lwt.return i64_extend_i32_u
  | 0xae -> Lwt.return i64_trunc_f32_s
  | 0xaf -> Lwt.return i64_trunc_f32_u
  | 0xb0 -> Lwt.return i64_trunc_f64_s
  | 0xb1 -> Lwt.return i64_trunc_f64_u
  | 0xb2 -> return_float f32_convert_i32_s
  | 0xb3 -> return_float f32_convert_i32_u
  | 0xb4 -> return_float f32_convert_i64_s
  | 0xb5 -> return_float f32_convert_i64_u
  | 0xb6 -> return_float f32_demote_f64
  | 0xb7 -> return_float f64_convert_i32_s
  | 0xb8 -> return_float f64_convert_i32_u
  | 0xb9 -> return_float f64_convert_i64_s
  | 0xba -> return_float f64_convert_i64_u
  | 0xbb -> return_float f64_promote_f32
  | 0xbc -> Lwt.return i32_reinterpret_f32
  | 0xbd -> Lwt.return i64_reinterpret_f64
  | 0xbe -> return_float f32_reinterpret_i32
  | 0xbf -> return_float f64_reinterpret_i64
  | 0xc0 -> Lwt.return i32_extend8_s
  | 0xc1 -> Lwt.return i32_extend16_s
  | 0xc2 -> Lwt.return i64_extend8_s
  | 0xc3 -> Lwt.return i64_extend16_s
  | 0xc4 -> Lwt.return i64_extend32_s
  | (0xc5 | 0xc6 | 0xc7 | 0xc8 | 0xc9 | 0xca | 0xcb | 0xcc | 0xcd | 0xce | 0xcf)
    as b ->
      illegal s pos b
  | 0xd0 ->
      let+ x = ref_type s in
      ref_null x
  | 0xd1 -> Lwt.return ref_is_null
  | 0xd2 ->
      let+ x = at var s in
      ref_func x
  | 0xfc as b -> (
      let* x = vu32 s in
      match x with
      | 0x00l -> Lwt.return i32_trunc_sat_f32_s
      | 0x01l -> Lwt.return i32_trunc_sat_f32_u
      | 0x02l -> Lwt.return i32_trunc_sat_f64_s
      | 0x03l -> Lwt.return i32_trunc_sat_f64_u
      | 0x04l -> Lwt.return i64_trunc_sat_f32_s
      | 0x05l -> Lwt.return i64_trunc_sat_f32_u
      | 0x06l -> Lwt.return i64_trunc_sat_f64_s
      | 0x07l -> Lwt.return i64_trunc_sat_f64_u
      | 0x08l ->
          let* x = at var s in
          let+ () = zero s in
          memory_init x
      | 0x09l ->
          let+ x = at var s in
          data_drop x
      | 0x0al ->
          let* () = zero s in
          let+ () = zero s in
          memory_copy
      | 0x0bl ->
          let+ () = zero s in
          memory_fill
      | 0x0cl ->
          let* y = at var s in
          let+ x = at var s in
          table_init x y
      | 0x0dl ->
          let+ x = at var s in
          elem_drop x
      | 0x0el ->
          let* x = at var s in
          let+ y = at var s in
          table_copy x y
      | 0x0fl ->
          let+ x = at var s in
          table_grow x
      | 0x10l ->
          let+ x = at var s in
          table_size x
      | 0x11l ->
          let+ x = at var s in
          table_fill x
      | n -> illegal2 s pos b n)
  | 0xfd -> (
      let* x = vu32 s in
      match x with
      | 0x00l ->
          let+ a, o = memop s in
          v128_load a o
      | 0x01l ->
          let+ a, o = memop s in
          v128_load8x8_s a o
      | 0x02l ->
          let+ a, o = memop s in
          v128_load8x8_u a o
      | 0x03l ->
          let+ a, o = memop s in
          v128_load16x4_s a o
      | 0x04l ->
          let+ a, o = memop s in
          v128_load16x4_u a o
      | 0x05l ->
          let+ a, o = memop s in
          v128_load32x2_s a o
      | 0x06l ->
          let+ a, o = memop s in
          v128_load32x2_u a o
      | 0x07l ->
          let+ a, o = memop s in
          v128_load8_splat a o
      | 0x08l ->
          let+ a, o = memop s in
          v128_load16_splat a o
      | 0x09l ->
          let+ a, o = memop s in
          v128_load32_splat a o
      | 0x0al ->
          let+ a, o = memop s in
          v128_load64_splat a o
      | 0x0bl ->
          let+ a, o = memop s in
          v128_store a o
      | 0x0cl ->
          let+ x = at v128 s in
          v128_const x
      | 0x0dl ->
          let+ l =
            Lwt_list.map_s (fun () -> u8 s) (List.init 16 (fun _ -> ()))
          in
          i8x16_shuffle l
      | 0x0el -> Lwt.return i8x16_swizzle
      | 0x0fl -> Lwt.return i8x16_splat
      | 0x10l -> Lwt.return i16x8_splat
      | 0x11l -> Lwt.return i32x4_splat
      | 0x12l -> Lwt.return i64x2_splat
      | 0x13l -> return_float f32x4_splat
      | 0x14l -> return_float f64x2_splat
      | 0x15l ->
          let+ i = u8 s in
          i8x16_extract_lane_s i
      | 0x16l ->
          let+ i = u8 s in
          i8x16_extract_lane_u i
      | 0x17l ->
          let+ i = u8 s in
          i8x16_replace_lane i
      | 0x18l ->
          let+ i = u8 s in
          i16x8_extract_lane_s i
      | 0x19l ->
          let+ i = u8 s in
          i16x8_extract_lane_u i
      | 0x1al ->
          let+ i = u8 s in
          i16x8_replace_lane i
      | 0x1bl ->
          let+ i = u8 s in
          i32x4_extract_lane i
      | 0x1cl ->
          let+ i = u8 s in
          i32x4_replace_lane i
      | 0x1dl ->
          let+ i = u8 s in
          i64x2_extract_lane i
      | 0x1el ->
          let+ i = u8 s in
          i64x2_replace_lane i
      | 0x1fl ->
          return_float_f @@ fun s ->
          let+ i = u8 s in
          f32x4_extract_lane i
      | 0x20l ->
          return_float_f @@ fun s ->
          let+ i = u8 s in
          f32x4_replace_lane i
      | 0x21l ->
          return_float_f @@ fun s ->
          let+ i = u8 s in
          f64x2_extract_lane i
      | 0x22l ->
          return_float_f @@ fun s ->
          let+ i = u8 s in
          f64x2_replace_lane i
      | 0x23l -> Lwt.return i8x16_eq
      | 0x24l -> Lwt.return i8x16_ne
      | 0x25l -> Lwt.return i8x16_lt_s
      | 0x26l -> Lwt.return i8x16_lt_u
      | 0x27l -> Lwt.return i8x16_gt_s
      | 0x28l -> Lwt.return i8x16_gt_u
      | 0x29l -> Lwt.return i8x16_le_s
      | 0x2al -> Lwt.return i8x16_le_u
      | 0x2bl -> Lwt.return i8x16_ge_s
      | 0x2cl -> Lwt.return i8x16_ge_u
      | 0x2dl -> Lwt.return i16x8_eq
      | 0x2el -> Lwt.return i16x8_ne
      | 0x2fl -> Lwt.return i16x8_lt_s
      | 0x30l -> Lwt.return i16x8_lt_u
      | 0x31l -> Lwt.return i16x8_gt_s
      | 0x32l -> Lwt.return i16x8_gt_u
      | 0x33l -> Lwt.return i16x8_le_s
      | 0x34l -> Lwt.return i16x8_le_u
      | 0x35l -> Lwt.return i16x8_ge_s
      | 0x36l -> Lwt.return i16x8_ge_u
      | 0x37l -> Lwt.return i32x4_eq
      | 0x38l -> Lwt.return i32x4_ne
      | 0x39l -> Lwt.return i32x4_lt_s
      | 0x3al -> Lwt.return i32x4_lt_u
      | 0x3bl -> Lwt.return i32x4_gt_s
      | 0x3cl -> Lwt.return i32x4_gt_u
      | 0x3dl -> Lwt.return i32x4_le_s
      | 0x3el -> Lwt.return i32x4_le_u
      | 0x3fl -> Lwt.return i32x4_ge_s
      | 0x40l -> Lwt.return i32x4_ge_u
      | 0x41l -> return_float f32x4_eq
      | 0x42l -> return_float f32x4_ne
      | 0x43l -> return_float f32x4_lt
      | 0x44l -> return_float f32x4_gt
      | 0x45l -> return_float f32x4_le
      | 0x46l -> return_float f32x4_ge
      | 0x47l -> return_float f64x2_eq
      | 0x48l -> return_float f64x2_ne
      | 0x49l -> return_float f64x2_lt
      | 0x4al -> return_float f64x2_gt
      | 0x4bl -> return_float f64x2_le
      | 0x4cl -> return_float f64x2_ge
      | 0x4dl -> Lwt.return v128_not
      | 0x4el -> Lwt.return v128_and
      | 0x4fl -> Lwt.return v128_andnot
      | 0x50l -> Lwt.return v128_or
      | 0x51l -> Lwt.return v128_xor
      | 0x52l -> Lwt.return v128_bitselect
      | 0x53l -> Lwt.return v128_any_true
      | 0x54l ->
          let* a, o = memop s in
          let+ lane = u8 s in
          v128_load8_lane a o lane
      | 0x55l ->
          let* a, o = memop s in
          let+ lane = u8 s in
          v128_load16_lane a o lane
      | 0x56l ->
          let* a, o = memop s in
          let+ lane = u8 s in
          v128_load32_lane a o lane
      | 0x57l ->
          let* a, o = memop s in
          let+ lane = u8 s in
          v128_load64_lane a o lane
      | 0x58l ->
          let* a, o = memop s in
          let+ lane = u8 s in
          v128_store8_lane a o lane
      | 0x59l ->
          let* a, o = memop s in
          let+ lane = u8 s in
          v128_store16_lane a o lane
      | 0x5al ->
          let* a, o = memop s in
          let+ lane = u8 s in
          v128_store32_lane a o lane
      | 0x5bl ->
          let* a, o = memop s in
          let+ lane = u8 s in
          v128_store64_lane a o lane
      | 0x5cl ->
          let+ a, o = memop s in
          v128_load32_zero a o
      | 0x5dl ->
          let+ a, o = memop s in
          v128_load64_zero a o
      | 0x5el -> return_float f32x4_demote_f64x2_zero
      | 0x5fl -> return_float f64x2_promote_low_f32x4
      | 0x60l -> Lwt.return i8x16_abs
      | 0x61l -> Lwt.return i8x16_neg
      | 0x62l -> Lwt.return i8x16_popcnt
      | 0x63l -> Lwt.return i8x16_all_true
      | 0x64l -> Lwt.return i8x16_bitmask
      | 0x65l -> Lwt.return i8x16_narrow_i16x8_s
      | 0x66l -> Lwt.return i8x16_narrow_i16x8_u
      | 0x67l -> return_float f32x4_ceil
      | 0x68l -> return_float f32x4_floor
      | 0x69l -> return_float f32x4_trunc
      | 0x6al -> return_float f32x4_nearest
      | 0x6bl -> Lwt.return i8x16_shl
      | 0x6cl -> Lwt.return i8x16_shr_s
      | 0x6dl -> Lwt.return i8x16_shr_u
      | 0x6el -> Lwt.return i8x16_add
      | 0x6fl -> Lwt.return i8x16_add_sat_s
      | 0x70l -> Lwt.return i8x16_add_sat_u
      | 0x71l -> Lwt.return i8x16_sub
      | 0x72l -> Lwt.return i8x16_sub_sat_s
      | 0x73l -> Lwt.return i8x16_sub_sat_u
      | 0x74l -> Lwt.return f64x2_ceil
      | 0x75l -> Lwt.return f64x2_floor
      | 0x76l -> Lwt.return i8x16_min_s
      | 0x77l -> Lwt.return i8x16_min_u
      | 0x78l -> Lwt.return i8x16_max_s
      | 0x79l -> Lwt.return i8x16_max_u
      | 0x7al -> Lwt.return f64x2_trunc
      | 0x7bl -> Lwt.return i8x16_avgr_u
      | 0x7cl -> Lwt.return i16x8_extadd_pairwise_i8x16_s
      | 0x7dl -> Lwt.return i16x8_extadd_pairwise_i8x16_u
      | 0x7el -> Lwt.return i32x4_extadd_pairwise_i16x8_s
      | 0x7fl -> Lwt.return i32x4_extadd_pairwise_i16x8_u
      | 0x80l -> Lwt.return i16x8_abs
      | 0x81l -> Lwt.return i16x8_neg
      | 0x82l -> Lwt.return i16x8_q15mulr_sat_s
      | 0x83l -> Lwt.return i16x8_all_true
      | 0x84l -> Lwt.return i16x8_bitmask
      | 0x85l -> Lwt.return i16x8_narrow_i32x4_s
      | 0x86l -> Lwt.return i16x8_narrow_i32x4_u
      | 0x87l -> Lwt.return i16x8_extend_low_i8x16_s
      | 0x88l -> Lwt.return i16x8_extend_high_i8x16_s
      | 0x89l -> Lwt.return i16x8_extend_low_i8x16_u
      | 0x8al -> Lwt.return i16x8_extend_high_i8x16_u
      | 0x8bl -> Lwt.return i16x8_shl
      | 0x8cl -> Lwt.return i16x8_shr_s
      | 0x8dl -> Lwt.return i16x8_shr_u
      | 0x8el -> Lwt.return i16x8_add
      | 0x8fl -> Lwt.return i16x8_add_sat_s
      | 0x90l -> Lwt.return i16x8_add_sat_u
      | 0x91l -> Lwt.return i16x8_sub
      | 0x92l -> Lwt.return i16x8_sub_sat_s
      | 0x93l -> Lwt.return i16x8_sub_sat_u
      | 0x94l -> Lwt.return f64x2_nearest
      | 0x95l -> Lwt.return i16x8_mul
      | 0x96l -> Lwt.return i16x8_min_s
      | 0x97l -> Lwt.return i16x8_min_u
      | 0x98l -> Lwt.return i16x8_max_s
      | 0x99l -> Lwt.return i16x8_max_u
      | 0x9bl -> Lwt.return i16x8_avgr_u
      | 0x9cl -> Lwt.return i16x8_extmul_low_i8x16_s
      | 0x9dl -> Lwt.return i16x8_extmul_high_i8x16_s
      | 0x9el -> Lwt.return i16x8_extmul_low_i8x16_u
      | 0x9fl -> Lwt.return i16x8_extmul_high_i8x16_u
      | 0xa0l -> Lwt.return i32x4_abs
      | 0xa1l -> Lwt.return i32x4_neg
      | 0xa3l -> Lwt.return i32x4_all_true
      | 0xa4l -> Lwt.return i32x4_bitmask
      | 0xa7l -> Lwt.return i32x4_extend_low_i16x8_s
      | 0xa8l -> Lwt.return i32x4_extend_high_i16x8_s
      | 0xa9l -> Lwt.return i32x4_extend_low_i16x8_u
      | 0xaal -> Lwt.return i32x4_extend_high_i16x8_u
      | 0xabl -> Lwt.return i32x4_shl
      | 0xacl -> Lwt.return i32x4_shr_s
      | 0xadl -> Lwt.return i32x4_shr_u
      | 0xael -> Lwt.return i32x4_add
      | 0xb1l -> Lwt.return i32x4_sub
      | 0xb5l -> Lwt.return i32x4_mul
      | 0xb6l -> Lwt.return i32x4_min_s
      | 0xb7l -> Lwt.return i32x4_min_u
      | 0xb8l -> Lwt.return i32x4_max_s
      | 0xb9l -> Lwt.return i32x4_max_u
      | 0xbal -> Lwt.return i32x4_dot_i16x8_s
      | 0xbcl -> Lwt.return i32x4_extmul_low_i16x8_s
      | 0xbdl -> Lwt.return i32x4_extmul_high_i16x8_s
      | 0xbel -> Lwt.return i32x4_extmul_low_i16x8_u
      | 0xbfl -> Lwt.return i32x4_extmul_high_i16x8_u
      | 0xc0l -> Lwt.return i64x2_abs
      | 0xc1l -> Lwt.return i64x2_neg
      | 0xc3l -> Lwt.return i64x2_all_true
      | 0xc4l -> Lwt.return i64x2_bitmask
      | 0xc7l -> Lwt.return i64x2_extend_low_i32x4_s
      | 0xc8l -> Lwt.return i64x2_extend_high_i32x4_s
      | 0xc9l -> Lwt.return i64x2_extend_low_i32x4_u
      | 0xcal -> Lwt.return i64x2_extend_high_i32x4_u
      | 0xcbl -> Lwt.return i64x2_shl
      | 0xccl -> Lwt.return i64x2_shr_s
      | 0xcdl -> Lwt.return i64x2_shr_u
      | 0xcel -> Lwt.return i64x2_add
      | 0xd1l -> Lwt.return i64x2_sub
      | 0xd5l -> Lwt.return i64x2_mul
      | 0xd6l -> Lwt.return i64x2_eq
      | 0xd7l -> Lwt.return i64x2_ne
      | 0xd8l -> Lwt.return i64x2_lt_s
      | 0xd9l -> Lwt.return i64x2_gt_s
      | 0xdal -> Lwt.return i64x2_le_s
      | 0xdbl -> Lwt.return i64x2_ge_s
      | 0xdcl -> Lwt.return i64x2_extmul_low_i32x4_s
      | 0xddl -> Lwt.return i64x2_extmul_high_i32x4_s
      | 0xdel -> Lwt.return i64x2_extmul_low_i32x4_u
      | 0xdfl -> Lwt.return i64x2_extmul_high_i32x4_u
      | 0xe0l -> return_float f32x4_abs
      | 0xe1l -> return_float f32x4_neg
      | 0xe3l -> return_float f32x4_sqrt
      | 0xe4l -> return_float f32x4_add
      | 0xe5l -> return_float f32x4_sub
      | 0xe6l -> return_float f32x4_mul
      | 0xe7l -> return_float f32x4_div
      | 0xe8l -> return_float f32x4_min
      | 0xe9l -> return_float f32x4_max
      | 0xeal -> return_float f32x4_pmin
      | 0xebl -> return_float f32x4_pmax
      | 0xecl -> return_float f64x2_abs
      | 0xedl -> return_float f64x2_neg
      | 0xefl -> return_float f64x2_sqrt
      | 0xf0l -> return_float f64x2_add
      | 0xf1l -> return_float f64x2_sub
      | 0xf2l -> return_float f64x2_mul
      | 0xf3l -> return_float f64x2_div
      | 0xf4l -> return_float f64x2_min
      | 0xf5l -> return_float f64x2_max
      | 0xf6l -> return_float f64x2_pmin
      | 0xf7l -> return_float f64x2_pmax
      | 0xf8l -> Lwt.return i32x4_trunc_sat_f32x4_s
      | 0xf9l -> Lwt.return i32x4_trunc_sat_f32x4_u
      | 0xfal -> Lwt.return f32x4_convert_i32x4_s
      | 0xfbl -> Lwt.return f32x4_convert_i32x4_u
      | 0xfcl -> Lwt.return i32x4_trunc_sat_f64x2_s_zero
      | 0xfdl -> Lwt.return i32x4_trunc_sat_f64x2_u_zero
      | 0xfel -> return_float f64x2_convert_low_i32x4_s
      | 0xffl -> return_float f64x2_convert_low_i32x4_u
      | n -> illegal s pos (I32.to_int_u n))
  | b -> illegal s pos b

let instr_block_step ~allow_floats s allocs cont =
  let open Lwt.Syntax in
  let* cont, stack = pop_at_most 3 cont in
  match cont with
  | _ :: _ :: _ :: _ :: _ ->
      Stdlib.failwith "More than 3 values popped from the stack"
  (* Enforces the number of values popped from the stack, which shouldn't fail. *)
  | [IKStop _] -> raise (Step_error Instr_block_step)
  | [IKStop lbl; IKBlock (bt, pos); IKNext plbl] ->
      let* () = end_ s in
      let e = Source.(block bt lbl @@ region s pos pos) in
      let+ () = add_to_block allocs plbl e in
      push_stack (IKNext plbl) stack
  | [IKStop lbl; IKLoop (bt, pos); IKNext plbl] ->
      let* () = end_ s in
      let e = Source.(loop bt lbl @@ region s pos pos) in
      let+ () = add_to_block allocs plbl e in
      push_stack (IKNext plbl) stack
  | [IKStop lbl1; IKIf1 (bt, pos); IKNext plbl] ->
      let* x = peek s in
      if x = Some 0x05 then
        (skip 1 s ;
         push_rev_values
           [IKNext (alloc_block allocs); IKIf2 (bt, pos, lbl1); IKNext plbl]
           stack)
        |> Lwt.return
      else
        let* () = end_ s in
        let e = Source.(if_ bt lbl1 empty_block @@ region s pos pos) in
        let+ () = add_to_block allocs plbl e in
        push_stack (IKNext plbl) stack
  | [IKStop lbl2; IKIf2 (bt, pos, lbl1); IKNext plbl] ->
      let* () = end_ s in
      let e = Source.(if_ bt lbl1 lbl2 @@ region s pos pos) in
      let+ () = add_to_block allocs plbl e in
      push_stack (IKNext plbl) stack
  | IKNext lbl :: ks -> (
      let* x = peek s in
      match x with
      | None | Some (0x05 | 0x0b) ->
          push_rev_values (IKStop lbl :: ks) stack |> Lwt.return
      | _ -> (
          let pos = pos s in
          let* tag = op s in
          match tag with
          | 0x02 ->
              let* bt = block_type ~allow_floats s in
              push_rev_values
                (IKNext (alloc_block allocs)
                :: IKBlock (bt, pos)
                :: IKNext lbl :: ks)
                stack
              |> Lwt.return
          | 0x03 ->
              let* bt = block_type ~allow_floats s in
              push_rev_values
                (IKNext (alloc_block allocs)
                :: IKLoop (bt, pos)
                :: IKNext lbl :: ks)
                stack
              |> Lwt.return
          | 0x04 ->
              let* bt = block_type ~allow_floats s in
              push_rev_values
                (IKNext (alloc_block allocs)
                :: IKIf1 (bt, pos)
                :: IKNext lbl :: ks)
                stack
              |> Lwt.return
          | _ ->
              let* i = instr ~allow_floats s pos tag in
              let e = Source.(i @@ region s pos pos) in
              let+ () = add_to_block allocs lbl e in
              push_rev_values (IKNext lbl :: ks) stack))
  (* Stop can only be followed a new block, or being the final state. *)
  | IKStop _ :: _ -> raise (Step_error Instr_block_step)
  (* These continuations never reduce directly and are always preceded by an end
     of block or the accumulation of parsed instructions (`IKNext`). *)
  | IKBlock _ :: _ | IKLoop _ :: _ | IKIf1 _ :: _ | IKIf2 _ :: _ ->
      raise (Step_error Instr_block_step)
  (* The empty continuation cannot reduce. *)
  | [] -> raise (Step_error Instr_block_step)

type block_kont =
  | BlockStart
  | BlockParse of instr_block_kont lazy_stack
  | BlockStop of block_label

let block_step ~allow_floats s allocs =
  let open Lwt.Syntax in
  function
  | BlockStart ->
      let lbl = alloc_block allocs in
      let vector = empty_stack () |> push_stack (IKNext lbl) in
      Lwt.return (BlockParse vector)
  | BlockParse (LazyStack {length; vector} as kont) ->
      if length = 1l then
        let* head = Vector.get 0l vector in
        match head with
        | IKStop lbl -> Lwt.return (BlockStop lbl)
        | _ ->
            let+ kont = instr_block_step ~allow_floats s allocs kont in
            BlockParse kont
      else
        let+ kont = instr_block_step ~allow_floats s allocs kont in
        BlockParse kont
  | BlockStop _ -> raise (Step_error Block_step)

(** Vector and size continuations *)

type 'a lazy_vec_kont = LazyVec of {offset : int32; vector : 'a Vector.t}

let is_end_of_vec (LazyVec {offset; vector}) =
  Vector.num_elements vector <= offset

let init_lazy_vec n = LazyVec {offset = 0l; vector = Vector.create n}

let lazy_vec_step v (LazyVec {offset; vector}) =
  LazyVec {offset = Int32.add offset 1l; vector = Vector.set offset v vector}

type pos = int

(** Size checking version of {!sized} for CPS-style parsing. *)
type size = {size : int; start : pos}

let size s =
  let open Lwt.Syntax in
  let+ size = len32 s in
  let start = pos s in
  {size; start}

let check_size {size; start} s =
  require (pos s = start + size) s start "section size mismatch"

type name_step =
  | NKStart  (** UTF8 name starting point. *)
  | NKParse of pos * Buffer.t * int  (** UTF8 char parsing. *)
  | NKStop of string  (** UTF8 name final step.*)

let name_step s =
  let open Lwt.Syntax in
  function
  | NKStart ->
      let pos = pos s in
      let+ len = len32 s in
      require (len <= 512) s pos "Names cannot exceed 512 bytes" ;
      NKParse (pos, Buffer.create len, len)
  | NKParse (_, buffer, 0) -> Lwt.return @@ NKStop (Buffer.contents buffer)
  | NKParse (pos, buffer, len) ->
      let* _, bytes =
        try Utf8.decode_step get s
        with Utf8 -> error s pos "malformed UTF-8 encoding"
      in
      List.iter (Buffer.add_int8 buffer) bytes ;
      Lwt.return @@ NKParse (pos, buffer, len - List.length bytes)
  (* final step, cannot reduce. *)
  | NKStop _ -> raise (Step_error Name_step)

let name s =
  let open Lwt.Syntax in
  let rec step = function
    | NKStop n -> Lwt.return n
    | k ->
        let* x = name_step s k in
        step x
  in
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
  let open Lwt.Syntax in
  let+ bo = peek s in
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
  let open Lwt.Syntax in
  let* x = id s in
  match x with
  | Some tag' when tag' = tag ->
      let* () = lwt_ignore @@ u8 s in
      sized f s
  | _ -> Lwt.return default

let section tag f default s = section_with_size tag (fun _ -> f) default s

(* Type section *)

type func_type_kont =
  | FKStart
  | FKIns of value_type lazy_vec_kont
  | FKOut of value_type Vector.t * value_type lazy_vec_kont
  | FKStop of func_type

let func_type_step ~allow_floats s =
  let open Lwt.Syntax in
  function
  | FKStart ->
      let* tag = vs7 s in
      let+ len = len32 s in
      if tag = -0x20 then FKIns (init_lazy_vec (Int32.of_int len))
      else error s (pos s - 1) "malformed function type"
  | FKIns (LazyVec {vector = ins; _} as vec) when is_end_of_vec vec ->
      let+ len = len32 s in
      FKOut (ins, init_lazy_vec (Int32.of_int len))
  | FKIns ins ->
      let+ vt = value_type ~allow_floats s in
      FKIns (lazy_vec_step vt ins)
  | FKOut (ins, (LazyVec {vector = out; _} as out_vec))
    when is_end_of_vec out_vec ->
      Lwt.return @@ FKStop (FuncType (ins, out))
  | FKOut (ins, out_vec) ->
      let+ vt = value_type ~allow_floats s in
      FKOut (ins, lazy_vec_step vt out_vec)
  | FKStop _ -> raise (Step_error Func_type_step)
(* cannot reduce *)

(* let _type_ s = at func_type s *)

(* Import section *)

let import_desc ~allow_floats s =
  let open Lwt.Syntax in
  let* x = u8 s in
  match x with
  | 0x00 ->
      let+ x = at var s in
      FuncImport x
  | 0x01 ->
      let+ x = table_type s in
      TableImport x
  | 0x02 ->
      let+ x = memory_type s in
      MemoryImport x
  | 0x03 ->
      let+ x = global_type ~allow_floats s in
      GlobalImport x
  | _ -> error s (pos s - 1) "malformed import kind"

type import_kont =
  | ImpKStart  (** Import parsing starting point. *)
  | ImpKModuleName of name_step
      (** Import module name parsing UTF8 char per char step. *)
  | ImpKItemName of Ast.name * name_step
      (** Import item name parsing UTF8 char per char step. *)
  | ImpKStop of import'  (** Import final step. *)

let import_step ~allow_floats s =
  let open Lwt.Syntax in
  function
  | ImpKStart -> Lwt.return @@ ImpKModuleName NKStart
  | ImpKModuleName (NKStop module_name) ->
      Lwt.return @@ ImpKItemName (module_name, NKStart)
  | ImpKModuleName nk ->
      let+ x = name_step s nk in
      ImpKModuleName x
  | ImpKItemName (module_name, NKStop item_name) ->
      let+ idesc = at (import_desc ~allow_floats) s in
      ImpKStop {module_name; item_name; idesc}
  | ImpKItemName (module_name, nk) ->
      let+ x = name_step s nk in
      ImpKItemName (module_name, x)
  (* Final step, cannot reduce *)
  | ImpKStop _ -> raise (Step_error Import_step)

(* Table section *)

let table s =
  let open Lwt.Syntax in
  let+ ttype = table_type s in
  {ttype}

(* Memory section *)

let memory s =
  let open Lwt.Syntax in
  let+ mtype = memory_type s in
  {mtype}

(* Export section *)

let export_desc s =
  let open Lwt.Syntax in
  let* x = u8 s in
  match x with
  | 0x00 ->
      let+ x = at var s in
      FuncExport x
  | 0x01 ->
      let+ x = at var s in
      TableExport x
  | 0x02 ->
      let+ x = at var s in
      MemoryExport x
  | 0x03 ->
      let+ x = at var s in
      GlobalExport x
  | _ -> error s (pos s - 1) "malformed export kind"

type export_kont =
  | ExpKStart  (** Export parsing starting point. *)
  | ExpKName of name_step  (** Export name parsing UTF8 char per char step. *)
  | ExpKStop of export'  (** Export final step. *)

let export_step s =
  let open Lwt.Syntax in
  function
  | ExpKStart -> Lwt.return @@ ExpKName NKStart
  | ExpKName (NKStop name) ->
      let+ edesc = at export_desc s in
      ExpKStop {name; edesc}
  | ExpKName nk ->
      let+ x = name_step s nk in
      ExpKName x
  (* Final step, cannot reduce *)
  | ExpKStop _ -> raise (Step_error Export_step)

(* Start section *)

let start s =
  let open Lwt.Syntax in
  let+ sfunc = at var s in
  {sfunc}

let start_section s = section `StartSection (opt (at start) true) None s

(* Code section *)

let local ~allow_floats s =
  let open Lwt.Syntax in
  let* n = vu32 s in
  let+ t = value_type ~allow_floats s in
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
      const_kont : block_kont;
    }  (** Parsing step of the body of a function. *)
  | CKStop of func  (** Final step of a parsed function, irreducible. *)

let at' left s x =
  let right = pos s in
  Source.(x @@ region s left right)

let code_step ~allow_floats s allocs =
  let open Lwt.Syntax in
  function
  | CKStart ->
      (* `at` left *)
      let left = pos s in
      let* size = size s in
      let pos = pos s in
      (* `vec` size *)
      let+ n = len32 s in
      CKLocalsParse
        {
          left;
          size;
          pos;
          locals_size = 0L;
          vec_kont = init_lazy_vec (Int32.of_int n);
        }
  | CKLocalsParse
      {
        left;
        size;
        pos;
        vec_kont = LazyVec {vector = types; _} as vec_kont;
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
          type_vec = LazyVec {offset = 0l; vector = types};
          curr_type = None;
        }
      |> Lwt.return
  | CKLocalsParse {left; size; pos; vec_kont; locals_size} ->
      let* local = local ~allow_floats s in
      (* small enough to fit in a tick *)
      let locals_size =
        I64.add locals_size (I64_convert.extend_i32_u (fst local))
      in
      CKLocalsParse
        {left; size; pos; vec_kont = lazy_vec_step local vec_kont; locals_size}
      |> Lwt.return
  | CKLocalsAccumulate
      {left; size; vec_kont = LazyVec {vector = locals; _} as vec_kont; _}
    when is_end_of_vec vec_kont ->
      CKBody {left; size; locals; const_kont = BlockStart} |> Lwt.return
  | CKLocalsAccumulate
      {
        left;
        size;
        pos;
        vec_kont;
        curr_type = None | Some (0l, _);
        type_vec = LazyVec {offset = n; vector = types};
      } ->
      let+ next_type = Vector.get n types in
      CKLocalsAccumulate
        {
          left;
          size;
          pos;
          vec_kont;
          curr_type = Some next_type;
          type_vec = LazyVec {offset = Int32.succ n; vector = types};
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
  | CKBody {left; size; locals; const_kont = BlockStop body} ->
      let* () = end_ s in
      check_size size s ;
      let func =
        at' left s @@ {locals; body; ftype = Source.(-1l @@ Source.no_region)}
      in
      CKStop func |> Lwt.return
  | CKBody {left; size; locals; const_kont} ->
      let+ const_kont = block_step ~allow_floats s allocs const_kont in
      CKBody {left; size; locals; const_kont}
  (* final step, cannot reduce *)
  | CKStop _ -> raise (Step_error Code_step)

(* Element section *)

let elem_index allocs s =
  let open Lwt.Syntax in
  let* x = at var s in
  let b = alloc_block allocs in
  let+ () = add_to_block allocs b Source.(ref_func x @@ x.at) in
  b

let elem_kind s =
  let open Lwt.Syntax in
  let+ x = u8 s in
  match x with
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
      offset_kont : pos * block_kont;
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
      einit_kont : pos * block_kont;
    }
      (** Element segment initialization code parsing step for constant values. *)
  | EKStop of elem_segment'  (** Final step of a segment parsing. *)

let ek_start s =
  let open Lwt.Syntax in
  let* v = vu32 s in
  match v with
  | 0x00l ->
      (* active_zero *)
      let index = Source.(0l @@ Source.no_region) in
      let left = pos s in
      Lwt.return
      @@ EKMode
           {
             left;
             index;
             index_kind = Indexed;
             early_ref_type = Some FuncRefType;
             offset_kont = (left, BlockStart);
           }
  | 0x01l ->
      (* passive *)
      let mode_pos = pos s in
      let* ref_type = elem_kind s in
      let+ n = len32 s in
      let mode = Source.(Passive @@ region s mode_pos mode_pos) in
      EKInitIndexed {mode; ref_type; einit_vec = init_lazy_vec (Int32.of_int n)}
  | 0x02l ->
      (* active *)
      let left = pos s in
      let+ index = at var s in
      let left_offset = pos s in
      EKMode
        {
          left;
          index;
          index_kind = Indexed;
          early_ref_type = None;
          offset_kont = (left_offset, BlockStart);
        }
  | 0x03l ->
      (* declarative *)
      let mode_pos = pos s in
      let mode = Source.(Declarative @@ region s mode_pos mode_pos) in
      let* ref_type = elem_kind s in
      let+ n = len32 s in
      EKInitIndexed {mode; ref_type; einit_vec = init_lazy_vec (Int32.of_int n)}
  | 0x04l ->
      (* active_zero *)
      let index = Source.(0l @@ Source.no_region) in
      let left = pos s in
      Lwt.return
      @@ EKMode
           {
             left;
             index;
             index_kind = Const;
             early_ref_type = Some FuncRefType;
             offset_kont = (left, BlockStart);
           }
  | 0x05l ->
      (* passive *)
      let mode_pos = pos s in
      let mode = Source.(Passive @@ region s mode_pos mode_pos) in
      let* ref_type = ref_type s in
      let+ n = len32 s in
      let left = pos s in
      EKInitConst
        {
          mode;
          ref_type;
          einit_vec = init_lazy_vec (Int32.of_int n);
          einit_kont = (left, BlockStart);
        }
  | 0x06l ->
      (* active *)
      let left = pos s in
      let+ index = at var s in
      let left_offset = pos s in
      EKMode
        {
          left;
          index;
          index_kind = Const;
          early_ref_type = None;
          offset_kont = (left_offset, BlockStart);
        }
  | 0x07l ->
      (* declarative *)
      let mode_pos = pos s in
      let mode = Source.(Declarative @@ region s mode_pos mode_pos) in
      let* ref_type = ref_type s in
      let+ n = len32 s in
      let left = pos s in
      EKInitConst
        {
          mode;
          ref_type;
          einit_vec = init_lazy_vec (Int32.of_int n);
          einit_kont = (left, BlockStart);
        }
  | _ -> error s (pos s - 1) "malformed elements segment kind"

let elem_step ~allow_floats s allocs =
  let open Lwt.Syntax in
  function
  | EKStart -> ek_start s
  | EKMode
      {
        left;
        index;
        index_kind;
        early_ref_type;
        offset_kont = left_offset, BlockStop offset;
      } ->
      let* () = end_ s in
      let right = pos s in
      let offset = Source.(offset @@ region s left_offset right) in
      let mode = Source.(Active {index; offset} @@ region s left right) in
      let* ref_type =
        match early_ref_type with
        | Some t -> Lwt.return t
        | None -> if index_kind = Indexed then elem_kind s else ref_type s
      in
      (* `vec` size *)
      let+ n = len32 s in
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
            einit_kont = (left, BlockStart);
          }
  | EKMode
      {left; index; index_kind; early_ref_type; offset_kont = left_offset, k} ->
      let+ k' = block_step ~allow_floats s allocs k in
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
      {mode; ref_type; einit_vec = LazyVec {vector = einit; _} as einit_vec; _}
  | EKInitIndexed
      {mode; ref_type; einit_vec = LazyVec {vector = einit; _} as einit_vec}
    when is_end_of_vec einit_vec ->
      EKStop {etype = ref_type; einit; emode = mode} |> Lwt.return
  (* Indexed *)
  | EKInitIndexed {mode; ref_type; einit_vec} ->
      let+ elem_index = at_s (elem_index allocs) s in
      EKInitIndexed
        {mode; ref_type; einit_vec = lazy_vec_step elem_index einit_vec}
  (* Const *)
  | EKInitConst {mode; ref_type; einit_vec; einit_kont = left, BlockStop einit}
    ->
      let* () = end_ s in
      let right = pos s in
      let einit = Source.(einit @@ region s left right) in
      EKInitConst
        {
          mode;
          ref_type;
          einit_vec = lazy_vec_step einit einit_vec;
          einit_kont = (right, BlockStart);
        }
      |> Lwt.return
  | EKInitConst {mode; ref_type; einit_vec; einit_kont = left, k} ->
      let+ k' = block_step ~allow_floats s allocs k in
      EKInitConst {mode; ref_type; einit_vec; einit_kont = (left, k')}
  (* Final step, cannot reduce *)
  | EKStop _ -> raise (Step_error Elem_step)

(* Data section *)

type data_kont =
  | DKStart  (** Starting point of a data segment parsing. *)
  | DKMode of {
      left : pos;
      index : int32 Source.phrase;
      offset_kont : pos * block_kont;
    }  (** Data segment mode parsing step. *)
  | DKInit of {dmode : segment_mode; init_kont : byte_vector_kont}
  | DKStop of data_segment'  (** Final step of a data segment parsing. *)

let data_start s =
  let open Lwt.Syntax in
  let* x = vu32 s in
  match x with
  | 0x00l ->
      (* active_zero *)
      let index = Source.(0l @@ Source.no_region) in
      let left = pos s in
      Lwt.return @@ DKMode {left; index; offset_kont = (left, BlockStart)}
  | 0x01l ->
      (* passive *)
      let mode_pos = pos s in
      let dmode = Source.(Passive @@ region s mode_pos mode_pos) in
      Lwt.return @@ DKInit {dmode; init_kont = VKStart}
  | 0x02l ->
      (* active *)
      let left = pos s in
      let+ index = at var s in
      let left_offset = pos s in
      DKMode {left; index; offset_kont = (left_offset, BlockStart)}
  | _ -> error s (pos s - 1) "malformed data segment kind"

let data_step ~allow_floats s allocs =
  let open Lwt.Syntax in
  function
  | DKStart -> data_start s
  | DKMode {left; index; offset_kont = left_offset, BlockStop offset} ->
      let* () = end_ s in
      let right = pos s in
      let offset = Source.(offset @@ region s left_offset right) in
      let dmode = Source.(Active {index; offset} @@ region s left right) in
      DKInit {dmode; init_kont = VKStart} |> Lwt.return
  | DKMode {left; index; offset_kont = left_offset, k} ->
      let+ k' = block_step ~allow_floats s allocs k in
      DKMode {left; index; offset_kont = (left_offset, k')}
  | DKInit {dmode; init_kont = VKStop dinit} ->
      DKStop {dmode; dinit} |> Lwt.return
  | DKInit {dmode; init_kont} ->
      let+ init_kont = byte_vector_step allocs s init_kont in
      DKInit {dmode; init_kont}
  (* final step, cannot reduce *)
  | DKStop _ -> raise (Step_error Data_step)

(* DataCount section *)

let data_count s =
  let open Lwt.Syntax in
  let+ x = vu32 s in
  Some x

let data_count_section s = section `DataCountSection data_count None s

(* Custom section *)

let custom size s =
  let open Lwt.Syntax in
  let start = pos s in
  let* id = name s in
  let+ bs = get_string (size - (pos s - start)) s in
  Some (id, bs)

let custom_section s = section_with_size `CustomSection custom None s

let non_custom_section s =
  let open Lwt.Syntax in
  let* x = id s in
  match x with
  | None | Some `CustomSection -> Lwt.return_none
  | _ ->
      skip 1 s ;
      let+ () = sized (fun pos s -> skip pos s |> Lwt.return) s in
      Some ()

(* Modules *)

let rec iterate f s =
  let open Lwt.Syntax in
  let* x = f s in
  if x <> None then iterate f s else Lwt.return_unit

let magic = 0x6d736100l

type vec_repr = VecRepr

type opt_repr = OptRepr

(** Sections representation. *)
type (_, _) field_type =
  | TypeField : (type_, vec_repr) field_type
  | ImportField : (import, vec_repr) field_type
  | FuncField : (var, vec_repr) field_type
  | TableField : (table, vec_repr) field_type
  | MemoryField : (memory, vec_repr) field_type
  | GlobalField : (global, vec_repr) field_type
  | ExportField : (export, vec_repr) field_type
  | StartField : (start, opt_repr) field_type
  | ElemField : (elem_segment, vec_repr) field_type
  | DataCountField : (int32, opt_repr) field_type
  | CodeField : (func, vec_repr) field_type
  | DataField : (data_segment, vec_repr) field_type

(** Result of a section parsing, being either a single value or a vector. *)
type field =
  | VecField : ('a, vec_repr) field_type * 'a Vector.t -> field
  | SingleField : ('a, opt_repr) field_type * 'a option -> field

(** Module parsing steps *)
type module_kont =
  | MKStart  (** Initial state of a module parsing *)
  | MKSkipCustom : ('a, 'repr) field_type option -> module_kont
      (** Custom section which are skipped, with the next section to parse. *)
  | MKFieldStart : ('a, 'repr) field_type -> module_kont
      (** Starting point of a section, handles parsing generic section header. *)
  | MKField : ('a, vec_repr) field_type * size * 'a lazy_vec_kont -> module_kont
      (** Section currently parsed, accumulating each element from the underlying vector. *)
  | MKElaborateFunc :
      var Vector.t
      * func Vector.t
      * func lazy_vec_kont
      * instr lazy_vec_kont lazy_vec_kont option
      * bool
      -> module_kont
      (** Elaboration of functions from the code section with their declared type in
      the func section, and accumulating invariants conditions associated to
      functions. *)
  | MKBuild of func Vector.t option * bool
      (** Accumulating the parsed sections vectors into a module and checking
      invariants. *)
  | MKStop of module_  (** Final step of the parsing, cannot reduce. *)
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/3120
       actually, should be module_
    *)
  | MKTypes of func_type_kont * pos * size * type_ lazy_vec_kont
      (** Function types section parsing. *)
  | MKImport of import_kont * pos * size * import lazy_vec_kont
      (** Import section parsing. *)
  | MKExport of export_kont * pos * size * export lazy_vec_kont
      (** Export section parsing. *)
  | MKGlobal of global_type * int * block_kont * size * global lazy_vec_kont
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

type building_state = {
  types : type_ Vector.t;
  imports : import Vector.t;
  vars : var Vector.t;
  tables : table Vector.t;
  memories : memory Vector.t;
  globals : global Vector.t;
  exports : export Vector.t;
  start : start option;
  elems : elem_segment Vector.t;
  data_count : int32 option;
  code : func Vector.t;
  datas : data_segment Vector.t;
}

let empty_building_state =
  {
    types = Vector.empty ();
    imports = Vector.empty ();
    vars = Vector.empty ();
    tables = Vector.empty ();
    memories = Vector.empty ();
    globals = Vector.empty ();
    exports = Vector.empty ();
    start = None;
    elems = Vector.empty ();
    data_count = None;
    code = Vector.empty ();
    datas = Vector.empty ();
  }

let add_field field state =
  match field with
  | VecField (TypeField, types) -> {state with types}
  | VecField (ImportField, imports) -> {state with imports}
  | VecField (FuncField, vars) -> {state with vars}
  | VecField (TableField, tables) -> {state with tables}
  | VecField (MemoryField, memories) -> {state with memories}
  | VecField (GlobalField, globals) -> {state with globals}
  | VecField (ExportField, exports) -> {state with exports}
  | SingleField (StartField, start) -> {state with start}
  | VecField (ElemField, elems) -> {state with elems}
  | SingleField (DataCountField, data_count) -> {state with data_count}
  | VecField (CodeField, code) -> {state with code}
  | VecField (DataField, datas) -> {state with datas}

type decode_kont = {
  building_state : building_state;  (** Accumulated parsed sections. *)
  module_kont : module_kont;
  allocation_state : Ast.allocations;
  stream_pos : int;
  stream_name : string;
}

let tag_of_field : type t repr. (t, repr) field_type -> section_tag = function
  | DataCountField -> `DataCountSection
  | StartField -> `StartSection
  | TypeField -> `TypeSection
  | ImportField -> `ImportSection
  | FuncField -> `FuncSection
  | TableField -> `TableSection
  | MemoryField -> `MemorySection
  | GlobalField -> `GlobalSection
  | ExportField -> `ExportSection
  | ElemField -> `ElemSection
  | CodeField -> `CodeSection
  | DataField -> `DataSection

let vec_field ty (LazyVec {vector; _}) = VecField (ty, vector)

let module_step ~allow_floats bytes state =
  let open Lwt.Syntax in
  let s = {name = state.stream_name; bytes; pos = state.stream_pos} in
  let next module_kont =
    Lwt.return {state with module_kont; stream_pos = s.pos}
  in
  let next_with_field field module_kont =
    Lwt.return
      {
        state with
        building_state = add_field field state.building_state;
        module_kont;
        stream_pos = s.pos;
      }
  in
  let allocs = state.allocation_state in
  match state.module_kont with
  | MKStart ->
      (* Module header *)
      let* header = u32 s in
      require (header = magic) s 0 "magic header not detected" ;
      let* version = u32 s in
      require (version = Encode.version) s 4 "unknown binary version" ;
      (* Module header *)
      next @@ MKSkipCustom (Some TypeField)
  | MKSkipCustom k -> (
      let* x = id s in
      match x with
      | Some `CustomSection ->
          (* section_with_size *)
          let* () = lwt_ignore @@ u8 s in
          (* sized *)
          let* l = len32 s in
          (* custom *)
          let start = pos s in
          let _id = name s in
          let _bs = get_string (l - (pos s - start)) s in
          next @@ MKSkipCustom k
      | _ -> (
          match k with
          | None ->
              let func_types = state.building_state.vars in
              let func_bodies = state.building_state.code in
              if Vector.(num_elements func_types <> num_elements func_bodies)
              then next @@ MKBuild (None, true)
              else
                next
                @@ MKElaborateFunc
                     ( func_types,
                       func_bodies,
                       init_lazy_vec (Vector.num_elements func_types),
                       None,
                       true )
          | Some ty -> next @@ MKFieldStart ty))
  (* Parsing of fields vector. *)
  | MKFieldStart ty -> (
      let next_vec (ty : (_, vec_repr) field_type) =
        let* x = id s in
        match x with
        | Some t when t = tag_of_field ty ->
            ignore (u8 s) ;
            let* size = size s in
            (* length of `vec` *)
            let* l = len32 s in
            next @@ MKField (ty, size, init_lazy_vec (Int32.of_int l))
        | _ ->
            let size = {size = 0; start = pos s} in
            next @@ MKField (ty, size, init_lazy_vec 0l)
      in
      match ty with
      | DataCountField ->
          let* v = data_count_section s in
          next_with_field
            (SingleField (DataCountField, v))
            (MKSkipCustom (Some CodeField))
      | StartField ->
          let* v = start_section s in
          next_with_field
            (SingleField (StartField, v))
            (MKSkipCustom (Some ElemField))
      | TypeField -> next_vec TypeField
      | ImportField -> next_vec ImportField
      | FuncField -> next_vec FuncField
      | TableField -> next_vec TableField
      | MemoryField -> next_vec MemoryField
      | GlobalField -> next_vec GlobalField
      | ExportField -> next_vec ExportField
      | ElemField -> next_vec ElemField
      | CodeField -> next_vec CodeField
      | DataField -> next_vec DataField)
  (* Transitions steps from the end of a section to the next one.

     The values accumulated from the section are accumulated into the building
     state..*)
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/3120
     maybe we can factor-out these similarly shaped module section transitions
  *)
  | MKField (TypeField, size, vec) when is_end_of_vec vec ->
      check_size size s ;
      next_with_field
        (vec_field TypeField vec)
        (MKSkipCustom (Some ImportField))
  | MKField (ImportField, size, vec) when is_end_of_vec vec ->
      check_size size s ;
      next_with_field
        (vec_field ImportField vec)
        (MKSkipCustom (Some FuncField))
  | MKField (FuncField, size, vec) when is_end_of_vec vec ->
      check_size size s ;
      next_with_field (vec_field FuncField vec) (MKSkipCustom (Some TableField))
  | MKField (TableField, size, vec) when is_end_of_vec vec ->
      check_size size s ;
      next_with_field
        (vec_field TableField vec)
        (MKSkipCustom (Some MemoryField))
  | MKField (MemoryField, size, vec) when is_end_of_vec vec ->
      check_size size s ;
      next_with_field
        (vec_field MemoryField vec)
        (MKSkipCustom (Some GlobalField))
  | MKField (GlobalField, size, vec) when is_end_of_vec vec ->
      check_size size s ;
      next_with_field
        (vec_field GlobalField vec)
        (MKSkipCustom (Some ExportField))
  | MKField (ExportField, size, vec) when is_end_of_vec vec ->
      check_size size s ;
      next_with_field
        (vec_field ExportField vec)
        (MKSkipCustom (Some StartField))
  | MKField (ElemField, size, vec) when is_end_of_vec vec ->
      check_size size s ;
      next_with_field
        (vec_field ElemField vec)
        (MKSkipCustom (Some DataCountField))
  | MKField (CodeField, size, vec) when is_end_of_vec vec ->
      check_size size s ;
      next_with_field (vec_field CodeField vec) (MKSkipCustom (Some DataField))
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
          let* f = at var s in
          (* small enough to fit in a tick *)
          next @@ MKField (ty, size, lazy_vec_step f vec)
      | TableField ->
          let* f = at table s in
          (* small enough to fit in a tick *)
          next @@ MKField (ty, size, lazy_vec_step f vec)
      | MemoryField ->
          let* f = at memory s in
          (* small enough to fit in a tick *)
          next @@ MKField (ty, size, lazy_vec_step f vec)
      | GlobalField ->
          let* gtype = global_type ~allow_floats s in
          next @@ MKGlobal (gtype, pos s, BlockStart, size, vec)
      | ExportField -> next @@ MKExport (ExpKStart, pos s, size, vec)
      | ElemField -> next @@ MKElem (EKStart, pos s, size, vec)
      | CodeField -> next @@ MKCode (CKStart, pos s, size, vec)
      | DataField ->
          next @@ MKData (DKStart, pos s, size, vec)
          (* These sections have a distinct step mechanism. *))
  | MKTypes (FKStop func_type, left, size, vec) ->
      let f = Source.(func_type @@ region s left (pos s)) in
      next @@ MKField (TypeField, size, lazy_vec_step f vec)
  | MKTypes (k, pos, size, curr_vec) ->
      let* x = func_type_step ~allow_floats s k in
      next @@ MKTypes (x, pos, size, curr_vec)
  | MKImport (ImpKStop import, left, size, vec) ->
      let f = Source.(import @@ region s left (pos s)) in
      next @@ MKField (ImportField, size, lazy_vec_step f vec)
  | MKImport (k, pos, size, curr_vec) ->
      let* x = import_step ~allow_floats s k in
      next @@ MKImport (x, pos, size, curr_vec)
  | MKExport (ExpKStop import, left, size, vec) ->
      let f = Source.(import @@ region s left (pos s)) in
      next @@ MKField (ExportField, size, lazy_vec_step f vec)
  | MKExport (k, pos, size, curr_vec) ->
      let* x = export_step s k in
      next @@ MKExport (x, pos, size, curr_vec)
  | MKGlobal (gtype, left, BlockStop res, size, vec) ->
      let* () = end_ s in
      let ginit = Source.(res @@ region s left (pos s)) in
      let f = Source.({gtype; ginit} @@ region s left (pos s)) in
      next @@ MKField (GlobalField, size, lazy_vec_step f vec)
  | MKGlobal (ty, pos, k, size, curr_vec) ->
      let* k = block_step ~allow_floats s allocs k in
      next @@ MKGlobal (ty, pos, k, size, curr_vec)
  | MKElem (EKStop elem, left, size, vec) ->
      let elem = Source.(elem @@ region s left (pos s)) in
      next @@ MKField (ElemField, size, lazy_vec_step elem vec)
  | MKElem (elem_kont, pos, size, curr_vec) ->
      let* elem_kont = elem_step ~allow_floats s allocs elem_kont in
      next @@ MKElem (elem_kont, pos, size, curr_vec)
  | MKData (DKStop data, left, size, vec) ->
      let data = Source.(data @@ region s left (pos s)) in
      next @@ MKField (DataField, size, lazy_vec_step data vec)
  | MKData (data_kont, pos, size, curr_vec) ->
      let* data_kont = data_step ~allow_floats s allocs data_kont in
      next @@ MKData (data_kont, pos, size, curr_vec)
  | MKCode (CKStop func, _, size, vec) ->
      next @@ MKField (CodeField, size, lazy_vec_step func vec)
  | MKCode (code_kont, pos, size, curr_vec) ->
      let* code_kont = code_step ~allow_floats s allocs code_kont in
      next @@ MKCode (code_kont, pos, size, curr_vec)
  | MKElaborateFunc
      ( _ft,
        _fb,
        (LazyVec {vector = func_types; _} as vec),
        None,
        no_datas_in_func )
    when is_end_of_vec vec ->
      next @@ MKBuild (Some func_types, no_datas_in_func)
  | MKElaborateFunc
      (fts, fbs, (LazyVec {offset; _} as vec), None, no_datas_in_func) ->
      let* ft = Vector.get offset fts in
      let* fb = Vector.get offset fbs in
      let fb' = Source.({fb.it with ftype = ft} @@ fb.at) in
      if no_datas_in_func then
        let (Block_label body) = fb'.it.body in
        let* instrs = Vector.get body allocs.blocks in
        next
        @@ MKElaborateFunc
             ( fts,
               fbs,
               lazy_vec_step fb' vec,
               Some
                 (LazyVec
                    {
                      offset = 0l;
                      vector =
                        Vector.singleton
                          (LazyVec {offset = 0l; vector = instrs});
                    }),
               no_datas_in_func )
      else
        next
        @@ MKElaborateFunc
             (fts, fbs, lazy_vec_step fb' vec, None, no_datas_in_func)
  | MKElaborateFunc (fts, fbs, vec, Some vec', no_datas_in_func)
    when is_end_of_vec vec' ->
      next @@ MKElaborateFunc (fts, fbs, vec, None, no_datas_in_func)
  | MKElaborateFunc
      (fts, fbs, vec, Some (LazyVec {offset; vector}), no_datas_in_func) ->
      let* (LazyVec {offset = i; vector = block} as veci) =
        Vector.get offset vector
      in
      if is_end_of_vec veci then
        next
        @@ MKElaborateFunc
             ( fts,
               fbs,
               vec,
               Some (LazyVec {offset = Int32.succ offset; vector}),
               no_datas_in_func )
      else
        let* instr = Vector.get i block in

        let rec push vec = function
          | Block_label x :: rst ->
              let* instrs = Vector.get x allocs.blocks in
              let vec, _ =
                Vector.append (LazyVec {offset = 0l; vector = instrs}) vec
              in
              push vec rst
          | [] -> Lwt.return vec
        in
        let no_data e =
          match e.Source.it with
          | MemoryInit _ | DataDrop _ -> (false, [])
          | Block (_, es) | Loop (_, es) -> (true, [es])
          | If (_, es1, es2) -> (true, [es1; es2])
          | _ -> (true, [])
        in
        let no_data, new_blocks = no_data instr in
        let* vector = push vector new_blocks in
        let vector =
          Vector.set
            offset
            (LazyVec {offset = Int32.succ i; vector = block})
            vector
        in
        let no_datas_in_func = no_data && no_datas_in_func in
        next
        @@ MKElaborateFunc
             ( fts,
               fbs,
               vec,
               (if no_datas_in_func then Some (LazyVec {offset; vector})
               else None),
               no_datas_in_func )
  | MKBuild (funcs, no_datas_in_func) ->
      let {
        types;
        data_count;
        datas;
        elems;
        start;
        tables;
        memories;
        globals;
        imports;
        exports;
        (* `vars` and `code` are already handled `MKElaborateFunc` and merged
           into `funcs` in the current state. *)
        vars = _;
        code = _;
      } =
        state.building_state
      in
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
      let m =
        Source.(
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
            allocations = state.allocation_state;
          }
          @@ region_ state.stream_name 0 state.stream_pos)
      in
      {state with module_kont = MKStop m} |> Lwt.return
  | MKStop _ (* Stop cannot reduce. *) -> raise (Step_error Module_step)

let initial_decode_kont ~name =
  let allocation_state = Ast.empty_allocations () in
  {
    building_state = empty_building_state;
    module_kont = MKStart;
    allocation_state;
    stream_pos = 0;
    stream_name = name;
  }

let module_ ~allow_floats name bytes =
  let open Lwt.Syntax in
  let rec loop = function
    | {module_kont = MKStop m; _} -> Lwt.return m
    | k ->
        let* next_state = module_step ~allow_floats bytes k in
        loop next_state
  in
  loop @@ initial_decode_kont ~name

let decode ~allow_floats ~name ~bytes = module_ ~allow_floats name bytes

let all_custom tag s =
  let open Lwt.Syntax in
  let* header = u32 s in
  require (header = magic) s 0 "magic header not detected" ;
  let* version = u32 s in
  require (version = Encode.version) s 4 "unknown binary version" ;
  let rec collect () =
    let* () = iterate non_custom_section s in
    let* x = custom_section s in
    match x with
    | None -> Lwt.return_nil
    | Some (n, s) when n = tag ->
        let+ rst = collect () in
        s :: rst
    | Some _ -> collect ()
  in
  collect ()

let decode_custom tag ~name ~bytes = all_custom tag (make_stream ~name ~bytes)
