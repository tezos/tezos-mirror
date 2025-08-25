(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Binary_error_types

(* Do not leak [Local_read_error] outside of this module.
   It is intended as a local control-flow mechanism only. *)
exception Local_read_error of read_error

let raise_read_error e = raise (Local_read_error e)

type state = {
  buffer : string;
  mutable offset : int;
  mutable remaining_bytes : int;
  mutable allowed_bytes : int option;
}

let check_allowed_bytes state size =
  match state.allowed_bytes with
  | Some len when len < size -> raise_read_error Size_limit_exceeded
  | Some len -> Some (len - size)
  | None -> None

let check_remaining_bytes state size =
  if state.remaining_bytes < size then raise_read_error Not_enough_data ;
  state.remaining_bytes - size

let read_atom size conv state =
  let offset = state.offset in
  state.remaining_bytes <- check_remaining_bytes state size ;
  state.allowed_bytes <- check_allowed_bytes state size ;
  state.offset <- state.offset + size ;
  conv state.buffer offset

(** Reader for all the atomic types. *)
module Atom = struct
  let uint8 = read_atom Binary_size.uint8 TzEndian.get_uint8_string

  let uint16 endianness state =
    read_atom Binary_size.uint16 (TzEndian.get_uint16_string endianness) state

  let int8 = read_atom Binary_size.int8 TzEndian.get_int8_string

  let int16 endianness state =
    read_atom Binary_size.int16 (TzEndian.get_int16_string endianness) state

  let int32 endianness state =
    read_atom Binary_size.int32 (TzEndian.get_int32_string endianness) state

  let int64 endianness state =
    read_atom Binary_size.int64 (TzEndian.get_int64_string endianness) state

  let float = read_atom Binary_size.float TzEndian.get_double_string

  let bool state = int8 state <> 0

  let uint30 endianness state =
    read_atom
      Binary_size.uint30
      (fun buffer ofs ->
        let v32 = TzEndian.get_int32_string endianness buffer ofs in
        let v = Int32.to_int v32 in
        if v < 0 then
          raise_read_error (Invalid_int {min = 0; v; max = (1 lsl 30) - 1}) ;
        v)
      state

  let int31 endianness state =
    read_atom
      Binary_size.int31
      (fun buffer ofs ->
        let r32 = TzEndian.get_int32_string endianness buffer ofs in
        let r = Int32.to_int r32 in
        if not (-0x4000_0000l <= r32 && r32 <= 0x3fff_ffffl) then
          raise_read_error
            (Invalid_int {min = -0x4000_0000; v = r; max = 0x3fff_ffff}) ;
        r)
      state

  let int kind endianness state =
    match kind with
    | `Int31 -> int31 endianness state
    | `Int16 -> int16 endianness state
    | `Int8 -> int8 state
    | `Uint30 -> uint30 endianness state
    | `Uint16 -> uint16 endianness state
    | `Uint8 -> uint8 state

  let ranged_int ~minimum ~endianness ~maximum state =
    let ranged =
      match Binary_size.range_to_size ~minimum ~maximum with
      | `Int8 -> int8 state
      | `Int16 -> int16 endianness state
      | `Int31 -> int31 endianness state
      | `Uint8 -> uint8 state
      | `Uint16 -> uint16 endianness state
      | `Uint30 -> uint30 endianness state
    in
    let ranged = if minimum > 0 then ranged + minimum else ranged in
    if not (minimum <= ranged && ranged <= maximum) then
      raise_read_error (Invalid_int {min = minimum; v = ranged; max = maximum}) ;
    ranged

  let ranged_float ~minimum ~maximum state =
    let ranged = float state in
    if not (minimum <= ranged && ranged <= maximum) then
      raise_read_error
        (Invalid_float {min = minimum; v = ranged; max = maximum}) ;
    ranged

  let rec read_z res value bit_in_value state =
    let byte = uint8 state in
    let value = value lor ((byte land 0x7F) lsl bit_in_value) in
    let bit_in_value = bit_in_value + 7 in
    let bit_in_value, value =
      if bit_in_value < 8 then (bit_in_value, value)
      else (
        Buffer.add_char res (Char.unsafe_chr (value land 0xFF)) ;
        (bit_in_value - 8, value lsr 8))
    in
    if byte land 0x80 = 0x80 then read_z res value bit_in_value state
    else (
      if bit_in_value > 0 then Buffer.add_char res (Char.unsafe_chr value) ;
      if byte = 0x00 then raise_read_error Trailing_zero ;
      Z.of_bits (Buffer.contents res))

  let n state =
    let first = uint8 state in
    let first_value = first land 0x7F in
    if first land 0x80 = 0x80 then
      read_z (Buffer.create 100) first_value 7 state
    else Z.of_int first_value

  let with_limit ~limit read state =
    let old_allowed_bytes = state.allowed_bytes in
    let limit =
      match state.allowed_bytes with
      | None -> limit
      | Some current_limit -> min current_limit limit
    in
    state.allowed_bytes <- Some limit ;
    let v = read state in
    let allowed_bytes =
      match old_allowed_bytes with
      | None -> None
      | Some old_limit ->
          let remaining =
            match state.allowed_bytes with
            | None -> assert false
            | Some remaining -> remaining
          in
          let read = limit - remaining in
          Some (old_limit - read)
    in
    state.allowed_bytes <- allowed_bytes ;
    v

  let uint30_like_n state =
    let v = with_limit ~limit:Binary_size.max_size_of_uint30_like_n n state in
    if Z.compare v (Z.of_int (Binary_size.max_int `N)) > 0 then
      let min = 0 and max = Binary_size.max_int `N in
      (* we use [min_int] to hint at the overlfow-like issue *)
      let v = Binary_size.min_int `Uint30 in
      raise_read_error (Invalid_int {min; v; max})
    else Z.to_int v

  let z state =
    let first = uint8 state in
    let first_value = first land 0x3F in
    let sign = first land 0x40 <> 0 in
    if first land 0x80 = 0x80 then
      let n = read_z (Buffer.create 100) first_value 6 state in
      if sign then Z.neg n else n
    else
      let n = Z.of_int first_value in
      if sign then Z.neg n else n

  let string_enum arr state =
    let read_index =
      match Binary_size.enum_size arr with
      | `Uint8 -> uint8
      | `Uint16 -> uint16 TzEndian.default_endianness
      | `Uint30 -> uint30 TzEndian.default_endianness
    in
    let index = read_index state in
    if index >= Array.length arr then raise_read_error No_case_matched ;
    arr.(index)

  let fixed_length_bytes length =
    read_atom length @@ fun buf ofs ->
    Bytes.unsafe_of_string @@ String.sub buf ofs length

  let fixed_length_string length =
    read_atom length @@ fun buf ofs -> String.sub buf ofs length

  let fixed_length_bigstring length =
    read_atom length @@ fun buf ofs ->
    Bigstringaf.of_string ~off:ofs ~len:length buf

  let tag = function
    | `Uint8 -> uint8
    | `Uint16 -> uint16 TzEndian.default_endianness
end

(** Main recursive reading function, in continuation passing style. *)
let rec read_rec : type ret. ret Encoding.t -> state -> ret =
 fun e state ->
  let open Encoding in
  match e.encoding with
  | Null -> ()
  | Empty -> ()
  | Constant _ -> ()
  | Ignore -> ()
  | Bool -> Atom.bool state
  | Int8 -> Atom.int8 state
  | Uint8 -> Atom.uint8 state
  | Int16 endianness -> Atom.int16 endianness state
  | Uint16 endianness -> Atom.uint16 endianness state
  | Int31 endianness -> Atom.int31 endianness state
  | Int32 endianness -> Atom.int32 endianness state
  | Int64 endianness -> Atom.int64 endianness state
  | N -> Atom.n state
  | Z -> Atom.z state
  | Float -> Atom.float state
  | Bytes (`Fixed n, _) -> Atom.fixed_length_bytes n state
  | Bytes (`Variable, _) -> Atom.fixed_length_bytes state.remaining_bytes state
  | String (`Fixed n, _) -> Atom.fixed_length_string n state
  | String (`Variable, _) ->
      Atom.fixed_length_string state.remaining_bytes state
  | Bigstring (`Fixed n, _) -> Atom.fixed_length_bigstring n state
  | Bigstring (`Variable, _) ->
      Atom.fixed_length_bigstring state.remaining_bytes state
  | Padded (e, n) ->
      let v = read_rec e state in
      ignore (Atom.fixed_length_string n state : string) ;
      v
  | RangedInt {minimum; endianness; maximum} ->
      Atom.ranged_int ~minimum ~endianness ~maximum state
  | RangedFloat {minimum; maximum} -> Atom.ranged_float ~minimum ~maximum state
  | String_enum (_, arr) -> Atom.string_enum arr state
  | Array {length_limit; length_encoding = None; elts = e} -> (
      match length_limit with
      | No_limit ->
          let l, size = read_variable_list Array_too_long max_int e state in
          Arrconv.array_of_list_size l size
      | At_most max_length ->
          let l, size = read_variable_list Array_too_long max_length e state in
          Arrconv.array_of_list_size l size
      | Exactly exact_length -> read_fixed_array exact_length e state)
  | Array
      {
        length_limit = At_most max_length;
        length_encoding = Some length_encoding;
        elts = e;
      } ->
      let len =
        try read_rec length_encoding state
        with
        (* translating uint_like_n overflow *)
        | Binary_error_types.Read_error (Invalid_int _) ->
          raise_read_error Array_too_long
      in
      if len < 0 then
        raise_read_error (Invalid_int {min = 0; v = len; max = max_length}) ;
      if len > max_length then raise_read_error Array_too_long ;
      read_fixed_array len e state
  | Array
      {length_limit = Exactly _ | No_limit; length_encoding = Some _; elts = _}
    ->
      assert false
  | List {length_limit; length_encoding = None; elts = e} -> (
      match length_limit with
      | No_limit -> fst (read_variable_list List_too_long max_int e state)
      | At_most max_length ->
          fst (read_variable_list List_too_long max_length e state)
      | Exactly exact_length -> read_fixed_list exact_length e state)
  | List
      {
        length_limit = At_most max_length;
        length_encoding = Some length_encoding;
        elts = e;
      } ->
      let len =
        try read_rec length_encoding state
        with
        (* translating uint_like_n overflow *)
        | Binary_error_types.Read_error (Invalid_int _) ->
          raise_read_error List_too_long
      in
      if len < 0 then
        raise_read_error (Invalid_int {min = 0; v = len; max = max_length}) ;
      if len > max_length then raise_read_error List_too_long ;
      read_fixed_list len e state
  | List
      {length_limit = Exactly _ | No_limit; length_encoding = Some _; elts = _}
    ->
      assert false
  | Obj (Req {encoding = e; _}) -> read_rec e state
  | Obj (Dft {encoding = e; _}) -> read_rec e state
  | Obj (Opt {kind = `Dynamic; encoding = e; _}) ->
      let present = Atom.bool state in
      if not present then None else Some (read_rec e state)
  | Obj (Opt {kind = `Variable; encoding = e; _}) ->
      if state.remaining_bytes = 0 then None else Some (read_rec e state)
  | Objs {kind = `Fixed sz; left; right} ->
      ignore (check_remaining_bytes state sz : int) ;
      ignore (check_allowed_bytes state sz : int option) ;
      let left = read_rec left state in
      let right = read_rec right state in
      (left, right)
  | Objs {kind = `Dynamic; left; right} ->
      let left = read_rec left state in
      let right = read_rec right state in
      (left, right)
  | Objs {kind = `Variable; left; right} -> read_variable_pair left right state
  | Tup e -> read_rec e state
  | Tups {kind = `Fixed sz; left; right} ->
      ignore (check_remaining_bytes state sz : int) ;
      ignore (check_allowed_bytes state sz : int option) ;
      let left = read_rec left state in
      let right = read_rec right state in
      (left, right)
  | Tups {kind = `Dynamic; left; right} ->
      let left = read_rec left state in
      let right = read_rec right state in
      (left, right)
  | Tups {kind = `Variable; left; right} -> read_variable_pair left right state
  | Conv {inj; encoding; _} ->
      let v = read_rec encoding state in
      inj v
  | Union {tag_size; tagged_cases; _} ->
      let ctag = Atom.tag tag_size state in
      if ctag >= Array.length tagged_cases then
        raise_read_error (Unexpected_tag ctag) ;
      let (Case {inj; encoding; _} as case) = tagged_cases.(ctag) in
      if is_undefined_case case then raise_read_error (Unexpected_tag ctag)
      else
        let e = read_rec encoding state in
        inj e
  | Dynamic_size {kind; encoding = e} ->
      let sz =
        match kind with
        | `N -> Atom.uint30_like_n state
        | #Binary_size.unsigned_integer as kind ->
            Atom.int kind TzEndian.default_endianness state
      in
      let remaining = check_remaining_bytes state sz in
      state.remaining_bytes <- sz ;
      ignore (check_allowed_bytes state sz : int option) ;
      let v = read_rec e state in
      if state.remaining_bytes <> 0 then raise_read_error Extra_bytes ;
      state.remaining_bytes <- remaining ;
      v
  | Check_size {limit; encoding = e} ->
      Atom.with_limit ~limit (read_rec e) state
  | Describe {encoding = e; _} -> read_rec e state
  | Splitted {encoding = e; _} -> read_rec e state
  | Mu {fix; _} ->
      let e = fix e in
      read_rec e state
  | Delayed f ->
      let e = f () in
      read_rec e state

and read_variable_pair : type left right.
    left Encoding.t -> right Encoding.t -> state -> left * right =
 fun e1 e2 state ->
  match (Encoding.classify e1, Encoding.classify e2) with
  | (`Dynamic | `Fixed _), `Variable ->
      let left = read_rec e1 state in
      let right = read_rec e2 state in
      (left, right)
  | `Variable, `Fixed n ->
      if n > state.remaining_bytes then raise_read_error Not_enough_data ;
      state.remaining_bytes <- state.remaining_bytes - n ;
      let left = read_rec e1 state in
      assert (state.remaining_bytes = 0) ;
      state.remaining_bytes <- n ;
      let right = read_rec e2 state in
      assert (state.remaining_bytes = 0) ;
      (left, right)
  | `Dynamic, (`Fixed _ | `Dynamic) -> assert false
  | `Fixed _, (`Fixed _ | `Dynamic) -> assert false
  | `Variable, (`Variable | `Dynamic) -> assert false
(* Should be rejected by [Encoding.Kind.combine] *)

and read_variable_list : type a.
    read_error -> int -> a Encoding.t -> state -> a list * int =
 fun error max_length e state ->
  let rec loop max_length acc size =
    if state.remaining_bytes = 0 then (List.rev acc, size)
    else if max_length = 0 then raise_read_error error
    else
      let v = read_rec e state in
      loop (max_length - 1) (v :: acc) (size + 1)
  in
  loop max_length [] 0

and read_fixed_list : type a. int -> a Encoding.t -> state -> a list =
 fun exact_length e state ->
  let rec loop exact_length acc =
    if exact_length = 0 then List.rev acc
    else if state.remaining_bytes = 0 then raise_read_error Not_enough_data
    else
      let v = read_rec e state in
      loop (exact_length - 1) (v :: acc)
  in
  loop exact_length []

and read_fixed_array : type a. int -> a Encoding.t -> state -> a array =
 fun exact_length e state ->
  if exact_length = 0 then [||]
  else if state.remaining_bytes = 0 then raise_read_error Not_enough_data
  else
    let v = read_rec e state in
    let arr = Array.make exact_length v in
    for i = 1 to exact_length - 1 do
      if state.remaining_bytes = 0 then raise_read_error Not_enough_data ;
      let v = read_rec e state in
      Array.unsafe_set arr i v
    done ;
    arr

(** ******************** *)

(** Various entry points *)

let wrap_reader f =
  try f () with
  | (Out_of_memory | Stack_overflow) as exc -> raise exc
  | Invariant_guard s -> raise (Read_error (User_invariant_guard s))
  | Local_read_error re -> raise (Read_error re)
  | exc ->
      let s = Printexc.to_string exc in
      raise (Read_error (Exception_raised_in_user_function s))

let read_exn encoding buffer ofs len =
  let state =
    {buffer; offset = ofs; remaining_bytes = len; allowed_bytes = None}
  in
  wrap_reader (fun () ->
      let v = read_rec encoding state in
      (state.offset, v))

let read encoding buffer ofs len =
  try Ok (read_exn encoding buffer ofs len) with Read_error err -> Error err

let read_opt encoding buffer ofs len =
  try Some (read_exn encoding buffer ofs len) with Read_error _ -> None

let of_string_exn encoding buffer =
  let len = String.length buffer in
  let state =
    {buffer; offset = 0; remaining_bytes = len; allowed_bytes = None}
  in
  wrap_reader (fun () ->
      let v = read_rec encoding state in
      if state.offset <> len then raise_read_error Extra_bytes ;
      v)

let of_string encoding buffer =
  try Ok (of_string_exn encoding buffer) with Read_error err -> Error err

let of_string_opt encoding buffer =
  try Some (of_string_exn encoding buffer) with Read_error _ -> None

let of_bytes_exn encoding buffer =
  of_string_exn encoding (Bytes.unsafe_to_string buffer)

let of_bytes encoding buffer =
  of_string encoding (Bytes.unsafe_to_string buffer)

let of_bytes_opt encoding buffer =
  of_string_opt encoding (Bytes.unsafe_to_string buffer)
