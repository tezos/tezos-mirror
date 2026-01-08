(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

let raise e = raise (Read_error e)

type slice = {name : string; value : string; pretty_printed : string}

(* state management *)

type slicer_state = {
  buffer : string;
  mutable offset : int;
  mutable remaining_bytes : int;
  mutable allowed_bytes : int option;
  mutable slices : slice list;
}

let make_slicer_state buffer ~offset ~length =
  if length < 0 || length > String.length buffer - offset then None
  else
    Some
      {
        buffer;
        offset;
        remaining_bytes = length;
        allowed_bytes = None;
        slices = [];
      }

let check_allowed_bytes state size =
  match state.allowed_bytes with
  | Some len when len < size -> raise Size_limit_exceeded
  | Some len -> Some (len - size)
  | None -> None

let check_remaining_bytes state size =
  if state.remaining_bytes < size then raise Not_enough_data ;
  state.remaining_bytes - size

let read_atom ?(pp = fun _ -> "") size conv name state =
  let offset = state.offset in
  state.remaining_bytes <- check_remaining_bytes state size ;
  state.allowed_bytes <- check_allowed_bytes state size ;
  state.offset <- state.offset + size ;
  let value = String.sub state.buffer offset size in
  let result = conv state.buffer offset in
  state.slices <- {name; value; pretty_printed = pp result} :: state.slices ;
  result

(** Reader for all the atomic types. *)
module Atom = struct
  let read_byte state =
    let size = Binary_size.int8 in
    let offset = state.offset in
    state.remaining_bytes <- check_remaining_bytes state size ;
    state.allowed_bytes <- check_allowed_bytes state size ;
    state.offset <- state.offset + size ;
    TzEndian.get_int8_string state.buffer offset

  let uint8 name st =
    read_atom
      ~pp:string_of_int
      Binary_size.uint8
      TzEndian.get_uint8_string
      name
      st

  let uint16 endianness name st =
    read_atom
      ~pp:string_of_int
      Binary_size.uint16
      (TzEndian.get_uint16_string endianness)
      name
      st

  let int8 name st =
    read_atom
      ~pp:string_of_int
      Binary_size.int8
      TzEndian.get_int8_string
      name
      st

  let int16 endianness name st =
    read_atom
      ~pp:string_of_int
      Binary_size.int16
      (TzEndian.get_int16_string endianness)
      name
      st

  let int32 endianness name st =
    read_atom
      ~pp:Int32.to_string
      Binary_size.int32
      (TzEndian.get_int32_string endianness)
      name
      st

  let int64 endianness name st =
    read_atom
      ~pp:Int64.to_string
      Binary_size.int64
      (TzEndian.get_int64_string endianness)
      name
      st

  let float =
    read_atom ~pp:string_of_float Binary_size.float TzEndian.get_double_string

  let bool state name =
    read_atom
      ~pp:(fun x -> string_of_bool (x <> 0))
      Binary_size.int8
      TzEndian.get_int8_string
      state
      name
    <> 0

  let uint30 endianness name st =
    read_atom
      ~pp:string_of_int
      Binary_size.uint30
      (fun buffer ofs ->
        let v32 = TzEndian.get_int32_string endianness buffer ofs in
        let v = Int32.to_int v32 in
        if v < 0 then raise (Invalid_int {min = 0; v; max = (1 lsl 30) - 1}) ;
        v)
      name
      st

  let int31 endianness name st =
    read_atom
      ~pp:string_of_int
      Binary_size.int31
      (fun buffer ofs ->
        let v32 = TzEndian.get_int32_string endianness buffer ofs in
        Int32.to_int v32)
      name
      st

  let int kind endianness name st =
    match kind with
    | `Int31 -> int31 endianness name st
    | `Int16 -> int16 endianness name st
    | `Int8 -> int8 name st
    | `Uint30 -> uint30 endianness name st
    | `Uint16 -> uint16 endianness name st
    | `Uint8 -> uint8 name st

  let ranged_int ~minimum ~endianness ~maximum name state =
    let ranged =
      match Binary_size.range_to_size ~minimum ~maximum with
      | `Int8 -> int8 name state
      | `Int16 -> int16 endianness name state
      | `Int31 -> int31 endianness name state
      | `Uint8 -> uint8 name state
      | `Uint16 -> uint16 endianness name state
      | `Uint30 -> uint30 endianness name state
    in
    let ranged = if minimum > 0 then ranged + minimum else ranged in
    if not (minimum <= ranged && ranged <= maximum) then
      raise (Invalid_int {min = minimum; v = ranged; max = maximum}) ;
    ranged

  let ranged_float ~minimum ~maximum name state =
    let ranged = float name state in
    if not (minimum <= ranged && ranged <= maximum) then
      raise (Invalid_float {min = minimum; v = ranged; max = maximum}) ;
    ranged

  let rec read_z res value bit_in_value name state initial_offset =
    let byte = read_byte state in
    let value = value lor ((byte land 0x7F) lsl bit_in_value) in
    let bit_in_value = bit_in_value + 7 in
    let bit_in_value, value =
      if bit_in_value < 8 then (bit_in_value, value)
      else (
        Buffer.add_char res (Char.unsafe_chr (value land 0xFF)) ;
        (bit_in_value - 8, value lsr 8))
    in
    if byte land 0x80 = 0x80 then
      read_z res value bit_in_value name state initial_offset
    else (
      if bit_in_value > 0 then Buffer.add_char res (Char.unsafe_chr value) ;
      if byte = 0x00 then raise Trailing_zero ;
      let result = Z.of_bits (Buffer.contents res) in
      let pretty_printed = Z.to_string result in
      let value =
        String.sub state.buffer initial_offset (state.offset - initial_offset)
      in
      state.slices <- {name; value; pretty_printed} :: state.slices ;
      result)

  let n name state =
    let initial_offset = state.offset in
    let first = read_byte state in

    let first_value = first land 0x7F in
    if first land 0x80 = 0x80 then
      read_z (Buffer.create 100) first_value 7 name state initial_offset
    else
      let result = Z.of_int first_value in
      let pretty_printed = Z.to_string result in
      let value =
        String.sub state.buffer initial_offset (state.offset - initial_offset)
      in
      state.slices <- {name; value; pretty_printed} :: state.slices ;
      result

  let with_limit ~limit read name state =
    let old_allowed_bytes = state.allowed_bytes in
    let limit =
      match state.allowed_bytes with
      | None -> limit
      | Some current_limit -> min current_limit limit
    in
    state.allowed_bytes <- Some limit ;
    let v = read name state in
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

  let uint30_like_n name state =
    let v =
      with_limit ~limit:Binary_size.max_size_of_uint30_like_n n name state
    in
    if Z.compare v (Z.of_int (Binary_size.max_int `N)) > 0 then
      let min = 0 and max = Binary_size.max_int `N in
      (* we use [min_int] to hint at the overlfow-like issue *)
      let v = Binary_size.min_int `Uint30 in
      raise (Invalid_int {min; v; max})
    else Z.to_int v

  let z name state =
    let initial_offset = state.offset in
    let first = read_byte state in

    let first_value = first land 0x3F in
    let sign = first land 0x40 <> 0 in
    if first land 0x80 = 0x80 then
      let n =
        read_z (Buffer.create 100) first_value 6 name state initial_offset
      in
      if sign then Z.neg n else n
    else
      let n = Z.of_int first_value in
      if sign then Z.neg n else n

  let string_enum arr name state =
    let read_index =
      match Binary_size.enum_size arr with
      | `Uint8 -> uint8
      | `Uint16 -> uint16 TzEndian.default_endianness
      | `Uint30 -> uint30 TzEndian.default_endianness
    in
    let index = read_index name state in
    if index >= Array.length arr then raise No_case_matched ;
    arr.(index)

  let fixed_length_bytes length =
    read_atom length @@ fun buf ofs ->
    Bytes.unsafe_of_string @@ String.sub buf ofs length

  let fixed_length_string length =
    read_atom ~pp:(Format.sprintf "%S") length @@ fun buf ofs ->
    String.sub buf ofs length

  let fixed_length_bigstring length =
    read_atom
      ~pp:(fun b -> Format.sprintf "%S" (Bigstringaf.to_string b))
      length
    @@ fun buf ofs -> Bigstringaf.of_string ~off:ofs ~len:length buf

  let tag = function
    | `Uint8 -> uint8
    | `Uint16 -> uint16 TzEndian.default_endianness
end

(** Main recursive reading function, in continuation passing style. *)
let rec read_rec : type ret.
    ret Encoding.t -> ?name:string -> slicer_state -> ret =
 fun e ?name state ->
  let ( !! ) x =
    match name with None -> x | Some name -> Format.sprintf "%S (%s)" name x
  in
  let open Encoding in
  match e.encoding with
  | Null -> ()
  | Empty -> ()
  | Constant _ -> ()
  | Ignore -> ()
  | Bool -> Atom.bool !!"bool" state
  | Int8 -> Atom.int8 !!"int8" state
  | Uint8 -> Atom.uint8 !!"uint8" state
  | Int16 endianness -> Atom.int16 endianness !!"int16" state
  | Uint16 endianness -> Atom.uint16 endianness !!"uint16" state
  | Int31 endianness -> Atom.int31 endianness !!"int31" state
  | Int32 endianness -> Atom.int32 endianness !!"int32" state
  | Int64 endianness -> Atom.int64 endianness !!"int64" state
  | N -> Atom.n !!"N" state
  | Z -> Atom.z !!"Z" state
  | Float -> Atom.float !!"float" state
  | Bytes (`Fixed n, _) -> Atom.fixed_length_bytes n !!"bytes" state
  | Bytes (`Variable, _) ->
      Atom.fixed_length_bytes state.remaining_bytes !!"bytes" state
  | String (`Fixed n, _) -> Atom.fixed_length_string n !!"string" state
  | String (`Variable, _) ->
      Atom.fixed_length_string state.remaining_bytes !!"string" state
  | Bigstring (`Fixed n, _) -> Atom.fixed_length_bigstring n !!"string" state
  | Bigstring (`Variable, _) ->
      Atom.fixed_length_bigstring state.remaining_bytes !!"string" state
  | Padded (e, n) ->
      let v = read_rec e ?name state in
      ignore (Atom.fixed_length_string n "padding" state : string) ;
      v
  | RangedInt {minimum; endianness; maximum} ->
      Atom.ranged_int ~minimum ~endianness ~maximum !!"ranged int" state
  | RangedFloat {minimum; maximum} ->
      Atom.ranged_float ~minimum ~maximum !!"ranged float" state
  | String_enum (_, arr) -> Atom.string_enum arr !!"enum" state
  | Array {length_limit; length_encoding = None; elts = e} -> (
      match length_limit with
      | No_limit ->
          let l, size =
            read_variable_list Array_too_long max_int e ?name state
          in
          Arrconv.array_of_list_size l size
      | At_most max_length ->
          let l, size =
            read_variable_list Array_too_long max_length e ?name state
          in
          Arrconv.array_of_list_size l size
      | Exactly exact_length -> read_fixed_array exact_length e ?name state)
  | Array
      {
        length_limit = At_most max_length;
        length_encoding = Some length_encoding;
        elts = e;
      } ->
      let len =
        try read_rec ~name:"array_length" length_encoding state
        with Binary_error_types.Read_error (Invalid_int _) ->
          raise Array_too_long
      in
      if len > max_length then raise Array_too_long ;
      read_fixed_array len e ?name state
  | Array
      {length_limit = Exactly _ | No_limit; length_encoding = Some _; elts = _}
    ->
      assert false
  | List {length_limit; length_encoding = None; elts = e} -> (
      match length_limit with
      | No_limit -> fst (read_variable_list List_too_long max_int e ?name state)
      | At_most max_length ->
          fst (read_variable_list List_too_long max_length e ?name state)
      | Exactly exact_length -> read_fixed_list exact_length e ?name state)
  | List
      {
        length_limit = At_most max_length;
        length_encoding = Some length_encoding;
        elts = e;
      } ->
      let len =
        try read_rec ~name:"list_length" length_encoding state
        with Binary_error_types.Read_error (Invalid_int _) ->
          raise List_too_long
      in
      if len > max_length then raise List_too_long ;
      read_fixed_list len e ?name state
  | List
      {length_limit = Exactly _ | No_limit; length_encoding = Some _; elts = _}
    ->
      assert false
  | Obj (Req {encoding = e; name; _}) -> read_rec e ~name state
  | Obj (Dft {encoding = e; name; _}) -> read_rec e ~name state
  | Obj (Opt {kind = `Dynamic; encoding = e; name; _}) ->
      let present = Atom.bool (name ^ " presence flag") state in
      if not present then None else Some (read_rec e ~name:!!name state)
  | Obj (Opt {kind = `Variable; encoding = e; name; _}) ->
      if state.remaining_bytes = 0 then None
      else Some (read_rec e ~name:!!name state)
  | Objs {kind = `Fixed sz; left; right} ->
      ignore (check_remaining_bytes state sz : int) ;
      ignore (check_allowed_bytes state sz : int option) ;
      let left = read_rec left ?name state in
      let right = read_rec right ?name state in
      (left, right)
  | Objs {kind = `Dynamic; left; right} ->
      let left = read_rec left ?name state in
      let right = read_rec right ?name state in
      (left, right)
  | Objs {kind = `Variable; left; right} ->
      read_variable_pair left right ?name state
  | Tup e -> read_rec e ?name state
  | Tups {kind = `Fixed sz; left; right} ->
      ignore (check_remaining_bytes state sz : int) ;
      ignore (check_allowed_bytes state sz : int option) ;
      let left = read_rec left ?name state in
      let right = read_rec right ?name state in
      (left, right)
  | Tups {kind = `Dynamic; left; right} ->
      let left = read_rec left ?name state in
      let right = read_rec right ?name state in
      (left, right)
  | Tups {kind = `Variable; left; right} ->
      read_variable_pair left right ?name state
  | Conv {inj; encoding; _} -> inj (read_rec encoding ?name state)
  | Union {tag_size; cases; _} ->
      let ctag = Atom.tag tag_size "DUMMY" state in
      let (Case {encoding; inj; _}) =
        try
          List.find
            (function
              | Case {tag = tg; title; _} ->
                  if Uint_option.is_some tg && Uint_option.get tg = ctag then (
                    let {value; pretty_printed; _} = List.hd state.slices in
                    state.slices <-
                      {name = title ^ " tag"; value; pretty_printed}
                      :: List.tl state.slices ;
                    true)
                  else false)
            cases
        with Not_found -> raise (Unexpected_tag ctag)
      in
      inj (read_rec encoding ?name state)
  | Dynamic_size {kind; encoding = e} ->
      let sz =
        match kind with
        | `N -> Atom.uint30_like_n "dynamic length" state
        | #Binary_size.unsigned_integer as kind ->
            Atom.int kind TzEndian.default_endianness "dynamic length" state
      in
      let remaining = check_remaining_bytes state sz in
      state.remaining_bytes <- sz ;
      ignore (check_allowed_bytes state sz : int option) ;
      let v = read_rec e ?name state in
      if state.remaining_bytes <> 0 then raise Extra_bytes ;
      state.remaining_bytes <- remaining ;
      v
  | Check_size {limit; encoding = e} ->
      Atom.with_limit
        ~limit
        (fun name state -> read_rec e ?name state)
        name
        state
  | Describe {encoding = e; id; _} -> read_rec e ~name:!!id state
  | Splitted {encoding = e; _} -> read_rec e ?name state
  | Mu {fix; name; _} -> read_rec (fix e) ~name:!!name state
  | Delayed f -> read_rec (f ()) ?name state

and read_variable_pair : type left right.
    left Encoding.t ->
    right Encoding.t ->
    ?name:string ->
    slicer_state ->
    left * right =
 fun e1 e2 ?name state ->
  match (Encoding.classify e1, Encoding.classify e2) with
  | (`Dynamic | `Fixed _), `Variable ->
      let left = read_rec e1 ?name state in
      let right = read_rec e2 ?name state in
      (left, right)
  | `Variable, `Fixed n ->
      if n > state.remaining_bytes then raise Not_enough_data ;
      state.remaining_bytes <- state.remaining_bytes - n ;
      let left = read_rec e1 ?name state in
      assert (state.remaining_bytes = 0) ;
      state.remaining_bytes <- n ;
      let right = read_rec e2 ?name state in
      assert (state.remaining_bytes = 0) ;
      (left, right)
  | `Dynamic, (`Fixed _ | `Dynamic) -> assert false
  | `Fixed _, (`Fixed _ | `Dynamic) -> assert false
  | `Variable, (`Variable | `Dynamic) -> assert false

and read_variable_list : type a.
    read_error ->
    int ->
    a Encoding.t ->
    ?name:string ->
    slicer_state ->
    a list * int =
 fun error max_length e ?name state ->
  let name = Option.map (fun name -> name ^ " element") name in
  let rec loop max_length acc numitems =
    if state.remaining_bytes = 0 then (List.rev acc, numitems)
    else if max_length = 0 then raise error
    else
      let v = read_rec e ?name state in
      loop (max_length - 1) (v :: acc) (numitems + 1)
  in
  loop max_length [] 0

and read_fixed_list : type a.
    int -> a Encoding.t -> ?name:string -> slicer_state -> a list =
 fun exact_length e ?name state ->
  let name = Option.map (fun name -> name ^ " element") name in
  let rec loop exact_length acc =
    if exact_length = 0 then List.rev acc
    else if state.remaining_bytes = 0 then raise Not_enough_data
    else
      let v = read_rec e ?name state in
      loop (exact_length - 1) (v :: acc)
  in
  loop exact_length []

and read_fixed_array : type a.
    int -> a Encoding.t -> ?name:string -> slicer_state -> a array =
 fun exact_length e ?name state ->
  assert (exact_length >= 0) ;
  if exact_length = 0 then [||]
  else
    let name = Option.map (fun name -> name ^ " element") name in
    if state.remaining_bytes = 0 then raise Not_enough_data ;
    let v = read_rec e ?name state in
    let array = Array.make exact_length v in
    for i = 1 to exact_length - 1 do
      if state.remaining_bytes = 0 then raise Not_enough_data ;
      let v = read_rec e ?name state in
      Array.unsafe_set array i v
    done ;
    array

(** Various entry points *)

let slice_exn (encoding : 'a Encoding.t) state =
  let (_ : 'a) = read_rec encoding state in
  List.rev state.slices

let slice encoding state =
  try Ok (slice_exn encoding state) with Read_error e -> Error e

let slice_opt encoding state =
  try Some (slice_exn encoding state) with Read_error _ -> None

let slice_string_exn (encoding : 'a Encoding.t) buffer =
  let len = String.length buffer in
  let state =
    {
      buffer;
      offset = 0;
      slices = [];
      remaining_bytes = len;
      allowed_bytes = None;
    }
  in
  let (_ : 'a) = read_rec encoding state in
  if state.offset <> len then raise Extra_bytes ;
  List.rev state.slices

let slice_string encoding buffer =
  try Ok (slice_string_exn encoding buffer) with Read_error e -> Error e

let slice_string_opt encoding buffer =
  try Some (slice_string_exn encoding buffer) with Read_error _ -> None

let slice_bytes e b = slice_string e (Bytes.unsafe_to_string b)

let slice_bytes_opt e b = slice_string_opt e (Bytes.unsafe_to_string b)

let slice_bytes_exn e b = slice_string_exn e (Bytes.unsafe_to_string b)
