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

let guarded_length ~upto xs =
  let rec loop n = function
    | _ :: xs -> if n >= upto then None else loop (n + 1) xs
    | [] -> Some n
  in
  assert (upto >= 0) ;
  loop 0 xs

let wrap_user_function f a =
  try f a with
  | (Out_of_memory | Stack_overflow) as exc -> raise exc
  | exc ->
      let s = Printexc.to_string exc in
      raise (Write_error (Exception_raised_in_user_function s))

let raise error = Stdlib.raise (Write_error error)

(** Imperative state of the binary writer. *)
type writer_state = {
  mutable buffer : Bytes.t;  (** The buffer where to write. *)
  mutable offset : int;
      (** The offset of the next byte to be written in [buffer]. *)
  mutable allowed_bytes : Uint_option.t;
      (** Maximum number of bytes that are allowed to be write in [buffer]
      (after [offset]) before to fail (unless it is [unlimited_bytes]). *)
}

let make_writer_state buffer ~offset ~allowed_bytes =
  if allowed_bytes < 0 || allowed_bytes > Bytes.length buffer - offset then None
  else
    let allowed_bytes = Uint_option.some allowed_bytes in
    Some {buffer; offset; allowed_bytes}

let unlimited_bytes = Uint_option.is_none

let limited_bytes = Uint_option.is_some

let check_allowed_bytes state size =
  if limited_bytes state.allowed_bytes then (
    let allowed_bytes = Uint_option.get state.allowed_bytes in
    if allowed_bytes < size then raise Size_limit_exceeded ;
    state.allowed_bytes <- Uint_option.(some (allowed_bytes - size)))

(** [may_resize state size] will first ensure there is enough
    space in [state.buffer] for writing [size] bytes (starting at
    [state.offset]).

    When the buffer does not have enough space for writing [size] bytes,
    but still has enough [allowed_bytes], it will replace the buffer
    with a buffer large enough.

    @raise [Binary_error.Write_error Size_limit_exceeded] when there is
           not enough allowed bytes to write [size] bytes. *)
let may_resize state size =
  check_allowed_bytes state size ;
  let buffer_len = Bytes.length state.buffer in
  if buffer_len - state.offset < size then (
    let new_buffer = Bytes.create (max (2 * buffer_len) (buffer_len + size)) in
    Bytes.blit state.buffer 0 new_buffer 0 state.offset ;
    state.buffer <- new_buffer) ;
  state.offset <- state.offset + size

(** Writer for all the atomic types. *)
module Atom = struct
  let check_int_range min v max =
    if v < min || max < v then raise (Invalid_int {min; v; max})

  let check_float_range min v max =
    if v < min || max < v then raise (Invalid_float {min; v; max})

  let set_int kind endianness buffer ofs v =
    match kind with
    | `Int31 | `Uint30 ->
        TzEndian.set_int32 endianness buffer ofs (Int32.of_int v)
    | `Int16 | `Uint16 -> TzEndian.set_int16 endianness buffer ofs v
    | `Int8 | `Uint8 -> TzEndian.set_int8 buffer ofs v

  let int kind endianness state v =
    check_int_range (Binary_size.min_int kind) v (Binary_size.max_int kind) ;
    let ofs = state.offset in
    may_resize state (Binary_size.integer_to_size kind) ;
    set_int kind endianness state.buffer ofs v

  let int8 = int `Int8 TzEndian.default_endianness

  let uint8 = int `Uint8 TzEndian.default_endianness

  let int16 endianness = int `Int16 endianness

  let uint16 endianness = int `Uint16 endianness

  let uint30 endianness = int `Uint30 endianness

  let int31 endianness = int `Int31 endianness

  let bool state v = uint8 state (if v then 255 else 0)

  let int32 endianness state v =
    let ofs = state.offset in
    may_resize state Binary_size.int32 ;
    TzEndian.set_int32 endianness state.buffer ofs v

  let int64 endianness state v =
    let ofs = state.offset in
    may_resize state Binary_size.int64 ;
    TzEndian.set_int64 endianness state.buffer ofs v

  let ranged_int ~minimum ~endianness ~maximum state v =
    check_int_range minimum v maximum ;
    let v = if minimum >= 0 then v - minimum else v in
    match Binary_size.range_to_size ~minimum ~maximum with
    | `Uint8 -> uint8 state v
    | `Uint16 -> uint16 endianness state v
    | `Uint30 -> uint30 endianness state v
    | `Int8 -> int8 state v
    | `Int16 -> int16 endianness state v
    | `Int31 -> int31 endianness state v

  let n state v =
    if Z.sign v < 0 then raise Invalid_natural ;
    if Z.equal v Z.zero then uint8 state 0x00
    else
      let bits = Z.numbits v in
      let get_chunk pos len = Z.to_int (Z.extract v pos len) in
      let length = Binary_length.n_length v in
      let offset = state.offset in
      may_resize state length ;
      for i = 0 to length - 1 do
        let pos = i * 7 in
        let chunk_len = if i = length - 1 then bits - pos else 7 in
        TzEndian.set_int8
          state.buffer
          (offset + i)
          ((if i = length - 1 then 0x00 else 0x80) lor get_chunk pos chunk_len)
      done

  let z state v =
    let sign = Z.sign v < 0 in
    let bits = Z.numbits v in
    if Z.equal v Z.zero then uint8 state 0x00
    else
      let v = Z.abs v in
      let get_chunk pos len = Z.to_int (Z.extract v pos len) in
      let length = Binary_length.z_length v in
      let offset = state.offset in
      may_resize state length ;
      TzEndian.set_int8
        state.buffer
        offset
        ((if sign then 0x40 else 0x00)
        lor (if bits > 6 then 0x80 else 0x00)
        lor get_chunk 0 6) ;
      for i = 1 to length - 1 do
        let pos = 6 + ((i - 1) * 7) in
        let chunk_len = if i = length - 1 then bits - pos else 7 in
        TzEndian.set_int8
          state.buffer
          (offset + i)
          ((if i = length - 1 then 0x00 else 0x80) lor get_chunk pos chunk_len)
      done

  let float state v =
    let ofs = state.offset in
    may_resize state Binary_size.float ;
    TzEndian.set_double state.buffer ofs v

  let ranged_float ~minimum ~maximum state v =
    check_float_range minimum v maximum ;
    float state v

  let string_enum tbl arr state v =
    let value =
      try snd (Hashtbl.find tbl v) with Not_found -> raise No_case_matched
    in
    match Binary_size.enum_size arr with
    | `Uint30 -> uint30 TzEndian.default_endianness state value
    | `Uint16 -> uint16 TzEndian.default_endianness state value
    | `Uint8 -> uint8 state value

  let fixed_kind_bytes length state s =
    if Bytes.length s <> length then
      raise (Invalid_bytes_length {expected = length; found = Bytes.length s}) ;
    let ofs = state.offset in
    may_resize state length ;
    Bytes.blit s 0 state.buffer ofs length

  let fixed_kind_string length state s =
    if String.length s <> length then
      raise (Invalid_string_length {expected = length; found = String.length s}) ;
    let ofs = state.offset in
    may_resize state length ;
    Bytes.blit_string s 0 state.buffer ofs length

  let fixed_kind_bigstring length state s =
    if Bigstringaf.length s <> length then
      raise
        (Invalid_string_length {expected = length; found = Bigstringaf.length s}) ;
    let ofs = state.offset in
    may_resize state length ;
    Bigstringaf.blit_to_bytes s ~src_off:0 state.buffer ~dst_off:ofs ~len:length

  let tag = function
    | `Uint8 -> uint8
    | `Uint16 -> uint16 TzEndian.default_endianness
end

(** Main recursive writing function. *)
let rec write_rec : type a. a Encoding.t -> writer_state -> a -> unit =
 fun e state value ->
  let open Encoding in
  match e.encoding with
  | Null -> ()
  | Empty -> ()
  | Constant _ -> ()
  | Ignore -> ()
  | Bool -> Atom.bool state value
  | Int8 -> Atom.int8 state value
  | Uint8 -> Atom.uint8 state value
  | Int16 endianness -> Atom.int16 endianness state value
  | Uint16 endianness -> Atom.uint16 endianness state value
  | Int31 endianness -> Atom.int31 endianness state value
  | Int32 endianness -> Atom.int32 endianness state value
  | Int64 endianness -> Atom.int64 endianness state value
  | N -> Atom.n state value
  | Z -> Atom.z state value
  | Float -> Atom.float state value
  | Bytes (`Fixed n, _) -> Atom.fixed_kind_bytes n state value
  | Bytes (`Variable, _) ->
      let length = Bytes.length value in
      Atom.fixed_kind_bytes length state value
  | String (`Fixed n, _) -> Atom.fixed_kind_string n state value
  | String (`Variable, _) ->
      let length = String.length value in
      Atom.fixed_kind_string length state value
  | Bigstring (`Fixed n, _) -> Atom.fixed_kind_bigstring n state value
  | Bigstring (`Variable, _) ->
      let length = Bigstringaf.length value in
      Atom.fixed_kind_bigstring length state value
  | Padded (e, n) ->
      write_rec e state value ;
      Atom.fixed_kind_string n state (String.make n '\000')
  | RangedInt {minimum; endianness; maximum} ->
      Atom.ranged_int ~minimum ~endianness ~maximum state value
  | RangedFloat {minimum; maximum} ->
      Atom.ranged_float ~minimum ~maximum state value
  | String_enum (tbl, arr) -> Atom.string_enum tbl arr state value
  | Array {length_limit = At_most max_length; length_encoding = Some le; elts}
    ->
      if Array.length value > max_length then raise Array_invalid_length ;
      write_rec le state (Array.length value) ;
      Array.iter (write_rec elts state) value
  | Array
      {length_limit = Exactly _ | No_limit; length_encoding = Some _; elts = _}
    ->
      assert false
  | Array {length_limit; length_encoding = None; elts} ->
      let () =
        match length_limit with
        | No_limit -> ()
        | At_most max_length ->
            if Array.length value > max_length then raise Array_invalid_length
        | Exactly exact_length ->
            if Array.length value <> exact_length then
              raise Array_invalid_length
      in
      Array.iter (write_rec elts state) value
  | List {length_limit = At_most max_length; length_encoding = Some le; elts}
    -> (
      match guarded_length ~upto:max_length value with
      | None -> raise List_invalid_length
      | Some l ->
          write_rec le state l ;
          List.iter (write_rec elts state) value)
  | List
      {length_limit = Exactly _ | No_limit; length_encoding = Some _; elts = _}
    ->
      assert false
  | List {length_limit; length_encoding = None; elts} ->
      let () =
        match length_limit with
        | No_limit -> ()
        | At_most max_length ->
            if List.compare_length_with value max_length > 0 then
              raise List_invalid_length
        | Exactly exact_length ->
            if List.compare_length_with value exact_length <> 0 then
              raise List_invalid_length
      in
      List.iter (write_rec elts state) value
  | Obj (Req {encoding = e; _}) -> write_rec e state value
  | Obj (Opt {kind = `Dynamic; encoding = e; _}) -> (
      match value with
      | None -> Atom.bool state false
      | Some value ->
          Atom.bool state true ;
          write_rec e state value)
  | Obj (Opt {kind = `Variable; encoding = e; _}) -> (
      match value with None -> () | Some value -> write_rec e state value)
  | Obj (Dft {encoding = e; _}) -> write_rec e state value
  | Objs {left; right; _} ->
      let v1, v2 = value in
      write_rec left state v1 ;
      write_rec right state v2
  | Tup e -> write_rec e state value
  | Tups {left; right; _} ->
      let v1, v2 = value in
      write_rec left state v1 ;
      write_rec right state v2
  | Conv {encoding = e; proj; _} ->
      let value = wrap_user_function proj value in
      write_rec e state value
  | Union {tag_size; match_case; _} ->
      let (Matched (tag, e, value)) = wrap_user_function match_case value in
      Atom.tag tag_size state tag ;
      write_rec e state value
  | Dynamic_size {kind; encoding = e} -> (
      let initial_offset = state.offset in
      match kind with
      | `N ->
          let size = Binary_length.length e value in
          if size > Binary_size.max_int `N then raise Size_limit_exceeded ;
          write_with_limit
            (Binary_length.n_length (Z.of_int size))
            n
            state
            (Z.of_int size) ;
          write_with_limit (Binary_size.max_int `N) e state value
      | #Binary_size.unsigned_integer as kind ->
          (* place holder for [size] *)
          Atom.int kind TzEndian.default_endianness state 0 ;
          write_with_limit (Binary_size.max_int kind) e state value ;
          (* patch the written [size] *)
          Atom.set_int
            kind
            TzEndian.default_endianness
            state.buffer
            initial_offset
            (state.offset - initial_offset - Binary_size.integer_to_size kind))
  | Check_size {limit; encoding = e} -> write_with_limit limit e state value
  | Describe {encoding = e; _} -> write_rec e state value
  | Splitted {encoding = e; _} -> write_rec e state value
  | Mu {fix; _} ->
      let e = wrap_user_function fix e in
      write_rec e state value
  | Delayed f ->
      let e = wrap_user_function f () in
      write_rec e state value

and write_with_limit : type a. int -> a Encoding.t -> writer_state -> a -> unit
    =
 fun limit e state value ->
  (* backup the current limit *)
  let old_limit = state.allowed_bytes in
  (* install the new limit (only if smaller than the current limit) *)
  let limit =
    if unlimited_bytes state.allowed_bytes then limit
    else
      let old_limit = Uint_option.get state.allowed_bytes in
      min old_limit limit
  in
  state.allowed_bytes <- Uint_option.some limit ;
  write_rec e state value ;
  (* restore the previous limit (minus the read bytes) *)
  if unlimited_bytes old_limit then state.allowed_bytes <- Uint_option.none
  else
    let remaining = Uint_option.get state.allowed_bytes in
    let read = limit - remaining in
    state.allowed_bytes <- Uint_option.(some (get old_limit - read))

(** ******************** *)

(** Various entry points *)

let write_exn e v state =
  write_rec e state v ;
  state.offset

let write e v state =
  try Ok (write_exn e v state) with Write_error err -> Error err

let write_opt e v state =
  try Some (write_exn e v state) with Write_error _ -> None

let to_bytes_exn ?(buffer_size = 128) e v =
  match Encoding.classify e with
  | `Fixed n ->
      (* Preallocate the complete buffer *)
      let state =
        {
          buffer = Bytes.create n;
          offset = 0;
          allowed_bytes = Uint_option.some n;
        }
      in
      write_rec e state v ;
      state.buffer
  | `Dynamic | `Variable ->
      (* Preallocate a minimal buffer and let's not hardcode a
         limit to its extension. *)
      let state =
        {
          buffer = Bytes.create buffer_size;
          offset = 0;
          allowed_bytes = Uint_option.none;
        }
      in
      write_rec e state v ;
      Bytes.sub state.buffer 0 state.offset

let to_bytes_opt ?buffer_size e v =
  Option.iter
    (fun buffer_size ->
      if buffer_size < 0 then
        Stdlib.raise
          (Invalid_argument
             "Data_encoding.Binary_writer.to_bytes_opt: negative length"))
    buffer_size ;
  try Some (to_bytes_exn ?buffer_size e v) with Write_error _ -> None

let to_bytes ?buffer_size e v =
  Option.iter
    (fun buffer_size ->
      if buffer_size < 0 then
        Stdlib.raise
          (Invalid_argument
             "Data_encoding.Binary_writer.to_bytes: negative length"))
    buffer_size ;
  try Ok (to_bytes_exn ?buffer_size e v) with Write_error err -> Error err

let to_bytes_exn ?buffer_size e v =
  Option.iter
    (fun buffer_size ->
      if buffer_size < 0 then
        Stdlib.raise
          (Invalid_argument
             "Data_encoding.Binary_writer.to_bytes: negative length"))
    buffer_size ;
  to_bytes_exn ?buffer_size e v

let to_string_opt ?buffer_size e v =
  Option.map Bytes.unsafe_to_string (to_bytes_opt ?buffer_size e v)

let to_string ?buffer_size e v =
  Stdlib.Result.map Bytes.unsafe_to_string (to_bytes ?buffer_size e v)

let to_string_exn ?buffer_size e v =
  Bytes.unsafe_to_string (to_bytes_exn ?buffer_size e v)
