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

(** Persistent state of the binary reader. *)
type state = {
  stream : Binary_stream.t;  (** All the remaining data to be read. *)
  remaining_bytes : int option;
      (** Total number of bytes that should be from 'stream' (None =
      unlimited). Reading less bytes should raise [Extra_bytes] and
      trying to read more bytes should raise [Not_enough_data]. *)
  allowed_bytes : int option;
      (** Maximum number of bytes that are allowed to be read from 'stream'
      before to fail (None = unlimited). *)
  total_read : int;
      (** Total number of bytes that has been read from [stream] since the
      beginning. *)
}

(** Return type for the function [read_rec]. See [Data_encoding] for its
    description. *)
type 'ret status =
  | Success of {result : 'ret; size : int; stream : Binary_stream.t}
  | Await of (Bytes.t -> 'ret status)
  | Error of read_error

let check_remaining_bytes state size =
  match state.remaining_bytes with
  | Some len when len < size -> raise_read_error Not_enough_data
  | Some len -> Some (len - size)
  | None -> None

let check_allowed_bytes state size =
  match state.allowed_bytes with
  | Some len when len < size -> raise_read_error Size_limit_exceeded
  | Some len -> Some (len - size)
  | None -> None

(** [read_atom resume size conv state k] reads [size] bytes from [state],
    pass it to [conv] to be decoded, and finally call the continuation [k]
    with the decoded value and the updated state.

    The function [conv] is also allowed to raise [Local_read_error err].
    In that case the exception is caught and [Error err] is returned.

    If there is not enough [remaining_bytes] to be read in [state], the
    function returns [Error Not_enough_data] instead of calling
    the continuation.

    If there is not enough [allowed_bytes] to be read in [state], the
    function returns [Error Size_limit_exceeded] instead of calling
    the continuation.

    If there is not enough bytes to be read in [state], the function
    returns [Await resume] instead of calling the continuation. *)
let read_atom resume size conv state k =
  match
    let remaining_bytes = check_remaining_bytes state size in
    let allowed_bytes = check_allowed_bytes state size in
    let res, stream = Binary_stream.read state.stream size in
    ( conv res.buffer res.ofs,
      {
        remaining_bytes;
        allowed_bytes;
        stream;
        total_read = state.total_read + size;
      } )
  with
  | exception Local_read_error error -> Error error
  | exception Binary_stream.Need_more_data -> Await resume
  | v -> k v

(* tail call *)

(** Reader for all the atomic types. *)
module Atom = struct
  let uint8 r st k = read_atom r Binary_size.uint8 TzEndian.get_uint8 st k

  let uint16 endianness r st k =
    read_atom r Binary_size.uint16 (TzEndian.get_uint16 endianness) st k

  let int8 r st k = read_atom r Binary_size.int8 TzEndian.get_int8 st k

  let int16 endianness r st k =
    read_atom r Binary_size.int16 (TzEndian.get_int16 endianness) st k

  let int32 endianness r st k =
    read_atom r Binary_size.int32 (TzEndian.get_int32 endianness) st k

  let int64 endianness r st k =
    read_atom r Binary_size.int64 (TzEndian.get_int64 endianness) st k

  let float r = read_atom r Binary_size.float TzEndian.get_double

  let bool resume state k =
    int8 resume state @@ fun (v, state) -> k (v <> 0, state)

  let uint30 endianness r st k =
    read_atom
      r
      Binary_size.uint30
      (fun buffer ofs ->
        let v32 = TzEndian.get_int32 endianness buffer ofs in
        let v = Int32.to_int v32 in
        if v < 0 then
          raise_read_error (Invalid_int {min = 0; v; max = (1 lsl 30) - 1}) ;
        v)
      st
      k

  let int31 endianness r st k =
    read_atom
      r
      Binary_size.int31
      (fun buffer ofs ->
        let r32 = TzEndian.get_int32 endianness buffer ofs in
        let r = Int32.to_int r32 in
        if not (-0x4000_0000l <= r32 && r32 <= 0x3fff_ffffl) then
          raise_read_error
            (Invalid_int {min = -0x4000_0000; v = r; max = 0x3fff_ffff}) ;
        r)
      st
      k

  let int kind endianness r st k =
    match kind with
    | `Int31 -> int31 endianness r st k
    | `Int16 -> int16 endianness r st k
    | `Int8 -> int8 r st k
    | `Uint30 -> uint30 endianness r st k
    | `Uint16 -> uint16 endianness r st k
    | `Uint8 -> uint8 r st k

  let ranged_int ~minimum ~endianness ~maximum resume state k =
    let read_int resume state k =
      match Binary_size.range_to_size ~minimum ~maximum with
      | `Int8 -> int8 resume state k
      | `Int16 -> int16 endianness resume state k
      | `Int31 -> int31 endianness resume state k
      | `Uint8 -> uint8 resume state k
      | `Uint16 -> uint16 endianness resume state k
      | `Uint30 -> uint30 endianness resume state k
    in
    read_int resume state @@ fun (ranged, state) ->
    let ranged = if minimum > 0 then ranged + minimum else ranged in
    if not (minimum <= ranged && ranged <= maximum) then
      Error (Invalid_int {min = minimum; v = ranged; max = maximum})
    else k (ranged, state)

  let ranged_float ~minimum ~maximum resume state k =
    float resume state @@ fun (ranged, state) ->
    if not (minimum <= ranged && ranged <= maximum) then
      Error (Invalid_float {min = minimum; v = ranged; max = maximum})
    else k (ranged, state)

  let rec read_z res value bit_in_value state k =
    let resume buffer =
      let stream = Binary_stream.push buffer state.stream in
      read_z res value bit_in_value {state with stream} k
    in
    uint8 resume state @@ fun (byte, state) ->
    let value = value lor ((byte land 0x7F) lsl bit_in_value) in
    let bit_in_value = bit_in_value + 7 in
    let bit_in_value, value =
      if bit_in_value < 8 then (bit_in_value, value)
      else (
        Buffer.add_char res (Char.unsafe_chr (value land 0xFF)) ;
        (bit_in_value - 8, value lsr 8))
    in
    if byte land 0x80 = 0x80 then read_z res value bit_in_value state k
    else (
      if bit_in_value > 0 then Buffer.add_char res (Char.unsafe_chr value) ;
      if byte = 0x00 then raise_read_error Trailing_zero ;
      k (Z.of_bits (Buffer.contents res), state))

  let n resume state k =
    uint8 resume state @@ fun (first, state) ->
    let first_value = first land 0x7F in
    if first land 0x80 = 0x80 then
      read_z (Buffer.create 100) first_value 7 state k
    else k (Z.of_int first_value, state)

  let with_limit ~limit whole read state k =
    let old_allowed_bytes = state.allowed_bytes in
    let limit =
      match state.allowed_bytes with
      | None -> limit
      | Some current_limit -> min current_limit limit
    in
    (match state.remaining_bytes with
    | Some remaining when whole && limit < remaining ->
        raise_read_error Size_limit_exceeded
    | Some _ | None -> ()) ;
    let state = {state with allowed_bytes = Some limit} in
    read state @@ fun (v, state) ->
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
    k (v, {state with allowed_bytes})

  let uint30_like_n resume state k =
    with_limit
      ~limit:Binary_size.max_size_of_uint30_like_n
      false
      (n resume)
      state
    @@ fun (v, state) ->
    if Z.compare v (Z.of_int (Binary_size.max_int `N)) > 0 then
      let min = 0 and max = Binary_size.max_int `N in
      (* we use [min_int] to hint at the overlfow-like issue *)
      let v = Binary_size.min_int `Uint30 in
      raise_read_error (Invalid_int {min; v; max})
    else
      let v = Z.to_int v in
      k (v, state)

  let z resume state k =
    uint8 resume state @@ fun (first, state) ->
    let first_value = first land 0x3F in
    let sign = first land 0x40 <> 0 in
    if first land 0x80 = 0x80 then
      read_z (Buffer.create 100) first_value 6 state @@ fun (n, state) ->
      k ((if sign then Z.neg n else n), state)
    else
      let n = Z.of_int first_value in
      k ((if sign then Z.neg n else n), state)

  let string_enum arr resume state k =
    let read_index =
      match Binary_size.enum_size arr with
      | `Uint8 -> uint8
      | `Uint16 -> uint16 TzEndian.default_endianness
      | `Uint30 -> uint30 TzEndian.default_endianness
    in
    read_index resume state @@ fun (index, state) ->
    if index >= Array.length arr then Error No_case_matched
    else k (arr.(index), state)

  let fixed_length_bytes length r =
    read_atom r length @@ fun buf ofs -> Bytes.sub buf ofs length

  let fixed_length_string length r =
    read_atom r length @@ fun buf ofs -> Bytes.sub_string buf ofs length

  let fixed_length_bigstring length r =
    read_atom r length @@ fun buf ofs ->
    let b = Bigstringaf.create length in
    Bigstringaf.blit_from_bytes buf ~src_off:ofs b ~dst_off:0 ~len:length ;
    b

  let tag = function
    | `Uint8 -> uint8
    | `Uint16 -> uint16 TzEndian.default_endianness
end

let rec skip n state k =
  let resume buffer =
    let stream = Binary_stream.push buffer state.stream in
    try skip n {state with stream} k with Local_read_error err -> Error err
  in
  Atom.fixed_length_string n resume state @@ fun ((_, state) : string * _) ->
  k state

(** Main recursive reading function, in continuation passing style. *)
let rec read_rec :
    type next ret.
    bool ->
    next Encoding.t ->
    state ->
    (next * state -> ret status) ->
    ret status =
 fun whole e state k ->
  let resume buffer =
    let stream = Binary_stream.push buffer state.stream in
    try read_rec whole e {state with stream} k
    with Local_read_error err -> Error err
  in
  let open Encoding in
  assert (Encoding.classify e <> `Variable || state.remaining_bytes <> None) ;
  match e.encoding with
  | Null -> k ((), state)
  | Empty -> k ((), state)
  | Constant _ -> k ((), state)
  | Ignore -> k ((), state)
  | Bool -> Atom.bool resume state k
  | Int8 -> Atom.int8 resume state k
  | Uint8 -> Atom.uint8 resume state k
  | Int16 endianness -> Atom.int16 endianness resume state k
  | Uint16 endianness -> Atom.uint16 endianness resume state k
  | Int31 endianness -> Atom.int31 endianness resume state k
  | Int32 endianness -> Atom.int32 endianness resume state k
  | Int64 endianness -> Atom.int64 endianness resume state k
  | N -> Atom.n resume state k
  | Z -> Atom.z resume state k
  | Float -> Atom.float resume state k
  | Bytes (`Fixed n, _) -> Atom.fixed_length_bytes n resume state k
  | Bytes (`Variable, _) ->
      let size = remaining_bytes_for_variable_encoding state in
      Atom.fixed_length_bytes size resume state k
  | String (`Fixed n, _) -> Atom.fixed_length_string n resume state k
  | String (`Variable, _) ->
      let size = remaining_bytes_for_variable_encoding state in
      Atom.fixed_length_string size resume state k
  | Bigstring (`Fixed n, _) -> Atom.fixed_length_bigstring n resume state k
  | Bigstring (`Variable, _) ->
      let size = remaining_bytes_for_variable_encoding state in
      Atom.fixed_length_bigstring size resume state k
  | Padded (e, n) ->
      read_rec false e state @@ fun (v, state) ->
      skip n state @@ fun state -> k (v, state)
  | RangedInt {minimum; endianness; maximum} ->
      Atom.ranged_int ~minimum ~endianness ~maximum resume state k
  | RangedFloat {minimum; maximum} ->
      Atom.ranged_float ~minimum ~maximum resume state k
  | String_enum (_, arr) -> Atom.string_enum arr resume state k
  | Array {length_limit; length_encoding = None; elts = e} -> (
      match length_limit with
      | No_limit ->
          read_variable_list Array_too_long max_int e state
          @@ fun (l, size, state) ->
          let arr = Arrconv.array_of_list_size l size in
          k (arr, state)
      | At_most max_length ->
          read_variable_list Array_too_long max_length e state
          @@ fun (l, size, state) ->
          let arr = Arrconv.array_of_list_size l size in
          k (arr, state)
      | Exactly exact_length -> read_fixed_array exact_length e state k)
  | Array
      {
        length_limit = At_most max_length;
        length_encoding = Some length_encoding;
        elts = e;
      } ->
      read_rec whole length_encoding state @@ fun (len, state) ->
      if len < 0 then
        raise_read_error (Invalid_int {min = 0; v = len; max = max_length}) ;
      if len > max_length then raise (Read_error Array_too_long) ;
      read_fixed_array len e state k
  | Array
      {length_limit = Exactly _ | No_limit; length_encoding = Some _; elts = _}
    ->
      assert false
  | List {length_limit; length_encoding = None; elts = e} -> (
      match length_limit with
      | No_limit ->
          read_variable_list List_too_long max_int e state
          @@ fun (l, _, state) -> k (l, state)
      | At_most max_length ->
          read_variable_list List_too_long max_length e state
          @@ fun (l, _, state) -> k (l, state)
      | Exactly exact_length -> read_fixed_list exact_length e state k)
  | List
      {
        length_limit = At_most max_length;
        length_encoding = Some length_encoding;
        elts = e;
      } ->
      read_rec whole length_encoding state @@ fun (len, state) ->
      if len < 0 then
        raise_read_error (Invalid_int {min = 0; v = len; max = max_length}) ;
      if len > max_length then raise (Read_error List_too_long) ;
      read_fixed_list len e state k
  | List
      {length_limit = Exactly _ | No_limit; length_encoding = Some _; elts = _}
    ->
      assert false
  | Obj (Req {encoding = e; _}) -> read_rec whole e state k
  | Obj (Dft {encoding = e; _}) -> read_rec whole e state k
  | Obj (Opt {kind = `Dynamic; encoding = e; _}) ->
      Atom.bool resume state @@ fun (present, state) ->
      if not present then k (None, state)
      else read_rec whole e state @@ fun (v, state) -> k (Some v, state)
  | Obj (Opt {kind = `Variable; encoding = e; _}) ->
      let size = remaining_bytes_for_variable_encoding state in
      if size = 0 then k (None, state)
      else read_rec whole e state @@ fun (v, state) -> k (Some v, state)
  | Objs {kind = `Fixed sz; left; right} ->
      ignore (check_remaining_bytes state sz : int option) ;
      ignore (check_allowed_bytes state sz : int option) ;
      read_rec false left state @@ fun (left, state) ->
      read_rec whole right state @@ fun (right, state) ->
      k ((left, right), state)
  | Objs {kind = `Dynamic; left; right} ->
      read_rec false left state @@ fun (left, state) ->
      read_rec whole right state @@ fun (right, state) ->
      k ((left, right), state)
  | Objs {kind = `Variable; left; right} ->
      read_variable_pair left right state k
  | Tup e -> read_rec whole e state k
  | Tups {kind = `Fixed sz; left; right} ->
      ignore (check_remaining_bytes state sz : int option) ;
      ignore (check_allowed_bytes state sz : int option) ;
      read_rec false left state @@ fun (left, state) ->
      read_rec whole right state @@ fun (right, state) ->
      k ((left, right), state)
  | Tups {kind = `Dynamic; left; right} ->
      read_rec false left state @@ fun (left, state) ->
      read_rec whole right state @@ fun (right, state) ->
      k ((left, right), state)
  | Tups {kind = `Variable; left; right} ->
      read_variable_pair left right state k
  | Conv {inj; encoding; _} ->
      read_rec whole encoding state @@ fun (v, state) -> k (inj v, state)
  | Union {tag_size; cases; _} -> (
      Atom.tag tag_size resume state @@ fun (ctag, state) ->
      match
        List.find_opt
          (fun (Case {tag; _}) ->
            Uint_option.fold tag ~none:false ~some:(fun tag -> tag = ctag))
          cases
      with
      | None -> Error (Unexpected_tag ctag)
      | Some (Case {encoding; inj; _}) ->
          read_rec whole encoding state @@ fun (v, state) -> k (inj v, state))
  | Dynamic_size {kind; encoding = e} ->
      (match kind with
      | `N -> Atom.uint30_like_n resume state
      | #Binary_size.unsigned_integer as kind ->
          Atom.int kind TzEndian.default_endianness resume state)
      @@ fun (sz, state) ->
      let remaining = check_remaining_bytes state sz in
      let state = {state with remaining_bytes = Some sz} in
      ignore (check_allowed_bytes state sz : int option) ;
      read_rec true e state @@ fun (v, state) ->
      if state.remaining_bytes <> Some 0 then Error Extra_bytes
      else k (v, {state with remaining_bytes = remaining})
  | Check_size {limit; encoding = e} ->
      Atom.with_limit
        ~limit
        whole
        (fun state k -> read_rec whole e state k)
        state
        k
  | Describe {encoding = e; _} -> read_rec whole e state k
  | Splitted {encoding = e; _} -> read_rec whole e state k
  | Mu {fix; _} ->
      let e = fix e in
      read_rec whole e state k
  | Delayed f ->
      let e = f () in
      read_rec whole e state k

and remaining_bytes_for_variable_encoding {remaining_bytes; _} =
  match remaining_bytes with
  | None ->
      (* This function should only be called with a variable encoding,
         for which the `remaining_bytes` should never be `None`. *)
      assert false
  | Some len -> len

and read_variable_pair :
    type left right ret.
    left Encoding.t ->
    right Encoding.t ->
    state ->
    ((left * right) * state -> ret status) ->
    ret status =
 fun e1 e2 state k ->
  let size = remaining_bytes_for_variable_encoding state in
  match (Encoding.classify e1, Encoding.classify e2) with
  | (`Dynamic | `Fixed _), `Variable ->
      read_rec false e1 state @@ fun (left, state) ->
      read_rec true e2 state @@ fun (right, state) -> k ((left, right), state)
  | `Variable, `Fixed n ->
      if n > size then Error Not_enough_data
      else
        let state = {state with remaining_bytes = Some (size - n)} in
        read_rec true e1 state @@ fun (left, state) ->
        assert (state.remaining_bytes = Some 0) ;
        let state = {state with remaining_bytes = Some n} in
        read_rec true e2 state @@ fun (right, state) ->
        assert (state.remaining_bytes = Some 0) ;
        k ((left, right), state)
  | `Dynamic, (`Fixed _ | `Dynamic)
  | `Fixed _, (`Fixed _ | `Dynamic)
  | `Variable, (`Variable | `Dynamic) ->
      (* Should be rejected by [Encoding.Kind.combine] *)
      assert false

and read_variable_list :
    type a ret.
    read_error ->
    int ->
    a Encoding.t ->
    state ->
    (a list * int * state -> ret status) ->
    ret status =
 fun error max_length e state k ->
  let rec loop state acc accsize max_length =
    let size = remaining_bytes_for_variable_encoding state in
    if size = 0 then k (List.rev acc, accsize, state)
    else if max_length = 0 then raise_read_error error
    else
      read_rec false e state @@ fun (v, state) ->
      loop state (v :: acc) (accsize + 1) (max_length - 1)
  in
  loop state [] 0 max_length

and read_fixed_list :
    type a ret.
    int -> a Encoding.t -> state -> (a list * state -> ret status) -> ret status
    =
 fun exact_length e state k ->
  let rec loop state acc exact_length =
    if exact_length = 0 then k (List.rev acc, state)
    else
      let () =
        match state.remaining_bytes with
        | Some size -> if size = 0 then raise_read_error Not_enough_data
        | None -> ()
      in
      read_rec false e state @@ fun (v, state) ->
      loop state (v :: acc) (exact_length - 1)
  in
  loop state [] exact_length

and read_fixed_array :
    type a ret.
    int ->
    a Encoding.t ->
    state ->
    (a array * state -> ret status) ->
    ret status =
 fun exact_length e state k ->
  if exact_length = 0 then k ([||], state)
  else
    let () =
      match state.remaining_bytes with
      | Some size -> if size = 0 then raise_read_error Not_enough_data
      | None -> ()
    in
    read_rec false e state @@ fun (v, state) ->
    let arr = Array.make exact_length v in
    let rec loop state index =
      if index = exact_length then k (arr, state)
      else
        let () =
          match state.remaining_bytes with
          | Some size -> if size = 0 then raise_read_error Not_enough_data
          | None -> ()
        in
        read_rec false e state @@ fun (v, state) ->
        Array.unsafe_set arr index v ;
        loop state (index + 1)
    in
    loop state 1

let read_rec e state k =
  try read_rec false e state k with
  | (Out_of_memory | Stack_overflow) as exc -> raise exc
  | Invariant_guard s -> Error (User_invariant_guard s)
  | Local_read_error re -> Error re
  | exc ->
      let s = Printexc.to_string exc in
      Error (Exception_raised_in_user_function s)

(** ******************** *)

(** Various entry points *)

let success (v, state) =
  Success {result = v; size = state.total_read; stream = state.stream}

let read_stream ?(init = Binary_stream.empty) encoding =
  match Encoding.classify encoding with
  | `Variable ->
      invalid_arg "Data_encoding.Binary.read_stream: variable encoding"
  | `Dynamic | `Fixed _ ->
      (* No hardcoded read limit in a stream. *)
      let state =
        {
          remaining_bytes = None;
          allowed_bytes = None;
          stream = init;
          total_read = 0;
        }
      in
      read_rec encoding state success
