(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type error += Rlp_decoding_error of string

type item = Value of bytes | List of item list

let rec encode_int buffer i =
  if i > 0 then (
    encode_int buffer (i lsr 8) ;
    Buffer.add_char buffer (Char.chr (i land 0xff)))

let rec encode_z buffer i =
  let open Z in
  if i > zero then (
    encode_z buffer (shift_right i 8) ;
    let byte = logand i (of_int 0xff) |> to_int in
    Buffer.add_char buffer (Char.chr byte))

let encode_value buffer bytes =
  let len = Bytes.length bytes in
  if len = 1 && Char.code (Bytes.get bytes 0) < 128 then
    Buffer.add_char buffer (Bytes.get bytes 0)
  else if len < 56 then (
    Buffer.add_char buffer (Char.chr (len + 0x80)) ;
    Buffer.add_bytes buffer bytes)
  else
    let len_buf = Buffer.create 0 in
    let () = encode_int len_buf len in
    let prefix = 0xb7 + Buffer.length len_buf in
    Buffer.add_char buffer (Char.chr prefix) ;
    Buffer.add_buffer buffer len_buf ;
    Buffer.add_bytes buffer bytes

let rec encode_list buffer list =
  let list_buffer = Buffer.create 0 in
  List.iter (encode list_buffer) list ;
  let len = Buffer.length list_buffer in
  if len < 56 then (
    Buffer.add_char buffer (Char.chr (0xc0 + len)) ;
    Buffer.add_buffer buffer list_buffer)
  else
    let len_buf = Buffer.create 0 in
    let () = encode_int len_buf len in
    let prefix = 0xf7 + Buffer.length len_buf in
    Buffer.add_char buffer (Char.chr prefix) ;
    Buffer.add_buffer buffer len_buf ;
    Buffer.add_buffer buffer list_buffer

and encode buffer = function
  | Value b -> encode_value buffer b
  | List l -> encode_list buffer l

let encode_int i =
  let buffer = Buffer.create 0 in
  encode_int buffer i ;
  Buffer.to_bytes buffer

let encode_z i =
  let buffer = Buffer.create 0 in
  encode_z buffer i ;
  Buffer.to_bytes buffer

let encode item =
  let buffer = Buffer.create 0 in
  encode buffer item ;
  Buffer.to_bytes buffer

let decode_int bytes offset len =
  let open Result_syntax in
  let rec decode acc i =
    if i >= len then acc
    else
      let byte = Bytes.get_uint8 bytes (offset + i) in
      decode ((acc lsl 8) + byte) (i + 1)
  in
  (* Checks the length is not negative and the encoded integer does not starts
     with leading zeros *)
  if len < 0 then tzfail (Rlp_decoding_error "decode_int")
  else if len > 0 && Bytes.get_uint8 bytes offset = 0 then
    tzfail (Rlp_decoding_error "decode_int")
  else return (decode 0 0)

let decode_z bytes offset len =
  let open Result_syntax in
  let rec decode acc i =
    if i >= len then acc
    else
      let byte = Char.code (Bytes.get bytes (offset + i)) |> Z.of_int in
      decode Z.(shift_left acc 8 + byte) (i + 1)
  in
  (* Checks the length is not negative and the encoded integer does not starts
     with leading zeros *)
  if len < 0 then tzfail (Rlp_decoding_error "decode_z")
  else if len > 0 && Bytes.get_uint8 bytes offset = 0 then
    tzfail (Rlp_decoding_error "decode_z")
  else return (decode Z.zero 0)

(* Check the data/list length cannot actually be encoded in the previous
   encoding, with the length encoding in the prefix. If that's the case, this
   implies the encoding should have been smaller. *)
let check_length (lower_bound, upper_bound) len =
  let open Result_syntax in
  if len <= upper_bound - lower_bound then
    tzfail (Rlp_decoding_error "check_length")
  else return_unit

(* If the value is a single byte whose value is less or equal than 0x7f, this
   means it could have been encoded as a single byte. *)
let check_byte bytes =
  let open Result_syntax in
  if Bytes.length bytes = 1 then
    if Char.code (Bytes.get bytes 0) < 128 then
      tzfail (Rlp_decoding_error "check_byte")
    else return_unit
  else return_unit

(* Bytes.sub, but returns an error. *)
let sub_bytes bytes offset length =
  let open Result_syntax in
  if Bytes.length bytes < offset + length then
    tzfail (Rlp_decoding_error "Bytes.sub")
  else return (Bytes.sub bytes offset length)

let decode_value bytes ~prefix ~offset =
  let open Result_syntax in
  if prefix <= 0x7f then return (Value (Bytes.make 1 (Char.chr prefix)), offset)
    (* Value of size < 56 case *)
  else if prefix <= 0xb7 then
    let length = prefix - 0x80 in
    let* value = sub_bytes bytes offset length in
    let* () = check_byte value in
    return (Value value, offset + length) (* Rest of the value case *)
  else if prefix <= 0xbf then
    let string_length = prefix - 0xb7 in
    let* length = decode_int bytes offset string_length in
    let* () = check_length (0x80, 0xb7) length in
    let value_offset = offset + string_length in
    let* value = sub_bytes bytes value_offset length in
    return (Value value, value_offset + length)
  else tzfail (Rlp_decoding_error "decode_value")

let rec decode bytes offset =
  let open Result_syntax in
  let* () =
    if Bytes.length bytes <= offset || bytes = Bytes.empty then
      tzfail (Rlp_decoding_error "decode")
    else return_unit
  in
  let prefix = Char.code (Bytes.get bytes offset) in
  if prefix <= 0xbf then decode_value bytes ~prefix ~offset:(offset + 1)
  else decode_list bytes ~prefix ~offset:(offset + 1)

and decode_list str ~prefix ~offset =
  let open Result_syntax in
  let rec decode_items acc offset closing_offset =
    if offset = closing_offset then return (List (List.rev acc), offset)
    else if offset > closing_offset then
      tzfail (Rlp_decoding_error "decode_list")
    else
      let* item, next_offset = decode str offset in
      decode_items (item :: acc) next_offset closing_offset
  in
  if prefix <= 0xf7 then
    let length = prefix - 0xc0 in
    decode_items [] offset (offset + length)
  else
    let list_length = prefix - 0xf7 in
    let* length = decode_int str offset list_length in
    let* () = check_length (0xc0, 0xf7) length in
    let list_offset = offset + list_length in
    decode_items [] list_offset (list_offset + length)

let decode_int bytes = decode_int bytes 0 (Bytes.length bytes)

let decode_z bytes = decode_z bytes 0 (Bytes.length bytes)

let decode str = decode str 0 |> Result.map fst

let rec pp ppf = function
  | Value data -> Hex.pp ppf (Hex.of_bytes data)
  | List items ->
      Format.fprintf
        ppf
        "List [%a]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ")
           pp)
        items
