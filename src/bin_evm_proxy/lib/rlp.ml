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
