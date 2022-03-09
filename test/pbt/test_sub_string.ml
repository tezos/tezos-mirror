(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

let tamper_with s =
  let open Bytes in
  set (unsafe_of_string s) 0 'X'

let () =
  (* test Stdlib.String.sub *)
  let ful = String.make 1 '0' in
  let sub = String.sub ful 0 1 in
  tamper_with ful ;
  assert (not (String.equal ful sub))

let () =
  (* test Fixed.string - to_/of_string *)
  let enc = Data_encoding.Fixed.string 1 in
  let ful = String.make 1 '0' in
  let bin = Data_encoding.Binary.to_string_exn enc ful in
  let nib = Data_encoding.Binary.of_string_exn enc bin in
  tamper_with bin ;
  assert (String.equal ful nib)

let () =
  (* test Fixed.string *)
  let enc = Data_encoding.Fixed.string 1 in
  let ful = String.make 1 '0' in
  let buf = Bytes.create 1 in
  let sta =
    Option.get
    @@ Data_encoding.Binary.make_writer_state buf ~offset:0 ~allowed_bytes:1
  in
  let wri = Data_encoding.Binary.write_exn enc ful sta in
  assert (wri = 1) ;
  let buf = Bytes.unsafe_to_string buf in
  let red, nib = Data_encoding.Binary.read_exn enc buf 0 1 in
  assert (red = 1) ;
  tamper_with buf ;
  assert (String.equal ful nib)

let () =
  (* test Fixed.bytes - to_/of_bytes *)
  let enc = Data_encoding.Fixed.bytes 1 in
  let ful = Bytes.make 1 '0' in
  let bin = Data_encoding.Binary.to_bytes_exn enc ful in
  let nib = Data_encoding.Binary.of_bytes_exn enc bin in
  Bytes.set bin 0 'X' ;
  assert (Bytes.equal ful nib)
