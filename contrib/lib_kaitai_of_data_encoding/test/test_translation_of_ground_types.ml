(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* This test suite is meant to test translation of ground encodings
   to [Kaitai.Types.ClassSpec.t] *)

let%expect_test "test uint8 translation" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"ground_uint8"
      Data_encoding.uint8
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: ground_uint8
      endian: be
    seq:
    - id: ground_uint8
      type: u1
  |}]

let%expect_test "test int8 translation" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"ground_int8"
      Data_encoding.int8
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: ground_int8
      endian: be
    seq:
    - id: ground_int8
      type: s1
  |}]

let%expect_test "test uint16 translation" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"ground_uint16"
      Data_encoding.uint16
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: ground_uint16
      endian: be
    seq:
    - id: ground_uint16
      type: u2
  |}]

let%expect_test "test int16 translation" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"ground_int16"
      Data_encoding.int16
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: ground_int16
      endian: be
    seq:
    - id: ground_int16
      type: s2
  |}]

let%expect_test "test int32 translation" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"ground_int32"
      Data_encoding.int32
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: ground_int32
      endian: be
    seq:
    - id: ground_int32
      type: s4
  |}]

let%expect_test "test int64 translation" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"ground_int64"
      Data_encoding.int64
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: ground_int64
      endian: be
    seq:
    - id: ground_int64
      type: s8
  |}]

let%expect_test "test int31 translation" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"ground_int31"
      Data_encoding.int31
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: ground_int31
      endian: be
    seq:
    - id: ground_int31
      type: s4
  |}]

let%expect_test "test float translation" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"ground_float"
      Data_encoding.float
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: ground_float
      endian: be
    seq:
    - id: ground_float
      type: f8
  |}]

let%expect_test "test bool translation" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"ground_bool"
      Data_encoding.bool
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: ground_bool
      endian: be
    enums:
      bool:
        0: false
        255: true
    seq:
    - id: ground_bool
      type: u1
      enum: bool
  |}]

let%expect_test "test dynamic size bytes translation" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"ground_bytes"
      Data_encoding.bytes
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: ground_bytes
      endian: be
    seq:
    - id: len_ground_bytes
      type: s4
    - id: ground_bytes
      size: len_ground_bytes |}]

let%expect_test "test fixed size bytes translation" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"bytes_fixed_32"
      (Data_encoding.Fixed.bytes 32)
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: bytes_fixed_32
      endian: be
    seq:
    - id: bytes_fixed_32
      size: 32 |}]

let%expect_test "test variable size bytes translation" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"bytes_variable"
      Data_encoding.Variable.bytes
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: bytes_variable
      endian: be
    seq:
    - id: bytes_variable
      size-eos: true |}]

let%expect_test "test dynamic size string translation" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"ground_string"
      Data_encoding.string
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: ground_string
      endian: be
    seq:
    - id: len_ground_string
      type: s4
    - id: ground_string
      size: len_ground_string |}]

let%expect_test "test big numbers translation" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"ground_n"
      Data_encoding.n
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: ground_n
      endian: be
    types:
      n:
        seq:
        - id: n
          type: n_chunk
          repeat: until
          repeat-until: not (_.has_more).as<bool>
      n_chunk:
        seq:
        - id: has_more
          type: b1be
        - id: payload
          type: b7be
    seq:
    - id: ground_n
      type: n
  |}]

let%expect_test "test big numbers translation" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"ground_z"
      Data_encoding.z
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: ground_z
      endian: be
    types:
      z:
        seq:
        - id: has_tail
          type: b1be
        - id: sign
          type: b1be
        - id: payload
          type: b6be
        - id: tail
          type: n_chunk
          repeat: until
          repeat-until: not (_.has_more).as<bool>
          if: has_tail.as<bool>
      n_chunk:
        seq:
        - id: has_more
          type: b1be
        - id: payload
          type: b7be
    seq:
    - id: ground_z
      type: z
  |}]
