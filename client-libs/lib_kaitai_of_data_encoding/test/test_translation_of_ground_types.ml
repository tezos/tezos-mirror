(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
    doc: ! 'Encoding id: ground_uint8'
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
    doc: ! 'Encoding id: ground_int8'
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
    doc: ! 'Encoding id: ground_uint16'
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
    doc: ! 'Encoding id: ground_int16'
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
    doc: ! 'Encoding id: ground_int32'
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
    doc: ! 'Encoding id: ground_int64'
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
    doc: ! 'Encoding id: ground_int31'
    types:
      int31:
        seq:
        - id: int31
          type: s4
          valid:
            min: -1073741824
            max: 1073741823
    seq:
    - id: ground_int31
      type: int31
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
    doc: ! 'Encoding id: ground_float'
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
    doc: ! 'Encoding id: ground_bool'
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
    doc: ! 'Encoding id: ground_bytes'
    types:
      bytes_dyn_uint30:
        seq:
        - id: len_bytes_dyn_uint30
          type: u4
          valid:
            max: 1073741823
        - id: bytes_dyn_uint30
          size: len_bytes_dyn_uint30
    seq:
    - id: ground_bytes
      type: bytes_dyn_uint30 |}]

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
    doc: ! 'Encoding id: bytes_fixed_32'
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
    doc: ! 'Encoding id: bytes_variable'
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
    doc: ! 'Encoding id: ground_string'
    types:
      bytes_dyn_uint30:
        seq:
        - id: len_bytes_dyn_uint30
          type: u4
          valid:
            max: 1073741823
        - id: bytes_dyn_uint30
          size: len_bytes_dyn_uint30
    seq:
    - id: ground_string
      type: bytes_dyn_uint30 |}]

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
    doc: ! 'Encoding id: ground_n'
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
    doc: ! 'Encoding id: ground_z'
    types:
      n_chunk:
        seq:
        - id: has_more
          type: b1be
        - id: payload
          type: b7be
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
    seq:
    - id: ground_z
      type: z
  |}]
