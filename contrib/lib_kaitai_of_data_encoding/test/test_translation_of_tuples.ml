(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let%expect_test "test tuple translation" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"simple_tuple"
      Data_encoding.(tup2 bool uint8)
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: simple_tuple
      endian: be
    enums:
      bool:
        0: false
        255: true
    seq:
    - id: simple_tuple_field0
      type: u1
      enum: bool
    - id: simple_tuple_field1
      type: u1
  |}]

let%expect_test "test long tuple translation" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"simple_tuple"
      Data_encoding.(tup5 bool uint8 bool uint8 uint8)
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: simple_tuple
      endian: be
    enums:
      bool:
        0: false
        255: true
    seq:
    - id: simple_tuple_field0
      type: u1
      enum: bool
    - id: simple_tuple_field1
      type: u1
    - id: simple_tuple_field2
      type: u1
      enum: bool
    - id: simple_tuple_field3
      type: u1
    - id: simple_tuple_field4
      type: u1 |}]

let%expect_test "test tup1 tuple translation" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"tup1"
      Data_encoding.(tup1 uint8)
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: tup1
      endian: be
    seq:
    - id: tup1
      type: u1
  |}]

let%expect_test "test tuples with tup1 translation" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"tup1tup"
      Data_encoding.(
        tup3 (tup1 bool) (tup2 uint8 bool) (tup2 (tup1 uint8) uint8))
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: tup1tup
      endian: be
    enums:
      bool:
        0: false
        255: true
    seq:
    - id: tup1tup_field1
      type: u1
      enum: bool
    - id: tup1tup_field3
      type: u1
    - id: tup1tup_field4
      type: u1
      enum: bool
    - id: tup1tup_field7
      type: u1
    - id: tup1tup_field8
      type: u1 |}]

let%expect_test "test tuples with n inside translation" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"tup1tup"
      Data_encoding.(tup3 (tup1 bool) (tup2 n bool) (tup2 (tup1 n) n))
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: tup1tup
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
    enums:
      bool:
        0: false
        255: true
    seq:
    - id: tup1tup_field1
      type: u1
      enum: bool
    - id: tup1tup_field3
      type: n
    - id: tup1tup_field4
      type: u1
      enum: bool
    - id: tup1tup_field7
      type: n
    - id: tup1tup_field8
      type: n |}]