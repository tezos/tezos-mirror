(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let%expect_test "test padded" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"simple_tuple_with_padding"
      Data_encoding.(tup2 bool Fixed.(add_padding uint8 8))
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: simple_tuple_with_padding
      endian: be
    doc: ! 'Encoding id: simple_tuple_with_padding'
    types:
      simple_tuple_with_padding_field1:
        seq:
        - id: simple_tuple_with_padding_field1
          type: u1
        - id: simple_tuple_with_padding_field1_padding
          size: 8
          doc: This field is for padding, ignore
    enums:
      bool:
        0: false
        255: true
    seq:
    - id: simple_tuple_with_padding_field0
      type: u1
      enum: bool
    - id: simple_tuple_with_padding_field1
      type: simple_tuple_with_padding_field1
  |}]
