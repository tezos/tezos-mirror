(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let%expect_test "test string enum" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"simple_union"
      Data_encoding.(string_enum [("one", 1); ("two", 2)])
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: simple_union
      endian: be
    enums:
      simple_union:
        0: one
        1: two
    seq:
    - id: simple_union
      type: u1
      enum: simple_union
  |}]

let%expect_test "test string enum" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"simple_union"
      Data_encoding.(
        string_enum [("one", 1); ("One", 2); ("One", 3); ("one ", 4)])
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: simple_union
      endian: be
    enums:
      simple_union:
        0: one
        1:
          id: one_0
          doc: One
        2:
          id: one_1
          doc: One
        3:
          id: one__
          doc: ! 'one '
    seq:
    - id: simple_union
      type: u1
      enum: simple_union
  |}]
