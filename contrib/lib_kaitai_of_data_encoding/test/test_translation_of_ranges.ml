(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let%expect_test "test small range" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"small_range"
      Data_encoding.(ranged_int 0 13)
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: small_range
      endian: be
    doc: ! 'Encoding id: small_range'
    seq:
    - id: small_range
      type: u1
      valid:
        max: 13
  |}]

let%expect_test "test bigger range" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"b_range"
      Data_encoding.(ranged_int 0 1025)
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: b_range
      endian: be
    doc: ! 'Encoding id: b_range'
    seq:
    - id: b_range
      type: u2
      valid:
        max: 1025
  |}]

let%expect_test "test biggest range" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"bb_range"
      Data_encoding.(ranged_int (-22299) 29290)
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: bb_range
      endian: be
    doc: ! 'Encoding id: bb_range'
    seq:
    - id: bb_range
      type: s2
      valid:
        min: -22299
        max: 29290
  |}]

let%expect_test "test float range" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"float_range"
      Data_encoding.(ranged_float (-22299.01) 3333333.33333)
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: float_range
      endian: be
    doc: ! 'Encoding id: float_range'
    seq:
    - id: float_range
      type: f8
      valid:
        min: -22299.01
        max: 3333333.33333
  |}]

let%expect_test "test small shifted range" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"shifted_small_range"
      Data_encoding.(ranged_int 3 13)
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: shifted_small_range
      endian: be
    doc: ! 'Encoding id: shifted_small_range'
    types:
      shifted_small_range_shifted_to_zero:
        instances:
          value:
            value: (shifted_small_range_shifted_to_zero + 3)
            doc: ! >-
              The interval is represented shifted towards 0 for compactness, this instance
              corrects the shift.
        seq:
        - id: shifted_small_range_shifted_to_zero
          type: u1
          valid:
            max: 10
    seq:
    - id: shifted_small_range
      type: shifted_small_range_shifted_to_zero |}]

let%expect_test "test bigger shifted range" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"shifted_big_range"
      Data_encoding.(ranged_int 103 100002)
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: shifted_big_range
      endian: be
    doc: ! 'Encoding id: shifted_big_range'
    types:
      shifted_big_range_shifted_to_zero:
        instances:
          value:
            value: (shifted_big_range_shifted_to_zero + 103)
            doc: ! >-
              The interval is represented shifted towards 0 for compactness, this instance
              corrects the shift.
        seq:
        - id: shifted_big_range_shifted_to_zero
          type: u4
          valid:
            max: 99899
    seq:
    - id: shifted_big_range
      type: shifted_big_range_shifted_to_zero |}]
