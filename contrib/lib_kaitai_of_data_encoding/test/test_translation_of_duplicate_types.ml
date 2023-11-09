(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let%expect_test "test duplicate types" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"my type"
      Data_encoding.(
        let e1 = def "fancy_type" bool in
        let e2 = def "fancy_type" int31 in
        let e3 = bool in
        tup3 e1 e2 e3)
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: my__type
      endian: be
    doc: ! 'Encoding id: my type'
    types:
      fancy_type:
        seq:
        - id: fancy_type
          type: u1
          enum: bool
      fancy_type_0:
        seq:
        - id: fancy_type
          type: s4
    enums:
      bool:
        0: false
        255: true
    seq:
    - id: my__type_field0
      type: fancy_type
      doc: fancy_type
    - id: my__type_field1
      type: fancy_type_0
      doc: fancy_type
    - id: my__type_field2
      type: u1
      enum: bool |}]
