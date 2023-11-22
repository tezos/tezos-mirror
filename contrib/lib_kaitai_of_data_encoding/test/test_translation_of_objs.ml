(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let%expect_test "test obj translation" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"simple_obj"
      Data_encoding.(obj2 (req "one" bool) (req "two" uint8))
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: simple_obj
      endian: be
    enums:
      bool:
        0: false
        255: true
    seq:
    - id: one
      type: u1
      enum: bool
    - id: two
      type: u1
  |}]

let%expect_test "test long obj translation" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"long_obj"
      Data_encoding.(
        obj5
          (req "one" bool)
          (req "un" uint8)
          (req "yi" bool)
          (req "uno" uint8)
          (req "wan" uint8))
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: long_obj
      endian: be
    enums:
      bool:
        0: false
        255: true
    seq:
    - id: one
      type: u1
      enum: bool
    - id: un
      type: u1
    - id: yi
      type: u1
      enum: bool
    - id: uno
      type: u1
    - id: wan
      type: u1 |}]

let%expect_test "test obj1 translation" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"obj1"
      Data_encoding.(obj1 (req "mono" uint8))
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: obj1
      endian: be
    seq:
    - id: mono
      type: u1
  |}]

let%expect_test "test objs with opt and dft fields" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"objreqdft"
      Data_encoding.(
        obj3
          (req "one" (obj2 (opt "one" bool) (dft "two" uint8 0)))
          (req "two" bool)
          (req "three" (obj2 (req "one" int64) (opt "two" uint16))))
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: objreqdft
      endian: be
    types:
      three:
        seq:
        - id: one
          type: s8
        - id: two_tag
          type: u1
          enum: bool
        - id: two
          type: u2
          if: (two_tag == bool::true)
      one:
        seq:
        - id: one_tag
          type: u1
          enum: bool
        - id: one
          type: u1
          if: (one_tag == bool::true)
          enum: bool
        - id: two
          type: u1
    enums:
      bool:
        0: false
        255: true
    seq:
    - id: one
      type: one
    - id: two
      type: u1
      enum: bool
    - id: three
      type: three |}]

let%expect_test "test objs with opt and dft fields and doc" =
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"objreqdftdoc"
      Data_encoding.(
        obj2
          (req
             "one"
             (obj2
                (opt
                   ~title:"Foo"
                   ~description:"Foo here there some text"
                   "one"
                   bool)
                (dft ~title:"Barrrr.bar" "two" uint8 0)))
          (req ~description:"The actual payload of the whole thing" "two" bool))
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
    meta:
      id: objreqdftdoc
      endian: be
    types:
      one:
        seq:
        - id: one_tag
          type: u1
          enum: bool
        - id: one
          type: u1
          if: (one_tag == bool::true)
          enum: bool
          doc: ! 'Foo: Foo here there some text'
        - id: two
          type: u1
          doc: Barrrr.bar
    enums:
      bool:
        0: false
        255: true
    seq:
    - id: one
      type: one
    - id: two
      type: u1
      enum: bool
      doc: The actual payload of the whole thing |}]

let%expect_test "test nested object translation" =
  let open Data_encoding in
  let protocol_hash_encoding = obj1 (req "protocol_hash" bytes) in
  let encoding =
    obj2
      (req "replaced_protocol" protocol_hash_encoding)
      (req "replacement_protocol" protocol_hash_encoding)
  in
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"nested_objects"
      encoding
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect
    {|
  meta:
    id: nested_objects
    endian: be
  types:
    protocol_hash:
      seq:
      - id: len_protocol_hash
        type: s4
      - id: protocol_hash
        size: len_protocol_hash
  seq:
  - id: replaced_protocol
    type: protocol_hash
  - id: replacement_protocol
    type: protocol_hash
  |}]

let%expect_test "test object translation with empty fields" =
  let open Data_encoding in
  let encoding =
    obj3
      (req "cst1" (constant "cst1"))
      (opt "cst2" (constant "cst2"))
      (dft "cst3" (constant "cst3") ())
  in
  let s =
    Kaitai_of_data_encoding.Translate.from_data_encoding
      ~id:"nested_objects"
      encoding
  in
  print_endline (Kaitai.Print.print s) ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "redirect_if_many: empty list not supported")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Kaitai_of_data_encoding__Translate.seq_field_of_field in file "contrib/lib_kaitai_of_data_encoding/translate.ml", line 460, characters 8-130
  Called from Kaitai_of_data_encoding__Translate.seq_field_of_data_encoding in file "contrib/lib_kaitai_of_data_encoding/translate.ml", line 266, characters 24-64
  Called from Kaitai_of_data_encoding__Translate.from_data_encoding in file "contrib/lib_kaitai_of_data_encoding/translate.ml", line 705, characters 8-63
  Called from Kaitai_of_data_encoding_test__Test_translation_of_objs.(fun) in file "contrib/lib_kaitai_of_data_encoding/test/test_translation_of_objs.ml", line 228, characters 4-98
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
