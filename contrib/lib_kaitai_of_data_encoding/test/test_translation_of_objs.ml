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
