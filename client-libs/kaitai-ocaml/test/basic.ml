(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let%expect_test _ =
  let s =
    {|
meta:
  id: bytes_with_io
  title: Byte array with an `_io` member
  license: MIT
doc: |
  Helper type to work around Kaitai Struct not providing an `_io` member for plain byte arrays.
seq:
  - id: data
    size-eos: true
    doc: The actual data.
|}
  in
  let k = Kaitai.Parse.parse s in
  print_endline "Parsing ok" ;
  let s = Kaitai.Print.print k in
  print_endline "Serialization ok" ;
  print_endline s ;
  [%expect
    {|
Parsing ok
Serialization ok
meta:
  id: bytes_with_io
doc: ! >
  Helper type to work around Kaitai Struct not providing an `_io` member for plain
  byte arrays.
seq:
- id: data
  size-eos: true
  doc: The actual data. |}]
