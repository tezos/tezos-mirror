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
    seq:
    - id: float_range
      type: f8
      valid:
        min: -22299.01
        max: 3333333.33333
  |}]
