(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

let documentation_mentions_correct_tag_bit_counts () =
  let open Data_encoding.Compact in
  let int = payload Data_encoding.int31 in
  assert (tag_bit_count (payload Data_encoding.unit) = 0) ;
  assert (tag_bit_count (payload Data_encoding.int32) = 0) ;
  assert (tag_bit_count int = 0) ;
  assert (tag_bit_count void = 0) ;
  assert (tag_bit_count (option (payload Data_encoding.unit)) = 1) ;
  assert (tag_bit_count (option int) = 1) ;
  assert (tag_bit_count (tup1 int) = 0) ;
  assert (tag_bit_count (tup1 (option int)) = 1) ;
  assert (tag_bit_count (tup2 (option int) int) = 1) ;
  assert (tag_bit_count (tup2 (option int) (option int)) = 2) ;
  assert (tag_bit_count (obj1 (req "one" int)) = 0) ;
  assert (tag_bit_count (obj1 (opt "one" int)) = 1) ;
  assert (tag_bit_count (obj2 (opt "one" int) (req "two" int)) = 1) ;
  assert (tag_bit_count (obj2 (opt "one" int) (opt "two" int)) = 2) ;
  assert (tag_bit_count int32 = 2) ;
  assert (tag_bit_count int64 = 2) ;
  assert (
    tag_bit_count
      (or_int32 ~int32_kind:"i32" ~alt_kind:"alt" Data_encoding.unit)
    = 2) ;
  assert (tag_bit_count (list ~bits:0 Data_encoding.int31) = 0) ;
  assert (tag_bit_count (list ~bits:1 Data_encoding.int31) = 1) ;
  assert (tag_bit_count (list ~bits:2 Data_encoding.int31) = 2) ;
  assert (tag_bit_count (list ~bits:3 Data_encoding.int31) = 3) ;
  assert (tag_bit_count (list ~bits:4 Data_encoding.int31) = 4) ;
  assert (
    tag_bit_count
      (union
         ~union_tag_bits:0
         ~cases_tag_bits:0
         [case "unit" Option.some Fun.id unit])
    = 0) ;
  assert (
    tag_bit_count
      (union
         ~union_tag_bits:1
         ~cases_tag_bits:0
         [case "unit" Option.some Fun.id unit])
    = 1) ;
  assert (
    tag_bit_count
      (union
         ~union_tag_bits:0
         ~cases_tag_bits:1
         [case "unit" Option.some Fun.id (option unit)])
    = 1) ;
  assert (
    tag_bit_count
      (union
         ~union_tag_bits:1
         ~cases_tag_bits:1
         [case "unit" Option.some Fun.id unit])
    = 2) ;
  assert (
    tag_bit_count
      (union
         ~union_tag_bits:3
         ~cases_tag_bits:2
         [case "unit" Option.some Fun.id unit])
    = 5) ;
  assert (
    tag_bit_count
      (union
         ~union_tag_bits:7
         ~cases_tag_bits:6
         [case "unit" Option.some Fun.id unit])
    = 13) ;
  ()

let tests =
  [
    ( "tag_bit_count documentation",
      `Quick,
      documentation_mentions_correct_tag_bit_counts );
  ]
