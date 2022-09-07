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

open Data_encoding

let pp encoding s =
  let json = Json.construct encoding s in
  let res = Ezjsonm.value_to_string json in
  print_endline res ;
  assert (Json.destruct encoding json = s)

let%expect_test _ =
  let s1 = "\x00\x01\x02\x03\xF0\x9F\x90\xAB" in

  let s2 = "\x00\x01\x02\xff\x01" in
  pp string s1 ;
  [%expect {| "\u0000\u0001\u0002\u0003🐫" |}] ;
  pp string s2 ;
  [%expect {| {"invalid_utf8_string":[0,1,2,255,1]} |}] ;
  pp (string' Plain) s1 ;
  [%expect {| "\u0000\u0001\u0002\u0003🐫" |}] ;
  pp (string' Plain) s2 ;
  [%expect {| {"invalid_utf8_string":[0,1,2,255,1]} |}] ;
  pp (string' Hex) s1 ;
  [%expect {| "00010203f09f90ab" |}] ;
  pp (string' Hex) s2 ;
  [%expect {| "000102ff01" |}]

let%expect_test _ =
  let s1 = Bytes.of_string "\x00\x01\x02\x03\xF0\x9F\x90\xAB" in

  let s2 = Bytes.of_string "\x00\x01\x02\xff\x01" in

  pp bytes s1 ;
  [%expect {| "00010203f09f90ab" |}] ;
  pp bytes s2 ;
  [%expect {| "000102ff01" |}] ;
  pp (bytes' Plain) s1 ;
  [%expect {| "\u0000\u0001\u0002\u0003🐫" |}] ;
  pp (bytes' Plain) s2 ;
  [%expect {| {"invalid_utf8_string":[0,1,2,255,1]} |}] ;
  pp (bytes' Hex) s1 ;
  [%expect {| "00010203f09f90ab" |}] ;
  pp (bytes' Hex) s2 ;
  [%expect {| "000102ff01" |}]
