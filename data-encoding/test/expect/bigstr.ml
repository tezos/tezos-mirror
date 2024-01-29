(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

open Data_encoding

let pp encoding s =
  let json = Json.construct encoding s in
  let res = Ezjsonm.value_to_string json in
  Format.printf "JSON: %s; BINARY: " res ;
  assert (Json.destruct encoding json = s) ;
  let binary = Stdlib.Result.get_ok @@ Binary.to_string encoding s in
  Seq.iter
    (fun char -> Format.printf "\\x%02x" (Char.code char))
    (String.to_seq binary) ;
  Format.printf "\n"

let%expect_test _ =
  let e = Data_encoding.string' Hex in
  let eb = Data_encoding.bigstring () in
  pp e "" ;
  pp eb (Bigstringaf.of_string ~off:0 ~len:0 "") ;
  [%expect
    {|
    JSON: ""; BINARY: \x00\x00\x00\x00
    JSON: ""; BINARY: \x00\x00\x00\x00 |}] ;
  pp e "\xde\xad" ;
  pp eb (Bigstringaf.of_string ~off:0 ~len:2 "\xde\xad") ;
  [%expect
    {|
    JSON: "dead"; BINARY: \x00\x00\x00\x02\xde\xad
    JSON: "dead"; BINARY: \x00\x00\x00\x02\xde\xad |}] ;
  ()

let%expect_test _ =
  let e = Data_encoding.string' ~length_kind:`Uint8 Hex in
  let eb = Data_encoding.bigstring ~length_kind:`Uint8 () in
  pp e "" ;
  pp eb (Bigstringaf.of_string ~off:0 ~len:0 "") ;
  [%expect {|
    JSON: ""; BINARY: \x00
    JSON: ""; BINARY: \x00 |}] ;
  pp e "\xde\xad" ;
  pp eb (Bigstringaf.of_string ~off:0 ~len:2 "\xde\xad") ;
  [%expect
    {|
    JSON: "dead"; BINARY: \x02\xde\xad
    JSON: "dead"; BINARY: \x02\xde\xad |}] ;
  ()

let%expect_test _ =
  let e = Data_encoding.string' ~length_kind:`N Hex in
  let eb = Data_encoding.bigstring ~length_kind:`N () in
  pp e "" ;
  pp eb (Bigstringaf.of_string ~off:0 ~len:0 "") ;
  [%expect {|
    JSON: ""; BINARY: \x00
    JSON: ""; BINARY: \x00 |}] ;
  pp e "\xde\xad" ;
  pp eb (Bigstringaf.of_string ~off:0 ~len:2 "\xde\xad") ;
  [%expect
    {|
    JSON: "dead"; BINARY: \x02\xde\xad
    JSON: "dead"; BINARY: \x02\xde\xad |}] ;
  ()

let%expect_test _ =
  let e n = Data_encoding.Fixed.string' Hex n in
  let eb n = Data_encoding.Fixed.bigstring n in
  pp (e 2) "\xde\xad" ;
  pp (eb 2) (Bigstringaf.of_string ~off:0 ~len:2 "\xde\xad") ;
  [%expect
    {|
    JSON: "dead"; BINARY: \xde\xad
    JSON: "dead"; BINARY: \xde\xad |}] ;
  ()
