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
  let binary = Stdlib.Result.get_ok @@ Binary.to_string encoding s in
  Seq.iter
    (fun char -> Format.printf "\\x%02x" (Char.code char))
    (String.to_seq binary) ;
  Format.printf "\n"

let%expect_test _ =
  pp uint8 0 ;
  [%expect {| \x00 |}] ;
  pp uint16 300 ;
  [%expect {| \x01\x2c |}] ;
  pp Little_endian.uint16 300 ;
  [%expect {| \x2c\x01 |}] ;
  pp int16 300 ;
  [%expect {| \x01\x2c |}] ;
  pp Little_endian.int16 300 ;
  [%expect {| \x2c\x01 |}] ;
  pp int16 (-300) ;
  [%expect {| \xfe\xd4 |}] ;
  pp Little_endian.int16 (-300) ;
  [%expect {| \xd4\xfe |}] ;
  pp int31 300300 ;
  [%expect {| \x00\x04\x95\x0c |}] ;
  pp Little_endian.int31 300300 ;
  [%expect {| \x0c\x95\x04\x00 |}] ;
  pp int31 (-300300) ;
  [%expect {| \xff\xfb\x6a\xf4 |}] ;
  pp Little_endian.int31 (-300300) ;
  [%expect {| \xf4\x6a\xfb\xff |}] ;
  pp int32 300300l ;
  [%expect {| \x00\x04\x95\x0c |}] ;
  pp Little_endian.int32 300300l ;
  [%expect {| \x0c\x95\x04\x00 |}] ;
  pp int32 (-300300l) ;
  [%expect {| \xff\xfb\x6a\xf4 |}] ;
  pp Little_endian.int32 (-300300l) ;
  [%expect {| \xf4\x6a\xfb\xff |}] ;
  pp int64 300300300300L ;
  [%expect {| \x00\x00\x00\x45\xeb\x4a\xf0\x0c |}] ;
  pp Little_endian.int64 300300300300L ;
  [%expect {| \x0c\xf0\x4a\xeb\x45\x00\x00\x00 |}] ;
  pp int64 (-300300300300L) ;
  [%expect {| \xff\xff\xff\xba\x14\xb5\x0f\xf4 |}] ;
  pp Little_endian.int64 (-300300300300L) ;
  [%expect {| \xf4\x0f\xb5\x14\xba\xff\xff\xff |}] ;
  pp (ranged_int 33 333333) 300 ;
  [%expect {| \x00\x00\x01\x0b |}] ;
  pp Little_endian.(ranged_int 33 333333) 300 ;
  [%expect {| \x0b\x01\x00\x00 |}] ;
  ()
