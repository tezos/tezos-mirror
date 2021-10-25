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

open Tezos_expect_helper

let%expect_test "require_does_not_raise_err" =
  require_does_not_raise ~loc:__LOC__ (fun () -> assert false) ;
  [%expect
    {|
    File "src/lib_expect_helper/test/test.ml", line 29, characters 30-37
    FIXME: require_does_not_raise: File "src/lib_expect_helper/test/test.ml", line 29, characters 49-55: Assertion failed |}]

let%expect_test "require_does_not_raise_ok" =
  require_does_not_raise ~loc:__LOC__ (fun () -> print_endline "Not raising") ;
  [%expect {| Not raising |}]

let%expect_test "require_does_raise_ok" =
  require_does_raise ~loc:__LOC__ (fun () -> assert false) ;
  [%expect {||}]

let%expect_test "require_does_raise_err" =
  require_does_raise ~loc:__LOC__ (fun () -> print_endline "Not raising") ;
  [%expect
    {|
    Not raising
    File "src/lib_expect_helper/test/test.ml", line 44, characters 26-33
    FIXME: require_does_raise |}]
