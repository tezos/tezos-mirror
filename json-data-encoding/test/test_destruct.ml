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

open Json_encoding

let object_encoding =
  obj2
    (opt "a" empty)
    (opt "b" (obj3 (req "x" float) (dft "y" string "y") (opt "z" bool)))

let empty_case = `O [("a", `O [("foo", `String ""); ("bar", `Float 0.2)])]

let unexpected_root = `O [("c", `Float 1.)]

let unexpected_field =
  `O [("b", `O [("x", `Float 0.); ("y", `String ""); ("w", `O [])])]

let test_empty_case () =
  try ignore (destruct object_encoding empty_case)
  with Cannot_destruct (p, Unexpected_field f) ->
    assert (p = [`Field "a"; `Field f]) ;
    assert (f = "foo" || f = "bar")

let test_unexpected_root () =
  try ignore (destruct object_encoding unexpected_root)
  with Cannot_destruct (p, Unexpected_field f) ->
    assert (p = [`Field "c"]) ;
    assert (f = "c")

let test_unexpected_field () =
  try ignore (destruct object_encoding unexpected_field)
  with Cannot_destruct (p, Unexpected_field f) ->
    assert (p = [`Field "b"; `Field "w"]) ;
    assert (f = "w")

let tests =
  [
    ("empty", `Quick, test_empty_case);
    ("unexpected_root", `Quick, test_unexpected_root);
    ("unexpected_field", `Quick, test_unexpected_field);
  ]

let () = Alcotest.run "json-data-encoding" [("exn", tests)]
