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

let list_encoding = list (obj2 (req "foo" float) (req "bar" float))

let list_test =
  `A
    [
      `O [("foo", `Float 1.0); ("bar", `Float 2.0)];
      `O [("foo", `Float 0.0); ("bar", `Float 0.0); ("baz", `Float 1.2)];
    ]

let test_empty_case () =
  (match destruct object_encoding empty_case with
  | exception Cannot_destruct (p, Unexpected_field f) ->
      assert (p = [`Field "a"; `Field f]) ;
      assert (f = "foo" || f = "bar")
  | _ -> assert false) ;
  let r = destruct ~ignore_extra_fields:true object_encoding empty_case in
  assert (r = (Some (), None))

let test_unexpected_root () =
  (match destruct object_encoding unexpected_root with
  | exception Cannot_destruct (p, Unexpected_field f) ->
      assert (p = [`Field "c"]) ;
      assert (f = "c")
  | _ -> assert false) ;
  let r = destruct ~ignore_extra_fields:true object_encoding unexpected_root in
  assert (r = (None, None))

let test_unexpected_field () =
  (match destruct object_encoding unexpected_field with
  | exception Cannot_destruct (p, Unexpected_field f) ->
      assert (p = [`Field "b"; `Field "w"]) ;
      assert (f = "w")
  | _ -> assert false) ;
  let r = destruct ~ignore_extra_fields:true object_encoding unexpected_field in
  assert (r = (None, Some (0., "", None)))

let test_list () =
  (match destruct list_encoding list_test with
  | exception Cannot_destruct (p, Unexpected_field f) ->
      assert (p = [`Index 1; `Field "baz"]) ;
      assert (f = "baz")
  | _ -> assert false) ;
  let r = destruct ~ignore_extra_fields:true list_encoding list_test in
  assert (r = [(1.0, 2.0); (0.0, 0.0)])

let tests =
  [
    ("empty", `Quick, test_empty_case);
    ("unexpected_root", `Quick, test_unexpected_root);
    ("unexpected_field", `Quick, test_unexpected_field);
    ("list", `Quick, test_list);
  ]

let () = Alcotest.run "json-data-encoding" [("destruct", tests)]
