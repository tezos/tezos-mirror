(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(** Testing
    -------
    Component:    Prevalidation
    Invocation:   dune build @src/lib_shell/runtest_prevalidation
    Subject:      Check that prevalidation behaves correctly
*)

let test_safe_decode () =
  let exception Custom_exception of string in
  let broken_encoding =
    Data_encoding.conv
      Fun.id
      (fun _ -> raise (Custom_exception "Should not leave the function scope"))
      Data_encoding.unit
  in
  let actual =
    Prevalidation.Internal_for_tests.safe_binary_of_bytes
      broken_encoding
      Bytes.empty
  in
  Alcotest.(
    check
      bool
      "A broken encoding should return None"
      (actual = error Validation_errors.Parse_error)
      true)

let () =
  Alcotest.run
    "Prevalidation"
    [
      ( "Corner cases",
        [
          Alcotest.test_case
            "Raising an exception in encoding doesn't break"
            `Quick
            test_safe_decode;
        ] );
    ]
