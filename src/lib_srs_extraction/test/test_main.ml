(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(* Testing
   -------
   Component:    Lib_srs_extraction
   Invocation:   dune exec src/lib_srs_extraction/test/main.exe \
                  -- --file test_main.ml
   Subject:      Test srs extraction
*)

open Libsrs

let path = project_root // Filename.dirname __FILE__

let test_lagrange () =
  let input_file = path // "phase1radix2m5" in
  let g1_elements = Powers_of_tau.to_g1s input_file in
  let g2_elements = Powers_of_tau.to_g2s input_file in
  Checks.equality g1_elements g2_elements

let tests = [("lagrange zcash_srs", test_lagrange)]

let () =
  let wrap = List.map (fun (name, f) -> Alcotest.test_case name `Quick f) in
  Alcotest.run ~__FILE__ "SRS" [("Srs", wrap tests)]
