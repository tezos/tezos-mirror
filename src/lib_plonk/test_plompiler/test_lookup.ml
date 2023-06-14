(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Plompiler
open Plonk_test
module CS = Plonk.Circuit
open Helpers

module Bool : Test =
functor
  (L : LIB)
  ->
  struct
    open L
    open L.Bool

    open Utils (L)

    let test_bor x y z () =
      (* A dummy input with the value of zero needs to be added if the number of
         rows in the table is smaller than the number of wires architecture *)
      let* _ = input ~kind:`Public (Input.bool false) in
      let* x = input ~kind:`Public x in
      let* y = input y in
      let* z = input z in
      let* z' = bor_lookup x y in
      assert_equal z z'

    let tests_bor =
      List.map
        (test ~valid:true ~name:"Bool.test_bor_lookup")
        Input.
          [
            test_bor (bool false) (bool false) (bool false);
            test_bor (bool false) (bool true) (bool true);
            test_bor (bool true) (bool false) (bool true);
            test_bor (bool true) (bool true) (bool true);
          ]

    let bor_lookup a b =
      let* l = map2M Bool.bor_lookup (of_list a) (of_list b) in
      ret @@ to_list l

    let test_bor_bytes a b z () =
      let* a = input ~kind:`Public a in
      let* b = input b in
      let* z = input z in
      let* z' = bor_lookup a b in
      assert_equal z z'

    let tests_bor_bytes =
      List.map
        (fun (valid, a, b, o) ->
          let a = Bytes.(input_bytes @@ Stdlib.Bytes.of_string a) in
          let b = Bytes.(input_bytes @@ Stdlib.Bytes.of_string b) in
          let o = Bytes.(input_bytes @@ Stdlib.Bytes.of_string o) in
          test ~valid ~name:"Bool.test_bor_bytes" @@ test_bor_bytes a b o)
        [(true, "\0011", "\0101", "\0111"); (false, "\0000", "\0000", "\0001")]

    let tests = tests_bor @ tests_bor_bytes
  end

let tests =
  let both = [("Bool", (module Bool : Test))] in
  List.map (fun (name, m) -> Alcotest.test_case name `Quick (to_test m)) both
  @ List.map
      (fun (name, m) ->
        Alcotest.test_case
          (name ^ " plonk")
          `Slow
          (to_test ~plonk:(module Plonk.Main_protocol) m))
      both
