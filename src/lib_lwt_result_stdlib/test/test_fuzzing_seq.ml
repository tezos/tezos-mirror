(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Test_fuzzing_tests
open Lib_test.Qcheck2_helpers

module SeqWithBase = struct
  type 'a elt = 'a

  include Support.Lib.Seq

  let of_list = List.to_seq

  let to_list = List.of_seq

  let name = "Seq"

  let pp fmt s = Format.pp_print_list Format.pp_print_int fmt (to_list s)
end

module type F = functor (S : module type of SeqWithBase) -> sig
  val tests : QCheck2.Test.t list
end

let wrap (name, (module Test : F)) =
  let module M = Test (SeqWithBase) in
  (name, qcheck_wrap M.tests)

let () =
  let name = "Test_fuzzing_seq" in
  let tests = [("TestSeqIterfold", (module TestIterFold : F))] in
  let tests = List.map wrap tests in
  Alcotest.run name tests
