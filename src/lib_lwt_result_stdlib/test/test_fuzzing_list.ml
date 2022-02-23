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
open Lib_test.Qcheck_helpers

module ListWithBase = struct
  type 'a elt = 'a

  include Support.Lib.List

  let of_list = Fun.id

  let to_list = Fun.id

  let name = "List"

  let pp = Format.pp_print_list Format.pp_print_int
end

module type F = functor (S : module type of ListWithBase) -> sig
  val tests : QCheck.Test.t list
end

let wrap (name, (module Test : F)) =
  let module M = Test (ListWithBase) in
  (name, qcheck_wrap M.tests)

let () =
  let name = "Test_fuzzing_list" in
  let tests =
    [
      (* Test internal consistency *)
      ("TestIterFold", (module TestIterFold : F));
      ("TestRevMapRevMap", (module TestRevMapRevMap : F));
      (* Test consistency with Stdlib *)
      ("ExistForall", (module TestExistForallAgainstStdlibList : F));
      ("Filter", (module TestFilterAgainstStdlibList : F));
      ("Filterp", (module TestFilterpAgainstStdlibList : F));
      ("Filtermap", (module TestFiltermapAgainstStdlibList : F));
      ("Filtermapp", (module TestFiltermappAgainstStdlibList : F));
      ("Concatmap", (module TestConcatmapAgainstStdlibList : F));
      ("Concatmapp", (module TestConcatmappAgainstStdlibList : F));
      ("Fold", (module TestFoldAgainstStdlibList : F));
      ("FoldRight", (module TestFoldRightAgainstStdlibList : F));
      ("FoldLeftMap", (module TestFoldLeftMapAgainstStdlibList : F));
      ("Iter", (module TestIterAgainstStdlibList : F));
      ("Iteri", (module TestIteriAgainstStdlibList : F));
      ("Iterp", (module TestIterMonotoneAgainstStdlibList : F));
      ("Map", (module TestMapAgainstStdlibList : F));
      ("Mapp", (module TestMappAgainstStdlibList : F));
      ("Find", (module TestFindStdlibList : F));
      ("Partition", (module TestPartitionStdlibList : F));
      ("Double", (module TestDoubleTraversorsStdlibList : F));
    ]
  in
  let tests = List.map wrap tests in
  Alcotest.run name tests
