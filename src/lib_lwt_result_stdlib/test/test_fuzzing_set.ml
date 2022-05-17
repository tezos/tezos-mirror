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

module IntSet : Support.Lib.Set.S with type elt = int = struct
  include Support.Lib.Set.Make (Int)
end

module SetWithBase = struct
  let name = "Set"

  type 'a elt = IntSet.elt

  type _alias_elt = IntSet.elt

  type 'a t = IntSet.t

  type _alias_t = IntSet.t

  module IntSet :
    Support.Lib.Set.S with type elt := _alias_elt and type t := _alias_t =
  struct
    include IntSet
  end

  include IntSet

  let of_list : int list -> _alias_t = of_list

  let to_list : _alias_t -> int list = elements

  let pp fmt s = Format.pp_print_list Format.pp_print_int fmt (to_list s)
end

module type F = functor (S : module type of SetWithBase) -> sig
  val tests : QCheck2.Test.t list
end

let wrap (name, (module Test : F)) =
  let module M = Test (SetWithBase) in
  (name, qcheck_wrap M.tests)

let () =
  let name = "Test_fuzzing_set" in
  let tests =
    [
      ("Iterp", (module TestIterMonotoneAgainstStdlibList : F));
      ("Fold", (module TestFoldMonotonicAgainstStdlibList : F));
    ]
  in
  let tests = List.map wrap tests in
  Alcotest.run name tests
