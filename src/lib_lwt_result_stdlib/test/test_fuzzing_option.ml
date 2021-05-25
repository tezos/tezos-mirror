(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Support.Lib
open Test_fuzzing_helpers

(* First series of tests: testing equivalence with size-1 lists *)
module TestIter = struct
  open QCheck
  open Monad

  let iter =
    Test.make
      ~name:"{Option,List([01])}.iter"
      (triple Test_fuzzing_helpers.Fn.arith one maybe)
      (fun (Fun (_, fn), init, input) ->
        eq
          (let acc = ref init in
           Option.iter (IterOf.fn acc fn) input ;
           !acc)
          (let acc = ref init in
           List.iter (IterOf.fn acc fn) (Option.to_list input) ;
           !acc))

  let iter_e =
    Test.make
      ~name:"{Option,List([01])}.iter_e"
      (triple Test_fuzzing_helpers.Fn.arith one maybe)
      (fun (Fun (_, fn), init, input) ->
        eq_e
          (let acc = ref init in
           Option.iter_e (IterEOf.fn acc fn) input >|? fun () -> !acc)
          (let acc = ref init in
           List.iter_e (IterEOf.fn acc fn) (Option.to_list input)
           >|? fun () -> !acc))

  let iter_s =
    Test.make
      ~name:"{Option,List([01])}.iter_s"
      (triple Test_fuzzing_helpers.Fn.arith one maybe)
      (fun (Fun (_, fn), init, input) ->
        eq_s
          (let acc = ref init in
           Option.iter_s (IterSOf.fn acc fn) input >|= fun () -> !acc)
          (let acc = ref init in
           List.iter_s (IterSOf.fn acc fn) (Option.to_list input)
           >|= fun () -> !acc))

  let iter_es =
    Test.make
      ~name:"{Option,List([01])}.iter_es"
      (triple Test_fuzzing_helpers.Fn.arith one maybe)
      (fun (Fun (_, fn), init, input) ->
        eq_es
          (let acc = ref init in
           Option.iter_es (IterESOf.fn acc fn) input >|=? fun () -> !acc)
          (let acc = ref init in
           List.iter_es (IterESOf.fn acc fn) (Option.to_list input)
           >|=? fun () -> !acc))

  let tests = [iter; iter_e; iter_s; iter_es]
end

let () =
  let tests =
    [("iter*", Lib_test.Qcheck_helpers.qcheck_wrap TestIter.tests)]
  in
  Alcotest.run "Option" tests
