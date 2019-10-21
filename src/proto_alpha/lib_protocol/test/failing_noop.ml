(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.ch>                      *)
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

open Protocol
open Alpha_context

let register_one_contract () =
  Context.init 1
  >>=? fun (b, contracts) ->
  let contract = List.nth contracts 0 in
  return (b, contract)

let failing_noop_expected_error = function
  | Apply.Failing_noop_error ->
      true
  | _ ->
      false

let failing_noop arbitrary ctxt source =
  Op.failing_noop (I ctxt) source arbitrary

let must_fail_with operation expected_error =
  register_one_contract ()
  >>=? fun (b, contract) ->
  Incremental.begin_construction b
  >>=? fun i ->
  match Contract.is_implicit contract with
  | None ->
      Alcotest.fail "only implicit accounts can sign"
  | Some mgr ->
      operation i mgr
      >>=? fun operation ->
      Block.bake ~operation b
      >>= fun res -> Assert.proto_error ~loc:__LOC__ res expected_error

let failing_noop_test arbitrary =
  must_fail_with (failing_noop arbitrary) failing_noop_expected_error

let failing_noop_test_one_word () = failing_noop_test "tezos"

let tests =
  [Test.tztest "Failing_noop arbitrary fails" `Quick failing_noop_test_one_word]
