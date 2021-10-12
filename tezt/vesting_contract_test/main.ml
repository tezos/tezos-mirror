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

(* Testing
   -------
   Component: Vesting contract
   Invocation: dune exec tezt/vesting_contract_test/main.exe
   Subject: This file runs a test suite for the vesting contract. It supports
            the original version of the contract, executed under Athens
            protocol which is the last protocol, where this version can be
            originated. It also can execute against the patched version under
            protocol Alpha.
*)

let tests =
  let open Vesting_test in
  [
    (transfer_and_pour_happy_path, "transfer_and_pour_happy_path", 6);
    (test_delegation, "test_delegation", 5);
    (test_invalid_transfers, "test_invalid_transfers", 6);
    (test_update_keys, "test_update_keys", 8);
    (test_all_sigs_required, "test_all_sigs_required", 7);
    (test_full_contract, "test_full_contract", 28);
  ]

let () =
  (* Register your tests here. *)
  Cli.init () ;
  List.iter
    (fun (test, title, user_count) ->
      Test.register
        ~__FILE__
        ~title
        ~tags:["vesting"; title]
        (Vesting_test.execute ~contract:"contract.tz" ~user_count test))
    tests ;
  (* [Test.run] must be the last function to be called. *)
  Test.run ()
