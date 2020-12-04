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

(* Testing
   -------
   Component: Client - mockup mode
   Invocation: dune exec tezt/tests/main.exe -- --file mockup.ml
   Subject: Unexhaustive tests of the client's --mode mockup. Unexhaustive,
            because most tests of the mockup are written with the python
            framework for now. It was important, though, to provide the
            mockup's API in tezt; for other tests that use the mockup.
  *)

(* Test.
   Call `tezos-client rpc list` and check that return code is 0.
 *)
let test_rpc_list protocol =
  Test.register
    ~__FILE__
    ~title:(sf "%s: rpc list (mockup)" (Protocol.name protocol))
    ~tags:[Protocol.tag protocol; "mockup"; "client"; "rpc"]
  @@ fun () ->
  let* client = Client.init_mockup ~protocol () in
  let* _ = Client.rpc_list client in
  Lwt.return_unit

let transfer_data = (Constant.bootstrap1.alias, 1, Constant.bootstrap2.alias)

let test_balances_after_transfer giver amount receiver =
  let (giver_balance_before, giver_balance_after) = giver in
  let (receiver_balance_before, receiver_balance_after) = receiver in
  if not (giver_balance_after < giver_balance_before -. amount) then
    Test.fail
      "Invalid balance of giver after transfer: %f (before it was %f)"
      giver_balance_after
      giver_balance_before ;
  Log.info "Balance of giver after transfer is valid: %f" giver_balance_after ;
  let receiver_expected_after = receiver_balance_before +. amount in
  if receiver_balance_after <> receiver_expected_after then
    Test.fail
      "Invalid balance of receiver after transfer: %f (expected %f)"
      receiver_balance_after
      receiver_expected_after ;
  Log.info
    "Balance of receiver after transfer is valid: %f"
    receiver_balance_after

(* Test.
   Transfer some tz and check balance changes are as expected.
 *)
let test_transfer protocol =
  Test.register
    ~__FILE__
    ~title:(sf "%s: mockup transfer" (Protocol.name protocol))
    ~tags:[Protocol.tag protocol; "mockup"; "client"; "transfer"]
  @@ fun () ->
  let (giver, amount, receiver) = transfer_data in
  let* client = Client.init_mockup ~protocol () in
  let* giver_balance_before = Client.get_balance_for ~account:giver client in
  let* receiver_balance_before =
    Client.get_balance_for ~account:receiver client
  in
  Log.info "About to transfer %d from %s to %s" amount giver receiver ;
  let* () = Client.transfer ~amount ~giver ~receiver client in
  let* giver_balance_after = Client.get_balance_for ~account:giver client in
  let* receiver_balance_after =
    Client.get_balance_for ~account:receiver client
  in
  test_balances_after_transfer
    (giver_balance_before, giver_balance_after)
    (float_of_int amount)
    (receiver_balance_before, receiver_balance_after) ;
  return ()

let register protocol = test_rpc_list protocol ; test_transfer protocol
