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

let test_gas_storage_limits =
  Protocol.register_test
    ~__FILE__
    ~title:"protocol limits"
    ~tags:["mockup"; "protocol"; "limits"]
    ~uses_node:false
  @@ fun protocol ->
  let parameters = JSON.parse_file (Protocol.parameter_file protocol) in
  let gas_limit =
    JSON.(parameters |-> "hard_gas_limit_per_operation" |> as_int)
  in
  let storage_limit =
    JSON.(parameters |-> "hard_storage_limit_per_operation" |> as_int)
  in
  let* client = Client.init_mockup ~protocol () in
  let giver = Constant.bootstrap1.alias in
  let receiver = Constant.bootstrap2.alias in
  let* initial_giver_balance = Client.get_balance_for ~account:giver client in
  let* initial_receiver_balance =
    Client.get_balance_for ~account:receiver client
  in
  let fee = Tez.one in
  let amount = Tez.of_int 3 in
  let* () =
    Client.transfer
      ~amount
      ~giver
      ~receiver
      ~fee
      ~gas_limit
      ~storage_limit
      client
  in
  let* giver_balance = Client.get_balance_for ~account:giver client in
  let* receiver_balance = Client.get_balance_for ~account:receiver client in
  let expected_receiver_balance = Tez.(initial_receiver_balance + amount) in
  let expected_giver_balance = Tez.(initial_giver_balance - (fee + amount)) in
  let* () =
    if receiver_balance <> expected_receiver_balance then
      Test.fail
        "test_gas_storage_limits: unexpected balance for receiver (got %s, \
         expected %s)"
        (Tez.to_string receiver_balance)
        (Tez.to_string expected_receiver_balance)
    else return ()
  in
  if giver_balance <> expected_giver_balance then
    Test.fail
      "test_gas_storage_limits: unexpected balance for giver (got %s, expected \
       %s)"
      (Tez.to_string giver_balance)
      (Tez.to_string expected_giver_balance)
  else return ()

let register ~protocols = test_gas_storage_limits protocols
