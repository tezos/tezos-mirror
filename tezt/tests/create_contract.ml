(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
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
   Component:    Michelson
   Invocation:   dune exec tezt/tests/main.exe -- --file create_contract.ml
   Subject:      Regression tests for contract that creates a contract
*)

let hooks = Tezos_regression.hooks

let test_call_contract_that_creates_contract =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"Create contract"
    ~tags:["client"; "michelson"]
    ~uses_node:false
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* _alias, contract_id =
    Client.originate_contract_at
      ~amount:(Tez.of_int 200)
      ~src:"bootstrap1"
      ~init:"Unit"
      ~burn_cap:Tez.one
      ~hooks
      client
      ["mini_scenarios"; "originate_contract"]
      protocol
  in
  let* () =
    Client.transfer
      ~amount:Tez.one
      ~arg:"Unit"
      ~burn_cap:(Tez.of_int 2)
      ~giver:"bootstrap2"
      ~receiver:contract_id
      ~hooks
      client
  in
  unit

let register ~protocols = test_call_contract_that_creates_contract protocols
