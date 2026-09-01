(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

(* Testing
   -------
   Component: Increase paid storage
   Invocation: dune exec tezt/tests/main.exe -- --file increase_paid_storage.ml
   Subject: Tests for increasing the paid storage of a smart contract
*)

let team = Tag.layer1

let test_increase_paid_storage =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"increase paid storage"
    ~tags:[team; "storage"; "paid_storage"; "increase_paid_storage"]
  @@ fun protocol ->
  let* _, client = Client.init_with_protocol ~protocol `Client () in
  let* _alias, contract =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~init:"Some \"initial storage\""
      ~burn_cap:Tez.(of_int 3)
      client
      ["mini_scenarios"; "str_id"]
      protocol
  in
  let* () = Client.bake_for_and_wait client in
  let payer = Constant.bootstrap2.alias in
  let* result =
    Client.increase_paid_storage ~contract ~amount:(Z.of_int 1000) ~payer client
  in
  Regression.capture result ;
  unit

let test_increase_paid_storage_z =
  Protocol.register_test
    ~__FILE__
    ~title:"Test increase paid storage amount overflow is rejected"
    ~tags:[team; "client"; "increase_paid_storage"]
  @@ fun protocol ->
  let* _node, client = Client.init_with_protocol ~protocol `Client () in
  let* _alias, contract =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:(Account.Bootstrap.alias 1)
      ~init:"Unit"
      ~burn_cap:(Tez.of_int 1)
      client
      ["opcodes"; "noop"]
      protocol
  in
  let* () = Client.bake_for_and_wait client in
  let amount = Z.(succ (of_int64 Int64.max_int)) in
  let err =
    rexf
      "Increase_paid_storage operation error: amount %s does not fit an int64"
      (Z.to_string amount)
  in
  Client.spawn_increase_paid_storage
    ~amount
    ~payer:(Account.Bootstrap.alias 2)
    ~contract
    client
  |> Process.check_error ~msg:err

let register ~protocols =
  test_increase_paid_storage protocols ;
  test_increase_paid_storage_z
    (List.filter (fun p -> p = Protocol.Alpha || p = Protocol.U025) protocols)
