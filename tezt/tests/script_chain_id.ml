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
   Component:    Michelson / Opcodes
   Invocation:   dune exec tezt/tests/main.exe -- --file script_chain_id.ml
   Subject:      Tests of the [CHAIN_ID] Michelson instruction.
*)

let test_chain_id_opcode =
  Protocol.register_test
    ~__FILE__
    ~title:"Chain ID Opcode"
    ~tags:["client"; "contract"]
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* chain_id_alias, _contract =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:Constant.bootstrap2.alias
      ~burn_cap:Tez.one
      ~init:"Unit"
      client
      ["opcodes"; "chain_id"]
      protocol
  in
  Client.transfer
    client
    ~amount:Tez.zero
    ~giver:Constant.bootstrap2.alias
    ~receiver:chain_id_alias

let test_chain_id_authentication =
  Protocol.register_test
    ~__FILE__
    ~title:"Chain ID Authentication"
    ~tags:["client"; "contract"]
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  Log.info "Originate contract" ;
  let pubkey = Account.Bootstrap.keys.(0).public_key in
  let* authentication_alias, contract_address =
    Client.originate_contract_at
      ~amount:(Tez.of_int 1000)
      ~src:Constant.bootstrap2.alias
      ~burn_cap:Tez.one
      ~init:(sf {|Pair 0 "%s"|} pubkey)
      client
      ["mini_scenarios"; "authentication"]
      protocol
  in
  Log.info "First run" ;
  let destination = Account.Bootstrap.keys.(1).public_key_hash in
  let operation =
    sf
      {|{DROP; NIL operation; PUSH address "%s"; CONTRACT unit; ASSERT_SOME; PUSH mutez 1000; UNIT; TRANSFER_TOKENS; CONS}|}
      destination
  in
  let* chain_id = Client.RPC.call client @@ RPC.get_chain_chain_id () in
  let* packed =
    let data =
      sf
        {|Pair (Pair "%s" "%s") (Pair %s 0)|}
        chain_id
        contract_address
        operation
    in
    let typ =
      {|pair (pair chain_id address) (pair (lambda unit (list operation)) nat)|}
    in
    let* hashes = Client.hash_data client ~data ~typ in
    return hashes.packed
  in
  let* signature =
    Client.sign_bytes client ~signer:Constant.bootstrap1.alias ~data:packed
  in
  Client.transfer
    client
    ~amount:Tez.zero
    ~giver:Constant.bootstrap2.alias
    ~receiver:authentication_alias
    ~arg:(sf {|Pair %s "%s"|} operation signature)

let register ~protocols =
  test_chain_id_opcode protocols ;
  test_chain_id_authentication protocols
