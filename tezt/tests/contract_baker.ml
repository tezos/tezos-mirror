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
   Invocation:   dune exec tezt/tests/main.exe -- --file contract_baker.ml
   Subject:      Test a simple contract origination and call
*)

let contract_path protocol kind contract =
  sf
    "file:tests_python/contracts_%s/%s/%s"
    (match protocol with
    | Protocol.Alpha -> "alpha"
    | _ -> sf "%03d" @@ Protocol.number protocol)
    kind
    contract

let test_origination_call =
  Protocol.register_test
    ~__FILE__
    ~title:"contract baker"
    ~tags:["client"; "michelson"]
  @@ fun protocol ->
  let* _node, client = Client.init_with_protocol `Client ~protocol () in
  Log.info "Contract origination" ;
  let _alias, process =
    Client.spawn_originate_contract_at
      ~src:Constant.bootstrap1.alias
      ~alias:"foobar"
      ~amount:(Tez.of_int 1000)
      ~init:"Unit"
      ~burn_cap:Tez.one
      client
      ["opcodes"; "transfer_tokens"]
      protocol
  in
  let* client_output = Process.check_and_read_stdout process in
  let* operation_hash =
    match client_output =~* rex "Operation hash is '?(o\\w{50})'" with
    | None ->
        Test.fail
          "Cannot extract operation hash from client_output: %s"
          client_output
    | Some hash -> return hash
  in
  let* contract =
    match client_output =~* rex "New contract ?(KT1\\w{33})" with
    | None ->
        Test.fail
          "Cannot extract contract hash from client_output: %s"
          client_output
    | Some hash -> return hash
  in
  let* () = Client.bake_for_and_wait client in
  let* op_hashes =
    Client.RPC.call client (RPC.get_chain_block_operation_hashes ())
  in
  Check.(
    (List.flatten op_hashes = [operation_hash])
      (list string)
      ~__LOC__
      ~error_msg:"Expected operation hash %R, got %L") ;
  Log.info "Contract call" ;
  let process =
    Client.spawn_call
      ~source:"bootstrap2"
      ~destination:contract
      ~arg:(sf "%S" Constant.bootstrap3.public_key_hash)
      client
  in
  let* client_output = Process.check_and_read_stdout process in
  let* call_operation_hash =
    match client_output =~* rex "Operation hash is '?(o\\w{50})'" with
    | None ->
        Test.fail
          "Cannot extract operation hash from client_output: %s"
          client_output
    | Some hash -> return hash
  in
  let* () = Client.bake_for_and_wait client in
  let* call_op_hashes =
    Client.RPC.call client (RPC.get_chain_block_operation_hashes ())
  in
  Check.(
    list_mem
      ~__LOC__
      (list string)
      [call_operation_hash]
      call_op_hashes
      ~error_msg:"Expected %R to be in the list of operation hashes %L") ;
  Log.info "Contract balance" ;
  let* balance =
    Client.get_balance_for client ~account:Constant.bootstrap3.alias
  in
  let* deposit =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_delegate_frozen_deposits
         Constant.bootstrap3.public_key_hash
  in
  Check.(Tez.(balance + deposit) = Tez.of_int 4000100)
    Tez.typ
    ~__LOC__
    ~error_msg:"Expected balance %R, got %L" ;
  Log.info "Contract query storage" ;
  let* json =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_contract_storage ~id:contract ()
  in
  let storage = JSON.(json |-> "prim" |> as_string) in
  Check.(
    ("Unit" = storage) string ~__LOC__ ~error_msg:"Expected storage %R, got %L") ;
  unit

let register ~protocols = test_origination_call protocols
