(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Rpc.Syntax

let register f =
  Test.register
    ~__FILE__
    ~uses_admin_client:false
    ~uses_client:false
    ~uses_node:false
    ~uses:
      [
        Constant.octez_evm_node;
        Constant.WASM.evm_kernel;
        Constant.smart_rollup_installer;
      ]
  @@ fun () ->
  let patch_config =
    Evm_node.patch_config_with_experimental_feature
      ~node_transaction_validation:true
      ()
  in
  let* sequencer = Helpers.init_sequencer_sandbox ~patch_config () in
  f sequencer

let test_validate_recover_caller () =
  register ~title:"Validate recovers caller" ~tags:["caller"]
  @@ fun sequencer ->
  let source = Eth_account.bootstrap_accounts.(0) in
  let* raw_tx =
    Cast.craft_tx
      ~source_private_key:source.private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:30_000
      ~address:"0xd77420f73b4612a7a99dba8c2afd30a1886b0344"
      ~value:Wei.zero
      ()
  in
  let*@ transaction_hash = Rpc.send_raw_transaction ~raw_tx sequencer in
  let*@! transaction_object =
    Rpc.get_transaction_by_hash ~transaction_hash sequencer
  in
  Check.(
    (String.lowercase_ascii transaction_object.from
    = String.lowercase_ascii source.address)
      string)
    ~error_msg:"Expected caller to be %R but got %L" ;
  unit

let test_validate_chain_id () =
  register ~title:"Validate chain id" ~tags:["chain_id"] @@ fun sequencer ->
  let source = Eth_account.bootstrap_accounts.(0) in
  let* invalid_chain_id =
    Cast.craft_tx
      ~source_private_key:source.private_key
      ~chain_id:1000
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:30_000
      ~address:"0xd77420f73b4612a7a99dba8c2afd30a1886b0344"
      ~value:Wei.zero
      ()
  in
  let*@? err = Rpc.send_raw_transaction ~raw_tx:invalid_chain_id sequencer in
  Check.((err.message = "Invalid chain id") string)
    ~error_msg:"the transaction has an invalid chain id, it should fail" ;

  let* valid_chain_id =
    Cast.craft_tx
      ~source_private_key:source.private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:30_000
      ~address:"0xd77420f73b4612a7a99dba8c2afd30a1886b0344"
      ~value:Wei.zero
      ()
  in
  let*@ _ok = Rpc.send_raw_transaction ~raw_tx:valid_chain_id sequencer in

  unit

let () =
  test_validate_recover_caller () ;
  test_validate_chain_id ()
