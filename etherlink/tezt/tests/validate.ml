(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Rpc.Syntax
open Helpers

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

let test_validate_compressed_sig () =
  register ~title:"Validate compressed signature" ~tags:["signature"; "caller"]
  @@ fun sequencer ->
  let account =
    Eth_account.
      {
        address = "0xA257edC8ad1D8f8f463aC0D947cc381000b3c863";
        private_key =
          "0xb80e5dd2ba9281e482589973600609bb0f10f6a075e6c733e4472d4dd2df238a";
      }
  in
  let* base_fee_per_gas = Rpc.get_gas_price sequencer in
  let base_fee_per_gas = Int32.to_int base_fee_per_gas in
  (* We have noticed that this transaction produces a compressed signature.
     We add this test as a non-regression test to make sure we handle
     compressed signature. *)
  let* raw_tx =
    Cast.craft_tx
      ~source_private_key:account.private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:base_fee_per_gas
      ~gas:100_000
      ~address:account.address
      ~value:Wei.zero
      ()
  in
  let*@ transaction_hash = Rpc.send_raw_transaction ~raw_tx sequencer in
  let*@! transaction_object =
    Rpc.get_transaction_by_hash ~transaction_hash sequencer
  in
  (* 64 because, 31 bytes in hexa (so 62), and 2 extra bytes for "0x". *)
  Check.((String.length transaction_object.s = 64) int)
    ~error_msg:"Signature.S was supposed to be compressed" ;
  Check.(
    (String.lowercase_ascii transaction_object.from
    = String.lowercase_ascii account.address)
      string)
    ~error_msg:"Expected caller to be %R but got %L" ;

  unit

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

let test_validate_nonce () =
  register ~title:"Validate nonce" ~tags:["nonce"] @@ fun sequencer ->
  (* Send one transaction so the nonce is 1. *)
  let source = Eth_account.bootstrap_accounts.(0) in
  let* _ =
    send_transaction_to_sequencer
      (Eth_cli.transaction_send
         ~source_private_key:source.private_key
         ~to_public_key:"0xE7f682c226d7269C7247b878B3F94c7a8d31FEf5"
         ~value:Wei.zero
         ~endpoint:(Evm_node.endpoint sequencer))
      sequencer
  in
  (* Nonce 10 is in the future, that's valid. *)
  let* nonce_10 =
    Cast.craft_tx
      ~source_private_key:source.private_key
      ~chain_id:1337
      ~nonce:10
      ~gas_price:1_000_000_000
      ~gas:30_000
      ~address:"0xd77420f73b4612a7a99dba8c2afd30a1886b0344"
      ~value:Wei.zero
      ()
  in
  let*@ _ok = Rpc.send_raw_transaction ~raw_tx:nonce_10 sequencer in
  (* Nonce 1 is expected nonce, that's valid. *)
  let* nonce_1 =
    Cast.craft_tx
      ~source_private_key:source.private_key
      ~chain_id:1337
      ~nonce:1
      ~gas_price:1_000_000_000
      ~gas:30_000
      ~address:"0xd77420f73b4612a7a99dba8c2afd30a1886b0344"
      ~value:Wei.zero
      ()
  in
  let*@ _ok = Rpc.send_raw_transaction ~raw_tx:nonce_1 sequencer in
  (* Nonce 0 is refused. *)
  let* nonce_0 =
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
  let*@? err = Rpc.send_raw_transaction ~raw_tx:nonce_0 sequencer in
  Check.((err.message = "Nonce too low") string)
    ~error_msg:"the transaction has an invalid nonce, it should fail" ;

  unit

let () =
  test_validate_compressed_sig () ;
  test_validate_recover_caller () ;
  test_validate_chain_id () ;
  test_validate_nonce ()
