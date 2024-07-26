(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Rpc.Syntax
open Helpers

type transaction_type = Legacy | Eip1559

let tag = function Legacy -> "Legacy" | Eip1559 -> "Eip1559"

let register ~title ~tags f tx_types =
  List.iter
    (fun tx_type ->
      let tag_tx = tag tx_type in
      let title = Format.sprintf "%s: %s" tag_tx title in
      Test.register
        ~__FILE__
        ~title
        ~tags:(String.lowercase_ascii tag_tx :: tags)
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
      f sequencer tx_type)
    tx_types

let test_validate_compressed_sig =
  register ~title:"Validate compressed signature" ~tags:["signature"; "caller"]
  @@ fun sequencer _tx_type ->
  let account =
    Eth_account.
      {
        address = "0xA257edC8ad1D8f8f463aC0D947cc381000b3c863";
        private_key =
          "0xb80e5dd2ba9281e482589973600609bb0f10f6a075e6c733e4472d4dd2df238a";
      }
  in
  (* Gives funds to the new account. *)
  let* _ =
    send_transaction_to_sequencer
      (Eth_cli.transaction_send
         ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
         ~to_public_key:account.address
         ~value:(Wei.of_eth_int 10)
         ~endpoint:(Evm_node.endpoint sequencer))
      sequencer
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

let test_validate_recover_caller =
  register ~title:"Validate recovers caller" ~tags:["caller"]
  @@ fun sequencer tx_type ->
  let source = Eth_account.bootstrap_accounts.(0) in
  let make_raw_tx ~legacy =
    Cast.craft_tx
      ~source_private_key:source.private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~legacy
      ~gas:30_000
      ~address:"0xd77420f73b4612a7a99dba8c2afd30a1886b0344"
      ~value:Wei.zero
      ()
  in
  let* raw_tx =
    match tx_type with
    | Legacy -> make_raw_tx ~legacy:true
    | Eip1559 -> make_raw_tx ~legacy:false
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

let test_validate_chain_id =
  register ~title:"Validate chain id" ~tags:["chain_id"]
  @@ fun sequencer tx_type ->
  let source = Eth_account.bootstrap_accounts.(0) in
  let make_tx_chain_id ~chain_id ~legacy =
    Cast.craft_tx
      ~legacy
      ~source_private_key:source.private_key
      ~chain_id
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:30_000
      ~address:"0xd77420f73b4612a7a99dba8c2afd30a1886b0344"
      ~value:Wei.zero
      ()
  in
  let* invalid_chain_id =
    match tx_type with
    | Legacy -> make_tx_chain_id ~chain_id:1000 ~legacy:true
    | Eip1559 -> make_tx_chain_id ~chain_id:1000 ~legacy:false
  in
  let*@? err = Rpc.send_raw_transaction ~raw_tx:invalid_chain_id sequencer in
  Check.((err.message = "Invalid chain id") string)
    ~error_msg:"the transaction has an invalid chain id, it should fail" ;

  let* valid_chain_id =
    match tx_type with
    | Legacy -> make_tx_chain_id ~chain_id:1337 ~legacy:true
    | Eip1559 -> make_tx_chain_id ~chain_id:1337 ~legacy:false
  in
  let*@ _ok = Rpc.send_raw_transaction ~raw_tx:valid_chain_id sequencer in

  unit

let test_validate_nonce =
  register ~title:"Validate nonce" ~tags:["nonce"] @@ fun sequencer tx_type ->
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
  let make_tx_nonce ~nonce ~legacy =
    Cast.craft_tx
      ~legacy
      ~source_private_key:source.private_key
      ~chain_id:1337
      ~nonce
      ~gas_price:1_000_000_000
      ~gas:30_000
      ~address:"0xd77420f73b4612a7a99dba8c2afd30a1886b0344"
      ~value:Wei.zero
      ()
  in
  (* Nonce 10 is in the future, that's valid. *)
  let* nonce_10 =
    match tx_type with
    | Legacy -> make_tx_nonce ~nonce:10 ~legacy:true
    | Eip1559 -> make_tx_nonce ~nonce:10 ~legacy:false
  in
  let*@ _ok = Rpc.send_raw_transaction ~raw_tx:nonce_10 sequencer in
  (* Nonce 1 is expected nonce, that's valid. *)
  let* nonce_1 =
    match tx_type with
    | Legacy -> make_tx_nonce ~nonce:1 ~legacy:true
    | Eip1559 -> make_tx_nonce ~nonce:1 ~legacy:false
  in
  let*@ _ok = Rpc.send_raw_transaction ~raw_tx:nonce_1 sequencer in
  (* Nonce 0 is refused. *)
  let* nonce_0 =
    match tx_type with
    | Legacy -> make_tx_nonce ~nonce:0 ~legacy:true
    | Eip1559 -> make_tx_nonce ~nonce:0 ~legacy:false
  in
  let*@? err = Rpc.send_raw_transaction ~raw_tx:nonce_0 sequencer in
  Check.((err.message = "Nonce too low") string)
    ~error_msg:"the transaction has an invalid nonce, it should fail" ;

  unit

let test_validate_max_fee_per_gas =
  register ~title:"Validate max fee per gas" ~tags:["max_fee_per_gas"]
  @@ fun sequencer tx_type ->
  let source = Eth_account.bootstrap_accounts.(0) in

  let* base_fee_per_gas = Rpc.get_gas_price sequencer in
  let base_fee_per_gas = Int32.to_int base_fee_per_gas in

  let make_tx_gas_price ~gas_price ~legacy =
    Cast.craft_tx
      ~legacy
      ~source_private_key:source.private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price
      ~gas:30_000
      ~address:"0xd77420f73b4612a7a99dba8c2afd30a1886b0344"
      ~value:Wei.zero
      ()
  in
  let* gas_price_below =
    match tx_type with
    | Legacy -> make_tx_gas_price ~gas_price:(base_fee_per_gas - 1) ~legacy:true
    | Eip1559 ->
        make_tx_gas_price ~gas_price:(base_fee_per_gas - 1) ~legacy:false
  in
  let*@? err = Rpc.send_raw_transaction ~raw_tx:gas_price_below sequencer in
  Check.((err.message = "Max gas fee too low") string)
    ~error_msg:"the transaction has gas price too low, it should fail" ;

  let* gas_price_enough =
    match tx_type with
    | Legacy -> make_tx_gas_price ~gas_price:base_fee_per_gas ~legacy:true
    | Eip1559 -> make_tx_gas_price ~gas_price:base_fee_per_gas ~legacy:false
  in
  let*@ _ok = Rpc.send_raw_transaction ~raw_tx:gas_price_enough sequencer in

  unit

let test_validate_pay_for_fees =
  register ~title:"Validate pay for fees" ~tags:["pay_for_fees"]
  @@ fun sequencer tx_type ->
  let empty_account =
    Eth_account.
      {
        address = "0xA257edC8ad1D8f8f463aC0D947cc381000b3c863";
        private_key =
          "0xb80e5dd2ba9281e482589973600609bb0f10f6a075e6c733e4472d4dd2df238a";
      }
  in

  let* base_fee_per_gas = Rpc.get_gas_price sequencer in
  let base_fee_per_gas = Int32.to_int base_fee_per_gas in

  let make_tx_pay_fees ~legacy ~gas_price =
    Cast.craft_tx
      ~source_private_key:empty_account.private_key
      ~chain_id:1337
      ~nonce:0
      ~legacy
      ~gas_price
      ~gas:100_000
      ~address:"0xd77420f73b4612a7a99dba8c2afd30a1886b0344"
      ~value:Wei.zero
      ()
  in
  (* The account has no funds, any transaction will be rejected. *)
  let* insufficient_funds =
    match tx_type with
    | Legacy -> make_tx_pay_fees ~legacy:true ~gas_price:base_fee_per_gas
    | Eip1559 -> make_tx_pay_fees ~legacy:false ~gas_price:base_fee_per_gas
  in
  let*@? err = Rpc.send_raw_transaction ~raw_tx:insufficient_funds sequencer in
  Check.((err.message = "Cannot prepay transaction.") string)
    ~error_msg:"the account has no funds, it should fail" ;

  (* We transfer enough funds to pay for `100_000 (gas unit) * gas price`. *)
  let* _ =
    send_transaction_to_sequencer
      (Eth_cli.transaction_send
         ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
         ~to_public_key:empty_account.address
         ~value:(Wei.of_string (string_of_int (100_000 * base_fee_per_gas)))
         ~endpoint:(Evm_node.endpoint sequencer))
      sequencer
  in

  (* Now the account can pay for at least the base_fee_per_gas. *)
  let* enough_funds =
    match tx_type with
    | Legacy -> make_tx_pay_fees ~legacy:true ~gas_price:base_fee_per_gas
    | Eip1559 -> make_tx_pay_fees ~legacy:false ~gas_price:base_fee_per_gas
  in
  let*@ _ok = Rpc.send_raw_transaction ~raw_tx:enough_funds sequencer in

  (* But it's the gas price provided by the user that's necessary, not
     the base fee per gas. If the user sets an higher gas price, it needs
     to be able to pay for it. *)
  let* insufficient_funds =
    match tx_type with
    | Legacy -> make_tx_pay_fees ~legacy:true ~gas_price:(base_fee_per_gas + 1)
    | Eip1559 -> make_tx_pay_fees ~legacy:false ~gas_price:(base_fee_per_gas + 1)
  in
  let*@? err = Rpc.send_raw_transaction ~raw_tx:insufficient_funds sequencer in
  Check.((err.message = "Cannot prepay transaction.") string)
    ~error_msg:"the account has no enough funds, it should fail" ;

  unit

let () =
  let all_types = [Legacy; Eip1559] in
  test_validate_compressed_sig [Legacy] ;
  test_validate_recover_caller all_types ;
  test_validate_chain_id all_types ;
  test_validate_nonce all_types ;
  test_validate_max_fee_per_gas all_types ;
  test_validate_pay_for_fees all_types
