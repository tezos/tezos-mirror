(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Rpc.Syntax
open Helpers

type transaction_type = Legacy | Eip1559 | Eip2930

let tag = function
  | Legacy -> "Legacy"
  | Eip1559 -> "Eip1559"
  | Eip2930 -> "Eip2930"

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
    | Eip2930 ->
        return
          "01f86f82053980843b9aca00825b0494b53dc01974176e5dff2298c5a94343c2585e3c54880de0b6b3a764000080c080a04cda4f1de731980d8f9bb4653f5bdf82cbacb342af15f512e925cb5e7bf44b9ba020874590087c9c2e481f5beb3abb96a9e2ce2712e064f9443760921d7e90d9eb"
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
    | Eip2930 ->
        return
          "0x01f8678205dc80843b9aca008261a8945d66ec78664f4a0b0929a41270316a6cd4d8bd4b8080c080a063db2a9f77795acaf9fa62465addfd5d8e77bf564868c6762e16f3ae0c084d20a02a57aaaff92cfdde10dfa1016127ff2a94df0bf620615b50e660e76e6044a844"
    | Eip1559 -> make_tx_chain_id ~chain_id:1000 ~legacy:false
  in
  let*@? err = Rpc.send_raw_transaction ~raw_tx:invalid_chain_id sequencer in
  Check.((err.message = "Invalid chain id") string)
    ~error_msg:"the transaction has an invalid chain id, it should fail" ;

  let* valid_chain_id =
    match tx_type with
    | Legacy -> make_tx_chain_id ~chain_id:1337 ~legacy:true
    | Eip2930 ->
        return
          "0x01f86782053980843b9aca008261a8945d66ec78664f4a0b0929a41270316a6cd4d8bd4b8080c001a05ba2acb79e66aaadd076b1ee6fdf09f5cb95541e2b24504209f89908b50a84dea0492d8cf5c29b5ec44b4730eebfb62ddf5ce486d5b8729c50a8002d6219f76412"
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
    | Eip2930 ->
        return
          "0x01f8678205390a843b9aca008261a8945d66ec78664f4a0b0929a41270316a6cd4d8bd4b8080c080a06c58350ee42312a79bfc488992cf3aaa56eecad645797b00ffb708f9ea540e88a0438a70e53e970b53f8ff6444132053296c3da19e877b6216b6c7c5834f52d7da"
    | Eip1559 -> make_tx_nonce ~nonce:10 ~legacy:false
  in
  let*@ _ok = Rpc.send_raw_transaction ~raw_tx:nonce_10 sequencer in
  (* Nonce 1 is expected nonce, that's valid. *)
  let* nonce_1 =
    match tx_type with
    | Legacy -> make_tx_nonce ~nonce:1 ~legacy:true
    | Eip2930 ->
        return
          "0x01f86782053901843b9aca008261a8945d66ec78664f4a0b0929a41270316a6cd4d8bd4b8080c080a0e4ea03b849ac0e339adde760cf024ffbc5e4e70d601ca3d597151f45ef0fe8daa057b1bdd92c0c549d38f01373935b1705f4b8310885efd8b2e23a4f4ac5da1b78"
    | Eip1559 -> make_tx_nonce ~nonce:1 ~legacy:false
  in
  let*@ _ok = Rpc.send_raw_transaction ~raw_tx:nonce_1 sequencer in
  (* Nonce 0 is refused. *)
  let* nonce_0 =
    match tx_type with
    | Legacy -> make_tx_nonce ~nonce:0 ~legacy:true
    | Eip2930 ->
        return
          "0x01f86782053980843b9aca008261a8945d66ec78664f4a0b0929a41270316a6cd4d8bd4b8080c001a05ba2acb79e66aaadd076b1ee6fdf09f5cb95541e2b24504209f89908b50a84dea0492d8cf5c29b5ec44b4730eebfb62ddf5ce486d5b8729c50a8002d6219f76412"
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
    | Eip2930 ->
        return
          "0x01f863820539800a8261a8945d66ec78664f4a0b0929a41270316a6cd4d8bd4b8080c001a0978d80876079de8efc0d3623ba127c19d3f74743bfbd8bffc461302c1b3ede23a063a107e1d83060ccbb38bbb02baccf01b4eebdf0cfefd0b21b9eafd7756fba3b"
    | Eip1559 ->
        make_tx_gas_price ~gas_price:(base_fee_per_gas - 1) ~legacy:false
  in
  let*@? err = Rpc.send_raw_transaction ~raw_tx:gas_price_below sequencer in
  Check.((err.message = "Max gas fee too low") string)
    ~error_msg:"the transaction has gas price too low, it should fail" ;

  let* gas_price_enough =
    match tx_type with
    | Legacy -> make_tx_gas_price ~gas_price:base_fee_per_gas ~legacy:true
    | Eip2930 ->
        return
          "0x01f86782053980843b9aca008261a8945d66ec78664f4a0b0929a41270316a6cd4d8bd4b8080c001a05ba2acb79e66aaadd076b1ee6fdf09f5cb95541e2b24504209f89908b50a84dea0492d8cf5c29b5ec44b4730eebfb62ddf5ce486d5b8729c50a8002d6219f76412"
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
    | Eip2930 ->
        Test.fail
          ~__LOC__
          "Eip2930 are hardcoded transaction, we can't use value like \
           base_fee_per_gas"
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
    | Eip2930 ->
        Test.fail
          ~__LOC__
          "Eip2930 are hardcoded transaction, we can't use value like \
           base_fee_per_gas"
    | Eip1559 -> make_tx_pay_fees ~legacy:false ~gas_price:base_fee_per_gas
  in
  let*@ _ok = Rpc.send_raw_transaction ~raw_tx:enough_funds sequencer in

  (* But it's the gas price provided by the user that's necessary, not
     the base fee per gas. If the user sets an higher gas price, it needs
     to be able to pay for it. *)
  let* insufficient_funds =
    match tx_type with
    | Legacy -> make_tx_pay_fees ~legacy:true ~gas_price:(base_fee_per_gas + 1)
    | Eip2930 ->
        Test.fail
          ~__LOC__
          "Eip2930 are hardcoded transaction, we can't use value like \
           base_fee_per_gas"
    | Eip1559 -> make_tx_pay_fees ~legacy:false ~gas_price:(base_fee_per_gas + 1)
  in
  let*@? err = Rpc.send_raw_transaction ~raw_tx:insufficient_funds sequencer in
  Check.((err.message = "Cannot prepay transaction.") string)
    ~error_msg:"the account has no enough funds, it should fail" ;

  unit

let () =
  let all_types = [Legacy; Eip1559; Eip2930] in
  test_validate_compressed_sig [Legacy] ;
  test_validate_recover_caller all_types ;
  test_validate_chain_id all_types ;
  test_validate_nonce all_types ;
  test_validate_max_fee_per_gas all_types ;
  test_validate_pay_for_fees [Legacy; Eip1559]
