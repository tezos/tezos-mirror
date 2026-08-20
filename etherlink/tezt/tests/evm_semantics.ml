(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Trilitech <contact@trili.tech>                         *)
(* Copyright (c) 2024-2026 Functori <contact@functori.com>                   *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Smart Optimistic Rollups: Etherlink Sequencer
   Requirement:  make -f etherlink.mk build
                 npm install eth-cli solc@0.8.31
                 # Install cast or foundry (see: https://book.getfoundry.sh/getting-started/installation)
                 curl -L https://foundry.paradigm.xyz | bash
                 foundryup
                 make fa-bridge-watchtower
                 ./scripts/install_dal_trusted_setup.sh
                 # Install websocat (see: https://github.com/vi/websocat?tab=readme-ov-file#installation)
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file evm_semantics.ml
   Subject:      EVM-level semantics: EIPs, transaction objects, gas estimation, DA fees.
*)

open Rpc.Syntax
open Transaction
open Test_helpers
open Setup
module Protocol = Test_helpers.Protocol_no_direct_register

let test_miner =
  let sequencer_pool_address =
    String.lowercase_ascii "0x8aaD6553Cf769Aa7b89174bE824ED0e53768ed70"
  in
  register_all
    ~__FILE__
    ~tags:["evm"; "miner"; "coinbase"]
    ~title:"Sequencer pool address is the block's miner"
    ~sequencer_pool_address
  @@ fun {sequencer; evm_version; _} _protocol ->
  let*@ block = Rpc.get_block_by_number ~block:"latest" sequencer in
  Check.((String.lowercase_ascii block.miner = sequencer_pool_address) string)
    ~error_msg:
      "Block miner should be the sequencer pool address, expected %R got %L" ;
  (* We deploy a contract that stores the block coinbase in its storage, and
     also has a view to get the block coinbase. *)
  let* coinbase_resolved = Solidity_contracts.coinbase evm_version in
  let* () =
    Eth_cli.add_abi ~label:coinbase_resolved.label ~abi:coinbase_resolved.abi ()
  in
  let* contract, _tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi:coinbase_resolved.abi
         ~bin:coinbase_resolved.bin)
      sequencer
  in
  let* storage_coinbase =
    Eth_cli.contract_call
      ~endpoint:(Evm_node.endpoint sequencer)
      ~abi_label:coinbase_resolved.label
      ~address:contract
      ~method_call:"getStorageCoinbase()"
      ()
  in
  Check.(
    (String.lowercase_ascii @@ String.trim storage_coinbase
    = sequencer_pool_address)
      string)
    ~error_msg:
      "Stored coinbase should be the sequencer pool address, expected %R got %L" ;
  let* view_coinbase =
    Eth_cli.contract_call
      ~endpoint:(Evm_node.endpoint sequencer)
      ~abi_label:coinbase_resolved.label
      ~address:contract
      ~method_call:"getStorageCoinbase()"
      ()
  in
  Check.(
    (String.lowercase_ascii @@ String.trim view_coinbase
    = sequencer_pool_address)
      string)
    ~error_msg:
      "Viewed coinbase should be the sequencer pool address, expected %R got %L" ;

  let*@ rpc_coinbase = Rpc.coinbase sequencer in
  Check.((rpc_coinbase = sequencer_pool_address) string)
    ~error_msg:
      "eth_coinbase should be the sequencer pool address, expected %R got %L" ;

  (*
     Regression test:

     Custom RPC call parameters to make sure we support it correctly. A bug
     made the node consider `"params" : []` to be invalid.
  *)
  let* r1 =
    Evm_node.jsonrpc sequencer {method_ = "eth_coinbase"; parameters = `Null}
  in
  let r1 = JSON.(r1 |-> "result" |> as_string) in
  let* r2 =
    Evm_node.jsonrpc sequencer {method_ = "eth_coinbase"; parameters = `A []}
  in
  let r2 = JSON.(r2 |-> "result" |> as_string) in
  Check.is_true
    (r1 = r2 && r2 = rpc_coinbase)
    ~error_msg:"Expected all coinbase addresses to be equal" ;

  unit

let test_da_fees_after_execution =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~tags:
      [
        "evm";
        "da_fees";
        (* Those tests take about 13 minutes in the CI. *)
        Tag.slow;
      ]
    ~da_fee:(Wei.of_string "4_000_000_000_000")
    ~title:"da fees test"
  @@ fun {sequencer; _} _protocol ->
  (* This is a non-regression test of the situation where the source of
     a transaction is able to prepay for the gas, but after the transfer
     is executed it cannot pay for the DA fees.

     Because obviously DA fees are accounted AFTER the execution.
  *)
  let account =
    Eth_account.
      {
        address = "0xA257edC8ad1D8f8f463aC0D947cc381000b3c863";
        private_key =
          "0xb80e5dd2ba9281e482589973600609bb0f10f6a075e6c733e4472d4dd2df238a";
      }
  in
  let* _ =
    send_transaction_to_sequencer
      (Eth_cli.transaction_send
         ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
         ~to_public_key:account.address
         ~value:(Wei.of_eth_int 1)
         ~endpoint:(Evm_node.endpoint sequencer))
      sequencer
  in

  let wait_for = Evm_node.wait_for_tx_queue_add_transaction sequencer in
  let res =
    Lwt.catch
      (fun () ->
        Eth_cli.transaction_send
          ~source_private_key:account.private_key
          ~to_public_key:"0x1074Fd1EC02cbeaa5A90450505cF3B48D834f3EB"
          ~value:(Wei.of_eth_string "0.9999")
          ~endpoint:(Evm_node.endpoint sequencer)
          ())
      (fun _exn -> Lwt.return "")
  in

  let* _ = wait_for in
  let*@ _ = produce_block sequencer in

  let* res in

  Check.((res = "") string) ~error_msg:"The transaction should have failed" ;

  unit

let test_estimate_gas_with_block_param =
  register_all
    ~__FILE__
    ~tags:["evm"; "eth_estimategas"; "simulate"; "estimate_gas"; "earliest"]
    ~title:"eth_estimateGas with block parameter"
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* gas_consumer = Solidity_contracts.even_block_gas_consumer evm_version in
  let* () =
    Eth_cli.add_abi ~label:gas_consumer.label ~abi:gas_consumer.abi ()
  in
  let* address_contract, tx =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:sender.private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi:gas_consumer.label
         ~bin:gas_consumer.bin)
      sequencer
  in
  let* receipt =
    Eth_cli.get_receipt ~endpoint:(Evm_node.endpoint sequencer) ~tx ()
  in
  match receipt with
  | Some {blockNumber; _} when blockNumber = 1l ->
      let blockNumber = Int32.to_int blockNumber in
      let* data =
        Eth_cli.encode_method
          ~abi_label:gas_consumer.label
          ~method_:"consume()"
          ()
      in
      let call_params =
        [
          ("from", `String sender.address);
          ("to", `String address_contract);
          ("input", `String data);
        ]
      in
      let* _ = produce_block sequencer in
      (* Estimate_gas does its estimations on a block about to be minted so if we run it on block `N` the solidity code execute itself with block.number = `N`+ 1 *)
      let*@ evenGasCost =
        Rpc.estimate_gas call_params sequencer ~block:(Number blockNumber)
      in
      let*@ oddGasCost =
        Rpc.estimate_gas call_params sequencer ~block:(Number (blockNumber + 1))
      in
      Check.(
        (evenGasCost > oddGasCost)
          int64
          ~error_msg:
            "When calling for the contract EvenBlockGasConsumer.consume(), The \
             cost of gas should be lower in odd block numbers than in even \
             block numbers but got even = %L and odd = %R") ;
      unit
  | _ -> Test.fail "Test contract deployment failed"

let test_estimate_gas_accounts_for_access_list =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "eth_estimategas"; "estimate_gas"; "access_list"; "eip2930"]
    ~title:"eth_estimateGas with access list gives enough gas"
    ~da_fee:(Wei.of_string "1_000_000_000_000")
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* eip2930 = Solidity_contracts.eip2930_storage_access evm_version in
  let* () = Eth_cli.add_abi ~label:eip2930.label ~abi:eip2930.abi () in
  let* contract, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:sender.private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi:eip2930.abi
         ~bin:eip2930.bin)
      sequencer
  in
  let storage_slot =
    "0x0000000000000000000000000000000000000000000000000000000000000000"
  in
  let* data = Cast.calldata ~args:["42"] "setValue(uint256)" in
  let data = String.trim data in
  let call_params =
    [
      ("from", `String sender.address);
      ("to", `String contract);
      ("input", `String data);
      ( "accessList",
        `A
          [
            `O
              [
                ("address", `String contract);
                ("storageKeys", `A [`String storage_slot]);
              ];
          ] );
    ]
  in
  let*@ estimated_gas = Rpc.estimate_gas call_params sequencer in
  let* gas_price = Rpc.get_gas_price sequencer in
  let gas_price = Int32.to_int gas_price in
  let* raw_tx =
    Cast.craft_tx
      ~signature:"setValue(uint256)"
      ~source_private_key:sender.private_key
      ~chain_id:1337
      ~nonce:1
      ~gas:(Int64.to_int estimated_gas)
      ~gas_price
      ~value:Wei.zero
      ~access_list:[(contract, [storage_slot])]
      ~address:contract
      ~arguments:["42"]
      ~legacy:false
      ()
  in
  let*@ tx_hash = Rpc.send_raw_transaction ~raw_tx sequencer in
  let* _ = produce_block sequencer in
  let*@! Transaction.{status; _} =
    Rpc.get_transaction_receipt ~tx_hash sequencer
  in
  Check.((status = true) bool)
    ~error_msg:
      "Transaction with access list should succeed with gas estimated by \
       eth_estimateGas" ;
  unit

let test_estimate_gas_accounts_for_authorization_list =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:
      [
        "evm"; "eth_estimategas"; "estimate_gas"; "authorization_list"; "eip7702";
      ]
    ~title:"eth_estimateGas with authorization list gives enough gas"
    ~da_fee:(Wei.of_string "1_000_000_000_000")
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  let whale = Eth_account.bootstrap_accounts.(0) in
  let sponsored = Eth_account.bootstrap_accounts.(1) in
  let endpoint = Evm_node.endpoint sequencer in
  let* eip7702 = Solidity_contracts.eip7702 evm_version in
  let* () = Eth_cli.add_abi ~label:eip7702.label ~abi:eip7702.abi () in
  let* eip7702_contract, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:whale.private_key
         ~endpoint
         ~abi:eip7702.abi
         ~bin:eip7702.bin)
      sequencer
  in
  let* gas_price = Rpc.get_gas_price sequencer in
  let gas_price = Int32.to_int gas_price in
  let*@ whale_nonce =
    Rpc.get_transaction_count ~address:whale.address sequencer
  in
  let* signed_auth =
    Cast.wallet_sign_auth
      ~authorization:eip7702_contract
      ~private_key:sponsored.private_key
      ~endpoint
      ()
  in
  let base_call =
    [("from", `String whale.address); ("to", `String sponsored.address)]
  in
  let auth_list_json =
    ( "authorizationList",
      `A
        [
          `O
            [
              ("chainId", `String "0x539");
              ("address", `String eip7702_contract);
              ("nonce", `String "0x0");
              ("yParity", `String "0x0");
              ("r", `String "0x0");
              ("s", `String "0x0");
            ];
        ] )
  in
  (* Estimate gas without and with authorization list *)
  let*@ gas_without_auth = Rpc.estimate_gas base_call sequencer in
  let*@ gas_with_auth =
    Rpc.estimate_gas (auth_list_json :: base_call) sequencer
  in
  (* The fix makes eth_estimateGas account for DA fee overhead of the
     authorization list. The estimate with auth list should be higher. *)
  Check.(
    (gas_with_auth > gas_without_auth)
      int64
      ~error_msg:
        "eth_estimateGas should return higher gas with authorization list (%L) \
         than without (%R)") ;
  let gas = Int64.to_int gas_with_auth in
  let* raw_tx =
    Cast.craft_tx
      ~source_private_key:whale.private_key
      ~chain_id:1337
      ~nonce:(Int64.to_int whale_nonce)
      ~gas
      ~gas_price
      ~value:Wei.zero
      ~authorization:signed_auth
      ~address:sponsored.address
      ~arguments:[]
      ~legacy:false
      ()
  in
  let*@ _tx_hash = Rpc.send_raw_transaction ~raw_tx sequencer in
  let* _ = produce_block sequencer in
  (* The bare call reverts (no fallback in the delegation contract) but
     the authorization is processed regardless. Verify the delegation
     code was set on the sponsored address. *)
  let*@ code = Rpc.get_code ~address:sponsored.address sequencer in
  Check.((code <> "0x") string)
    ~error_msg:
      "Expected the sponsored EOA to have delegation code after EIP-7702 tx" ;
  unit

let test_transaction_object expected_type_ name make_transaction =
  register_all
    ~__FILE__
    ~tags:[name; "transaction_object"]
    ~time_between_blocks:Nothing
    ~kernels:[Latest]
    ~use_dal:Register_without_feature
    ~title:
      (sf "RPC returns the correct transaction object for %s transactions" name)
  @@ fun {sequencer; _} _protocol ->
  let* hash =
    send_transaction_to_sequencer
      (fun () ->
        let* raw_tx = make_transaction () in

        let*@ hash = Rpc.send_raw_transaction ~raw_tx sequencer in
        return hash)
      sequencer
  in
  let* json =
    Evm_node.jsonrpc
      sequencer
      (Rpc.Request.eth_getTransactionByHash ~transaction_hash:hash)
  in
  let type_ = JSON.(json |-> "result" |-> "type" |> as_string) in
  Check.(
    (type_ = expected_type_)
      string
      ~error_msg:
        ("Type was expected to be %R (" ^ name ^ "), but the RPC returned %L")) ;
  unit

let test_eip2930_transaction_object =
  test_transaction_object
    "0x1"
    "eip2930"
    (Cast.craft_tx
       ~legacy:true
       ~access_list:
         [
           ( Eth_account.bootstrap_accounts.(0).address,
             ["0x" ^ String.make 64 '0'] );
         ]
       ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
       ~chain_id:1337
       ~nonce:0
       ~gas_price:1_000_000_000
         (* BASE_CALL_GAS_COST + 1xACCESS_LIST_ADDRESS_COST + 1xACCESS_LIST_STORAGE_KEY_COST
            = 21_000 + 2400 + 1900 = 25_300 *)
       ~gas:25_300
       ~value:Wei.zero
       ~address:Eth_account.bootstrap_accounts.(1).address)

let test_eip1559_transaction_object =
  test_transaction_object
    "0x2"
    "eip1559"
    (Cast.craft_tx
       ~legacy:false
       ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
       ~chain_id:1337
       ~nonce:0
       ~gas_price:1_000_000_000
       ~gas:23_300
       ~value:Wei.zero
       ~address:Eth_account.bootstrap_accounts.(1).address)

let test_eip2930_storage_access =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "eip2930"]
    ~title:"Check EIP-2930's semantic correctness"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  let whale = Eth_account.bootstrap_accounts.(0) in
  let* eip2930_storage_access =
    Solidity_contracts.eip2930_storage_access evm_version
  in
  let* eip2930_contract, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:whale.private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi:eip2930_storage_access.abi
         ~bin:eip2930_storage_access.bin)
      sequencer
  in
  let* gas_price = Rpc.get_gas_price sequencer in
  let gas_price = Int32.to_int gas_price in
  let base_tx ~nonce ~access_list ~arg =
    Cast.craft_tx
      ~signature:"setValue(uint256)"
      ~source_private_key:whale.private_key
      ~chain_id:1337
      ~nonce
      ~gas:100_000
      ~gas_price
      ~value:Wei.zero
      ~access_list
      ~address:eip2930_contract
      ~arguments:[arg]
      ~legacy:false
      ()
  in
  let* raw_tx_with_storage_slot_access_list =
    base_tx
      ~nonce:1
      ~access_list:
        [
          ( eip2930_contract,
            (* slot 0 =  [uint256 public value] in [eip2930_storage_access.sol] *)
            [
              "0x0000000000000000000000000000000000000000000000000000000000000000";
            ] );
        ]
      ~arg:"42"
  in
  let* raw_tx_without_storage_slot_access_list =
    base_tx ~nonce:2 ~access_list:[(eip2930_contract, [])] ~arg:"43"
  in
  let*@ tx_with_storage_slot_access_list_hash =
    Rpc.send_raw_transaction
      ~raw_tx:raw_tx_with_storage_slot_access_list
      sequencer
  in
  let* _ = produce_block sequencer in
  let*@ tx_without_storage_slot_access_list_hash =
    Rpc.send_raw_transaction
      ~raw_tx:raw_tx_without_storage_slot_access_list
      sequencer
  in
  let* _ = produce_block sequencer in
  let*@! Transaction.{gasUsed = gas_with_storage_slot_access_list; _} =
    Rpc.get_transaction_receipt
      ~tx_hash:tx_with_storage_slot_access_list_hash
      sequencer
  in
  let*@! Transaction.{gasUsed = gas_without_storage_slot_access_list; _} =
    Rpc.get_transaction_receipt
      ~tx_hash:tx_without_storage_slot_access_list_hash
      sequencer
  in
  Check.(
    Int64.(
      sub gas_without_storage_slot_access_list gas_with_storage_slot_access_list
      = of_int 200)
      int64)
    ~error_msg:
      "The gas consumption with the preheated slot should have saved exactly \
       %R but got %L" ;
  unit

let test_eip2537 =
  register_all
    ~__FILE__
    ~tags:["evm"; "bls"; "precompile"; "eip2537"]
    ~title:"EIP-2537 is activated on Etherlink"
    ~kernels:[Latest]
  @@ fun {sequencer; _} _protocol ->
  let bls_g1add = "0x000000000000000000000000000000000000000b" in
  (* First entry of BLS12_G1ADD at https://eips.ethereum.org/assets/eip-2537/test-vectors *)
  let input =
    "0x0000000000000000000000000000000017f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb0000000000000000000000000000000008b3f481e3aaa0f1a09e30ed741d8ae4fcf5e095d5d00af600db18cb2c04b3edd03cc744a2888ae40caa232946c5e7e100000000000000000000000000000000112b98340eee2777cc3c14163dea3ec97977ac3dc5c70da32e6e87578f44912e902ccef9efe28d4a78b8999dfbca942600000000000000000000000000000000186b28d92356c4dfec4b5201ad099dbdede3781f8998ddf929b4cd7756192185ca7b8f4ef7088f813270ac3d48868a21"
  in
  let expected =
    "0x000000000000000000000000000000000a40300ce2dec9888b60690e9a41d3004fda4886854573974fab73b046d3147ba5b7a5bde85279ffede1b45b3918d82d0000000000000000000000000000000006d3d887e9f53b9ec4eb6cedf5607226754b07c01ace7834f57f3e7315faefb739e59018e22c492006190fba4a870025"
  in
  let* value =
    Cast.raw_call
      ~endpoint:(Evm_node.endpoint sequencer)
      ~address:bls_g1add
      ~arg:input
  in
  Check.((value = expected) string)
    ~error_msg:(Format.sprintf "BLS12_G1ADD mismatch for test bls_g1add_g1+p1") ;
  unit

let pad_to_n_bytes_le bytes length =
  let current_length = Bytes.length bytes in
  if current_length >= length then bytes
  else
    let padding_length = length - current_length in
    let padding = Bytes.make padding_length '\x00' in
    Bytes.cat bytes padding

let test_validate_encoding_compatibility_accounts =
  register_all
    ~__FILE__
    ~title:"Validate encoding compatibility of accounts"
    ~tags:["encoding"; "accounts"; "foo"]
  @@ fun {sequencer; observer; kernel; _} _protocol ->
  let nonce = Z.of_int 1234 in
  let source = Eth_account.bootstrap_accounts.(0) in
  let bits = Z.to_bits nonce |> Bytes.of_string in
  let encoded_nonce = pad_to_n_bytes_le bits 4 in
  let* () = Evm_node.terminate sequencer in
  let* () = Evm_node.terminate observer in
  let* () =
    Evm_node.patch_state
      sequencer
      ~key:(Durable_storage_path.nonce kernel source.address)
      ~value:(Hex.of_bytes encoded_nonce |> Hex.show)
  in
  let* () =
    Evm_node.patch_state
      observer
      ~key:(Durable_storage_path.nonce kernel source.address)
      ~value:(Hex.of_bytes encoded_nonce |> Hex.show)
  in
  let* () = Evm_node.run sequencer in
  let* () = Evm_node.run observer in
  let desination = Eth_account.bootstrap_accounts.(1) in
  let*@ balance = Rpc.get_balance ~address:desination.address sequencer in
  (* First execution of the transaction should lazy migrate the storage values
  to the new `info` format and the others transactions should execute correctly*)
  let* _ =
    repeat 5 (fun () ->
        let* _ =
          send_transaction_to_sequencer
            (Eth_cli.transaction_send
               ~source_private_key:source.private_key
               ~to_public_key:desination.address
               ~value:Wei.zero
               ~endpoint:(Evm_node.endpoint sequencer))
            sequencer
        in
        unit)
  in
  let*@ balance_after = Rpc.get_balance ~address:desination.address sequencer in
  Check.((balance_after = balance) Wei.typ)
    ~error_msg:"Expected balance of %R wei, got %L wei after %R transactions" ;
  unit

let test_eip3607_disabled_for_simulation =
  register_all
    ~__FILE__
    ~tags:["evm"; "eip3607"; "simulation"; "eth_call"]
    ~title:"EIP-3607 is disabled in simulation"
    ~kernels:[Latest]
  @@ fun {sequencer; evm_version; _} _protocol ->
  let whale = Eth_account.bootstrap_accounts.(0) in
  let* contract = Solidity_contracts.eip7702 evm_version in
  let* contract_address, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:whale.private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi:contract.abi
         ~bin:contract.bin)
      sequencer
  in
  let contract_address = Ezjsonm.encode_string contract_address in
  let* call_result =
    Evm_node.(
      call_evm_rpc
        sequencer
        {
          method_ = "eth_call";
          parameters =
            `A
              [
                `O [("from", contract_address); ("to", contract_address)];
                `String "latest";
              ];
        })
  in
  let call_result =
    Evm_node.extract_error_message call_result |> JSON.as_string
  in
  (*
    If EIP-3607 was not disabled we would get:
      {
        "jsonrpc": "2.0",
        "error": {
            "code": -32003,
            "message": "Execution error: REVM error Transaction(RejectCallerWithCode)"
        },
        "id": "0"
      }
  *)
  Check.((call_result = "execution reverted") string)
    ~error_msg:"Expected error msg %R but got %L" ;
  unit

let protocols = [Protocol.Alpha]

let () =
  test_miner protocols ;
  test_da_fees_after_execution protocols ;
  test_estimate_gas_with_block_param protocols ;
  test_estimate_gas_accounts_for_access_list [Alpha] ;
  test_estimate_gas_accounts_for_authorization_list [Alpha] ;
  test_eip2930_transaction_object [Alpha] ;
  test_eip1559_transaction_object [Alpha] ;
  test_eip2930_storage_access [Alpha] ;
  test_validate_encoding_compatibility_accounts [Alpha] ;
  test_eip2537 [Alpha] ;
  test_eip3607_disabled_for_simulation [Alpha]
