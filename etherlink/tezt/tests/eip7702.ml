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
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file eip7702.ml
   Subject:      EIP-7702 set-code authorizations.
*)

open Rpc.Syntax
open Contract_path
open Transaction
open Delayed_inbox
open Test_helpers
open Setup
open Fa_bridge_helpers
module Protocol = Test_helpers.Protocol_no_direct_register

(* Regression test for the DA fee reservation vulnerability (F62).
   The attacker delegates their EOA to a drain contract via EIP-7702,
   then sends a tx (value=0) calling drain() on their own address,
   transferring their entire balance to a receiver during execution.
   Without the fix, the DA fee reserve stays in the balance during
   execution, the drain succeeds completely, and the post-execution
   fee settlement fails — reverting the entire block at zero cost.
   With the fix, the DA fee is deducted before execution, so the
   drain can only take what remains after the DA fee. *)
let test_da_fee_drain_via_eip7702 =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~tags:["evm"; "da_fees"; "eip7702"]
    ~da_fee:(Wei.of_string "4_000_000_000_000")
    ~title:"DA fee reservation cannot be drained via EIP-7702"
    ~kernels:[Kernel.Latest]
  @@ fun {sequencer; evm_version; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let whale = Eth_account.bootstrap_accounts.(0) in
  let attacker =
    Eth_account.
      {
        address = "0xA257edC8ad1D8f8f463aC0D947cc381000b3c863";
        private_key =
          "0xb80e5dd2ba9281e482589973600609bb0f10f6a075e6c733e4472d4dd2df238a";
      }
  in
  let receiver = "0x1074Fd1EC02cbeaa5A90450505cF3B48D834f3EB" in
  (* Step 1: Fund the attacker with 1 ETH. *)
  let* _ =
    send_transaction_to_sequencer
      (Eth_cli.transaction_send
         ~source_private_key:whale.private_key
         ~to_public_key:attacker.address
         ~value:(Wei.of_eth_int 1)
         ~endpoint)
      sequencer
  in
  (* Deploy the drain contract. *)
  let* drain = Solidity_contracts.drain_balance evm_version in
  let* () = Eth_cli.add_abi ~label:drain.label ~abi:drain.abi () in
  let* drain_contract, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:whale.private_key
         ~endpoint
         ~abi:drain.abi
         ~bin:drain.bin)
      sequencer
  in
  let* gas_price = Rpc.get_gas_price sequencer in
  let gas_price = Int32.to_int gas_price in
  (* Step 2: The whale sends an EIP-7702 tx that sets the attacker's
     delegation to the drain contract. The attacker signs the auth
     but the whale pays for the tx. *)
  let* signed_auth =
    Cast.wallet_sign_auth
      ~authorization:drain_contract
      ~private_key:attacker.private_key
      ~endpoint
      ()
  in
  let*@ whale_nonce =
    Rpc.get_transaction_count ~address:whale.address sequencer
  in
  let auth_list_json =
    ( "authorizationList",
      `A
        [
          `O
            [
              ("chainId", `String "0x539");
              ("address", `String drain_contract);
              ("nonce", `String "0x0");
              ("yParity", `String "0x0");
              ("r", `String "0x0");
              ("s", `String "0x0");
            ];
        ] )
  in
  let*@ estimated_gas =
    Rpc.estimate_gas
      [
        ("from", `String whale.address);
        ("to", `String attacker.address);
        auth_list_json;
      ]
      sequencer
  in
  let* raw_setup =
    Cast.craft_tx
      ~source_private_key:whale.private_key
      ~chain_id:1337
      ~nonce:(Int64.to_int whale_nonce)
      ~gas_price
      ~gas:(Int64.to_int estimated_gas)
      ~value:Wei.zero
      ~authorization:signed_auth
      ~legacy:false
      ~address:attacker.address
      ~arguments:[]
      ()
  in
  let*@ _setup_hash = Rpc.send_raw_transaction ~raw_tx:raw_setup sequencer in
  let*@ _ = produce_block sequencer in
  (* Step 3: The attacker sends a tx to themselves with value=0.
     Because their address now has the drain contract's code, this
     calls drain(receiver), transferring the balance out during
     execution. The attacker's nonce is 1 (bumped by the auth). *)
  let*@ attacker_nonce =
    Rpc.get_transaction_count ~address:attacker.address sequencer
  in
  let* raw_attack =
    Cast.craft_tx
      ~source_private_key:attacker.private_key
      ~chain_id:1337
      ~nonce:(Int64.to_int attacker_nonce)
      ~gas_price
      ~gas:1_000_000
      ~value:Wei.zero
      ~legacy:true
      ~address:attacker.address
      ~signature:"drain(address)"
      ~arguments:[receiver]
      ()
  in
  let*@ tx_hash = Rpc.send_raw_transaction ~raw_tx:raw_attack sequencer in
  let*@ _ = produce_block sequencer in
  let*@! Transaction.{status; _} =
    Rpc.get_transaction_receipt ~tx_hash sequencer
  in
  (* The transaction must be included (no block revert). *)
  Log.info "Transaction included, status = %b" status ;
  (* The receiver should have received less than 1 ETH: the DA fee
     was deducted from the attacker before execution. *)
  let*@ receiver_balance = Rpc.get_balance ~address:receiver sequencer in
  Check.((receiver_balance < Wei.of_eth_int 1) Wei.typ)
    ~error_msg:
      "Receiver should have less than 1 ETH (DA fee was deducted), got %L" ;
  Log.info
    "Receiver balance: %s (less than 1 ETH as expected)"
    (Wei.to_string receiver_balance) ;
  unit

let test_eip7702 =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "eip7702"]
    ~title:"Check EIP-7702's semantic correctness"
      (* See: https://eips.ethereum.org/EIPS/eip-7702. *)
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  let whale = Eth_account.bootstrap_accounts.(0) in
  let sponsored =
    Eth_account.
      {
        address = "0x202dFc8a729ac2cdE90D3B0e7A0424b6Ed6f6c34";
        private_key =
          "0x7d597ae2d861eda61e148e757478c9a07d950be9f355958195f1fc75a0cdd8b2";
      }
  in
  (* To show the benefits of having EIP-7702 we'll use an EOA with 0 balance
     on purpose. *)
  let*@ balance = Rpc.get_balance ~address:sponsored.address sequencer in
  Check.((balance = Wei.zero) Wei.typ)
    ~error_msg:
      "Expected balance of the sponsored address of zero wei, got %L wei" ;
  let endpoint = Evm_node.endpoint sequencer in
  let* eip7702 = Solidity_contracts.eip7702 evm_version in
  let* () = Eth_cli.add_abi ~label:eip7702.label ~abi:eip7702.abi () in
  (* We start by deploying a simple contract that emits a log. This will be
     the delegation contract that will be executed from the EOA's address. *)
  let* eip7702_contract, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:whale.private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi:eip7702.abi
         ~bin:eip7702.bin)
      sequencer
  in
  let* gas_price = Rpc.get_gas_price sequencer in
  let gas_price = Int32.to_int gas_price in
  let base_tx ~nonce ~authorization =
    Cast.craft_tx
      ~source_private_key:whale.private_key
      ~chain_id:1337
      ~nonce
      ~gas:100_000
      ~gas_price
      ~value:Wei.zero
      ~authorization
      ~address:sponsored.address
      ~arguments:[]
      ~legacy:false
      ()
  in
  (* The EOA can sign with no money the authorization list. Here the delegation
     is done to `eip7702_contract`.*)
  let* signed_auth =
    Cast.wallet_sign_auth
      ~authorization:eip7702_contract
      ~private_key:sponsored.private_key
      ~endpoint
      ()
  in
  (* We craft the EIP-7702 transaction thanks to cast. The sponsor that has the
     necessary balance will use the signed authorization and post in on chain. *)
  let* raw_set_eoa = base_tx ~nonce:1 ~authorization:signed_auth in
  let*@ set_eoa_hash = Rpc.send_raw_transaction ~raw_tx:raw_set_eoa sequencer in
  let* _ = produce_block sequencer in
  let*@! Transaction.{type_; _} =
    Rpc.get_transaction_receipt ~tx_hash:set_eoa_hash sequencer
  in
  (* Type 4 = EIP-7702 *)
  Check.((type_ = Int32.of_int 4) int32)
    ~error_msg:"Expected tx.type of %R, got %L" ;
  (* We can retrieve the authorization list from the transaction object: *)
  let*@! Transaction.{authorizationList; _} =
    Rpc.get_transaction_by_hash ~transaction_hash:set_eoa_hash sequencer
  in
  (match authorizationList with
  | Some [{address; _}] ->
      Check.(
        (String.lowercase_ascii address
        = String.lowercase_ascii eip7702_contract)
          string)
        ~error_msg:"Expected msg.sender of %R, got %L"
  | Some _ -> failwith "Authorization list should only contain one element."
  | None -> failwith "Authorization list should not be empty.") ;
  (* The EOA now has code that can be called. We can even reuse the abi label
     from the delegation contract. *)
  let* call_eoa_hash =
    send_transaction_to_sequencer
      (Eth_cli.contract_send
         ~source_private_key:whale.private_key
         ~endpoint
         ~abi_label:eip7702.label
         ~address:sponsored.address (* EOA's account *)
         ~method_call:"emitEvent()")
      sequencer
  in
  let*@! Transaction.{logs; _} =
    Rpc.get_transaction_receipt ~tx_hash:call_eoa_hash sequencer
  in
  (* Exactly one event emitted, see the code of `eip7702_contract`. *)
  let Transaction.{data; _} = List.hd logs in
  let data_without_padding = "0x" ^ String.sub data 26 40 in
  Check.(
    (String.lowercase_ascii data_without_padding
    = String.lowercase_ascii whale.address)
      string)
    ~error_msg:"Expected msg.sender of %R, got %L" ;
  let*@ code = Rpc.get_code ~address:sponsored.address sequencer in
  (* `0xef0100` is the current prefix of EIP-7702 transactions, see the
      exact specification for more details. *)
  let expected_code =
    "0xef0100" ^ String.(lowercase_ascii @@ sub eip7702_contract 2 40)
  in
  Check.((code = expected_code) string) ~error_msg:"Expected code of %R, got %L" ;
  (* Another property of EIP-7702 is allowing an EOA to re-delegate its code to
     another address. *)
  let* redelegation_contract =
    Solidity_contracts.eip2930_storage_access evm_version
  in
  let* redelegation_contract_address, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:whale.private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi:redelegation_contract.abi
         ~bin:redelegation_contract.bin)
      sequencer
  in
  let* signed_auth =
    Cast.wallet_sign_auth
      ~authorization:redelegation_contract_address
      ~private_key:sponsored.private_key
      ~endpoint
      ()
  in
  let* raw_set_eoa = base_tx ~nonce:4 ~authorization:signed_auth in
  let*@ set_eoa_hash = Rpc.send_raw_transaction ~raw_tx:raw_set_eoa sequencer in
  let*@ _ = produce_block sequencer in
  let*@! Transaction.{type_; _} =
    Rpc.get_transaction_receipt ~tx_hash:set_eoa_hash sequencer
  in
  (* Type 4 = EIP-7702 *)
  Check.((type_ = Int32.of_int 4) int32)
    ~error_msg:"Expected tx.type of %R, got %L" ;
  (* We can retrieve the authorization list from the transaction object: *)
  let*@! Transaction.{authorizationList; _} =
    Rpc.get_transaction_by_hash ~transaction_hash:set_eoa_hash sequencer
  in
  (match authorizationList with
  | Some [{address; _}] ->
      Check.(
        (String.lowercase_ascii address
        = String.lowercase_ascii redelegation_contract_address)
          string)
        ~error_msg:"Expected msg.sender of %R, got %L"
  | Some _ -> failwith "Authorization list should only contain one element."
  | None -> failwith "Authorization list should not be empty.") ;
  let expected_storage_slot = 2 in
  let* gas_price = Rpc.get_gas_price sequencer in
  let gas_price = Int32.to_int gas_price in
  let* raw_call_eoa_hash =
    Cast.craft_tx
      ~signature:"setValue(uint256)"
      ~source_private_key:whale.private_key
      ~chain_id:1337
      ~nonce:5
      ~gas:100_000
      ~gas_price
      ~value:Wei.zero
      ~access_list:[]
      ~address:sponsored.address
      ~arguments:["2"]
      ~legacy:false
      ()
  in
  let*@ call_eoa_hash =
    Rpc.send_raw_transaction ~raw_tx:raw_call_eoa_hash sequencer
  in
  let*@ _ = produce_block sequencer in
  let*@! Transaction.{blockNumber; _} =
    Rpc.get_transaction_receipt ~tx_hash:call_eoa_hash sequencer
  in
  let*@ storage_slot =
    Rpc.get_storage_at
      ~address:sponsored.address
      ~pos:"0x0"
      ~block:
        (Block_number
           {number = Int32.to_int blockNumber; require_canonical = false})
      sequencer
  in
  Check.((storage_slot = Printf.sprintf "0x%064x" expected_storage_slot) string)
    ~error_msg:"Expected storage slot of %R, got %L" ;
  let* _ =
    send_transaction_to_sequencer
      (Eth_cli.transaction_send
         ~source_private_key:whale.private_key
         ~to_public_key:sponsored.address
         ~value:Wei.one_eth
         ~endpoint)
      sequencer
  in
  (* EIP-7702 authorized account can also send transactions to the sequencer. *)
  let* _ =
    send_transaction_to_sequencer
      (Eth_cli.transaction_send
         ~source_private_key:sponsored.private_key
         ~to_public_key:whale.address
         ~value:(Wei.of_string "1125")
         ~endpoint)
      sequencer
  in
  unit

let test_deposits_on_eip7702_accounts =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "eip7702"; "deposit"]
    ~title:"Check that deposit do not break EIP-7702 accounts semantic."
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@
  fun {
        sequencer;
        evm_version;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        client;
        _;
      }
      _protocol
    ->
  let whale = Eth_account.bootstrap_accounts.(0) in
  let sponsored =
    Eth_account.
      {
        address = "0x202dFc8a729ac2cdE90D3B0e7A0424b6Ed6f6c34";
        private_key =
          "0x7d597ae2d861eda61e148e757478c9a07d950be9f355958195f1fc75a0cdd8b2";
      }
  in
  (* The future EIP-7702 account will have 0 balance before the deposit. *)
  let*@ balance = Rpc.get_balance ~address:sponsored.address sequencer in
  Check.((balance = Wei.zero) Wei.typ)
    ~error_msg:
      "Expected balance of the sponsored address of zero wei, got %L wei" ;
  let endpoint = Evm_node.endpoint sequencer in
  let* eip7702 = Solidity_contracts.eip7702_fallback evm_version in
  let* () = Eth_cli.add_abi ~label:eip7702.label ~abi:eip7702.abi () in
  (* We start by deploying a simple contract that emits a log. This will be
     the delegation contract that will be executed from the EOA's address
     when the deposit happens. *)
  let* eip7702_contract, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:whale.private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi:eip7702.abi
         ~bin:eip7702.bin)
      sequencer
  in
  let* gas_price = Rpc.get_gas_price sequencer in
  let gas_price = Int32.to_int gas_price in
  let base_tx ~nonce ~authorization =
    Cast.craft_tx
      ~source_private_key:whale.private_key
      ~chain_id:1337
      ~nonce
      ~gas:100_000
      ~gas_price
      ~value:Wei.zero
      ~authorization
      ~address:sponsored.address
      ~arguments:[]
      ~legacy:false
      ()
  in
  let* signed_auth =
    Cast.wallet_sign_auth
      ~authorization:eip7702_contract
      ~private_key:sponsored.private_key
      ~endpoint
      ()
  in
  (* We craft the EIP-7702 transaction thanks to cast. The sponsor that has the
     necessary balance will use the signed authorization and post in on chain. *)
  let* raw_set_eoa = base_tx ~nonce:1 ~authorization:signed_auth in
  let*@ set_eoa_hash = Rpc.send_raw_transaction ~raw_tx:raw_set_eoa sequencer in
  let* _ = produce_block sequencer in
  let*@! Transaction.{type_; _} =
    Rpc.get_transaction_receipt ~tx_hash:set_eoa_hash sequencer
  in
  (* Type 4 = EIP-7702 *)
  Check.((type_ = Int32.of_int 4) int32)
    ~error_msg:"Expected tx.type of %R, got %L" ;
  (* We can retrieve the authorization list from the transaction object: *)
  let*@! Transaction.{authorizationList; _} =
    Rpc.get_transaction_by_hash ~transaction_hash:set_eoa_hash sequencer
  in
  (match authorizationList with
  | Some [{address; _}] ->
      Check.(
        (String.lowercase_ascii address
        = String.lowercase_ascii eip7702_contract)
          string)
        ~error_msg:"Expected msg.sender of %R, got %L"
  | Some _ -> failwith "Authorization list should only contain one element."
  | None -> failwith "Authorization list should not be empty.") ;
  (* At this point, the EOA has code that can be called on deposit. *)
  let scenario_for ~address ~deposit_id =
    let depositor = Constant.bootstrap5 in
    let amount = Tez.of_int 1125 in
    let deposit_info = {receiver = EthereumAddr address; chain_id = None} in
    let* () =
      send_deposit_to_delayed_inbox
        ~amount
        ~bridge:l1_contracts.bridge
        ~depositor
        ~deposit_info
        ~sc_rollup_node
        ~sc_rollup_address
        client
    in
    let* () =
      wait_for_delayed_inbox_add_tx_and_injected
        ~sequencer
        ~sc_rollup_node
        ~client
    in
    let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in
    let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in
    (* Since it's an address that contains code that we deposit in, the balance
       should still be zero, it's a two step process, we need to claim it. *)
    let*@ balance = Rpc.get_balance ~address sequencer in
    Check.((balance = Wei.zero) Wei.typ)
      ~error_msg:
        "Expected balance of the sponsored address of zero wei, got %L wei" ;
    (* Check that the deposit was queued for the address with code. *)
    let* deposit_receipt = get_one_receipt_from_latest_or_fail sequencer in
    let deposit_log : Transaction.tx_log = List.hd deposit_receipt.logs in
    assert (Solidity_contracts.Precompile.xtz_bridge = deposit_log.address) ;
    assert (revm_queued_xtz_deposit_event_topic = List.hd deposit_log.topics) ;
    (* Claim the queued deposit. *)
    let* () =
      Eth_cli.add_abi ~label:"claim_xtz" ~abi:(predep_xtz_bridge_abi_path ()) ()
    in
    let claim =
      Eth_cli.contract_send
        ~source_private_key:whale.private_key
        ~endpoint:(Evm_node.endpoint sequencer)
        ~abi_label:"claim_xtz"
        ~address:Solidity_contracts.Precompile.xtz_bridge
        ~method_call:(sf {|claim_xtz(%d)|} deposit_id)
    in
    let produce_block () = Rpc.produce_block sequencer in
    let* _res = wait_for_application ~produce_block claim in
    (* Check that the address with code was called when the claim happened. *)
    let* claim_receipt = get_one_receipt_from_latest_or_fail sequencer in
    let claim_logs : Transaction.tx_log list = claim_receipt.logs in
    let contract_call_log = List.nth claim_logs 0 in
    assert (String.lowercase_ascii address = contract_call_log.address) ;
    assert (eip7702_fallback_event_topic = List.hd contract_call_log.topics) ;
    let deposit_log = List.nth claim_logs 1 in
    assert (Solidity_contracts.Precompile.xtz_bridge = deposit_log.address) ;
    assert (revm_xtz_deposit_event_topic = List.hd deposit_log.topics) ;

    (* Check that the balance was properly deposited. *)
    let*@ balance = Rpc.get_balance ~address sequencer in
    Check.((balance = Wei.of_tez amount) Wei.typ)
      ~error_msg:"Expected balance of the address with code of %R, got %L wei" ;
    unit
  in
  (* The scenario has to work for both the EOA (EIP-7702) account and the
     regular smart contract. *)
  let* () = scenario_for ~address:sponsored.address ~deposit_id:0 in
  scenario_for ~address:eip7702_contract ~deposit_id:1

let test_eip7702_auto_sign =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "eip7702"; "auto_sign"]
    ~title:
      "Check EIP-7702's semantic correctness regarding auth and tx sender and \
       signer"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  let whale = Eth_account.bootstrap_accounts.(0) in
  let endpoint = Evm_node.endpoint sequencer in
  let* eip7702 = Solidity_contracts.eip7702 evm_version in
  let* () = Eth_cli.add_abi ~label:eip7702.label ~abi:eip7702.abi () in
  let* eip7702_contract, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:whale.private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi:eip7702.abi
         ~bin:eip7702.bin)
      sequencer
  in
  let* gas_price = Rpc.get_gas_price sequencer in
  let gas_price = Int32.to_int gas_price in
  let base_tx ~nonce ~authorization =
    Cast.craft_tx
      ~source_private_key:whale.private_key
      ~chain_id:1337
      ~nonce
      ~gas:100_000
      ~gas_price
      ~value:Wei.zero
      ~authorization
      ~address:whale.address
      ~arguments:[]
      ~legacy:false
      ()
  in
  let signed_auth ~nonce =
    Cast.wallet_sign_auth
      ~nonce
      ~authorization:eip7702_contract
      ~private_key:whale.private_key
      ~endpoint
      ()
  in
  let* authorization = signed_auth ~nonce:2 in
  let* raw_set_eoa = base_tx ~nonce:1 ~authorization in
  let*@ set_eoa_hash = Rpc.send_raw_transaction ~raw_tx:raw_set_eoa sequencer in
  let* _ = produce_block sequencer in
  let*@! Transaction.{type_; _} =
    Rpc.get_transaction_receipt ~tx_hash:set_eoa_hash sequencer
  in
  (* Type 4 = EIP-7702 *)
  Check.((type_ = Int32.of_int 4) int32)
    ~error_msg:"Expected tx.type of %R, got %L" ;
  (* We can retrieve the authorization list from the transaction object: *)
  let*@! Transaction.{authorizationList; _} =
    Rpc.get_transaction_by_hash ~transaction_hash:set_eoa_hash sequencer
  in
  (match authorizationList with
  | Some [{address; _}] ->
      Check.(
        (String.lowercase_ascii address
        = String.lowercase_ascii eip7702_contract)
          string)
        ~error_msg:"Expected msg.sender of %R, got %L"
  | Some _ -> failwith "Authorization list should only contain one element."
  | None -> failwith "Authorization list should not be empty.") ;
  let* call_eoa_hash =
    send_transaction_to_sequencer
      (Eth_cli.contract_send
         ~source_private_key:whale.private_key
         ~endpoint
         ~abi_label:eip7702.label
         ~address:whale.address
         ~method_call:"emitEvent()")
      sequencer
  in
  let*@! Transaction.{logs; _} =
    Rpc.get_transaction_receipt ~tx_hash:call_eoa_hash sequencer
  in
  let Transaction.{data; _} = List.hd logs in
  let data_without_padding = "0x" ^ String.sub data 26 40 in
  Check.(
    (String.lowercase_ascii data_without_padding
    = String.lowercase_ascii whale.address)
      string)
    ~error_msg:"Expected msg.sender of %R, got %L" ;
  unit

(* Per EIP-7702, a transaction carrying an authorization with a stale nonce
   should not be rejected by the node — the invalid authorization is silently
   skipped during execution. This test sends a type-4 tx to set a delegation,
   then sends a second type-4 tx reusing the same (now stale) authorization.
   The second tx must be accepted into a block. *)
let test_eip7702_stale_auth_is_skipped =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "eip7702"; "nonce"; "stale_auth"]
    ~title:"EIP-7702 stale authorization is skipped, not rejected"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  let whale = Eth_account.bootstrap_accounts.(0) in
  let authority = Eth_account.bootstrap_accounts.(1) in
  let endpoint = Evm_node.endpoint sequencer in
  (* Deploy the delegation contract. *)
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
  let whale_nonce = Int64.to_int whale_nonce in
  (* Authority signs an authorization at nonce 0. *)
  let* signed_auth =
    Cast.wallet_sign_auth
      ~nonce:0
      ~authorization:eip7702_contract
      ~private_key:authority.private_key
      ~endpoint
      ()
  in
  (* Build the eth_call JSON for gas estimation with the authorization list. *)
  let base_call =
    [("from", `String whale.address); ("to", `String authority.address)]
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
  let*@ gas = Rpc.estimate_gas (auth_list_json :: base_call) sequencer in
  let gas = Int64.to_int gas in
  (* TX1: whale sends type-4 tx with the fresh authorization.
     The bare call may revert (delegation contract has no fallback), but
     the authorization is processed regardless and sets the delegation. *)
  let* raw_tx1 =
    Cast.craft_tx
      ~source_private_key:whale.private_key
      ~chain_id:1337
      ~nonce:whale_nonce
      ~gas
      ~gas_price
      ~value:Wei.zero
      ~authorization:signed_auth
      ~address:authority.address
      ~arguments:[]
      ~legacy:false
      ()
  in
  let*@ tx1_hash = Rpc.send_raw_transaction ~raw_tx:raw_tx1 sequencer in
  let*@ _ = produce_block sequencer in
  let*@! _tx1_receipt =
    Rpc.get_transaction_receipt ~tx_hash:tx1_hash sequencer
  in
  (* Verify the delegation was set and authority nonce bumped. *)
  let*@ code = Rpc.get_code ~address:authority.address sequencer in
  let expected_code =
    "0xef0100" ^ String.(lowercase_ascii @@ sub eip7702_contract 2 40)
  in
  Check.((code = expected_code) string)
    ~error_msg:"Expected delegation code %R after TX1, got %L" ;
  let*@ authority_nonce =
    Rpc.get_transaction_count ~address:authority.address sequencer
  in
  Check.((authority_nonce = 1L) int64)
    ~error_msg:"Expected authority nonce of 1 after TX1, got %L" ;
  (* TX2: whale sends another type-4 tx reusing the SAME signed_auth.
     The authorization has nonce 0, but authority's nonce is now 1.
     Per EIP-7702 spec, the stale auth is silently skipped during execution.
     The node must accept and include this transaction — not reject it. *)
  let* raw_tx2 =
    Cast.craft_tx
      ~source_private_key:whale.private_key
      ~chain_id:1337
      ~nonce:(whale_nonce + 1)
      ~gas
      ~gas_price
      ~value:Wei.zero
      ~authorization:signed_auth
      ~address:authority.address
      ~arguments:[]
      ~legacy:false
      ()
  in
  let*@ tx2_hash = Rpc.send_raw_transaction ~raw_tx:raw_tx2 sequencer in
  let*@ _ = produce_block sequencer in
  (* The critical check: TX2 must have been included in a block.
     Getting a receipt proves the node accepted the transaction despite
     the stale authorization. *)
  let*@! tx2_receipt =
    Rpc.get_transaction_receipt ~tx_hash:tx2_hash sequencer
  in
  Check.((tx2_receipt.Transaction.type_ = Int32.of_int 4) int32)
    ~error_msg:"Expected TX2 to be type 4, got %L" ;
  (* The delegation should still be intact — the stale auth was skipped,
     it did not overwrite the existing delegation. *)
  let*@ code_after = Rpc.get_code ~address:authority.address sequencer in
  Check.((code_after = expected_code) string)
    ~error_msg:"Expected delegation code unchanged after stale auth, got %L" ;
  (* The stale authorization must not have bumped the authority nonce. *)
  let*@ authority_nonce_after =
    Rpc.get_transaction_count ~address:authority.address sequencer
  in
  Check.((authority_nonce_after = 1L) int64)
    ~error_msg:"Expected authority nonce still 1 after stale auth, got %L" ;
  unit

let protocols = [Protocol.Alpha]

let () =
  test_da_fee_drain_via_eip7702 protocols ;
  test_eip7702 [Alpha] ;
  test_deposits_on_eip7702_accounts [Alpha] ;
  test_eip7702_auto_sign [Alpha] ;
  test_eip7702_stale_auth_is_skipped [Alpha]
