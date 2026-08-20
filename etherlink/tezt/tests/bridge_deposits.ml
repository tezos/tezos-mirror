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
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file bridge_deposits.ml
   Subject:      FA and XTZ deposits, claims, deposit events and the watchtower.
*)

open Rpc.Syntax
open Contract_path
open Transaction
open Delayed_inbox
open Test_helpers
open Setup
open Fa_bridge_helpers
module Protocol = Test_helpers.Protocol_no_direct_register

let test_fa_bridge_feature_flag =
  register_all
    ~__FILE__
    ~tags:["fa_bridge"; "feature_flag"]
    ~title:"FA bridge feature is set in storage"
    ~enable_fa_bridge:true
  @@ fun {sequencer; kernel; _} _protocol ->
  (* We simply check that the flag is set in the storage. *)
  let path = Durable_storage_path.enable_fa_bridge kernel in
  let*@ flag = Rpc.state_value sequencer path in
  Check.is_true
    (Option.is_some flag)
    ~error_msg:(sf "Expected to have a value at %s" path) ;
  unit

let capture_logs ~header logs =
  Regression.capture (sf "# %s" header) ;
  List.iteri
    (fun i tx_log ->
      Regression.capture (sf "## Log %d" i) ;
      Regression.capture (sf "Address: %s" tx_log.Transaction.address) ;
      Regression.capture (sf "Topics:") ;
      List.iter (fun t -> Regression.capture (sf "- %s" t)) tx_log.topics ;
      Regression.capture (sf "Data: %s" tx_log.data))
    logs

(* {Note timestamp}

   Some events emitted by Etherlink contains timestamp. In order for the
   regression test to always generate the same trace, we control the timestamp
   of each L2 blocks using the {!timestamp_generator} helper. *)

let timestamp_generator () =
  let block_count = ref 0 in
  fun () ->
    let res =
      sf "2020-01-01T00:%02d:%02dZ" (!block_count / 60) (!block_count mod 60)
    in
    incr block_count ;
    res

let test_deposit_event =
  Protocol.register_regression_test
    ~__FILE__
    ~tags:["evm"; "deposit"; "event"]
    ~title:"Regression test for the deposit event"
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
      ])
  @@ fun protocol ->
  (* See {Note timestamp} *)
  let next_timestamp = timestamp_generator () in
  let genesis_timestamp = next_timestamp () in
  let* {sequencer; l1_contracts; sc_rollup_node; sc_rollup_address; client; _} =
    Setup.setup_sequencer
      ~genesis_timestamp:Client.(At (Time.of_notation_exn genesis_timestamp))
      ~time_between_blocks:Nothing
      ~enable_dal:false
      protocol
  in
  (* Send a deposit to the delayed inbox *)
  let deposit_info =
    {
      receiver = EthereumAddr Eth_account.bootstrap_accounts.(0).address;
      chain_id = None;
    }
  in
  let* () =
    send_deposit_to_delayed_inbox
      ~amount:Tez.one
      ~bridge:l1_contracts.bridge
      ~depositor:Constant.bootstrap5
      ~deposit_info
      ~sc_rollup_node
      ~sc_rollup_address
      client
  in
  (* Wait for the sequencer to detect and include the deposit. *)
  let* () =
    wait_for_delayed_inbox_add_tx_and_injected
      ~sequencer
      ~sc_rollup_node
      ~client
  in

  (* Fetch the deposit events from the block that included the deposit *)
  let* receipt = get_one_receipt_from_latest_or_fail sequencer in

  (* Capture the log *)
  capture_logs ~header:"XTZ Deposit" receipt.logs ;

  unit

let test_withdrawal_events =
  Protocol.register_regression_test
    ~__FILE__
    ~tags:["evm"; "withdrawal"; "event"]
    ~title:"Regression test for the withdrawal events"
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
      ])
  @@ fun protocol ->
  (* See {Note timestamp} *)
  let next_timestamp = timestamp_generator () in
  let genesis_timestamp = next_timestamp () in
  let* {sequencer; _} =
    Setup.setup_sequencer
      ~genesis_timestamp:Client.(At (Time.of_notation_exn genesis_timestamp))
      ~time_between_blocks:Nothing
      ~enable_dal:false
      ~enable_fast_withdrawal:true
      ~instant_confirmations:true
      protocol
  in

  (* Make a regular withdrawal *)
  let* () =
    Eth_cli.add_abi ~label:"withdraw" ~abi:(predep_xtz_bridge_abi_path ()) ()
  in
  let* _ =
    send_transaction_to_sequencer
      ~timestamp:(next_timestamp ())
      (Eth_cli.contract_send
         ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi_label:"withdraw"
         ~address:Solidity_contracts.Precompile.xtz_bridge
         ~method_call:
           (sf {|withdraw_base58("%s")|} Constant.bootstrap5.public_key_hash)
         ~value:Wei.one_eth
         ~gas:16_000_000)
      sequencer
  in

  (* Fetch the withdrawal event *)
  let* receipt = get_one_receipt_from_latest_or_fail sequencer in

  (* Capture the log *)
  capture_logs ~header:"XTZ Withdrawal" receipt.logs ;

  (* Do the same thing for fast withdrawal *)
  let* () =
    Eth_cli.add_abi
      ~label:"fast_withdraw_base58"
      ~abi:(predep_xtz_bridge_abi_path ())
      ()
  in
  let* _ =
    send_transaction_to_sequencer
      ~timestamp:(next_timestamp ())
      (Eth_cli.contract_send
         ~source_private_key:Eth_account.bootstrap_accounts.(1).private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi_label:"fast_withdraw_base58"
         ~address:Solidity_contracts.Precompile.xtz_bridge
         ~method_call:
           (sf
              {|fast_withdraw_base58("%s","%s","%s")|}
              Constant.bootstrap5.public_key_hash
              "KT1TczPwz5KjAuuJKvkTmttS7bBioT5gjQ4Y"
              "0x0000000000000000000000000000000000000000000000000000000000000001")
         ~value:Wei.one_eth
         ~gas:16_000_000)
      sequencer
  in

  (* Fetch the fast withdrawal event *)
  let* receipt = get_one_receipt_from_latest_or_fail sequencer in

  (* Capture the log *)
  capture_logs ~header:"XTZ Fast Withdrawal" receipt.logs ;

  unit

let test_fa_deposit_and_withdrawals_events =
  Protocol.register_regression_test
    ~__FILE__
    ~tags:["evm"; "fa_bridge"; "event"]
    ~title:"Regression test for the FA deposit and withdrawal events"
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
        Constant.octez_codec;
      ])
  @@ fun protocol ->
  (* See {Note timestamp} *)
  let next_timestamp = timestamp_generator () in
  let* {sequencer; l1_contracts; sc_rollup_node; sc_rollup_address; client; _} =
    let genesis_timestamp = next_timestamp () in
    Setup.setup_sequencer
      ~genesis_timestamp:Client.(At (Time.of_notation_exn genesis_timestamp))
      ~time_between_blocks:Nothing
      ~enable_dal:false
      ~enable_fa_bridge:true
      ~enable_fast_fa_withdrawal:true
      ~instant_confirmations:true
      protocol
  in
  (* Send a FA deposit to the delayed inbox *)
  let* () =
    send_fa_deposit_to_delayed_inbox
      ~amount:42
      ~l1_contracts
      ~depositor:Constant.bootstrap5
      ~receiver:Eth_account.bootstrap_accounts.(0).address
      ~sc_rollup_node
      ~sc_rollup_address
      client
  in
  (* Bake two blocks to let the sequencer see the FA deposit *)
  let* () =
    (* Keep the following number >= 4, otherwise the test becomes flaky. *)
    repeat 4 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  (* Produce an Etherlink block *)
  let*@ nb_txns = produce_block ~timestamp:(next_timestamp ()) sequencer in
  Check.(
    (nb_txns = 1)
      int
      ~error_msg:
        "Expecting the block to contain %R transaction (the deposit), got %L") ;

  (* Fetch the deposit events *)
  let* receipt = get_one_receipt_from_latest_or_fail sequencer in

  (* Capture the log *)
  capture_logs ~header:"FA Deposit" receipt.logs ;

  (* Now, we can do a withdrawal *)
  let* ticketer = ticket_creator l1_contracts.ticket_router_tester in
  let* content = ticket_content 0 in
  (* Withdrawing to the zero implicit account *)
  let routing_info =
    String.concat
      ""
      [
        "00000000000000000000000000000000000000000000";
        ticketer |> Hex.of_bytes |> Hex.show;
      ]
  in
  let* _ =
    call_fa_withdraw
      ~timestamp:(next_timestamp ())
      ~sender:Eth_account.bootstrap_accounts.(0)
      ~ticket_owner:Eth_account.bootstrap_accounts.(0).address
      ~endpoint:(Evm_node.endpoint sequencer)
      ~evm_node:sequencer
      ~routing_info
      ~amount:40
      ~ticketer:(ticketer |> Hex.of_bytes |> Hex.show)
      ~content:(content |> Hex.of_bytes |> Hex.show)
      ()
  in

  (* Fetch the deposit events *)
  let* receipt = get_one_receipt_from_latest_or_fail sequencer in

  (* Capture the log *)
  capture_logs ~header:"FA Withdrawal" receipt.logs ;

  let* fast_withdrawal_contract_address =
    Client.originate_contract
      ~alias:"fast_withdrawal_contract_address"
      ~amount:Tez.zero
      ~src:Constant.bootstrap5.public_key_hash
      ~init:
        (sf
           "Pair {} (Pair %S %S -1) {}"
           l1_contracts.exchanger
           sc_rollup_address)
      ~prg:(fast_withdrawal_path ())
      ~burn_cap:Tez.one
      client
  in
  let* () = Client.bake_for_and_wait ~keys:[] client in

  (* Withdrawing to the zero implicit account *)
  let* _ =
    call_fa_fast_withdraw
      ~timestamp:(next_timestamp ())
      ~sender:Eth_account.bootstrap_accounts.(0)
      ~sequencer
      ~ticket_owner:Eth_account.bootstrap_accounts.(0).address
      ~amount:2
      ~ticketer:(ticketer |> Hex.of_bytes |> Hex.show)
      ~content:(content |> Hex.of_bytes |> Hex.show)
      ~receiver:Constant.bootstrap5.public_key_hash
      ~fast_withdrawal_contract_address
      ()
  in

  (* Fetch the deposit events *)
  let* receipt = get_one_receipt_from_latest_or_fail sequencer in

  (* Capture the log *)
  capture_logs ~header:"FA Fast Withdrawal" receipt.logs ;
  unit

let get_deposit_nonce_from_latest_block evm_node =
  let*@ block =
    Rpc.get_block_by_number ~full_tx_objects:true ~block:"latest" evm_node
  in

  let*@ json =
    match block.transactions with
    | Block.Full (tx :: _) ->
        Rpc.trace_transaction
          ~transaction_hash:tx.Transaction.hash
          ~tracer:"callTracer"
          evm_node
    | _ -> Test.fail "Inconsistent result"
  in

  let data =
    JSON.(json |-> "logs" |> as_list |> List.hd |-> "data" |> as_string)
  in
  let cleaned = String.sub data 2 (String.length data - 2) in
  let nonce_string = String.sub cleaned 0 32 in
  let nonce = Z.of_string_base 16 nonce_string in
  return nonce

let produce_proxy_owned_fa_deposit_and_claim ~client ~sequencer
    ~sc_rollup_address ~evm_version ~l1_contracts ?(regression = false) () =
  let* ticketer_bytes = ticket_creator l1_contracts.fa_deposit in
  let* content_bytes = ticket_content 0 in
  let ticketer = ticketer_bytes |> Hex.of_bytes |> Hex.show in
  let content = content_bytes |> Hex.of_bytes |> Hex.show in

  let* proxy = Solidity_contracts.etherlink_fa_proxy_mock evm_version in

  let* proxy, _tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~args:(sf {|["0x%s", "0x%s"]|} ticketer content)
         ~abi:proxy.abi
         ~bin:proxy.bin)
      sequencer
  in

  Log.info "Proxy: %s" proxy ;

  let proxy_without_0x = String.sub proxy 2 (String.length proxy - 2) in

  let* () =
    Client.transfer
      ~giver:Constant.bootstrap4.public_key_hash
      ~receiver:l1_contracts.fa_deposit
      ~arg:
        (sf
           {|Pair 50 (Pair 0x%s%s "%s")|}
           proxy_without_0x
           proxy_without_0x
           sc_rollup_address)
      ~amount:Tez.zero
      ~burn_cap:Tez.one
      client
  in
  let* () = repeat 5 (fun () -> Client.bake_for_and_wait client) in

  let*@ _ = produce_block sequencer in

  let* receipt = get_one_receipt_from_latest_or_fail sequencer in

  if regression then capture_logs ~header:"Queue deposit" receipt.logs ;

  let* nonce = get_deposit_nonce_from_latest_block sequencer in

  let* () =
    Eth_cli.add_abi ~label:"claim" ~abi:(predep_fa_bridge_abi_path ()) ()
  in

  let claim =
    Eth_cli.contract_send
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~endpoint:(Evm_node.endpoint sequencer)
      ~abi_label:"claim"
      ~address:Solidity_contracts.Precompile.fa_bridge
      ~method_call:(sf {|claim(%d)|} (Z.to_int nonce))
      ~value:Wei.zero
  in
  let produce_block () = Rpc.produce_block sequencer in
  let* _res = wait_for_application ~produce_block claim in
  return (nonce, proxy, ticketer, content)

let test_fa_deposit_can_be_claimed_and_withdrawn =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "fa_deposit"; "claim"; "withdraw"]
    ~time_between_blocks:Nothing
    ~use_dal:Register_without_feature
    ~enable_fa_bridge:true
    ~maximum_allowed_ticks:2_000_000_000L
    ~title:"FA deposit can be claimed and withdrawn"
    ~additional_uses:[Constant.octez_codec]
  @@
  fun {client; sequencer; sc_rollup_address; evm_version; l1_contracts; _}
      _protocol
    ->
  let* nonce, proxy, ticketer, content =
    produce_proxy_owned_fa_deposit_and_claim
      ~client
      ~sequencer
      ~sc_rollup_address
      ~evm_version
      ~l1_contracts
      ()
  in

  let*@ block =
    Rpc.get_block_by_number ~full_tx_objects:true ~block:"latest" sequencer
  in

  let*@ _receipt =
    match block.transactions with
    | Block.Full (tx :: _) ->
        Rpc.get_transaction_receipt ~tx_hash:tx.Transaction.hash sequencer
    | _ -> Test.fail "Inconsistent result"
  in

  let* dummy_l1_receiver = ticket_creator Constant.bootstrap4.public_key_hash in
  let routing_info =
    String.concat "" [dummy_l1_receiver |> Hex.of_bytes |> Hex.show; ticketer]
  in

  let account = Eth_account.bootstrap_accounts.(0) in
  let* _tx =
    call_fa_withdraw
      ~sender:account
      ~endpoint:(Evm_node.endpoint sequencer)
      ~evm_node:sequencer
      ~ticket_owner:proxy
      ~routing_info
      ~amount:25
      ~ticketer
      ~content
      ()
  in

  let* () =
    expect_failure "Claiming the same deposit twice should fail" @@ fun () ->
    Eth_cli.contract_send
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~endpoint:(Evm_node.endpoint sequencer)
      ~abi_label:"claim"
      ~address:Solidity_contracts.Precompile.fa_bridge
      ~method_call:(sf {|claim(%d)|} (Z.to_int nonce))
      ~value:Wei.zero
      ()
  in

  let* () =
    expect_failure "Invalid claim should fail" @@ fun () ->
    Eth_cli.contract_send
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~endpoint:(Evm_node.endpoint sequencer)
      ~abi_label:"claim"
      ~address:Solidity_contracts.Precompile.fa_bridge
      ~method_call:(sf {|claim(42)|})
      ~value:Wei.zero
      ()
  in

  unit

let test_fast_fa_deposit_can_be_claimed_and_withdrawn =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "fa_deposit"; "claim"; "withdraw"]
    ~time_between_blocks:Nothing
    ~use_dal:Register_without_feature
    ~enable_fa_bridge:true
    ~maximum_allowed_ticks:2_000_000_000L
    ~title:"Fast FA deposit can be claimed and withdrawn"
    ~additional_uses:[Constant.octez_codec]
  @@
  fun {client; sequencer; sc_rollup_address; evm_version; l1_contracts; _}
      _protocol
    ->
  let* _nonce, proxy, ticketer, content =
    produce_proxy_owned_fa_deposit_and_claim
      ~client
      ~sequencer
      ~sc_rollup_address
      ~evm_version
      ~l1_contracts
      ()
  in

  let*@ block =
    Rpc.get_block_by_number ~full_tx_objects:true ~block:"latest" sequencer
  in

  let*@ _receipt =
    match block.transactions with
    | Block.Full (tx :: _) ->
        Rpc.get_transaction_receipt ~tx_hash:tx.Transaction.hash sequencer
    | _ -> Test.fail "Inconsistent result"
  in

  let dummy_l1_receiver = Constant.bootstrap4.public_key_hash in
  let dummy_fast_withdrawal = Constant.bootstrap3.public_key_hash in

  let* _tx =
    call_fa_fast_withdraw
      ~sequencer
      ~sender:Eth_account.bootstrap_accounts.(0)
      ~ticket_owner:proxy
      ~amount:25
      ~ticketer
      ~content
      ~fast_withdrawal_contract_address:dummy_fast_withdrawal
      ~receiver:dummy_l1_receiver
      ()
  in

  unit

let test_claim_deposit_event =
  Protocol.register_regression_test
    ~__FILE__
    ~tags:["evm"; "fa_bridge"; "claim"; "deposit"; "event"]
    ~title:"Regression test for the claimed FA deposit event"
    ~uses:(fun _protocol ->
      [
        Constant.octez_codec;
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
      ])
  @@ fun protocol ->
  (* See {Note timestamp} *)
  let next_timestamp = timestamp_generator () in
  let* {sequencer; sc_rollup_address; client; evm_version; l1_contracts; _} =
    let genesis_timestamp = next_timestamp () in
    Setup.setup_sequencer
      ~genesis_timestamp:Client.(At (Time.of_notation_exn genesis_timestamp))
      ~time_between_blocks:Nothing
      ~enable_dal:false
      ~enable_fa_bridge:true
      ~enable_fast_fa_withdrawal:true
      protocol
  in
  let* _ =
    produce_proxy_owned_fa_deposit_and_claim
      ~client
      ~sequencer
      ~sc_rollup_address
      ~evm_version
      ~l1_contracts
      ~regression:true
      ()
  in
  let* receipt = get_one_receipt_from_latest_or_fail sequencer in
  capture_logs ~header:"Claimed deposit" receipt.logs ;
  unit

let test_fa_deposit_watchtower =
  register_test_for_kernels
    ~__FILE__
    ~tags:["evm"; "fa_deposit"; "watchtower"]
    ~title:"FA deposit is claimed by the watchtower"
    ~enable_fa_bridge:true
    ~enable_dal:false
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
    ~additional_uses:[Constant.octez_codec; Constant.watchtower]
    ~websockets:true
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        evm_version;
        kernel;
        _;
      }
      _protocol
    ->
  (* The following test is hack-ish to mimic the behavior of FA deposits + a proxy involved.
     Deploying a proper proxy is pretty tedious, so we will deploy a dummy one that just
     does nothing and succeeds so we can test the behavior of the watchtower.
     BACKLOG (for better testing):
     - Deploy an actual ERC20 proxy like:
       https://explorer.etherlink.com/address/0x9121B153bbCF8C23F20eE43b494F08760B91aD64?tab=contract
     - Deploy the according L1 contract and use its data as the constructor of the proxy contract.
     - At the end of the test also check the receiver's balance on the proxy contract. *)
  let whale = Eth_account.bootstrap_accounts.(0) in
  let* dummy_proxy = Solidity_contracts.dummy_proxy evm_version in
  let* () = Eth_cli.add_abi ~label:dummy_proxy.label ~abi:dummy_proxy.abi () in
  (* Dummy proxy just to enable the two step process queue/claim for the watchtower. *)
  let* proxy, _tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:whale.private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi:dummy_proxy.abi
         ~bin:dummy_proxy.bin)
      sequencer
  in
  let amount = 1125 in
  let receiver = "0x1074Fd1EC02cbeaa5A90450505cF3B48D834f3EB" in
  let* _watchtower =
    let config =
      Watchtower.
        {
          evm_node_endpoint = Evm_node.endpoint sequencer;
          gas_limit = None;
          max_fee_per_gas = None;
          rpc = None;
          secret_key = Some whale.private_key;
          whitelist = Some [{proxy; ticket_hashes = None}];
        }
    in
    Watchtower.init ~config ~first_block:1 ()
  in
  let* () =
    send_fa_deposit_to_delayed_inbox
      ~proxy
      ~amount
      ~l1_contracts
      ~depositor:Constant.bootstrap5
      ~receiver
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
  let* () =
    (* We let some time to the watchtower to inject the claim. This avoids flakyness. *)
    repeat 4 (fun () ->
        let* _ = produce_block sequencer in
        unit)
  in
  (* We fetch the only new ticket hash that was produced via the durable storage. *)
  let*@ ticket_hashes =
    Rpc.state_subkeys
      sequencer
      (Durable_storage_path.Ticket_table.ticket_table kernel)
  in
  let ticket_hash =
    match ticket_hashes with
    | Some [t] -> t
    | _ -> failwith "ticket hash not found"
  in
  (* Since the proxy call succeeded the owner is the proxy, we make sure of that.
     NB: If the proxy was not a dummy one we could check the ERC20 balance of the
     receiver's address as an additional check. *)
  let* ticket_balance_via_sequencer =
    ticket_balance ~kernel ~ticket_hash ~account:proxy (Either.Right sequencer)
  in
  Check.((amount = ticket_balance_via_sequencer) int)
    ~error_msg:
      "After deposit we expect %L ticket balance in the sequencer, got %R" ;
  unit

let test_xtz_deposit_watchtower =
  (* TODO: When 6.2 is released, replace `register_test` by
     `register_test_for_kernels`.*)
  register_test
    ~__FILE__
    ~kernel:Kernel.Latest
    ~tags:["evm"; "xtz_deposit"; "watchtower"]
    ~title:"XTZ deposit is claimed by the watchtower"
    ~enable_fa_bridge:true
    ~enable_dal:false
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
    ~additional_uses:[Constant.octez_codec; Constant.watchtower]
    ~websockets:true
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        evm_version;
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
  let* _watchtower =
    let config =
      Watchtower.
        {
          evm_node_endpoint = Evm_node.endpoint sequencer;
          gas_limit = None;
          max_fee_per_gas = None;
          rpc = None;
          secret_key = Some whale.private_key;
          whitelist = None;
        }
    in
    Watchtower.init ~config ~first_block:1 ()
  in
  let scenario_for ~address =
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
    let* () =
      (* We let some time to the watchtower to inject the claim. This avoids flakyness. *)
      repeat 4 (fun () ->
          let* _ = produce_block sequencer in
          unit)
    in
    (* Check that the balance was properly deposited. *)
    let*@ balance = Rpc.get_balance ~address sequencer in
    Check.((balance = Wei.of_tez amount) Wei.typ)
      ~error_msg:"Expected balance of the address with code of %R, got %L wei" ;
    unit
  in
  (* The scenario has to work for both the EOA (EIP-7702) account and the
     regular smart contract. *)
  let* () = scenario_for ~address:sponsored.address in
  scenario_for ~address:eip7702_contract

let test_xtz_deposit_watchtower_with_fa_whitelist =
  register_test
    ~__FILE__
    ~kernel:Kernel.Latest
    ~tags:["evm"; "xtz_deposit"; "watchtower"; "whitelist"]
    ~title:
      "XTZ deposit is claimed by the watchtower even when an FA whitelist is \
       configured"
    ~enable_fa_bridge:true
    ~enable_dal:false
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
    ~additional_uses:[Constant.watchtower]
    ~websockets:true
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        evm_version;
        _;
      }
      _protocol
    ->
  let whale = Eth_account.bootstrap_accounts.(0) in
  (* Deploy a dummy proxy so we can configure the watchtower with a
     non-empty FA whitelist. *)
  let* dummy_proxy = Solidity_contracts.dummy_proxy evm_version in
  let* () = Eth_cli.add_abi ~label:dummy_proxy.label ~abi:dummy_proxy.abi () in
  let* proxy, _tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:whale.private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi:dummy_proxy.abi
         ~bin:dummy_proxy.bin)
      sequencer
  in
  (* Deploy an eip7702 contract so we have an address with code to receive
     the XTZ deposit (the two-step queue/claim path). *)
  let* eip7702 = Solidity_contracts.eip7702_fallback evm_version in
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
  (* Start the watchtower with an FA whitelist. Before the fix, the
     whitelist topic filter would prevent XTZ deposit logs (which have no
     indexed ticket_hash/proxy topics) from being returned by getLogs,
     so the watchtower would never see or claim them. *)
  let rpc_port = Port.fresh () in
  let* _watchtower =
    let config =
      Watchtower.
        {
          evm_node_endpoint = Evm_node.endpoint sequencer;
          gas_limit = None;
          max_fee_per_gas = None;
          rpc = Some {addr = "127.0.0.1"; port = rpc_port};
          secret_key = Some whale.private_key;
          whitelist = Some [{proxy; ticket_hashes = None}];
        }
    in
    Watchtower.init ~config ~first_block:1 ()
  in
  let depositor = Constant.bootstrap5 in
  let amount = Tez.of_int 1125 in
  let deposit_info =
    {receiver = EthereumAddr eip7702_contract; chain_id = None}
  in
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
  (* The deposit goes to an address with code, so it is queued, not
     credited directly. Balance should still be zero. *)
  let*@ balance = Rpc.get_balance ~address:eip7702_contract sequencer in
  Check.((balance = Wei.zero) Wei.typ)
    ~error_msg:"Expected zero balance before claim, got %L wei" ;
  let* () =
    repeat 4 (fun () ->
        let* _ = produce_block sequencer in
        unit)
  in
  (* After the watchtower claims, the balance should match the deposit. *)
  let*@ balance = Rpc.get_balance ~address:eip7702_contract sequencer in
  Check.((balance = Wei.of_tez amount) Wei.typ)
    ~error_msg:
      "Expected balance of %R after watchtower claim, got %L wei (XTZ deposit \
       was likely not seen due to whitelist topic filter)" ;
  (* Also verify the deposit appears in GET /deposits?receiver=... — this
     exercises the list_by_receiver query which had the same whitelist JOIN
     bug as get_unclaimed. *)
  let* deposits_json =
    let url =
      sf "http://127.0.0.1:%d/deposits?receiver=%s" rpc_port eip7702_contract
    in
    Curl.get_raw ~name:"watchtower_get_deposits" url |> Runnable.run
  in
  let deposits = JSON.parse ~origin:"watchtower_get_deposits" deposits_json in
  Check.((JSON.as_list deposits |> List.length = 1) int)
    ~error_msg:
      "Expected 1 deposit in GET /deposits?receiver=..., got %L (XTZ deposit \
       was likely excluded by whitelist JOIN in list_by_receiver query)" ;
  unit

let protocols = [Protocol.Alpha]

let () =
  test_fa_bridge_feature_flag protocols ;
  test_deposit_event [Alpha] ;
  test_withdrawal_events [Alpha] ;
  test_fa_deposit_and_withdrawals_events [Alpha] ;
  test_fa_deposit_can_be_claimed_and_withdrawn [Alpha] ;
  test_fast_fa_deposit_can_be_claimed_and_withdrawn [Alpha] ;
  test_claim_deposit_event [Alpha] ;
  test_fa_deposit_watchtower [Alpha] ;
  test_xtz_deposit_watchtower [Alpha] ;
  test_xtz_deposit_watchtower_with_fa_whitelist [Alpha]
