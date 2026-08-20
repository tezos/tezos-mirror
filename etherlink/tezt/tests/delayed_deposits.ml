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
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file delayed_deposits.ml
   Subject:      Deposits going through the delayed inbox.
*)

open Rpc.Syntax
open Contract_path
open Delayed_inbox
open Test_helpers
open Setup
open Fa_bridge_helpers
module Protocol = Test_helpers.Protocol_no_direct_register

let test_delayed_deposit_is_included =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "inclusion"; "deposit"]
    ~title:"Delayed deposit is included"
    ~use_dal:ci_enabled_dal_registration
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
  let endpoint = Evm_node.endpoint sequencer in

  let amount = Tez.of_int 16 in
  let depositor = Constant.bootstrap5 in
  let receiver =
    Eth_account.
      {
        address = "0x1074Fd1EC02cbeaa5A90450505cF3B48D834f3EB";
        private_key =
          "0xb7c548b5442f5b28236f0dcd619f65aaaafd952240908adcf9642d8e616587ee";
      }
  in
  let* receiver_balance_prev =
    Eth_cli.balance ~account:receiver.address ~endpoint ()
  in
  let deposit_info =
    {receiver = EthereumAddr receiver.address; chain_id = None}
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
  let* receiver_balance_next =
    Eth_cli.balance ~account:receiver.address ~endpoint ()
  in
  Check.((receiver_balance_next > receiver_balance_prev) Wei.typ)
    ~error_msg:"Expected a bigger balance" ;
  unit

let test_empty_deposit_info_does_not_block_valid_deposits =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~da_fee:arb_da_fee_for_delayed_inbox
      (* TODO: Remove once the Mainnet kernel has been updated *)
    ~kernels:[Latest]
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "deposit"; "empty"]
    ~title:"Empty deposit info does not block valid deposits"
    ~use_dal:ci_enabled_dal_registration
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
  let endpoint = Evm_node.endpoint sequencer in
  let amount = Tez.of_int 16 in
  let depositor = Constant.bootstrap5 in
  let receiver =
    Eth_account.
      {
        address = "0x1074Fd1EC02cbeaa5A90450505cF3B48D834f3EB";
        private_key =
          "0xb7c548b5442f5b28236f0dcd619f65aaaafd952240908adcf9642d8e616587ee";
      }
  in
  let* receiver_balance_prev =
    Eth_cli.balance ~account:receiver.address ~endpoint ()
  in
  (* First, send a deposit with empty deposit info (without baking). *)
  let* () =
    Client.transfer
      ~entrypoint:"deposit"
      ~arg:(sf "Pair %S 0x" sc_rollup_address)
      ~amount
      ~giver:depositor.Account.public_key_hash
      ~receiver:l1_contracts.bridge
      ~burn_cap:Tez.one
      client
  in
  (* Then, send a valid deposit. Both will be included in the same L1 block
     since we have not baked yet. *)
  let deposit_info =
    {receiver = EthereumAddr receiver.address; chain_id = None}
  in
  let* () =
    send_deposit_to_delayed_inbox
      ~amount
      ~bridge:l1_contracts.bridge
      ~depositor:
        (* Use a different depositor to avoid counter collision. *)
        Constant.bootstrap4
      ~deposit_info
      ~sc_rollup_node
      ~sc_rollup_address
      client
  in
  (* Wait for the valid deposit to be picked up and included. *)
  let* () =
    wait_for_delayed_inbox_add_tx_and_injected
      ~sequencer
      ~sc_rollup_node
      ~client
  in
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in
  let* receiver_balance_next =
    Eth_cli.balance ~account:receiver.address ~endpoint ()
  in
  Check.((receiver_balance_next > receiver_balance_prev) Wei.typ)
    ~error_msg:
      "Expected a bigger balance after the valid deposit, the empty deposit \
       info should not have blocked it" ;
  unit

let test_delayed_fa_deposit_is_included =
  register_all
    ~__FILE__
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:
      [
        "evm"; "sequencer"; "delayed_inbox"; "inclusion"; "fa_deposit"; "enabled";
      ]
    ~title:"Delayed FA deposit is included"
    ~enable_fa_bridge:true
    ~kernels:[Kernel.Latest]
    ~additional_uses:[Constant.octez_codec]
    ~time_between_blocks:Nothing
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        kernel;
        _;
      }
      _protocol
    ->
  (* let endpoint = Evm_node.endpoint sequencer in *)
  let amount = 42 in
  let depositor = Constant.bootstrap5 in
  let receiver = "0x1074Fd1EC02cbeaa5A90450505cF3B48D834f3EB" in

  let* () =
    send_fa_deposit_to_delayed_inbox
      ~amount
      ~l1_contracts
      ~depositor
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

  let* zero_ticket_hash = ticket_hash l1_contracts.ticket_router_tester 0 in

  let* ticket_balance_via_rollup_node =
    ticket_balance
      ~kernel
      ~ticket_hash:zero_ticket_hash
      ~account:receiver
      (Either.Left sc_rollup_node)
  in
  Check.((amount = ticket_balance_via_rollup_node) int)
    ~error_msg:
      "After deposit we expect %L ticket balance in the rollup node, got %R" ;
  let* ticket_balance_via_sequencer =
    ticket_balance
      ~kernel
      ~ticket_hash:zero_ticket_hash
      ~account:receiver
      (Either.Right sequencer)
  in
  Check.((amount = ticket_balance_via_sequencer) int)
    ~error_msg:
      "After deposit we expect %L ticket balance in the sequencer, got %R" ;
  unit

let test_delayed_fa_deposit_is_ignored_if_feature_disabled =
  register_all
    ~__FILE__
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:
      [
        "evm";
        "sequencer";
        "delayed_inbox";
        "inclusion";
        "fa_deposit";
        "disabled";
      ]
    ~title:"Delayed FA deposit is ignored if bridge feature is disabled"
    ~enable_fa_bridge:false
    ~kernels:[Kernel.Latest]
    ~additional_uses:[Constant.octez_codec]
    ~time_between_blocks:Nothing
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        kernel;
        _;
      }
      _protocol
    ->
  (* let endpoint = Evm_node.endpoint sequencer in *)
  let amount = 42 in
  let depositor = Constant.bootstrap5 in
  let receiver = "0x1074Fd1EC02cbeaa5A90450505cF3B48D834f3EB" in

  let* () =
    send_fa_deposit_to_delayed_inbox
      ~amount
      ~l1_contracts
      ~depositor
      ~receiver
      ~sc_rollup_node
      ~sc_rollup_address
      client
  in
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in

  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in

  let* zero_ticket_hash = ticket_hash l1_contracts.ticket_router_tester 0 in

  let* ticket_balance_via_rollup_node =
    ticket_balance
      ~kernel
      ~ticket_hash:zero_ticket_hash
      ~account:receiver
      (Either.Left sc_rollup_node)
  in
  Check.((0 = ticket_balance_via_rollup_node) int)
    ~error_msg:
      "The deposit should have been refused by rollup node, but balance is %R" ;
  let* ticket_balance_via_sequencer =
    ticket_balance
      ~kernel
      ~ticket_hash:zero_ticket_hash
      ~account:receiver
      (Either.Right sequencer)
  in
  Check.((0 = ticket_balance_via_sequencer) int)
    ~error_msg:
      "The deposit should have been refused by sequencer, but balance is %R" ;
  unit

let test_fa_withdrawal_is_included =
  register_all
    ~__FILE__
    ~da_fee:(Wei.of_string "1_000_000_000_000")
    ~tags:
      [
        "evm";
        "sequencer";
        "delayed_outbox";
        "inclusion";
        "fa_withdrawal";
        "enabled";
      ]
    ~title:"FA withdrawal is included"
    ~enable_fa_bridge:true
    ~commitment_period:5
    ~challenge_window:5
    ~kernels:[Kernel.Latest]
    ~time_between_blocks:Nothing
    ~additional_uses:[Constant.octez_codec]
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        kernel;
        _;
      }
      _protocol
    ->
  (* 1. Deposit some tickets *)
  let amount = 42 in
  let depositor = Constant.bootstrap5 in
  let receiver = Eth_account.bootstrap_accounts.(0).address in

  let* () =
    send_fa_deposit_to_delayed_inbox
      ~amount
      ~l1_contracts
      ~depositor
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

  (* Check that deposit is successful *)
  let* zero_ticket_hash = ticket_hash l1_contracts.ticket_router_tester 0 in

  let* ticket_balance_after_deposit =
    ticket_balance
      ~kernel
      ~ticket_hash:zero_ticket_hash
      ~account:receiver
      (Either.Right sequencer)
  in
  Check.((amount = ticket_balance_after_deposit) int)
    ~error_msg:
      "After deposit we expect %L ticket balance in the sequencer, got %R" ;

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

  (* Initiate withdrawal *)
  let* withdrawal_level = Client.level client in
  let* _tx =
    call_fa_withdraw
      ~sender:Eth_account.bootstrap_accounts.(0)
      ~endpoint:(Evm_node.endpoint sequencer)
      ~evm_node:sequencer
      ~ticket_owner:receiver
      ~routing_info
      ~amount
      ~ticketer:(ticketer |> Hex.of_bytes |> Hex.show)
      ~content:(content |> Hex.of_bytes |> Hex.show)
      ()
  in

  let* () = bake_until_sync ~sequencer ~sc_rollup_node ~client () in

  (* Check that tickets are gone *)
  let* ticket_balance_after_withdraw =
    ticket_balance
      ~kernel
      ~ticket_hash:zero_ticket_hash
      ~account:receiver
      (Either.Right sequencer)
  in
  Check.((0 = ticket_balance_after_withdraw) int)
    ~error_msg:
      "After withdrawal we expect %L ticket balance in the sequencer, got %R" ;

  (* Switch ticket tester contract to proxy mode *)
  let* () =
    Client.transfer
      ~entrypoint:"set"
      ~arg:(sf "Pair %S (Pair (Left Unit) 0)" depositor.public_key_hash)
      ~amount:Tez.zero
      ~giver:depositor.Account.public_key_hash
      ~receiver:l1_contracts.ticket_router_tester
      ~burn_cap:Tez.one
      client
  in
  let* () = Client.bake_for_and_wait ~keys:[] client in

  (* Wait till the cementation and execute outbox message *)
  let* _ =
    find_and_execute_withdrawal
      ~outbox_lookup_depth:100
      ~withdrawal_level
      ~commitment_period:5
      ~challenge_window:5
      ~sc_rollup_node
      ~sc_rollup_address
      ~client
      ()
  in

  (* Check ticket balance for the zero account on L1 *)
  let* l1_balance =
    Client.ticket_balance
      ~contract:"tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU"
      ~ticketer:l1_contracts.ticket_router_tester
      ~content_type:"pair nat (option bytes)"
      ~content:"Pair 0 None"
      client
  in
  Check.(("42" = String.trim l1_balance) string)
    ~error_msg:
      "After outbox message execution we expect %L ticket balance for the \
       receiver, got %R" ;

  unit

let test_fa_reentrant_deposit_reverts =
  register_all
    ~__FILE__
    ~da_fee:(Wei.of_string "1_000_000_000_000")
    ~tags:
      [
        "evm";
        "sequencer";
        "delayed_outbox";
        "fa_deposit";
        "enabled";
        "reentrancy";
      ]
    ~title:"FA Deposit cannot do reentrancy"
    ~enable_fa_bridge:true
    ~kernels:[Kernel.Latest]
    ~time_between_blocks:Nothing
    ~additional_uses:[Constant.octez_codec]
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
  let* reentrancy = Solidity_contracts.reentrancy_test evm_version in
  let* () = Eth_cli.add_abi ~label:reentrancy.label ~abi:reentrancy.abi () in

  let sender = Eth_account.bootstrap_accounts.(0) in
  let* reentrancy, _create_tx =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~args:
           (sf
              {|["%s", "0x", 0, "0x00000000000000000000000000000000000000000000", "0x", 5]|}
              Solidity_contracts.Precompile.xtz_bridge)
         ~source_private_key:sender.private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi:reentrancy.label
         ~bin:reentrancy.bin)
      sequencer
  in

  let amount = 42 in
  let depositor = Constant.bootstrap5 in
  let receiver = Eth_account.bootstrap_accounts.(0).address in

  let* () =
    send_fa_deposit_to_delayed_inbox
      ~proxy:reentrancy
      ~amount
      ~l1_contracts
      ~depositor
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

  let* () =
    Eth_cli.add_abi ~label:"claim" ~abi:(predep_fa_bridge_abi_path ()) ()
  in
  let claim =
    Eth_cli.contract_send
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~endpoint:(Evm_node.endpoint sequencer)
      ~abi_label:"claim"
      ~address:Solidity_contracts.Precompile.fa_bridge
      ~method_call:(sf {|claim(%d)|} 0)
  in
  let produce_block () = Rpc.produce_block sequencer in
  let* _res = wait_for_application ~produce_block claim in

  (* If the call to the proxy fails, the owner of the ticket is the receiver,
     not the proxy.

     As the proxy is doing reeantrancy, the inner call should revert, therefore
     the receiver should be the owner.
  *)
  let* zero_ticket_hash = ticket_hash l1_contracts.ticket_router_tester 0 in
  let* receiver_balance =
    ticket_balance
      ~kernel
      ~ticket_hash:zero_ticket_hash
      ~account:receiver
      (Either.Right sequencer)
  in
  Check.((receiver_balance > 0) int)
    ~error_msg:"As the deposit fails, the receiver should own the ticket" ;
  let* proxy_balance =
    ticket_balance
      ~kernel
      ~ticket_hash:zero_ticket_hash
      ~account:reentrancy
      (Either.Right sequencer)
  in
  Check.((proxy_balance = 0) int)
    ~error_msg:"As the deposit fails, the proxy must not own the ticket" ;

  unit

let test_delayed_deposit_from_init_rollup_node =
  register_all
    ~__FILE__
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "init"]
    ~title:"Delayed inbox is populated at init from rollup node"
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
  let receiver = "0x1074Fd1EC02cbeaa5A90450505cF3B48D834f3EB" in
  let* receiver_balance_prev =
    Eth_cli.balance ~account:receiver ~endpoint:(Evm_node.endpoint sequencer) ()
  in
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in
  (* We don't care about this sequencer. *)
  let* () = Evm_node.terminate sequencer in
  (* Send the deposit to delayed inbox, no sequencer will be listening to the
     event. *)
  let amount = Tez.of_int 16 in
  let depositor = Constant.bootstrap5 in
  let deposit_info = {receiver = EthereumAddr receiver; chain_id = None} in
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
  (* Bake an extra block for a finalized deposit. *)
  let* _lvl = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* _lvl = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

  (* Run a new sequencer that is initialized from a rollup node that has the
     delayed deposit in its state. *)
  let new_sequencer =
    Evm_node.create
      ~node_setup:(Evm_node.make_setup ())
      ~mode:(Evm_node.mode sequencer)
      ()
  in
  let* () = Process.check @@ Evm_node.spawn_init_config new_sequencer in
  let* () =
    Evm_node.init_from_rollup_node_data_dir new_sequencer sc_rollup_node
  in
  let* () = Evm_node.run new_sequencer in

  Log.info "sequencer restarted" ;
  (* The block will include the delayed deposit. *)
  let* () =
    let*@ _lvl = produce_block new_sequencer in
    unit
  and* _hash = Evm_node.wait_for_block_producer_tx_injected new_sequencer in
  let* () =
    bake_until_sync ~sc_rollup_node ~sequencer:new_sequencer ~client ()
  in
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in
  let* receiver_balance_next =
    Eth_cli.balance
      ~account:receiver
      ~endpoint:(Evm_node.endpoint new_sequencer)
      ()
  in
  Check.((receiver_balance_next > receiver_balance_prev) Wei.typ)
    ~error_msg:"Expected a bigger balance" ;
  unit

let protocols = [Protocol.Alpha]

let () =
  test_delayed_deposit_is_included protocols ;
  test_empty_deposit_info_does_not_block_valid_deposits protocols ;
  test_delayed_fa_deposit_is_included protocols ;
  test_delayed_fa_deposit_is_ignored_if_feature_disabled protocols ;
  test_fa_reentrant_deposit_reverts protocols ;
  test_fa_withdrawal_is_included protocols ;
  test_delayed_deposit_from_init_rollup_node protocols
