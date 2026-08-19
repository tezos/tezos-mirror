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
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file flush_reorg.ml
   Subject:      Delayed inbox flushing and the resulting blueprint reorganisations.
*)

open Rpc.Syntax
open Delayed_inbox
open Test_helpers
open Setup
module Protocol = Test_helpers.Protocol_no_direct_register

let test_delayed_inbox_flushing_event =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~delayed_inbox_timeout:0
    ~delayed_inbox_min_levels:1
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "timeout"; "flush"]
    ~title:"Flush delayed inbox event"
    ~use_dal:Register_without_feature
    ~kernels:[Latest]
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        observer;
        _;
      }
      _protocol
    ->
  let* () = Evm_node.terminate observer in
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

  (* This is a transfer from Eth_account.bootstrap_accounts.(0) to
     Eth_account.bootstrap_accounts.(1). *)
  let* raw_transfer =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:23_300
      ~value:(Wei.of_eth_int 1)
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  in
  (* Add a transaction to the delayed inbox. *)
  let* _hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      raw_transfer
  in
  (* Bake a new L1 block to force the flush. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* l1_level = Client.level client in
  (* Wait for the events and finalize the level. *)
  let wait_for_processed_l1_level =
    Evm_node.wait_for_processed_l1_level ~level:l1_level sequencer
  in
  let wait_for_flush = Evm_node.wait_for_flush_delayed_inbox sequencer in

  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

  (* Wait until the event is completely processed. Head of sequencer and rollup node
     should be in sync. *)
  let* _ = wait_for_flush and* _ = wait_for_processed_l1_level in
  let* () =
    check_rollup_head_consistency ~sc_rollup_node ~evm_node:sequencer ()
  in

  unit

let test_flushed_blueprint_reorg =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~delayed_inbox_timeout:0
    ~delayed_inbox_min_levels:1
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "timeout"; "flush"; "reorg"]
    ~title:"Flush delayed inbox event leads to reorg"
    ~use_dal:Register_without_feature
    ~kernels:[Latest]
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        observer;
        _;
      }
      _protocol
    ->
  let* () = Evm_node.terminate observer in
  (* Observer does not track rollup node events, therefore it will blindly
     follow the sequencer then reorganizes when the sequencer will push the
     flushed blueprint. *)
  let* () =
    Evm_node.run ~extra_arguments:["--dont-track-rollup-node"] observer
  in
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

  let craft_raw_transfer account =
    Cast.craft_tx
      ~source_private_key:account.Eth_account.private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:23_300
      ~value:(Wei.of_eth_int 1)
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  in
  let* raw_transfer = craft_raw_transfer Eth_account.bootstrap_accounts.(0) in
  let* raw_transfer_2 = craft_raw_transfer Eth_account.bootstrap_accounts.(1) in
  (* Add a transaction to the delayed inbox. *)
  let* _hash =
    send_raw_transaction_to_delayed_inbox
      ~wait_for_next_level:false
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      raw_transfer
  in
  let* _hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      ~sender:Constant.bootstrap3
      raw_transfer_2
  in

  (* We mark at which level the delayed inbox item was added. *)
  let* add_delayed_tx_level = Client.level client in
  let wait_for_add_delayed_inbox =
    Evm_node.wait_for_processed_l1_level ~level:add_delayed_tx_level sequencer
  in
  (* We mark at which level the delayed inbox was flushed. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* flushed_delayed_inbox_level = Client.level client in
  let wait_for_processed_l1_level =
    Evm_node.wait_for_processed_l1_level
      ~level:flushed_delayed_inbox_level
      sequencer
  in
  let wait_for_flush = Evm_node.wait_for_flush_delayed_inbox sequencer in
  (* A new block will make the {!add_delayed_tx_level} final. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* _ = wait_for_add_delayed_inbox in

  (* Produce a bunch of L2 blocks. The sequencer is aware of the delayed inbox
     item but refuses to include it. *)
  let* () =
    repeat 5 (fun _ ->
        let*@ _ =
          Rpc.produce_block ~with_delayed_transactions:false sequencer
        in
        unit)
  in
  let*@ speculative_head = Rpc.get_block_by_number ~block:"latest" sequencer in

  (* A new block will make the {!flushed_delayed_inbox_levle} final. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

  (* Wait until the event is completely processed. Head of sequencer and rollup node
     should be in sync. *)
  let* _ = wait_for_flush and* _ = wait_for_processed_l1_level in
  let* () =
    check_rollup_head_consistency ~sc_rollup_node ~evm_node:sequencer ()
  in

  (* Speculative head has been removed as the sequencer was forced to
     reorganize the blocks because of the flushed blueprint. *)
  let*@? (err : Rpc.error) =
    Rpc.get_block_by_hash ~block:speculative_head.hash sequencer
  in
  Check.(err.message =~ rex "Block .* not found")
    ~error_msg:"Expected error to match %R, got %L" ;

  (* Sequencer resumes block production. *)
  let* () =
    repeat 5 (fun _ ->
        let*@ _ = Rpc.produce_block sequencer in
        unit)
  in
  let*@ sequencer_head = Rpc.block_number sequencer in

  (* As the observer was down during the reorganization, restarting it
     will be enough, it'll apply the new branch as expected. *)
  let* () =
    Evm_node.wait_for_blueprint_applied observer (Int32.to_int sequencer_head)
  in

  unit

let test_multiple_flushed_blueprints =
  register_all
    ~__FILE__
    ~max_delayed_inbox_blueprint_length:1
    ~time_between_blocks:Nothing
    ~delayed_inbox_timeout:0
    ~delayed_inbox_min_levels:1
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:
      [
        "evm";
        "sequencer";
        "delayed_inbox";
        "timeout";
        "flush";
        "reorg";
        "multiple";
      ]
    ~title:"Multiple flushed blueprints"
    ~use_dal:Register_without_feature
    ~kernels:[Latest]
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        observer;
        _;
      }
      _protocol
    ->
  let* () = Evm_node.terminate observer in
  (* Observer does not track rollup node events, therefore it will blindly
     follow the sequencer then reorganizes when the sequencer will push the
     flushed blueprint. *)
  let* () =
    Evm_node.run ~extra_arguments:["--dont-track-rollup-node"] observer
  in
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

  (* Add 2 transactions to the delayed inbox. *)
  let* tx1 =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:23_300
      ~value:(Wei.of_eth_int 1)
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  and* tx2 =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:1
      ~gas_price:1_000_000_000
      ~gas:23_300
      ~value:(Wei.of_eth_int 1)
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  in

  let* _hash1 =
    send_raw_transaction_to_delayed_inbox
      ~wait_for_next_level:false
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      ~sender:Constant.bootstrap2
      tx1
  and* _hash2 =
    send_raw_transaction_to_delayed_inbox
      ~wait_for_next_level:false
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      ~sender:Constant.bootstrap3
      tx2
  in

  let*@ rollup_head_before_flush = rollup_level sc_rollup_node in

  (* Both delayed transaction added *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  (* Flush of delayed inbox in kernel *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  (* We mark at which level the delayed inbox was flushed. *)
  let* flushed_delayed_inbox_level = Client.level client in

  let wait_for_processed_l1_level =
    Evm_node.wait_for_processed_l1_level
      ~level:flushed_delayed_inbox_level
      sequencer
  in
  let wait_for_first_flush =
    Evm_node.wait_for_flush_delayed_inbox ~level:1 sequencer
  in
  let wait_for_second_flush =
    Evm_node.wait_for_flush_delayed_inbox ~level:2 sequencer
  in

  (* Make the {!flushed_delayed_inbox_level} final and ait until the
     event is completely processed. Head of sequencer and rollup node should
     be in sync. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client
  and* _ = wait_for_first_flush
  and* _ = wait_for_second_flush
  and* _ = wait_for_processed_l1_level in

  let* () =
    check_rollup_head_consistency ~sc_rollup_node ~evm_node:sequencer ()
  in

  let*@ rollup_head_after_flush = rollup_level sc_rollup_node in
  Check.(
    (Int32.to_int rollup_head_before_flush + 2
    = Int32.to_int rollup_head_after_flush)
      int)
    ~error_msg:
      (sf
         "Expected 2 flushed blueprints, rollup level before flush is %ld, \
          after is %%R"
         rollup_head_before_flush) ;

  (* Sequencer resumes block production. *)
  let* () =
    repeat 5 (fun _ ->
        let*@ _ = Rpc.produce_block sequencer in
        unit)
  in
  let*@ sequencer_head = Rpc.block_number sequencer in

  let* () =
    Evm_node.wait_for_blueprint_applied observer (Int32.to_int sequencer_head)
  in

  unit

let test_observer_reorg_on_blueprint_stream =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~delayed_inbox_timeout:0
    ~delayed_inbox_min_levels:1
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "timeout"; "flush"; "reorg"]
    ~title:
      "Observer tracking a rollup node reorganizes after blueprint on stream"
    ~use_dal:Register_without_feature
    ~kernels:[Latest]
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        observer;
        _;
      }
      _protocol
    ->
  (*
     This test focuses on the case where an observer is connected to a rollup
     node.

     The observer will start by detecting a divergence and reset to the state
     prior to the delayed inbox flushing.

     It will then reset itself (again) and start accepting the new blueprints
     from the sequencer.
  *)
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

  (* This is a transfer from Eth_account.bootstrap_accounts.(0) to
     Eth_account.bootstrap_accounts.(1). *)
  let* raw_transfer =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:23_300
      ~value:(Wei.of_eth_int 1)
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  in
  (* Add a transaction to the delayed inbox. *)
  let* _hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      raw_transfer
  in
  (* We mark at which level the delayed inbox item was added. *)
  let* add_delayed_inbox_level = Client.level client in
  let wait_for_add_delayed_inbox =
    Evm_node.wait_for_processed_l1_level
      ~level:add_delayed_inbox_level
      sequencer
  in
  (* We mark at which level the delayed inbox was flushed. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* flushed_delayed_inbox_level = Client.level client in
  let wait_for_processed_l1_level =
    Evm_node.wait_for_processed_l1_level
      ~level:flushed_delayed_inbox_level
      sequencer
  in
  let wait_for_flush = Evm_node.wait_for_flush_delayed_inbox sequencer in
  let wait_for_observer_reset = Evm_node.wait_for_reset observer in
  (* A new block will make the {!add_delayed_inbox_level} final. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* _ = wait_for_add_delayed_inbox in

  (* Produce a bunch of L2 blocks. The sequencer is aware of the delayed inbox
     item but refuses to include it. *)
  let* () =
    repeat 5 (fun _ ->
        let*@ _ =
          Rpc.produce_block ~with_delayed_transactions:false sequencer
        in
        unit)
  in

  (* A new block will make the {!flushed_delayed_inbox_level} final. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

  (* Wait until the event is completely processed. Head of sequencer and rollup
     should be in sync. *)
  let* _ = wait_for_flush
  and* _ = wait_for_processed_l1_level
  and* () = wait_for_observer_reset in

  let* () =
    check_rollup_head_consistency ~sc_rollup_node ~evm_node:sequencer ()
  in

  (* Sequencer resumes block production. *)
  let* () =
    repeat 5 (fun _ ->
        let*@ _ = Rpc.produce_block sequencer in
        unit)
  in
  let*@ sequencer_head = Rpc.block_number sequencer in
  (* Observer accepts the new blocks. *)
  let* () =
    Evm_node.wait_for_blueprint_applied observer (Int32.to_int sequencer_head)
  in

  unit

let test_observer_reorg_on_blueprint_catchup =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~delayed_inbox_timeout:0
    ~delayed_inbox_min_levels:1
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "timeout"; "flush"; "reorg"]
    ~title:"Observer reorganizes after blueprint on catchup"
    ~use_dal:Register_without_feature
    ~kernels:[Latest]
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        observer;
        _;
      }
      _protocol
    ->
  (*
     This test focuses on the case where an observer detects a divergence
     and reset to the state prior to the delayed inbox flushing, when the
     blueprint is seen during a catchup.
  *)
  let* () = Evm_node.terminate observer in
  let* () =
    Evm_node.run ~extra_arguments:["--dont-track-rollup-node"] observer
  in
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

  (* This is a transfer from Eth_account.bootstrap_accounts.(0) to
     Eth_account.bootstrap_accounts.(1). *)
  let* raw_transfer =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:23_300
      ~value:(Wei.of_eth_int 1)
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  in
  (* Add a transaction to the delayed inbox. *)
  let* _hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      raw_transfer
  in
  (* We mark at which level the delayed inbox item was added. *)
  let* add_delayed_inbox_level = Client.level client in
  let wait_for_add_delayed_inbox =
    Evm_node.wait_for_processed_l1_level
      ~level:add_delayed_inbox_level
      sequencer
  in
  (* We mark at which level the delayed inbox was flushed. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* flushed_delayed_inbox_level = Client.level client in
  let wait_for_processed_l1_level =
    Evm_node.wait_for_processed_l1_level
      ~level:flushed_delayed_inbox_level
      sequencer
  in
  let wait_for_flush = Evm_node.wait_for_flush_delayed_inbox sequencer in
  (* A new block will make the {!add_delayed_inbox_level} final. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* _ = wait_for_add_delayed_inbox in

  (* Produce a bunch of L2 blocks. The sequencer is aware of the delayed inbox
     item but refuses to include it. *)
  let* () =
    repeat 2 (fun _ ->
        let*@ _ =
          Rpc.produce_block ~with_delayed_transactions:false sequencer
        in
        unit)
  in
  let*@ sequencer_head = Rpc.block_number sequencer in
  (* Observer accepts these blocks and shutdowns. *)
  let* () =
    Evm_node.wait_for_blueprint_applied observer (Int32.to_int sequencer_head)
  in
  let* () = Evm_node.terminate observer in

  (* A new block will make the {!flushed_delayed_inbox_level} final. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

  (* Wait until the event is completely processed. Head of sequencer and rollup
     should be in sync. *)
  let* _ = wait_for_flush and* _ = wait_for_processed_l1_level in

  let* () =
    check_rollup_head_consistency ~sc_rollup_node ~evm_node:sequencer ()
  in

  (* Sequencer resumes block production. *)
  let* () =
    repeat 5 (fun _ ->
        let*@ _ = Rpc.produce_block sequencer in
        unit)
  in
  let*@ sequencer_head = Rpc.block_number sequencer in

  (* Restart the observer, observer will be at level N. The flushed delayed
     blueprint happened before level N, therefore it will have to realize
     something is wrong and traverse the chain backward. *)
  let* () =
    Evm_node.run ~extra_arguments:["--dont-track-rollup-node"] observer
  in

  (* Observer accepts the new blocks. *)
  let* () =
    Evm_node.wait_for_blueprint_applied observer (Int32.to_int sequencer_head)
  in

  unit

(* Same test as reorg, but the delayed transaction event is applied at the same
   L2 level as the flush event. *)
let test_flushed_blueprint_reorg_late =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~delayed_inbox_timeout:0
    ~delayed_inbox_min_levels:1
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:
      ["evm"; "sequencer"; "delayed_inbox"; "timeout"; "flush"; "reorg"; "late"]
    ~title:"Flush delayed inbox event leads to reorg, including late delayed tx"
    ~use_dal:Register_without_feature
    ~kernels:[Latest]
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        observer;
        _;
      }
      _protocol
    ->
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

  let* raw_transfer =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:23_300
      ~value:(Wei.of_eth_int 1)
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  in
  (* Add a transaction to the delayed inbox. *)
  let* _hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      raw_transfer
  in
  (* We mark at which level the delayed inbox item was added. *)
  let* add_delayed_tx_level = Client.level client in

  (* the sequencer produces blocks before seeing the delayed transactions
     (speculative execution). *)
  let* () =
    repeat 4 (fun _ ->
        let*@ _ =
          Rpc.produce_block ~with_delayed_transactions:false sequencer
        in
        unit)
  in

  (* We mark at which level the delayed inbox was flushed. *)
  let* flushed_delayed_inbox_level =
    Rollup.next_rollup_node_level ~sc_rollup_node ~client
  in

  (* A new block will make the add delayed inbox event final. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client
  and* _ =
    Evm_node.wait_for_processed_l1_level ~level:add_delayed_tx_level sequencer
  in

  (* Produce a few l2 blocks (speculative execution) after seeing delayed tx *)
  let* () =
    repeat 4 (fun _ ->
        let*@ _ =
          Rpc.produce_block ~with_delayed_transactions:false sequencer
        in
        unit)
  in

  let*@ speculative_head = Rpc.get_block_by_number ~block:"latest" sequencer in

  (* A new block will make the {!flushed_delayed_inbox_level} final. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client
  and* _ =
    (* Wait until the event is completely processed. Head of sequencer and rollup
       should be in sync. *)
    Evm_node.wait_for_processed_l1_level
      ~level:flushed_delayed_inbox_level
      sequencer
  and* _ = Evm_node.wait_for_flush_delayed_inbox sequencer in
  let* () =
    check_rollup_head_consistency ~sc_rollup_node ~evm_node:sequencer ()
  in

  (* Speculative head has been removed as the sequencer was forced to
     reorganize the blocks because of the flushed blueprint. *)
  let*@? (err : Rpc.error) =
    Rpc.get_block_by_hash ~block:speculative_head.hash sequencer
  in

  Check.(err.message =~ rex "Block .* not found")
    ~error_msg:"Expected error to match %R, got %L" ;

  (* Sequencer resumes block production. *)
  let* () =
    repeat 4 (fun _ ->
        let*@ _ = Rpc.produce_block sequencer in
        unit)
  in
  let*@ sequencer_head = Rpc.block_number sequencer in

  (* As the observer was done during the reorganization, restarting it
     will be enough, it'll apply the new branch as expected. *)
  let* () =
    Evm_node.wait_for_blueprint_applied observer (Int32.to_int sequencer_head)
  in
  let* () = check_head_consistency ~left:observer ~right:sequencer () in

  unit

(* Same test as reorg, but the delayed transaction event is applied at the same
   L2 level as the flush event, and transactions were included but too late. *)
let test_flushed_blueprint_reorg_done_late =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~delayed_inbox_timeout:0
    ~delayed_inbox_min_levels:1
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:
      [
        "evm";
        "sequencer";
        "delayed_inbox";
        "timeout";
        "flush";
        "reorg";
        "late";
        "reinject";
      ]
    ~title:
      "Flush delayed inbox event leads to reorg, including delayed tx included \
       too late"
    ~use_dal:Register_without_feature
    ~kernels:[Latest]
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        observer;
        _;
      }
      _protocol
    ->
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
  let* raw_transfer =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:23_300
      ~value:(Wei.of_eth_int 1)
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  in
  (* Add a transaction to the delayed inbox. *)
  let* _hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      raw_transfer
  in
  (* We mark at which level the delayed inbox item was added. *)
  let* add_delayed_tx_level = Client.level client in

  (* The sequencer produces blocks before seeing the delayed transactions
     (speculative execution). *)
  let* () =
    repeat 4 (fun _ ->
        let*@ _ =
          Rpc.produce_block ~with_delayed_transactions:false sequencer
        in
        unit)
  in

  (* We mark at which level the delayed inbox was flushed. *)
  let* flushed_delayed_inbox_level =
    Rollup.next_rollup_node_level ~sc_rollup_node ~client
  in

  (* A new block will make the add delayed event. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client
  and* _ =
    Evm_node.wait_for_processed_l1_level ~level:add_delayed_tx_level sequencer
  in

  (* Produce a few l2 blocks (speculative execution) after seeing delayed tx *)
  let* () =
    repeat 4 (fun _ ->
        let*@ _ =
          Rpc.produce_block ~with_delayed_transactions:false sequencer
        in
        unit)
  in
  (* Produce blocks **with** delayed transactions, but too late. *)
  let* () =
    repeat 4 (fun _ ->
        let*@ _ = Rpc.produce_block ~with_delayed_transactions:true sequencer in
        unit)
  in
  let*@ speculative_head = Rpc.get_block_by_number ~block:"latest" sequencer in

  (* A new block will make the {!flushed_delayed_inbox_level} final. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client
  and* _ =
    (* Wait until the event is completely processed. Head of sequencer and rollup node
       should be in sync. *)
    Evm_node.wait_for_processed_l1_level
      ~level:flushed_delayed_inbox_level
      sequencer
  and* _ = Evm_node.wait_for_flush_delayed_inbox sequencer in

  (* Wait until the event is completely processed. Head of sequencer and rollup node
     should be in sync. *)
  let* () =
    check_rollup_head_consistency ~sc_rollup_node ~evm_node:sequencer ()
  in

  (* Speculative head has been removed as the sequencer was forced to
     reorganize the blocks because of the flushed blueprint. *)
  let*@? (err : Rpc.error) =
    Rpc.get_block_by_hash ~block:speculative_head.hash sequencer
  in

  Check.(err.message =~ rex "Block .* not found")
    ~error_msg:"Expected error to match %R, got %L" ;

  (* Sequencer resumes block production. *)
  let* () =
    repeat 4 (fun _ ->
        let*@ _ = Rpc.produce_block sequencer in
        unit)
  in
  let*@ sequencer_head = Rpc.block_number sequencer in

  (* As the observer was done during the reorganization, restarting it
     will be enough, it'll apply the new branch as expected. *)
  let* () =
    Evm_node.wait_for_blueprint_applied observer (Int32.to_int sequencer_head)
  in
  let* () = check_head_consistency ~left:observer ~right:sequencer () in

  unit

let test_upgrade_injected_before_flush_level =
  let genesis_timestamp = "2020-01-01T00:00:00Z" in
  let activation_timestamp = "2020-01-01T00:01:00Z" in
  let after_timestamp = "2020-01-01T00:02:00Z" in
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~time_between_blocks:Nothing
    ~genesis_timestamp:Client.(At (Time.of_notation_exn genesis_timestamp))
    ~delayed_inbox_timeout:0
    ~delayed_inbox_min_levels:1
    ~tags:
      [
        "evm";
        "sequencer";
        "delayed_inbox";
        "timeout";
        "flush";
        "reorg";
        "upgrade";
        "before";
      ]
    ~title:"Upgrade injected before flushed level"
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        observer;
        kernel;
        _;
      }
      _protocol
    ->
  (* This test the situation where the injected before of the upgrade
     event is before the flushed level.

     It can be a bit tedious to read the test, here infinitys what happens:

     (l2_level)           0  <- Upgrade event is seen at head = 0.
                          1  <- Upgrade event is at injected_before here.
                          2  <- Delayed inbox item event is seen.
                          3' <- Flushed blueprint

     The flushed blueprint logic will not be able to find the upgrade event
     in the invalid branch, but the "generic" logic to reset to finalized level
     is supposed to find the latest unapplied upgrade that was seen
     before the flushed level.

     To ensure everything is OK after the flush we trigger the update and check
     that both the sequencer and observer are consistent with the rollup node.
  *)
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
  (* Set a pending upgrade before the flush level. *)
  let* _root_hash =
    upgrade
      ~sc_rollup_node
      ~sc_rollup_address
      ~admin:Constant.bootstrap2.public_key_hash
      ~admin_contract:l1_contracts.admin
      ~client
      ~upgrade_to:(Kernel.to_uses kernel)
      ~activation_timestamp
  in
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

  (* Produces 2 valid blocks and wait until they are synchronized. *)
  let*@ _ = produce_block ~timestamp:genesis_timestamp sequencer in
  let*@ _ = produce_block ~timestamp:genesis_timestamp sequencer in
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

  (* Send a delayed transaction. *)
  let* _hash =
    let* tx =
      Cast.craft_tx
        ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
        ~chain_id:1337
        ~nonce:0
        ~gas_price:1_000_000_000
        ~gas:23_300
        ~value:(Wei.of_eth_int 1)
        ~address:Eth_account.bootstrap_accounts.(1).address
        ()
    in
    send_raw_transaction_to_delayed_inbox
      ~wait_for_next_level:true
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      tx
  in
  (* Flush the delayed inbox. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* flush_l1_level = Client.level client in

  (* Make the flush final. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

  let* _ = Evm_node.wait_for_processed_l1_level ~level:flush_l1_level sequencer
  and* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

  (* The sequencer should trigger the upgrade *)
  let* _ =
    repeat 2 (fun () ->
        let* _ = produce_block ~timestamp:after_timestamp sequencer in
        unit)
  and* rh_seq, lvl_seq = Evm_node.wait_for_successful_upgrade sequencer in

  let*@ sequencer_head = Rpc.block_number sequencer in

  (* The observer should follow the reorg and reach the same head *)
  let* () =
    Evm_node.wait_for_blueprint_applied observer (Int32.to_int sequencer_head)
  and* rh_obs, lvl_obs = Evm_node.wait_for_successful_upgrade observer in

  Check.((rh_seq = rh_obs) string) ~error_msg:"Root hash should be the same" ;
  Check.((lvl_seq = lvl_obs) int)
    ~error_msg:"Level of upgrade should be the same" ;

  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

  let* () =
    check_rollup_head_consistency ~sc_rollup_node ~evm_node:sequencer ()
  in
  unit

let test_upgrade_activated_after_flush_level =
  let genesis_timestamp = "2020-01-01T00:00:00Z" in
  let activation_timestamp = "2020-01-01T00:01:00Z" in
  let after_timestamp = "2020-01-01T00:02:00Z" in
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~time_between_blocks:Nothing
    ~genesis_timestamp:Client.(At (Time.of_notation_exn genesis_timestamp))
    ~delayed_inbox_timeout:0
    ~delayed_inbox_min_levels:1
    ~tags:
      [
        "evm";
        "sequencer";
        "delayed_inbox";
        "timeout";
        "flush";
        "reorg";
        "upgrade";
        "after";
        "reactivate";
      ]
    ~title:
      "Upgrade injected and activated after flushed level (on invalid branch)"
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        observer;
        kernel;
        _;
      }
      _protocol
    ->
  (* This test the situation where the injected before of the upgrade
     event is AT the flushed level and activated in invalid branch.

     It can be a bit tedious to read the test, here is what happens:

     (l2_level)             0
                            1
                            2
                           /
                          /
        Upgrade event -> 3
                         5
        Activation    -> 6
                         7

     Then a flush happens at level 3, invalidating the branch and creating a new
     one.

     (l2_level)             0
                            1
                            2
                           / \
                          /   \
                         X     3' <- Flushed blueprint, upgrade event
                               4' <- activation

     The upgrade event must be retrieved from the invalid branch before
     resetting to level 2 and applying level 3'.
  *)
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
  (* Set a pending upgrade at the flush level. *)
  let* _root_hash =
    upgrade
      ~sc_rollup_node
      ~sc_rollup_address
      ~admin:Constant.bootstrap2.public_key_hash
      ~admin_contract:l1_contracts.admin
      ~client
      ~upgrade_to:(Kernel.to_uses kernel)
      ~activation_timestamp
  in
  (* Produces 2 valid blocks and wait until they are synchronized. *)
  let*@ _ = produce_block ~timestamp:genesis_timestamp sequencer in
  let*@ _ = produce_block ~timestamp:genesis_timestamp sequencer in
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

  (* Shutdown the rollup, restart it without Batcher mode, we will be able
     to create the invalid branch without publishing it. *)
  let* () = Sc_rollup_node.terminate sc_rollup_node in
  Sc_rollup_node.change_node_mode sc_rollup_node Observer ;
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup_address [] in
  let*@ _ = produce_block ~timestamp:genesis_timestamp sequencer in

  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client
  and* _ =
    repeat 2 (fun () ->
        let* _ = produce_block ~timestamp:after_timestamp sequencer in
        unit)
  (* make sure the upgrade is done on invalid branch *)
  and* _ = Evm_node.wait_for_successful_upgrade sequencer in

  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  (* Send a delayed transaction. *)
  let* _hash =
    let* tx =
      Cast.craft_tx
        ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
        ~chain_id:1337
        ~nonce:0
        ~gas_price:1_000_000_000
        ~gas:23_300
        ~value:(Wei.of_eth_int 1)
        ~address:Eth_account.bootstrap_accounts.(1).address
        ()
    in
    send_raw_transaction_to_delayed_inbox
      ~wait_for_next_level:true
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      tx
  in
  (* Flush the delayed inbox. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* flush_l1_level = Client.level client in

  (* Make the flush final. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

  let* _ = Evm_node.wait_for_processed_l1_level ~level:flush_l1_level sequencer
  and* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* () =
    check_rollup_head_consistency ~sc_rollup_node ~evm_node:sequencer ()
  in

  let* _ =
    repeat 2 (fun () ->
        let* _ = produce_block ~timestamp:after_timestamp sequencer in
        unit)
  (* Make sure the upgrade still happens *)
  and* rh_seq, lvl_seq = Evm_node.wait_for_successful_upgrade sequencer in
  let*@ sequencer_head = Rpc.block_number sequencer in

  (* The observer should follow the reorg and reach the same head *)
  let* () =
    Evm_node.wait_for_blueprint_applied observer (Int32.to_int sequencer_head)
  and* rh_obs, lvl_obs = Evm_node.wait_for_successful_upgrade observer in
  Check.((rh_seq = rh_obs) string) ~error_msg:"Root hash should be the same" ;
  Check.((lvl_seq = lvl_obs) int)
    ~error_msg:"Level of upgrade should be the same" ;

  let* () = check_head_consistency ~left:observer ~right:sequencer () in
  unit

let test_upgrade_injected_after_flush_level =
  let genesis_timestamp = "2020-01-01T00:00:00Z" in
  let activation_timestamp = "2020-01-01T00:01:00Z" in
  let after_timestamp = "2020-01-01T00:02:00Z" in
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~time_between_blocks:Nothing
    ~genesis_timestamp:Client.(At (Time.of_notation_exn genesis_timestamp))
    ~delayed_inbox_timeout:0
    ~delayed_inbox_min_levels:1
    ~tags:
      [
        "evm";
        "sequencer";
        "delayed_inbox";
        "timeout";
        "flush";
        "reorg";
        "upgrade";
        "after";
      ]
    ~title:"Upgrade injected after flushed level (on invalid branch)"
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        observer;
        kernel;
        _;
      }
      _protocol
    ->
  (* This test the situation where the injected before of the upgrade
     event is after the flushed level.

     It can be a bit tedious to read the test, here is what happens:

     (l2_level)             0
                            1
                            2
                           /
                          /
                         3
                         4
        Upgrade event -> 5

     Then a flush happens at level 3, invalidating the branch and creating a new
     one.

     (l2_level)             0
                            1
                            2
                           / \
                          /   \
                         X     3' <- Flushed blueprint

     The upgrade event must be retrieved from the invalid branch before
     resetting to level 2 and applying level 3'.
  *)
  (* Produces 2 valid blocks and wait until they are synchronized. *)
  let*@ _ = produce_block ~timestamp:genesis_timestamp sequencer in
  let*@ _ = produce_block ~timestamp:genesis_timestamp sequencer in
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

  (* Shutdown the rollup, restart it without Batcher mode, we will be able
     to create the invalid branch without publishing it. *)
  let* () = Sc_rollup_node.terminate sc_rollup_node in
  Sc_rollup_node.change_node_mode sc_rollup_node Observer ;
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup_address [] in

  let*@ _ = produce_block ~timestamp:genesis_timestamp sequencer in
  let*@ _ = produce_block ~timestamp:genesis_timestamp sequencer in
  let*@ _ = produce_block ~timestamp:genesis_timestamp sequencer in

  (* Set a pending upgrade. *)
  let* _root_hash =
    upgrade
      ~sc_rollup_node
      ~sc_rollup_address
      ~admin:Constant.bootstrap2.public_key_hash
      ~admin_contract:l1_contracts.admin
      ~client
      ~upgrade_to:(Kernel.to_uses kernel)
      ~activation_timestamp
  in
  let* _ =
    repeat 2 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  and* _ = Evm_node.wait_for_pending_upgrade sequencer in

  (* Send a delayed transaction. *)
  let* _hash =
    let* tx =
      Cast.craft_tx
        ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
        ~chain_id:1337
        ~nonce:0
        ~gas_price:1_000_000_000
        ~gas:23_300
        ~value:(Wei.of_eth_int 1)
        ~address:Eth_account.bootstrap_accounts.(1).address
        ()
    in
    send_raw_transaction_to_delayed_inbox
      ~wait_for_next_level:true
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      tx
  in
  (* Flush the delayed inbox. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* flush_l1_level = Client.level client in

  (* Make the flush final. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

  let* _ = Evm_node.wait_for_processed_l1_level ~level:flush_l1_level sequencer
  and* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

  let* _ =
    repeat 2 (fun () ->
        let* _ = produce_block ~timestamp:after_timestamp sequencer in
        unit)
  and* rh_seq, lvl_seq = Evm_node.wait_for_successful_upgrade sequencer in
  let*@ sequencer_head = Rpc.block_number sequencer in

  (* The observer should follow the reorg and reach the same head *)
  let* () =
    Evm_node.wait_for_blueprint_applied observer (Int32.to_int sequencer_head)
  and* rh_obs, lvl_obs = Evm_node.wait_for_successful_upgrade observer in
  Check.((rh_seq = rh_obs) string) ~error_msg:"Root hash should be the same" ;
  Check.((lvl_seq = lvl_obs) int)
    ~error_msg:"Level of upgrade should be the same" ;
  let* () = check_head_consistency ~left:observer ~right:sequencer () in
  unit

(* test of reorg: the sequencer sees the "upgrade to do" event at the same
   time as the "flushed blueprint" event*)
let test_flushed_blueprint_reorg_upgrade =
  (* Add a delay between first block and activation timestamp. *)
  let genesis_timestamp =
    Client.(At (Time.of_notation_exn "2020-01-01T00:00:00Z"))
  in
  let before_timestamp = "2020-01-01T00:00:05Z" in
  let activation_timestamp = "2020-01-01T00:00:10Z" in
  let after_timestamp = "2020-01-01T00:00:15Z" in
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~time_between_blocks:Nothing
    ~genesis_timestamp
    ~delayed_inbox_timeout:0
    ~delayed_inbox_min_levels:1
    ~tags:
      [
        "evm";
        "sequencer";
        "delayed_inbox";
        "timeout";
        "flush";
        "reorg";
        "upgrade";
        "at";
      ]
    ~title:"Flush delayed inbox event leads to reorg, including upgrade"
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        observer;
        kernel;
        _;
      }
      _protocol
    ->
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

  let* raw_transfer =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:23_300
      ~value:(Wei.of_eth_int 1)
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  in
  let* _hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      raw_transfer
  in
  (* We mark at which level the delayed inbox item was added. *)
  let* add_delayed_inbox_level = Client.level client in

  (* The sequencer produces a few blocks before seeing the delayed transaction
     (speculative execution) *)
  let* () =
    repeat 4 (fun _ ->
        let*@ _ =
          Rpc.produce_block
            ~timestamp:before_timestamp
            ~with_delayed_transactions:false
            sequencer
        in
        unit)
  in

  (* upgrade message will also trigger flush*)
  let* _root_hash =
    upgrade
      ~sc_rollup_node
      ~sc_rollup_address
      ~admin:Constant.bootstrap2.public_key_hash
      ~admin_contract:l1_contracts.admin
      ~client
      ~upgrade_to:(Kernel.to_uses kernel)
      ~activation_timestamp
  in

  (* We mark at which level the delayed inbox was flushed. *)
  let* flushed_delayed_inbox_level = Client.level client in

  (* produce a few l2 blocks (speculative execution) after seeing delayed tx *)
  let* () =
    repeat 4 (fun _ ->
        let*@ _ =
          Rpc.produce_block
            ~timestamp:after_timestamp
            ~with_delayed_transactions:false
            sequencer
        in
        unit)
  in

  let* _ =
    Evm_node.wait_for_processed_l1_level
      ~level:add_delayed_inbox_level
      sequencer
  and* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let*@ speculative_head = Rpc.get_block_by_number ~block:"latest" sequencer in

  (* Wait until the event is completely processed. Head of sequencer and rollup
     should be in sync. *)
  let* _ =
    Evm_node.wait_for_processed_l1_level
      ~level:flushed_delayed_inbox_level
      sequencer
  and* _ = Evm_node.wait_for_flush_delayed_inbox sequencer
  and* _ = Evm_node.wait_for_pending_upgrade sequencer
  (* A new block will make the {!flushed_delayed_inbox_level} final. *)
  and* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

  let* () =
    check_rollup_head_consistency ~sc_rollup_node ~evm_node:sequencer ()
  in

  (* Speculative head has been removed as the sequencer was forced to
     reorganize the blocks because of the flushed blueprint. *)
  let*@? (err : Rpc.error) =
    Rpc.get_block_by_hash ~block:speculative_head.hash sequencer
  in

  Check.(err.message =~ rex "Block .* not found")
    ~error_msg:"Expected error to match %R, got %L" ;

  (* The sequencer should trigger the upgrade *)
  let* rh_seq, lvl_seq = Evm_node.wait_for_successful_upgrade sequencer
  and* _ =
    repeat 2 (fun () ->
        let* _ = produce_block ~timestamp:after_timestamp sequencer in
        unit)
  in

  let*@ sequencer_head = Rpc.block_number sequencer in

  (* The observer should follow the reorg and reach the same head *)
  let* () =
    Evm_node.wait_for_blueprint_applied observer (Int32.to_int sequencer_head)
  and* rh_obs, lvl_obs = Evm_node.wait_for_successful_upgrade observer in
  Check.((rh_seq = rh_obs) string) ~error_msg:"Root hash should be the same" ;
  Check.((lvl_seq = lvl_obs) int)
    ~error_msg:"Level of upgrade should be the same" ;
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
  let* () = check_head_consistency ~left:observer ~right:sequencer () in
  unit

let protocols = [Protocol.Alpha]

let () =
  test_delayed_inbox_flushing_event protocols ;
  test_flushed_blueprint_reorg protocols ;
  test_multiple_flushed_blueprints protocols ;
  test_observer_reorg_on_blueprint_stream protocols ;
  test_observer_reorg_on_blueprint_catchup protocols ;
  test_flushed_blueprint_reorg_late protocols ;
  test_flushed_blueprint_reorg_done_late protocols ;
  test_upgrade_injected_before_flush_level protocols ;
  test_upgrade_injected_after_flush_level protocols ;
  test_upgrade_activated_after_flush_level protocols ;
  test_flushed_blueprint_reorg_upgrade protocols
