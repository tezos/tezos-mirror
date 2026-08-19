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
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file tx_queue.ml
   Subject:      The transaction queue and transaction batching.
*)

open Rpc.Syntax
open Transaction
open Delayed_inbox
open Test_helpers
open Setup
module Protocol = Test_helpers.Protocol_no_direct_register

let test_batch_limit_size_rpc =
  register_all
    ~__FILE__
    ~eth_bootstrap_accounts:Eth_account.lots_of_address
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
    ~title:"Test batch size limit"
    ~tags:["rpc"; "batch_limit"]
    ~time_between_blocks:Nothing
  @@ fun {sequencer; _} _protocol ->
  (* We restart the sequencer to impose a batch limit on the sequencer. *)
  let* () = Evm_node.terminate sequencer in
  let* () =
    Evm_node.run ~extra_arguments:["--rpc-batch-limit"; "5"] sequencer
  in
  let all_txs = read_tx_from_file () in
  (* Considering the chosen limit (5), we prepare two batches of transactions.
     The first one with the first three transactions of our tx file, the second
     one with the 10 ones that comes after.*)
  let below_limit_txs =
    List.filteri (fun i _ -> i < 3) all_txs |> List.map fst
  in
  let above_limit_txs =
    List.filteri (fun i _ -> 3 <= i && i < 13) all_txs |> List.map fst
  in

  (* We first check that we can inject the first batch. *)
  let* _ = batch_n_transactions ~evm_node:sequencer below_limit_txs in

  (* We then demonstrate that we cannot inject the second batch. *)
  let* has_failed =
    Lwt.catch
      (fun () ->
        let* _ = batch_n_transactions ~evm_node:sequencer above_limit_txs in
        return false)
      (fun _ -> return true)
  in

  if not has_failed then
    Test.fail "Large batch exceeding the limit should have failed." ;

  (* We restart the sequencer, and explicitly lift the limit. *)
  let* () = Evm_node.terminate sequencer in
  let* () =
    Evm_node.run ~extra_arguments:["--rpc-batch-limit"; "unlimited"] sequencer
  in

  (* We demonstrate it is now possible to inject the batch. *)
  let* _ = batch_n_transactions ~evm_node:sequencer above_limit_txs in

  unit

let test_batch_eth_send_raw_transaction_sync_rpc () =
  register_sandbox_with_observer
    ~patch_config:
      (Evm_node.patch_config_with_experimental_feature
         ~preconfirmation_stream_enabled:true
         ())
    ~__FILE__
    ~tags:["evm"; "delayed_transaction"; "batch"]
    ~title:"batch eth_sendRawTransactionSync waits for receipt"
  @@ fun {sandbox; observer} ->
  let* gas_price = Rpc.get_gas_price sandbox in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let receiver = Eth_account.bootstrap_accounts.(1) in
  let batch_tx_size = 10 in
  let raw_tx nonce =
    Cast.craft_tx
      ~source_private_key:sender.private_key
      ~chain_id:1337
      ~nonce
      ~gas_price:(Int32.to_int gas_price)
      ~gas:23_300
      ~value:(Wei.of_eth_int 2)
      ~address:receiver.address
      ()
  in
  let compute_tx_hash raw_tx =
    `Hex raw_tx |> Hex.to_bytes |> Tezos_crypto.Hacl.Hash.Keccak_256.digest
    |> Hex.of_bytes |> Hex.show |> Format.sprintf "0x%s"
  in
  let wait_txs_injected tx_hashes evm_node =
    List.map
      (fun hash -> Evm_node.wait_for_tx_queue_add_transaction ~hash evm_node)
      tx_hashes
    |> Lwt.all
  in
  let wait_txs_included tx_hashes evm_node =
    List.map (fun hash -> Evm_node.wait_for_inclusion ~hash evm_node) tx_hashes
    |> Lwt.all
  in
  let aux ~start_nonce ~receiver ~sequencer =
    let* raw_txs =
      List.init batch_tx_size (fun i -> raw_tx (start_nonce + i)) |> Lwt.all
    in
    let batch =
      List.map
        (fun raw_tx ->
          Rpc.Request.eth_sendRawTransactionSync ~block:Rpc.Latest ~raw_tx ())
        raw_txs
    in
    let tx_hashes = List.map compute_tx_hash raw_txs in
    let txs_added_promise evm_node = wait_txs_injected tx_hashes evm_node in
    let txs_added_to_sequencer = txs_added_promise sequencer in
    let txs_included evm_node = wait_txs_included tx_hashes evm_node in
    let txs_included_to_receiver = txs_included receiver in
    let receipts_promise = Evm_node.batch_evm_rpc receiver batch in
    Check.((Lwt.is_sleeping receipts_promise = true) bool)
      ~error_msg:"eth_sendRawTransactionSync should wait for inclusion" ;
    let* _ = txs_added_to_sequencer and* _ = txs_included_to_receiver in
    let*@ nb_txs = produce_block sequencer in
    Check.(
      (nb_txs = batch_tx_size)
        int
        ~error_msg:"Block should contains %R but got %L") ;
    let* receipts = receipts_promise in
    List.iter
      (fun response ->
        let receipt =
          Evm_node.extract_result response
          |> Transaction.transaction_receipt_of_json
        in
        Check.(
          (receipt.status = true)
            bool
            ~error_msg:"Transaction should have been included and successful"))
      receipts ;
    unit
  in
  let*@ _nb_txs = produce_block sandbox in
  let* _ = Evm_node.wait_for_blueprint_applied observer 1 in
  let* () = aux ~start_nonce:0 ~sequencer:sandbox ~receiver:sandbox in
  let* () =
    aux ~start_nonce:batch_tx_size ~sequencer:sandbox ~receiver:observer
  in
  let* rpc_sequencer = run_new_rpc_endpoint sandbox in
  let*@ _nb_txs = produce_block sandbox in
  let* _ = Evm_node.wait_for_blueprint_applied sandbox 4 in
  let* () =
    aux
      ~start_nonce:(2 * batch_tx_size)
      ~sequencer:sandbox
      ~receiver:rpc_sequencer
  in
  let* rpc_observer = run_new_rpc_endpoint observer in
  let*@ _nb_txs = produce_block sandbox in
  let* _ = Evm_node.wait_for_blueprint_applied observer 6 in
  let* () =
    aux
      ~start_nonce:(3 * batch_tx_size)
      ~sequencer:sandbox
      ~receiver:rpc_observer
  in
  unit

let test_tx_pool_pending_nonce () =
  register_sandbox
    ~__FILE__
    ~tags:["evm"; "tx_pool"]
    ~title:"Transaction pool pending nonce"
  @@ fun sequencer ->
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* gas_price = Rpc.get_gas_price sequencer in
  let gas_price = Int32.to_int gas_price in
  let craft_tx ?(gas = 21_000) ?(gas_price = gas_price) nonce =
    Cast.craft_tx
      ~source_private_key:sender.private_key
      ~chain_id:1337
      ~nonce
      ~gas_price
      ~gas
      ~value:(Wei.of_eth_int 10)
      ~address:"0x0000000000000000000000000000000000000000"
      ()
  in
  (* Send 3 transactions, but don't produce a block. *)
  let* tx_0 = craft_tx 0 in
  let* tx_1 = craft_tx 1 in
  let* tx_2 = craft_tx 2 in
  let*@ _ = Rpc.send_raw_transaction ~raw_tx:tx_0 sequencer in
  let*@ _ = Rpc.send_raw_transaction ~raw_tx:tx_1 sequencer in
  let*@ _ = Rpc.send_raw_transaction ~raw_tx:tx_2 sequencer in
  (* Ask the nonce, it should be 3 as we have 3 pending transactions. *)
  let*@ nonce =
    Rpc.get_transaction_count ~block:"pending" ~address:sender.address sequencer
  in
  Check.((nonce = 3L) int64)
    ~error_msg:"Nonce should be 3 as we have 3 pending transactions, but got %L" ;
  unit

let test_node_correctly_uses_batcher_heap =
  let max_blueprints_lag = 10 in
  let max_blueprints_catchup = 10 in
  let catchup_cooldown = 4 in
  register_test
    ~__FILE__
    ~kernel:Kernel.Latest
    ~enable_dal:false
    ~max_blueprints_lag
    ~max_blueprints_catchup
    ~catchup_cooldown
    ~tags:["blueprint"; "injection"; "batcher"]
    ~title:"EVM node uses batcher heap ordering when injecting blueprints."
    ~time_between_blocks:Nothing
  @@
  fun {sequencer; sc_rollup_node; client; sc_rollup_address; node; _}
      _protocol
    ->
  let*@ _ = produce_block sequencer in
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in
  let* l1_level = Node.get_level node in
  let wait_for_level_processed =
    Lwt.join
      [
        (* l1_level + 2 is seen, so l1_level is finalized *)
        (let* _ = Sc_rollup_node.wait_for_level sc_rollup_node (l1_level + 2) in
         unit);
        (* l1_level is finalized and processed. *)
        (let* _ =
           Evm_node.wait_for_processed_l1_level sequencer ~level:l1_level
         in
         unit);
      ]
  in
  let* () =
    (* So the EVM node see the level as finalized *)
    repeat 2 (fun () ->
        let* _ = Client.bake_for_and_wait client in
        unit)
  and* () = wait_for_level_processed in

  let*@ before_level = Rpc.block_number sequencer in
  let before_level = Int32.to_int before_level in
  Log.info
    "Stop the rollup node then produce a first block than won't be in the \
     rollup inbox." ;
  let* () = Sc_rollup_node.terminate sc_rollup_node in
  let* () =
    let*@ _ = produce_block sequencer in
    unit
  and* () =
    Evm_node.wait_for_blueprint_injection_failure
      ~level:(before_level + 1)
      sequencer
  in
  Log.info "Produce more blocks to create a blueprints lag." ;
  let* () =
    (* Produces L2 blocks to grow the blueprint lag but not enough to
       trigger a catchup. *)
    repeat (max_blueprints_lag - 2) (fun () ->
        let*@ _txn = produce_block sequencer in
        unit)
  in
  Log.info "Restart the rollup node." ;
  let* () =
    Sc_rollup_node.run ~event_level:`Debug sc_rollup_node sc_rollup_address []
  and* () =
    Evm_node.wait_for_rollup_node_follower_connection_acquired sequencer
  in
  Log.info "Continue to produce blocks to fills the batcher queue." ;
  let* () =
    (* 2 * max_blueprints_lag: just a random number to have a lot of
       blueprints injected. *)
    repeat (2 * max_blueprints_lag) (fun () ->
        let*@ _txn = produce_block sequencer in
        let*@ level = Rpc.block_number sequencer in
        Evm_node.wait_for_blueprint_injected sequencer (Int32.to_int level))
  in
  Log.info "Produce an L1 block that triggers the catchup." ;
  let wait_for_catchup = Evm_node.wait_for_blueprint_catchup sequencer in
  let wait_for_last_blueprint_catchup_injected =
    Evm_node.wait_for_blueprint_injected
      sequencer
      (before_level + max_blueprints_catchup)
  in
  (* Bake L1 blocks to trigger catchup. *)
  let* _ = Client.bake_for_and_wait client
  and* min, max = wait_for_catchup
  and* () =
    (* last blueprint of catchup injected *)
    wait_for_last_blueprint_catchup_injected
  in
  Check.(
    (min = before_level + 1)
      ~__LOC__
      int
      ~error_msg:
        "Blueprint catchup check \"from\" failed, found %L, expected %R") ;
  Check.(
    (max = before_level + max_blueprints_catchup)
      ~__LOC__
      int
      ~error_msg:"Blueprint catchup check \"to\" failed, found %L, expected %R") ;
  Log.info
    "Catchup is correct, baking 2 l1 blocks to let the rollup node process the \
     inbox." ;
  (* Bake first block to trigger blueprints injection. *)
  let* l1_level = Node.get_level node in
  let l1_level_blueprints_in_inbox = l1_level + 1 in
  let* _ = Client.bake_for_and_wait client
  and* _ =
    Sc_rollup_node.wait_for_level sc_rollup_node l1_level_blueprints_in_inbox
  in

  let l1_level_blueprints_processed = l1_level + 2 in
  (* Bake second block so the rollup node eval blueprints. *)
  let* _ = Client.bake_for_and_wait client
  and* _ =
    Sc_rollup_node.wait_for_level sc_rollup_node l1_level_blueprints_processed
  in

  let*@ rollup_level = rollup_level sc_rollup_node in
  (* Check that the rollup is at least about the expected catchup. *)
  Check.(
    (Int32.to_int rollup_level >= before_level + max_blueprints_catchup)
      ~__LOC__
      int
      ~error_msg:"Check rollup head failed. Found %L, expected >= %R") ;
  let l1_level_blueprints_processed_finalized =
    l1_level_blueprints_processed + 2
  in
  let finalized_blueprint_ref = ref None in
  let wait_for_catchup_l1_level_finalized =
    Lwt.join
      [
        (* l1_level + 2 is seen, so l1_level is finalized. *)
        (let* _ =
           Sc_rollup_node.wait_for_level
             sc_rollup_node
             l1_level_blueprints_processed_finalized
         in
         unit);
        (* l1_level is finalized and processed. *)
        (let* {l1_level = _; finalized_blueprint} =
           Evm_node.wait_for_processed_l1_level
             sequencer
             ~level:l1_level_blueprints_processed
         in
         finalized_blueprint_ref := Some finalized_blueprint ;
         unit);
      ]
  in
  (* 2 block so catchup blueprints processed by the rollup node are
     marked as finalized *)
  let* () =
    repeat 2 (fun () ->
        let* _ = Client.bake_for_and_wait client in
        unit)
  and* _ = wait_for_catchup_l1_level_finalized in

  Check.(
    (!finalized_blueprint_ref >= Some (before_level + max_blueprints_catchup))
      ~__LOC__
      (option int)
      ~error_msg:"Finalized blueprint check failed. Found %L, expected >= %R") ;
  unit

let test_tx_queue =
  register_all
    ~__FILE__
    ~tags:["observer"; "tx_queue"]
    ~time_between_blocks:Nothing
    ~kernels:[Latest] (* node only test *)
    ~use_dal:Register_without_feature
    ~websockets:false
    ~tx_queue:
      {
        max_size = 1000;
        max_lifespan = 100000 (* absurd value so no TX are dropped *);
        tx_per_addr_limit = 100000;
      }
    ~title:"Submits a transaction to an observer with a tx queue."
  @@ fun {sequencer; observer; _} _protocol ->
  let* () =
    let*@ _ = produce_block sequencer in
    unit
  and* () = Evm_node.wait_for_blueprint_applied observer 1 in
  let check_tx_is_found ~__LOC__ ~hash ~node =
    let*@ tx = Rpc.get_transaction_by_hash ~transaction_hash:hash node in
    Check.(
      is_true
        (Option.is_some tx)
        ~__LOC__
        ~error_msg:"Expected to find the transaction but it was not found.") ;
    unit
  in
  (* helper to craft a tx with given nonce. *)
  let raw_tx ~nonce =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce
      ~gas_price:1_000_000_000
      ~gas:23_300
      ~value:Wei.one
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  in

  (* number of transactions that we are going to process (submit,
      inject, ...) *)
  let nb_txs = 10 in

  let wait_for_all_tx_process_p ~hashes ~name ~waiter =
    let* () = Lwt_list.iter_p (fun hash -> waiter ~hash) hashes in
    Log.info "All (%d) txs processed: \"%s\"." (List.length hashes) name ;
    unit
  in

  (* Promises that checks that [nb_txs] txs have been seen with different
     events. *)

  (* Test start here *)
  Log.info
    "Sending %d transactions to the observer and check after each submission \
     that the tx can be retrieved"
    nb_txs ;
  let* hashes =
    fold nb_txs [] @@ fun i hashes ->
    let* raw_tx = raw_tx ~nonce:i in
    let* hash =
      let*@ hash = Rpc.send_raw_transaction ~raw_tx observer in
      return hash
    and* _ = Evm_node.wait_for_tx_queue_add_transaction observer
    and* _ = Evm_node.wait_for_tx_queue_add_transaction sequencer
    and* _ = Evm_node.wait_for_tx_queue_injecting_transaction observer in
    let* () = check_tx_is_found ~__LOC__ ~hash ~node:observer
    and* () = check_tx_is_found ~__LOC__ ~hash ~node:sequencer in
    return (hash :: hashes)
  in

  Log.info
    "Produce enough block to include all txs and make sure they are confirmed \
     by the observer" ;
  (* Checks that all txs were confirmed in the observer *)
  let observer_wait_tx_confirmed =
    let waiter ~hash =
      let* _ =
        Evm_node.wait_for_tx_queue_transaction_confirmed observer ~hash
      in
      unit
    in
    wait_for_all_tx_process_p ~hashes ~name:"tx confirmed in observer" ~waiter
  in

  (* Checks that all txs were included in a unique block by the sequencer *)
  let* () =
    let*@ included_nb_txs = produce_block sequencer in
    Check.(
      (included_nb_txs = nb_txs)
        int
        ~__LOC__
        ~error_msg:"Produce block included %L transaction expected %R") ;
    unit
  and* _ = observer_wait_tx_confirmed in

  Log.info
    "Verifying that all transactions can be retrieved both in the observer and \
     in the sequencer" ;
  let* () =
    Lwt_list.iter_p
      (fun hash ->
        let* () = check_tx_is_found ~__LOC__ ~hash ~node:observer
        and* () = check_tx_is_found ~__LOC__ ~hash ~node:sequencer in
        unit)
      hashes
  in
  unit

let test_tx_queue_clear =
  register_all
    ~__FILE__
    ~title:"Tx_queue clears after delayed inbox flush"
    ~tags:["evm"; "tx_queue"; "clear"; "delayed_inbox"; "flush"]
    ~tx_queue:
      {
        max_size = 1000;
        max_lifespan = 100000 (* absurd value so no TX are dropped *);
        tx_per_addr_limit = 100000;
      }
    ~time_between_blocks:Nothing
    ~delayed_inbox_timeout:0
    ~delayed_inbox_min_levels:1
    ~kernels:[Latest]
    ~use_dal:Register_without_feature
    ~websockets:false
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
  let* () =
    Evm_node.run ~extra_arguments:["--dont-track-rollup-node"] observer
  in

  let* () = Evm_node.wait_for_blueprint_applied observer 0 in
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

  let* raw_tx =
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
  let* _ =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      ~sender:Constant.bootstrap3
      raw_tx
  in

  let wait_for_tx_queue_injected =
    Evm_node.wait_for_tx_queue_injecting_transaction observer
  in
  let* _ = Rpc.send_raw_transaction ~raw_tx observer
  and* _ = wait_for_tx_queue_injected in
  let*@ pending, queued = Rpc.txpool_content observer in
  Check.((List.length pending = 1) int)
    ~error_msg:"Amount of 'Pending' tx should be 1" ;
  Check.((List.length queued = 0) int)
    ~error_msg:"Amount of 'Queued' tx should be 0" ;

  (* Mark at which level the delayed inbox item was added. *)
  let* add_level = Client.level client in
  let wait_for_processed_l1_level_add =
    Evm_node.wait_for_processed_l1_level ~level:add_level sequencer
  in
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

  (* Mark at which level the delayed inbox was flushed. *)
  let* flushed_level = Client.level client in
  let wait_for_processed_l1_level_flushed =
    Evm_node.wait_for_processed_l1_level ~level:flushed_level sequencer
  in
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client
  and* _ = wait_for_processed_l1_level_add in

  (* Produce one L2 block. The sequencer is aware of the delayed inbox
     item but refuses to include it. *)
  let wait_for_flush = Evm_node.wait_for_flush_delayed_inbox sequencer in
  let wait_for_clear = Evm_node.wait_for_tx_queue_cleared observer in
  let*@ _ = Rpc.produce_block ~with_delayed_transactions:false sequencer in

  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client
  and* _ = wait_for_flush
  and* _ = wait_for_processed_l1_level_flushed
  and* () = wait_for_clear in

  let*@ pending, queued = Rpc.txpool_content observer in
  Check.((List.length pending + List.length queued = 0) int)
    ~error_msg:"'Pending' and 'Queued' tx should be empty" ;

  unit

let test_tx_queue_nonce =
  register_all
    ~__FILE__
    ~tags:["observer"; "tx_queue"; "nonce"]
    ~time_between_blocks:Nothing
    ~kernels:[Latest] (* node only test *)
    ~use_dal:Register_without_feature
    ~websockets:false
    ~tx_queue:
      {
        max_size = 1000;
        max_lifespan = 100000 (* absurd value so no TX are dropped *);
        tx_per_addr_limit = 100000;
      }
    ~title:
      "Submits transactions to an observer with a tx queue and make sure it \
       can respond to getTransactionCount."
  @@ fun {sequencer; observer; _} _protocol ->
  let* () =
    let*@ _ = produce_block sequencer in
    unit
  and* () = Evm_node.wait_for_blueprint_applied observer 1 in

  let check_nonce ~__LOC__ ~evm_node ~block ~expected =
    let*@ nonce =
      Rpc.get_transaction_count
        evm_node
        ~block
        ~address:Eth_account.bootstrap_accounts.(0).address
    in
    Check.(
      (Int64.to_int nonce = expected)
        int
        ~__LOC__
        ~error_msg:"Expected nonce %R found %L") ;
    unit
  in

  let check_nonce ~__LOC__ ?(check_observer = false) ?(check_sequencer = false)
      ~block ~expected () =
    let* () =
      if check_observer then
        check_nonce ~__LOC__ ~evm_node:observer ~block ~expected
      else unit
    in
    if check_sequencer then
      check_nonce ~__LOC__ ~evm_node:sequencer ~block ~expected
    else unit
  in

  (* helper to craft a tx with given nonce. *)
  let send_raw_tx ~nonce =
    let* raw_tx =
      Cast.craft_tx
        ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
        ~chain_id:1337
        ~nonce
        ~gas_price:1_000_000_000
        ~gas:23_300
        ~value:Wei.one
        ~address:Eth_account.bootstrap_accounts.(1).address
        ()
    in
    Rpc.send_raw_transaction ~raw_tx observer
  in

  let send_and_wait_sequencer_receive ~nonce =
    let wait_sequencer_see_tx =
      Evm_node.wait_for_tx_queue_add_transaction sequencer
    in
    let* hash =
      let*@ hash = send_raw_tx ~nonce in
      return hash
    and* _ = wait_sequencer_see_tx in
    return hash
  in

  let wait_for_all_tx_process_p ~hashes ~name ~waiter =
    let* () = Lwt_list.iter_p (fun hash -> waiter ~hash) hashes in
    Log.info "All (%d) txs processed: \"%s\"." (List.length hashes) name ;
    unit
  in

  (* Test start here *)
  let* () =
    check_nonce ~__LOC__ ~check_observer:true ~block:"pending" ~expected:0 ()
  in

  (* number of transactions that we are going to process (submit,
      inject, ...) *)
  let nb_txs = 5 in
  Log.info
    "Sending %d transactions to the observer and check after each that the \
     nonce in pending is correct"
    nb_txs ;
  let* hashes =
    fold nb_txs [] @@ fun i hashes ->
    let* hash = send_and_wait_sequencer_receive ~nonce:i in
    let* () =
      check_nonce
        ~__LOC__
        ~check_observer:true
        ~check_sequencer:true
        ~block:"pending"
        ~expected:(i + 1)
        ()
    in
    return (hash :: hashes)
  in

  Log.info
    "Send another txs to create a gap and check that the nonce in pending is \
     still the same" ;
  let observer_dropped_one =
    Evm_node.wait_for_tx_queue_transaction_dropped observer
  in
  let* _lost_hash = send_and_wait_sequencer_receive ~nonce:(nb_txs + 1) in

  let* () =
    check_nonce
      ~__LOC__
      ~check_observer:true
      ~check_sequencer:true
      ~block:"pending"
      ~expected:nb_txs
      ()
  in

  Log.info
    "Send missing nonce to fill the gap and check that the nonce in pending is \
     now correct" ;
  let* hash = send_and_wait_sequencer_receive ~nonce:nb_txs in

  Log.info
    "produce enough block to include all txs and make sure the nonce of latest \
     and pending is equal." ;
  (* Checks that all txs were confirmed in the observer *)
  let observer_wait_tx_confirmed () =
    let waiter ~hash =
      let* _ =
        Evm_node.wait_for_tx_queue_transaction_confirmed ~hash observer
      in
      unit
    in
    let hashes = hash :: hashes in
    wait_for_all_tx_process_p ~hashes ~name:"tx confirmed in observer" ~waiter
  in

  let observer_txs_confirmed = observer_wait_tx_confirmed () in
  (* Checks that all txs were included in a unique block by the sequencer *)
  let* () =
    let*@ included_nb_txs = produce_block sequencer in
    Check.(
      (included_nb_txs = nb_txs + 1)
        int
        ~__LOC__
        ~error_msg:"Produce block included %L transaction expected %R") ;
    unit
  and* _ = observer_txs_confirmed
  and* _ = observer_dropped_one in

  let* () =
    check_nonce
      ~__LOC__
      ~check_observer:true
      ~check_sequencer:true
      ~block:"latest"
      ~expected:(nb_txs + 1)
        (* latest two transaction were submitted in the wrong order,
           only the second one is correctly applied. The other still
           lives in the pending state of the observer tx_queue for
           now, it's reason for "pending" still it. *)
      ()
  in
  let* () =
    (* In the sequencer the wrong ordered tx is dropped when creating
        In the observer the wrong ordered tx has been dropped with the
       instant confirmation. *)
    check_nonce
      ~__LOC__
      ~check_observer:true
      ~check_sequencer:true
      ~block:"pending"
      ~expected:(nb_txs + 1)
      ()
  in

  Log.info "Try to send a transaction with a nonce in the past." ;
  let*@? _hash = send_raw_tx ~nonce:nb_txs in

  (* still true with a valid tx in pending. *)
  let* _hash = send_and_wait_sequencer_receive ~nonce:(nb_txs + 1) in
  let*@? _hash = send_raw_tx ~nonce:nb_txs in

  Log.info "Try to send a transaction with an nonce already pending, is valid." ;
  let noop_inject =
    Evm_node.wait_for_event observer ~event:"transaction_already_present.v0"
    @@ Fun.const (Some ())
  in
  let* _hash = send_raw_tx ~nonce:(nb_txs + 1) and* () = noop_inject in
  unit

let test_tx_queue_limit =
  let max_number_of_txs = 10 in
  register_all
    ~__FILE__
    ~tags:["observer"; "tx_queue"; "limit"]
    ~time_between_blocks:Nothing
    ~kernels:[Latest] (* node only test *)
    ~use_dal:Register_without_feature
    ~websockets:false
    ~tx_queue:
      {
        max_size = 1000;
        max_lifespan = 100000 (* absurd value so no TX are dropped *);
        tx_per_addr_limit = max_number_of_txs;
      }
    ~title:
      "Submits transactions to an observer with a tx queue and make sure its \
       limit are respected."
  @@ fun {sequencer; observer; _} _protocol ->
  let* () =
    let*@ _ = produce_block sequencer in
    unit
  and* () = Evm_node.wait_for_blueprint_applied observer 1 in

  (* helper to craft a tx with given nonce. *)
  let send_raw_tx ~nonce =
    let* raw_tx =
      Cast.craft_tx
        ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
        ~chain_id:1337
        ~nonce
        ~gas_price:1_000_000_000
        ~gas:23_300
        ~value:Wei.one
        ~address:Eth_account.bootstrap_accounts.(1).address
        ()
    in
    Rpc.send_raw_transaction ~raw_tx observer
  in

  let send_and_wait_sequencer_receive ~nonce =
    let wait_sequencer_see_tx =
      Evm_node.wait_for_tx_queue_add_transaction sequencer
    in
    let* hash =
      let*@ hash = send_raw_tx ~nonce in
      return hash
    and* _ = wait_sequencer_see_tx in
    return hash
  in

  Log.info
    "send %d txs, all are successfully added to the queue"
    max_number_of_txs ;
  let* hashes =
    fold max_number_of_txs [] (fun i hashes ->
        let* hash = send_and_wait_sequencer_receive ~nonce:i in
        return (hash :: hashes))
  in

  Log.info "Then send an additional txs, that fails" ;
  let*@? _error = send_raw_tx ~nonce:max_number_of_txs in

  Log.info "Produce a block, all transaction are confirmed" ;
  let observer_wait_tx_confirmed () =
    let* () =
      Lwt_list.iter_p
        (fun hash ->
          let* _ =
            Evm_node.wait_for_tx_queue_transaction_confirmed ~hash observer
          in
          Log.debug "tx %s confirmed" hash ;
          unit)
        hashes
    in
    Log.info "All (%d) txs confirmed in observer" max_number_of_txs ;
    unit
  in

  (* Checks that all txs were included in a unique block by the sequencer *)
  let* () =
    let*@ included_nb_txs = produce_block sequencer in
    Check.(
      (included_nb_txs = max_number_of_txs)
        int
        ~__LOC__
        ~error_msg:"Produce block included %L transaction expected %R") ;
    unit
  and* _ = observer_wait_tx_confirmed () in

  Log.info
    "Resend %d txs, all are successfully added to the queue"
    max_number_of_txs ;
  let* () =
    Lwt_list.iter_s
      (fun i ->
        let* _ = send_and_wait_sequencer_receive ~nonce:i in
        unit)
      (range max_number_of_txs ((2 * max_number_of_txs) - 1))
  in
  unit

let test_locked_tx_queue_timestamp () =
  register_sandbox_with_observer
    ~genesis_timestamp
    ~patch_config:
      (Evm_node.patch_config_with_experimental_feature
         ~preconfirmation_stream_enabled:true
         ())
    ~__FILE__
    ~tags:["evm"; "sequencer"; "locked"; "timestamp"]
    ~title:"Locked tx queue produces blocks with correct timestamp"
  @@ fun {sandbox; observer} ->
  (* Use a stale timestamp after genesis (2020-01-01) so that if the fix
     regresses, the kernel accepts it and the assertion catches the error
     instead of a TimestampFromPast crash. *)
  let*@ () = Rpc.lock_block_production sandbox in
  let stale_timestamp = "2020-01-02T00:00:00Z" in
  let*@ () =
    Rpc.propose_next_block_timestamp ~timestamp:stale_timestamp sandbox
  in

  (* produce_block fails while locked, but the fix refreshes the timestamp *)
  let* _ = Rpc.produce_block sandbox in

  let*@ () = Rpc.unlock_block_production sandbox in

  (* After unlock, the block should use a current timestamp, not the stale one *)
  let*@ _ = produce_block sandbox in
  let* () = Evm_node.wait_for_blueprint_applied observer 1 in

  let*@ block = Rpc.get_block_by_number ~block:"1" sandbox in
  let block_timestamp = Tezos_base.Time.Protocol.to_notation block.timestamp in
  Check.((block_timestamp <> stale_timestamp) string)
    ~error_msg:"Block timestamp should be current, not stale (got %L)" ;
  unit

let protocols = [Protocol.Alpha]

let () =
  test_batch_limit_size_rpc protocols ;
  test_batch_eth_send_raw_transaction_sync_rpc () ;
  test_tx_pool_pending_nonce () ;
  test_node_correctly_uses_batcher_heap [Protocol.Alpha] ;
  test_tx_queue [Alpha] ;
  test_tx_queue_clear [Alpha] ;
  test_tx_queue_nonce [Alpha] ;
  test_tx_queue_limit [Alpha] ;
  test_locked_tx_queue_timestamp ()
