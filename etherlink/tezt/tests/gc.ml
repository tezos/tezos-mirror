(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    EVM Node garbage collector
   Requirement:  make -f etherlink.mk build
                 npm install eth-cli solc@0.8.31
                 # Install cast or foundry (see: https://book.getfoundry.sh/getting-started/installation)
                 curl -L https://foundry.paradigm.xyz | bash
                 foundryup
                 ./scripts/install_dal_trusted_setup.sh
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file gc.ml
*)

open Rpc.Syntax
open Test_helpers
open Setup
open Delayed_inbox

let register ?genesis_timestamp ?(history_mode = Evm_node.Rolling 5) ~title
    ~tags f =
  Test.register
    ~__FILE__
    ~title
    ~tags
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
  let patch_config = Evm_node.patch_config_with_experimental_feature () in
  let* sequencer =
    init_sequencer_sandbox ?genesis_timestamp ~history_mode ~patch_config ()
  in
  f sequencer

let test_gc_boundaries () =
  register
    ~genesis_timestamp
    ~history_mode:(Rolling 3)
    ~title:"GC boundaries"
    ~tags:["boundaries"; "earliest"]
  @@ fun sequencer ->
  let wait_for_split = Evm_node.wait_for_split ~level:9 sequencer in
  let wait_for_gc =
    Evm_node.wait_for_gc_finished ~gc_level:6 ~head_level:9 sequencer
  in
  (* Produce blocks, one per second. Enough to trigger a garbage collector. *)
  let*@ level = Rpc.block_number sequencer in
  let level = Int32.to_int level in
  let* _ =
    fold 9 () (fun i () ->
        let level = level + i + 1 in
        let timestamp = get_timestamp i in
        let wait_for_split = Evm_node.wait_for_split ~level sequencer in
        let wait_for_gc =
          if level > 3 then
            Some
              (Evm_node.wait_for_gc_finished
                 ~gc_level:(level - 3)
                 ~head_level:level
                 sequencer)
          else None
        in
        let* _ = wait_for_split
        and* _ =
          match wait_for_gc with
          | None -> unit
          | Some wait_for_gc ->
              let* _ = wait_for_gc in
              unit
        and* res = Rpc.produce_block ~timestamp sequencer in
        (* Just a sanity check to make sure it's a success, but it should
           be obviously true as it would have failed on the wait for events. *)
        assert (Result.is_ok res) ;
        unit)
  in
  let* _ = wait_for_split and* _ = wait_for_gc in
  (* The GC happenned at level 6, therefore the block 6 is supposed to be the
     earliest block. *)
  let*@ block = Rpc.get_block_by_number ~block:"earliest" sequencer in
  Check.((block.number = 6l) int32)
    ~error_msg:"Earliest block should be 6, but got %L" ;
  let*@ _balance =
    Rpc.get_balance
      ~address:"0xB53dc01974176E5dFf2298C5a94343c2585E3c54"
      ~block:(Number 6)
      sequencer
  in
  (* The block storage should also have cleaned everything behind 6. *)
  let*@? err = Rpc.get_block_by_number ~block:"5" sequencer in
  Check.(err.message =~ rex "Block 5 not found")
    ~error_msg:"The block 5 should be missing" ;
  let*@? err =
    Rpc.get_balance
      ~address:"0xB53dc01974176E5dFf2298C5a94343c2585E3c54"
      ~block:(Number 5)
      sequencer
  in
  Check.(err.message =~ rex "No state available for block 5")
    ~error_msg:"The state for block 5 should be missing" ;

  unit

let test_switch_history_mode () =
  let retention_period = 2 in
  let blocks_before_switch = 3 in
  register
    ~genesis_timestamp
    ~title:"Switch history mode"
    ~history_mode:Archive
    ~tags:["history_mode"]
  @@ fun sequencer ->
  (* Switch history mode downwards: archive -> full -> rolling *)
  let* _ =
    fold blocks_before_switch () (fun i () ->
        let timestamp = get_timestamp i in
        let* _ = Rpc.produce_block ~timestamp sequencer in
        unit)
  in
  (* Restart the node in full mode *)
  let* () = Evm_node.terminate sequencer in
  let* () =
    Evm_node.Config_file.update
      sequencer
      (Evm_node.patch_config_gc ~history_mode:(Full retention_period))
  in
  (* Config update is not enough *)
  let process = Evm_node.spawn_run sequencer in
  let* () =
    Process.check_error
      ~msg:(rex "cannot be run with history mode full")
      process
  in
  (* Switch history explicitely *)
  let*! () = Evm_node.switch_history_mode sequencer (Full retention_period) in
  let wait_for_full =
    Evm_node.wait_for_start_history_mode
      ~history_mode:(Format.sprintf "full:%d" retention_period)
      sequencer
  in
  let* () = Evm_node.run sequencer and* _ = wait_for_full in
  (* Show that we indeed gc *)
  let wait_for_gc = Evm_node.wait_for_gc_finished sequencer in
  let* _ =
    fold blocks_before_switch () (fun i () ->
        let timestamp = get_timestamp (blocks_before_switch + i + 1) in
        let* _ = Rpc.produce_block ~timestamp sequencer in
        unit)
  and* _ = wait_for_gc in
  (* Restart the node in rolling mode *)
  let* () = Evm_node.terminate sequencer in
  let*! () =
    Evm_node.switch_history_mode sequencer (Rolling retention_period)
  in
  let wait_for_rolling =
    Evm_node.wait_for_start_history_mode
      ~history_mode:(Format.sprintf "rolling:%d" retention_period)
      sequencer
  in
  let* () = Evm_node.run sequencer and* _ = wait_for_rolling in
  unit

let test_switch_history_mode_shorter_retention_period () =
  let initial_retention_period = 5 in
  let new_retention_period = 2 in
  let blocks_to_produce = initial_retention_period + 1 in
  register
    ~genesis_timestamp
    ~title:"Switch history mode with shorter retention period"
    ~history_mode:(Rolling initial_retention_period)
    ~tags:["history_mode"]
  @@ fun sequencer ->
  let wait_for_gc = Evm_node.wait_for_gc_finished sequencer in
  let* _ =
    fold blocks_to_produce () (fun i () ->
        let timestamp = get_timestamp i in
        let* _ = Rpc.produce_block ~timestamp sequencer in
        unit)
  and* _ = wait_for_gc in

  let*@ earliest_block = Rpc.get_block_by_number ~block:"earliest" sequencer in
  Check.(
    (earliest_block.number
    = Int32.of_int (blocks_to_produce - initial_retention_period))
      int32
      ~error_msg:"Expected the head to be %R after GC, got %L") ;

  let* () = Evm_node.terminate sequencer in
  let*! () =
    Evm_node.switch_history_mode sequencer (Rolling new_retention_period)
  in
  let* () = Evm_node.run sequencer in

  (* Create one block, that will be enough to trigger a new gc *)
  let wait_for_gc = Evm_node.wait_for_gc_finished sequencer in
  let* _ =
    let timestamp = get_timestamp blocks_to_produce in
    let* _ = Rpc.produce_block ~timestamp sequencer in
    unit
  and* _ = wait_for_gc in

  let*@ earliest_block = Rpc.get_block_by_number ~block:"earliest" sequencer in
  Check.(
    (earliest_block.number
    = Int32.of_int (blocks_to_produce + 1 - new_retention_period))
      int32
      ~error_msg:"Expected the head to be %R after second GC, got %L") ;

  unit

let test_invalid_switch_history_mode () =
  register
    ~history_mode:(Rolling 2)
    ~title:"Invalid switch history mode"
    ~tags:["history_mode"]
  @@ fun sequencer ->
  let* () = Evm_node.terminate sequencer in
  let {Runnable.value = process; _} =
    Evm_node.switch_history_mode sequencer (Full 2)
  in
  let* () =
    Process.check_error
      ~msg:(rex "cannot be run with history mode full")
      process
  in
  unit

let test_full_history_mode_gc () =
  register
    ~genesis_timestamp
    ~history_mode:(Full 2)
    ~title:"Full history mode GC"
    ~tags:["boundaries"; "gc"; "history_mode"]
  @@ fun sequencer ->
  let raw_tx nonce =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce
      ~gas_price:1_000_000_000
      ~gas:25_000
      ~value:Wei.one
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  in
  (* Produce txs and trigger one garbage collector *)
  let* list =
    fold 3 [] (fun i acc ->
        let* raw_tx = raw_tx i in
        let*@ hash = Rpc.send_raw_transaction ~raw_tx sequencer in
        let* _ = Rpc.produce_block ~timestamp:(get_timestamp i) sequencer in
        return (hash :: acc))
  and* _ = Evm_node.wait_for_gc_finished sequencer in

  (* Block 0 is still present *)
  let*@ earliest_block = Rpc.get_block_by_number ~block:"0" sequencer in
  Check.((earliest_block.number = 0l) int32)
    ~error_msg:"Garbage collector should *not* have removed genesis block" ;

  (* Query the transactions by hash *)
  let* () =
    Lwt_list.iter_s
      (fun transaction_hash ->
        let*@ tx_obj =
          Rpc.get_transaction_by_hash ~transaction_hash sequencer
        in
        Check.is_true (Option.is_some tx_obj) ~error_msg:"Transaction missing" ;
        unit)
      list
  in

  (* But get_balance fails as it requires the context *)
  let*@? _e =
    Rpc.get_balance
      ~address:Eth_account.bootstrap_accounts.(0).address
      ~block:(Number 0)
      sequencer
  in
  unit

let get_blueprint sequencer number =
  Runnable.run
  @@ Curl.get
       ~name:("curl#" ^ Evm_node.name sequencer)
       ~args:["--fail"]
       (Evm_node.endpoint sequencer
       ^ "/evm/v2/blueprint/" ^ Int64.to_string number)

let start_fresh_observer sequencer =
  Evm_node.init
    ~node_setup:
      (Evm_node.make_setup
         ~name:"fresh_observer"
         ?initial_kernel:(Evm_node.initial_kernel sequencer)
         ~preimages_dir:(Evm_node.preimages_dir sequencer)
         ())
    ~mode:
      (Observer
         {
           rollup_node_endpoint = None;
           evm_node_endpoint = Evm_node.endpoint sequencer;
         })
    ~extra_arguments:["--dont-track-rollup-node"]
    ()

let test_seed_history_mode_gc () =
  let retention_period = 3 in
  register
    ~genesis_timestamp
    ~history_mode:(Seed retention_period)
    ~title:"Seed history mode: GC runs and blueprints remain"
    ~tags:["seed"; "history_mode"; "gc"]
  @@ fun sequencer ->
  let blocks_to_produce = retention_period + 1 in
  let wait_for_gc = Evm_node.wait_for_gc_finished sequencer in
  let* _ =
    fold blocks_to_produce () (fun i () ->
        let*@ _ = Rpc.produce_block ~timestamp:(get_timestamp i) sequencer in
        unit)
  and* _ = wait_for_gc in
  (* Blueprint REST endpoint works for a recent block after GC *)
  let* _ = get_blueprint sequencer (Int64.of_int blocks_to_produce) in
  let* _ = get_blueprint sequencer (Int64.of_int 0) in
  (* Blocks should have been pruned by the GC (gc_level=1, so block 0 is gone) *)
  let*@? err = Rpc.get_block_by_number ~block:"0" sequencer in
  Check.(err.message =~ rex "Block 0 not found")
    ~error_msg:"Block 0 should have been pruned" ;
  unit

let test_switch_to_seed_mode () =
  let retention_period = 2 in
  let blocks_before_switch = 3 in
  register
    ~genesis_timestamp
    ~title:"Switch to seed history mode"
    ~history_mode:Archive
    ~tags:["seed"; "history_mode"; "switch"]
  @@ fun sequencer ->
  let* _ =
    fold blocks_before_switch () (fun i () ->
        let*@ _ = Rpc.produce_block ~timestamp:(get_timestamp i) sequencer in
        unit)
  in
  let* () = Evm_node.terminate sequencer in
  let*! () = Evm_node.switch_history_mode sequencer (Seed retention_period) in
  let wait_for_blueprint =
    Evm_node.wait_for_start_history_mode
      ~history_mode:(Format.sprintf "seed:%d" retention_period)
      sequencer
  in
  let* () = Evm_node.run sequencer and* _ = wait_for_blueprint in
  unit

let test_seed_history_mode_invalid_switch () =
  register
    ~history_mode:(Seed 2)
    ~title:"Seed history mode: invalid switch"
    ~tags:["seed"; "history_mode"; "switch"]
  @@ fun sequencer ->
  let* () = Evm_node.terminate sequencer in
  let {Runnable.value = process; _} =
    Evm_node.switch_history_mode sequencer Archive
  in
  let* () =
    Process.check_error
      ~msg:(rex "cannot be run with history mode archive")
      process
  in
  unit

let test_switch_seed_to_rolling () =
  let retention_period = 3 in
  let blocks_before_switch = retention_period + 1 in
  register
    ~genesis_timestamp
    ~history_mode:(Seed retention_period)
    ~title:"Switch from seed to rolling"
    ~tags:["seed"; "history_mode"; "switch"; "rolling"]
  @@ fun sequencer ->
  (* Produce blocks and trigger GC in seed mode *)
  let wait_for_gc = Evm_node.wait_for_gc_finished sequencer in
  let* _ =
    fold blocks_before_switch () (fun i () ->
        let*@ _ = Rpc.produce_block ~timestamp:(get_timestamp i) sequencer in
        unit)
  and* _ = wait_for_gc in
  (* In seed mode: block 0 is pruned but blueprints survive *)
  let*@? err = Rpc.get_block_by_number ~block:"0" sequencer in
  Check.(err.message =~ rex "Block 0 not found")
    ~error_msg:"Block 0 should have been pruned in seed mode" ;
  let* _ = get_blueprint sequencer (Int64.of_int 1) in
  (* Switch to rolling mode *)
  let* () = Evm_node.terminate sequencer in
  let*! () =
    Evm_node.switch_history_mode sequencer (Rolling retention_period)
  in
  let wait_for_rolling =
    Evm_node.wait_for_start_history_mode
      ~history_mode:(Format.sprintf "rolling:%d" retention_period)
      sequencer
  in
  let* () = Evm_node.run sequencer and* _ = wait_for_rolling in
  (* Produce more blocks to trigger GC in rolling mode.
     Total blocks = blocks_before_switch * 2 = 8,
     expected gc_level = 8 - 3 = 5, earliest block = 5. *)
  let total_blocks = blocks_before_switch * 2 in
  let expected_gc_level = total_blocks - retention_period in
  let wait_for_gc =
    Evm_node.wait_for_gc_finished
      ~gc_level:expected_gc_level
      ~head_level:total_blocks
      sequencer
  in
  let* _ =
    fold blocks_before_switch () (fun i () ->
        let*@ _ =
          Rpc.produce_block
            ~timestamp:(get_timestamp (blocks_before_switch + i))
            sequencer
        in
        unit)
  and* _ = wait_for_gc in
  (* In rolling mode, the earliest block matches the GC boundary *)
  let*@ earliest_block = Rpc.get_block_by_number ~block:"earliest" sequencer in
  Check.(
    (earliest_block.number = Int32.of_int expected_gc_level)
      int32
      ~error_msg:"Expected earliest block to be %R after rolling GC, got %L") ;
  unit

let test_seed_history_mode_gc_with_deposit =
  let retention_period = 3 in
  register_all
    ~__FILE__
    ~kernels:[Kernel.Latest]
    ~tags:["seed"; "history_mode"; "gc"; "deposit"]
    ~title:"Seed history mode: blueprints with deposits survive GC"
    ~time_between_blocks:Nothing
    ~history_mode:(Seed retention_period)
    ~use_dal:Register_without_feature
    ~use_multichain:Register_without_feature
  @@
  fun {sequencer; sc_rollup_node; client; l1_contracts; sc_rollup_address; _}
      _protocol
    ->
  (* Send a deposit to the delayed inbox *)
  let* () =
    send_deposit_to_delayed_inbox
      ~amount:(Tez.of_int 2)
      ~bridge:l1_contracts.bridge
      ~depositor:Constant.bootstrap5
      ~deposit_info:
        {
          receiver = EthereumAddr Eth_account.bootstrap_accounts.(0).address;
          chain_id = None;
        }
      ~sc_rollup_node
      ~sc_rollup_address
      client
  in
  (* Wait for the deposit to be included in a block. We use
     [wait_for_delayed_inbox_add_tx_and_injected] which produces blocks at
     wall-clock time — this is necessary because the kernel only processes L1
     inbox messages at timestamps compatible with L1. *)
  let* () =
    wait_for_delayed_inbox_add_tx_and_injected
      ~sequencer
      ~sc_rollup_node
      ~client
  in
  (* Record which block contains the deposit *)
  let*@ deposit_level = Rpc.block_number sequencer in
  let deposit_level = Int32.to_int deposit_level in
  (* Produce enough blocks with future timestamps to trigger GC.
     Timestamps start after the deposit block's wall-clock timestamp. *)
  let now =
    match Ptime.of_float_s (Unix.gettimeofday ()) with
    | Some t -> t
    | None -> Test.fail "Could not get current time"
  in
  let future_timestamp i =
    Ptime.add_span now (days (i + 1)) |> Option.get |> Client.Time.to_notation
  in
  let wait_for_gc = Evm_node.wait_for_gc_finished sequencer in
  let* _ =
    fold (retention_period + 1) () (fun i () ->
        let*@ _ = Rpc.produce_block ~timestamp:(future_timestamp i) sequencer in
        unit)
  and* _ = wait_for_gc in
  (* Blueprint containing the deposit event should survive GC *)
  let* blueprint = get_blueprint sequencer (Int64.of_int deposit_level) in
  let delayed_txs = JSON.(blueprint |-> "delayed_transactions" |> as_list) in
  Check.is_true
    (List.length delayed_txs > 0)
    ~error_msg:"Blueprint should contain deposit events after GC" ;
  (* A fresh observer should catch up from the sequencer *)
  let*@ head_level = Rpc.block_number sequencer in
  let* fresh_observer = start_fresh_observer sequencer in
  let* _ =
    Evm_node.wait_for_blueprint_applied fresh_observer (Int32.to_int head_level)
  in
  unit

let () =
  test_gc_boundaries () ;
  test_switch_history_mode () ;
  test_switch_history_mode_shorter_retention_period () ;
  test_invalid_switch_history_mode () ;
  test_full_history_mode_gc () ;
  test_seed_history_mode_gc () ;
  test_switch_to_seed_mode () ;
  test_seed_history_mode_invalid_switch () ;
  test_switch_seed_to_rolling () ;
  test_seed_history_mode_gc_with_deposit Protocol.[Alpha]
