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

let () =
  test_gc_boundaries () ;
  test_switch_history_mode () ;
  test_switch_history_mode_shorter_retention_period () ;
  test_invalid_switch_history_mode () ;
  test_full_history_mode_gc ()
