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
                 npm install eth-cli solc@0.8.26
                 # Install cast or foundry (see: https://book.getfoundry.sh/getting-started/installation)
                 curl -L https://foundry.paradigm.xyz | bash
                 foundryup
                 make -f etherlink.mk octez-dsn-node
                 ./scripts/install_dal_trusted_setup.sh
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file gc.ml
*)

open Rpc.Syntax

let register ?genesis_timestamp
    ?(garbage_collector_parameters =
      Evm_node.{split_frequency_in_seconds = 100; number_of_chunks = 5}) ~title
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
  let patch_config =
    Evm_node.patch_config_with_experimental_feature
      ~block_storage_sqlite3:true
      ~garbage_collector_parameters
      ~history_mode:Rolling
      ()
  in
  let* sequencer =
    Helpers.init_sequencer_sandbox ?genesis_timestamp ~patch_config ()
  in
  f sequencer

let test_gc_boundaries () =
  let genesis_timestamp =
    Client.(At (Time.of_notation_exn "2020-01-01T00:00:00Z"))
  in
  let garbage_collector_parameters =
    Evm_node.{split_frequency_in_seconds = 1; number_of_chunks = 3}
  in
  register
    ~genesis_timestamp
    ~garbage_collector_parameters
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
        let timestamp = sf "2020-01-01T00:00:0%dZ" (i + 1) in
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
  let genesis_timestamp =
    Client.(At (Time.of_notation_exn "2020-01-01T00:00:00Z"))
  in
  let garbage_collector_parameters =
    Evm_node.{split_frequency_in_seconds = 1; number_of_chunks = 2}
  in
  register
    ~genesis_timestamp
    ~garbage_collector_parameters
    ~title:"Switch history mode (rolling -> archive)"
    ~tags:["history_mode"]
  @@ fun sequencer ->
  let* () = Evm_node.terminate sequencer in
  let wait_for_rolling =
    Evm_node.wait_for_start_history_mode ~history_mode:"rolling" sequencer
  in
  let* () = Evm_node.run sequencer and* _ = wait_for_rolling in
  (* We want the evm-node to trigger at least one garbage collector to
     have only partial history. *)
  let wait_for_gc = Evm_node.wait_for_gc_finished sequencer in
  let* _ =
    fold 3 () (fun i () ->
        let* _ =
          Rpc.produce_block
            ~timestamp:(sf "2020-01-01T00:00:0%dZ" (i + 1))
            sequencer
        in
        unit)
  and* _ = wait_for_gc in
  let*@ earliest_block = Rpc.get_block_by_number ~block:"earliest" sequencer in
  Check.((earliest_block.number > 0l) int32)
    ~error_msg:"Garbage collector should have removed genesis block" ;
  (* Restart the node in archive mode. *)
  let* () = Evm_node.terminate sequencer in
  let* () =
    Evm_node.Config_file.update
      sequencer
      JSON.(
        update
          "experimental_features"
          (put
             ( "history_mode",
               annotate ~origin:"history_mode" (`String "archive") )))
  in
  let wait_for_archive =
    Evm_node.wait_for_start_history_mode ~history_mode:"archive" sequencer
  in
  let wait_for_incomplete_history =
    Evm_node.wait_for_event
      sequencer
      ~event:"evm_context_rolling_to_archive_incomplete_history.v0"
      (fun _json -> Some ())
  in
  let* () = Evm_node.run sequencer
  and* () = wait_for_incomplete_history
  and* _ = wait_for_archive in
  unit

let () =
  test_gc_boundaries () ;
  test_switch_history_mode ()
