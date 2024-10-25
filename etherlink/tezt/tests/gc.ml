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
    ?(garbage_collector =
      Evm_node.
        {split_frequency_in_seconds = 100; history_to_keep_in_seconds = 100})
    ~title ~tags f =
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
      ~node_transaction_validation:true
      ~block_storage_sqlite3:true
      ~garbage_collector
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
  let garbage_collector =
    Evm_node.{split_frequency_in_seconds = 1; history_to_keep_in_seconds = 3}
  in
  register
    ~genesis_timestamp
    ~garbage_collector
    ~title:"GC boundaries"
    ~tags:["boundaries"; "earliest"]
  @@ fun sequencer ->
  let wait_for_split = Evm_node.wait_for_split ~level:9 sequencer in
  let wait_for_gc =
    Evm_node.wait_for_gc_finished ~gc_level:6 ~head_level:9 sequencer
  in
  (* Produce blocks, one per second. Enough to trigger a garbage collector. *)
  let* _ =
    fold 9 () (fun i () ->
        let timestamp = sf "2020-01-01T00:00:0%dZ" (i + 1) in
        let*@ _ = Rpc.produce_block ~timestamp sequencer in
        unit)
  in
  let* _ = wait_for_split in
  let* _ = wait_for_gc in
  (* The GC happenned at level 6, therefore the block 6 is supposed to be the
     earliest block. *)
  let*@ block = Rpc.get_block_by_number ~block:"earliest" sequencer in
  Check.((block.number = 6l) int32)
    ~error_msg:"Earliest block should be 6, but got %L" ;
  (* The block storage should also have cleaned everything behind 6. *)
  let*@? err = Rpc.get_block_by_number ~block:"5" sequencer in
  Check.(err.message =~ rex "Block 5 not found")
    ~error_msg:"The block 5 should be missing" ;
  unit

let () = test_gc_boundaries ()
