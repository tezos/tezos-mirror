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
    Helpers.init_sequencer_sandbox
      ?genesis_timestamp
      ~history_mode
      ~patch_config
      ()
  in
  f sequencer

let days n = Ptime.Span.of_int_s (n * 86400)

let test_gc_boundaries () =
  let genesis_time = Client.Time.of_notation_exn "2020-01-01T00:00:00Z" in
  let genesis_timestamp = Client.At genesis_time in
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
        let timestamp =
          (* Move one day every block *)
          Ptime.add_span genesis_time (days (i + 1))
          |> Option.get |> Client.Time.to_notation
        in
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
  let genesis_time = Client.Time.of_notation_exn "2020-01-01T00:00:00Z" in
  let genesis_timestamp = Client.At genesis_time in
  let retention_period = 2 in
  let get_timestamp i =
    (* Move one day every block *)
    Ptime.add_span genesis_time (days (i + 1))
    |> Option.get |> Client.Time.to_notation
  in
  let blocks_before_switch = 3 in
  register
    ~genesis_timestamp
    ~title:"Switch history mode (archive -> rolling)"
    ~history_mode:Archive
    ~tags:["history_mode"]
  @@ fun sequencer ->
  let* () = Evm_node.terminate sequencer in
  let wait_for_archive =
    Evm_node.wait_for_start_history_mode ~history_mode:"archive" sequencer
  in
  let* () = Evm_node.run sequencer and* _ = wait_for_archive in
  let* _ =
    fold blocks_before_switch () (fun i () ->
        let timestamp = get_timestamp i in
        let* _ = Rpc.produce_block ~timestamp sequencer in
        unit)
  in
  (* Restart the node in archive mode. *)
  let* () = Evm_node.terminate sequencer in
  let* () =
    Evm_node.Config_file.update
      sequencer
      (Evm_node.patch_config_gc ~history_mode:(Rolling retention_period))
  in
  let process = Evm_node.spawn_run sequencer in
  let* () =
    Process.check_error ~msg:(rex "cannot be run with history mode") process
  in
  let*! () =
    Evm_node.switch_history_mode sequencer (Rolling retention_period)
  in
  let wait_for_rolling =
    Evm_node.wait_for_start_history_mode
      ~history_mode:(Format.sprintf "rolling:%d" retention_period)
      sequencer
  in
  let* () = Evm_node.run sequencer and* _ = wait_for_rolling in
  (* show that we indeed gc *)
  let wait_for_gc = Evm_node.wait_for_gc_finished sequencer in
  let* _ =
    fold (retention_period + 1) () (fun i () ->
        let timestamp = get_timestamp (blocks_before_switch + i + 1) in
        let* _ = Rpc.produce_block ~timestamp sequencer in
        unit)
  and* _ = wait_for_gc in
  unit

let () =
  test_gc_boundaries () ;
  test_switch_history_mode ()
