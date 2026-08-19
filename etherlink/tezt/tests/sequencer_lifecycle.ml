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
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file sequencer_lifecycle.ml
   Subject:      Sequencer lifecycle: blueprint publication, snapshots, divergence, reset.
*)

open Sc_rollup_helpers
open Rpc.Syntax
open Delayed_inbox
open Test_helpers
open Setup
module Protocol = Test_helpers.Protocol_no_direct_register

let test_remove_sequencer =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "admin"]
    ~title:"Remove sequencer via sequencer admin contract"
  @@
  fun {
        sequencer;
        sc_rollup_node;
        client;
        sc_rollup_address;
        l1_contracts;
        observer;
        _;
      }
      _protocol
    ->
  (* Produce blocks to show that both the sequencer and rollup node are not
     progressing. *)
  let* _ =
    repeat 5 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  (* Both are at genesis *)
  let*@ sequencer_head = Rpc.block_number sequencer in
  let*@ rollup_head = rollup_level sc_rollup_node in
  Check.((sequencer_head = 0l) int32)
    ~error_msg:"Sequencer should be at genesis" ;
  Check.((sequencer_head = rollup_head) int32)
    ~error_msg:"Sequencer and rollup node should have the same block number" ;
  (* Remove the sequencer via the sequencer-admin contract. *)
  let* () =
    Client.transfer
      ~amount:Tez.zero
      ~giver:Constant.bootstrap2.public_key_hash
      ~receiver:l1_contracts.sequencer_governance
      ~arg:(sf "Pair %S 0x" sc_rollup_address)
      ~burn_cap:Tez.one
      client
  in
  let* exit_code = Evm_node.wait_for_shutdown_event sequencer
  and* missing_block_nb = Evm_node.wait_for_rollup_node_ahead observer
  and* () =
    (* Produce L1 blocks to show that only the rollup node is progressing *)
    repeat 5 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  Check.((exit_code = Some 100) (option int))
    ~error_msg:"Expected exit code %R, got %L" ;
  (* Sequencer is at genesis, rollup node is at [advance]. *)
  Check.((missing_block_nb = 1) int)
    ~error_msg:"Sequencer should be missing block %L" ;
  let*@ rollup_head = rollup_level sc_rollup_node in
  Check.((rollup_head > 0l) int32) ~error_msg:"Rollup node should have advanced" ;
  unit

let test_persistent_state () =
  register_sandbox_with_observer
    ~__FILE__
    ~tags:["evm"; "sequencer"; "observer"]
    ~title:"EVM node state is persistent across runs"
  @@ fun {sandbox; observer} ->
  (* Force the sequencer to produce a block. *)
  let* () =
    let*@ _ = produce_block sandbox in
    Lwt.return_unit
  and* () = Evm_node.wait_for_blueprint_applied observer 1 in
  (* Ask for the current block. *)
  let*@ block_number = Rpc.block_number sandbox in
  let*@ observer_block_number = Rpc.block_number observer in
  Check.is_true
    ~__LOC__
    (block_number = 1l)
    ~error_msg:"The sequencer should have produced a block" ;
  Check.is_true
    ~__LOC__
    (observer_block_number = 1l)
    ~error_msg:"The observer should have received the block" ;

  (* Terminate the sequencer. *)
  let* () = Evm_node.terminate sandbox in

  (* Restart it. *)
  let* () = Evm_node.run sandbox in
  (* Assert the block number is at least [block_number]. Asserting
     that the block number is exactly the same as {!block_number} can
     be flaky if a block is produced between the restart and the
     RPC. *)
  let*@ new_block_number = Rpc.block_number sandbox in
  Check.is_true
    ~__LOC__
    (new_block_number = block_number)
    ~error_msg:"The sequencer should have produced a block" ;

  (* same with the observer *)
  let* () = Evm_node.terminate observer in
  (* Restart it. *)
  let* () =
    Evm_node.run ~extra_arguments:["--dont-track-rollup-node"] observer
  in
  (* Assert the block number is at least [block_number]. Asserting
       that the block number is exactly the same as {!block_number} can
       be flaky if a block is produced between the restart and the
       RPC. *)
  let*@ new_observer_block_number = Rpc.block_number observer in
  Check.is_true
    ~__LOC__
    (new_observer_block_number = block_number)
    ~error_msg:"The observer should have the same value" ;
  unit

(* Helper to setup snapshot test. This function stops the observer and sequencer
   and exports two snapshots at one block interval. *)
let snapshots_setup ~desync {sequencer; observer; _} =
  let*@ prev_block_number = Rpc.block_number sequencer in
  let block_number = Int32.succ prev_block_number in
  let observer_sync =
    Evm_node.wait_for_blueprint_applied observer (Int32.to_int block_number)
  in
  let*@ _ = produce_block sequencer in
  let* () = observer_sync in
  Log.info "Test setup: Exporting snapshots for test." ;
  Log.info " - Exporting older snapshot from observer." ;
  Log.info "   - Terminate the observer." ;
  let* () = Evm_node.terminate observer in
  Log.info "   - Export observer snapshot." ;
  let*! snapshot_file_before = Evm_node.export_snapshot ~desync observer in
  let*! snapshot_info_before =
    Evm_node.snapshot_info ~desync ~snapshot_file:snapshot_file_before
  in
  Check.(snapshot_info_before =~ rex "First level:\\s*0")
    ~error_msg:"Snapshot info should have %R, got %L" ;
  Check.(snapshot_info_before =~ rex (sf "Current level:\\s*%ld" block_number))
    ~error_msg:"Snapshot info should have %R, got %L" ;
  Log.info " - Exporting current snapshot from sequencer." ;
  Log.info "   - Force the sequencer to produce a block." ;
  let*@ _ = produce_block sequencer in
  Log.info "   - Ask for the current block." ;
  let*@ block_number = Rpc.block_number sequencer in
  Check.is_true
    ~__LOC__
    (block_number > 0l)
    ~error_msg:"The sequencer should have produced a block" ;
  Log.info "   - Terminate the sequencer." ;
  let* () = Evm_node.terminate sequencer in
  Log.info "   - Export snapshot for block %ld." block_number ;
  let*! snapshot_file = Evm_node.export_snapshot ~desync sequencer in
  let*! snapshot_info_after = Evm_node.snapshot_info ~desync ~snapshot_file in
  Check.(snapshot_info_after =~ rex "First level:\\s*0")
    ~error_msg:"Snapshot info should have %R, got %L" ;
  Check.(snapshot_info_after =~ rex (sf "Current level:\\s*%ld" block_number))
    ~error_msg:"Snapshot info should have %R, got %L" ;
  return (snapshot_file_before, snapshot_file, block_number)

let add_desync_tag desync tags = if desync then tags @ ["desync"] else tags

let test_snapshots_lock ~desync =
  register_all
    ~__FILE__
    ~tags:(["evm"; "sequencer"; "snapshots"] |> add_desync_tag desync)
    ~title:
      (sf
         "Sequencer %ssnapshots export require lock"
         (if desync then "desync " else ""))
    ~time_between_blocks:Nothing
  @@ fun {sequencer; _} _protocol ->
  Log.info "Export without stopping evm node." ;
  let*? locked = Evm_node.export_snapshot ~desync sequencer in
  let* () =
    Process.check_error
      ~msg:(rex "EVM node is locked by another process")
      locked
  in
  unit

let test_snapshots_import_empty ~desync =
  register_all
    ~__FILE__
    ~tags:(["evm"; "sequencer"; "snapshots"] |> add_desync_tag desync)
    ~title:
      (sf
         "Import %ssequencer snapshot in empty data dir"
         (if desync then "desync " else ""))
    ~time_between_blocks:Nothing
    ~history_mode:(Rolling 1)
  @@ fun ({sequencer; sc_rollup_node = _; _} as setup) _protocol ->
  let* _snapshot_file_before, snapshot_file, block_number =
    snapshots_setup ~desync setup
  in
  Log.info "Create new sequencer from snapshot." ;
  let new_sequencer =
    let mode = Evm_node.mode sequencer in
    Evm_node.create ~node_setup:(Evm_node.make_setup ()) ~mode ()
  in
  let* () = Process.check @@ Evm_node.spawn_init_config new_sequencer in
  let*! () = Evm_node.import_snapshot ~desync new_sequencer ~snapshot_file in
  Log.info "Start new sequencer." ;
  let* () = Evm_node.run new_sequencer in
  Log.info "The new sequencer should have the current block of the snapshot." ;
  let*@ _block =
    Rpc.get_block_by_number new_sequencer ~block:(Int32.to_string block_number)
  in
  Log.info "Restart old sequencer." ;
  let* () = Evm_node.run sequencer in
  let* () = check_head_consistency ~left:sequencer ~right:new_sequencer () in
  Log.info "Force the new sequencer to produce a block." ;
  let*@ _ = produce_block new_sequencer in
  let*@ block_number2 = Rpc.block_number new_sequencer in
  Check.((block_number2 > block_number) int32)
    ~error_msg:
      "The sequencer should have produced a new block but is now at %L and was \
       previously at %R" ;
  unit

let test_snapshots_import_populated ~desync =
  register_all
    ~__FILE__
    ~tags:(["evm"; "sequencer"; "snapshots"] |> add_desync_tag desync)
    ~title:
      (sf
         "Import %ssequencer snapshot in populated data dir"
         (if desync then "desync " else ""))
    ~time_between_blocks:Nothing
  @@ fun ({observer; sequencer; _} as setup) _protocol ->
  let* _snapshot_file_before, snapshot_file, block_number =
    snapshots_setup ~desync setup
  in
  Log.info "Should not be able to import snapshot in populated node." ;
  let*? populated = Evm_node.import_snapshot ~desync observer ~snapshot_file in
  let* () = Process.check_error ~msg:(rex "is already populated") populated in
  Log.info "Should be able to import snapshot in populated node with --force." ;
  let*! () =
    Evm_node.import_snapshot ~desync observer ~snapshot_file ~force:true
  in
  Log.info "Restart sequencer." ;
  let* () = Evm_node.run sequencer in
  Log.info "Restart observer." ;
  let* () = Evm_node.run observer in
  Log.info "The new observer should have the current block of the snapshot." ;
  let*@ _block =
    Rpc.get_block_by_number observer ~block:(Int32.to_string block_number)
  in
  unit

let test_snapshots_import_outdated ~desync =
  register_all
    ~__FILE__
    ~tags:(["evm"; "sequencer"; "snapshots"] |> add_desync_tag desync)
    ~title:
      (sf
         "Import outdated %ssequencer snapshot"
         (if desync then "desync " else ""))
    ~time_between_blocks:Nothing
  @@ fun ({sequencer; _} as setup) _protocol ->
  let* snapshot_file_before, _snapshot_file, _block_number =
    snapshots_setup ~desync setup
  in
  Log.info "Cannot import outdated snapshot." ;
  let*? outdated =
    Evm_node.import_snapshot
      ~desync
      sequencer
      ~snapshot_file:snapshot_file_before
      ~force:true
  in
  let* () =
    Process.check_error ~msg:(rex "The snapshot is outdated") outdated
  in
  unit

(* A test for the fix introduced in
   https://gitlab.com/tezos/tezos/-/merge_requests/14794. *)
let test_snapshots_reexport ~desync ?history_mode =
  let archive = history_mode = Some Evm_node.Archive in
  let tags =
    ["evm"; "sequencer"; "snapshots"] |> add_desync_tag desync |> fun tags ->
    if archive then tags @ ["archive"] else tags
  in
  register_all
    ~__FILE__
    ~tags
    ~title:
      (sf
         "Import %s%ssequencer snapshot and re-export"
         (if desync then "desync " else "")
         (if archive then "archive " else ""))
    ~time_between_blocks:Nothing
    ?history_mode
  @@ fun ({sequencer; sc_rollup_node = _; _} as setup) _protocol ->
  let* _snapshot_file_before, snapshot_file, _block_number =
    snapshots_setup ~desync setup
  in
  Log.info "Create new sequencer from snapshot." ;
  let new_sequencer = Evm_node.create ~mode:(Evm_node.mode sequencer) () in
  let* () = Process.check @@ Evm_node.spawn_init_config new_sequencer in
  let*! () = Evm_node.import_snapshot ~desync new_sequencer ~snapshot_file in
  Log.info "Re-export snapshot from new sequencer." ;
  let*! _file = Evm_node.export_snapshot ~desync sequencer in
  unit

let test_snapshots ~desync protocols =
  test_snapshots_lock protocols ~desync ;
  test_snapshots_import_empty ~desync protocols ;
  test_snapshots_import_populated ~desync protocols ;
  test_snapshots_import_outdated ~desync protocols ;
  test_snapshots_reexport ~desync protocols

let test_publish_blueprints =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "data"]
    ~title:"Sequencer publishes the blueprints to L1"
    ~use_dal:ci_enabled_dal_registration
  @@ fun {sequencer; client; sc_rollup_node; enable_dal; _} _protocol ->
  let* _ =
    repeat 5 (fun () ->
        let*@ _ = produce_block sequencer in
        unit)
  in

  (* Wait more to avoid flakiness, in particular with DAL *)
  let timeout = if enable_dal then 50. else 5. in
  let* () = Evm_node.wait_for_blueprint_injected ~timeout sequencer 5 in

  (* At this point, the evm node should call the batcher endpoint to publish
     all the blueprints. Stopping the node is then not a problem. *)
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
  check_rollup_head_consistency ~evm_node:sequencer ~sc_rollup_node ()

(** Same as {!test_publish_blueprints} with signatory. *)
let test_publish_blueprints_signatory =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "data"; "signatory"; "ci_disabled"]
    ~title:"Sequencer publishes the blueprints to L1 with signatory"
    ~use_dal:ci_enabled_dal_registration
    ~signatory:true
  @@ fun {sequencer; client; sc_rollup_node; enable_dal; _} _protocol ->
  let* _ =
    repeat 5 (fun () ->
        let*@ _ = produce_block sequencer in
        unit)
  in
  let timeout = if enable_dal then 50. else 5. in
  let* () = Evm_node.wait_for_blueprint_injected ~timeout sequencer 5 in
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
  let* () = Lwt_unix.sleep 2. in
  check_rollup_head_consistency ~evm_node:sequencer ~sc_rollup_node ()

(* L2-1747: a blueprint chunk whose RLP `number` field is longer than 32 bytes
   panics the kernel during RLP decode -- upstream of the envelope and
   signature checks -- because `decode_field_u256_le` hands the oversized slice
   straight to `U256::from_little_endian`, whose `assert!(len <= 32)` traps. Any
   L1 account can trigger it with no sequencer key and no valid signature.

   On this branch the rollup node falls back to the slow WASM PVM by default
   when fast execution traps, so the daemon survives. This test injects a batch
   of invalid blueprint chunks in a single inbox level and logs how long the
   rollup node takes to process that block: fast (clean rejection) with the
   `> 32` guard in `decode_field_u256_le`, slow (fallback through the trap) if
   the guard is reverted. *)
let test_blueprint_chunk_oversized_u256_does_not_panic =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "blueprint"; "cov10"]
    ~title:"Oversized U256 in a blueprint chunk does not panic the kernel"
  @@ fun {sequencer; client; sc_rollup_node; sc_rollup_address; _} _protocol ->
  (* Establish a baseline both sides agree on. *)
  let* _ =
    repeat 2 (fun () ->
        let*@ _ = produce_block sequencer in
        unit)
  in
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
  let*@ baseline = rollup_level sc_rollup_node in
  (* Build the poisoned blueprint chunks.

     A sequencer blueprint chunk reaches the kernel framed as an external
     message:
       0x00                      (ExternalMessageFrame::Targetted tag)
       <20-byte rollup address>
       0x03                      (SEQUENCER_BLUEPRINT_TAG)
       <RLP-encoded SequencerBlueprint>
     We build the frame ourselves -- no signing key is involved at all, which
     is the point: the decode panics long before any signature is checked. *)
  let frame_prefix =
    (* tag + address + tag *)
    "\000"
    ^ Tezos_crypto.Hashed.Smart_rollup_address.(
        of_b58check_exn sc_rollup_address |> to_string)
    ^ "\003"
    |> Hex.of_string |> Hex.show
  in
  (* RLP of SequencerBlueprint = list [chunk; number; nb_chunks; chunk_index;
     signature] (no optional chain_id, so 5 items):
       - chunk       = ""               -> 0x80
       - number      = 33 bytes         -> 0xa1 (string, len 0x21=33) + 33 bytes
                        ** 33 > 32: this is the poison **
       - nb_chunks   = 1 (u16 LE)       -> 0x82 0x01 0x00
       - chunk_index = 0 (u16 LE)       -> 0x82 0x00 0x00
       - signature   = ""               -> 0x80  (never reached)
     payload = 42 bytes -> list header 0xc0 + 42 = 0xea.

     [fill] is the hex byte used for the 33-byte `number`; varying it yields
     distinct-but-equally-poisonous chunks so we can inject several at once. *)
  let poisoned_chunk fill =
    String.concat
      ""
      [
        frame_prefix;
        "ea";
        "80";
        "a1";
        String.make (33 * 2) fill;
        "820100";
        "820000";
        "80";
      ]
  in
  (* Five distinct invalid blueprint chunks (number filled with 0xaa..0xee). *)
  let poisoned_chunks = List.map poisoned_chunk ['a'; 'b'; 'c'; 'd'; 'e'] in
  (* Inject all the invalid blueprint chunks at once, from a plain L1 account
     (not the sequencer) to show no key or valid signature is required. They
     land in the same inbox level. *)
  let* () =
    let messages =
      `A (List.map (fun c -> `String c) poisoned_chunks)
      |> JSON.annotate ~origin:"poisoned_blueprint_chunks"
      |> JSON.encode
    in
    Client.Sc_rollup.send_message
      ?wait:None
      ~msg:("hex:" ^ messages)
      ~src:Constant.bootstrap2.alias
      client
  in
  (* Time how long the rollup node takes to process the block carrying the
     invalid blueprints. We arm the listener before baking and bake with
     [bake_for_and_wait_level] (L1 only) rather than [next_rollup_node_level],
     whose 30s wait on the rollup node would time out if the guard is reverted
     and the slow VM runs. The first processed head after baking is the one
     carrying the poison (we are synced and the sequencer is idle). *)
  let wait_processed =
    Sc_rollup_node.wait_for
      ~timeout:600.
      sc_rollup_node
      "smart_rollup_node_daemon_new_head_processed.v0"
      (fun json ->
        Some
          ( JSON.(json |-> "level" |> as_int),
            JSON.(json |-> "process_time" |> encode) ))
  in
  let start = Unix.gettimeofday () in
  let* _ =
    repeat 3 (fun () ->
        let* _ = Client.bake_for_and_wait_level ~keys:[] client in
        unit)
  in
  let* processed_level, process_time = wait_processed in
  Log.info
    "Rollup node processed the block with the invalid blueprints (L1 level %d) \
     in %.3fs wall-clock (kernel process_time: %s)"
    processed_level
    (Unix.gettimeofday () -. start)
    process_time ;
  (* The kernel is still alive: fresh blueprints are applied and the rollup
     node catches back up past the baseline. *)
  let* _ =
    repeat 2 (fun () ->
        let*@ _ = produce_block sequencer in
        unit)
  in
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
  let*@ final = rollup_level sc_rollup_node in
  Check.((final > baseline) int32)
    ~error_msg:
      "Rollup node should keep applying blueprints after the poisoned chunks \
       (rollup head %L should exceed baseline %R)" ;
  check_rollup_head_consistency ~evm_node:sequencer ~sc_rollup_node ()

let test_sequencer_too_ahead =
  let max_blueprints_ahead = 5 in
  register_all
    ~__FILE__
    ~max_blueprints_ahead
    ~time_between_blocks:Nothing
    ~tags:["evm"; "max_blueprint_ahead"]
    ~title:"Sequencer locks production if it's too ahead"
    ~use_dal:ci_enabled_dal_registration
  @@ fun {sequencer; sc_rollup_node; client; sc_rollup_address; _} _protocol ->
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in
  let* () = Sc_rollup_node.terminate sc_rollup_node in
  let* () =
    repeat (max_blueprints_ahead + 1) (fun () ->
        let*@ _ = produce_block sequencer in
        unit)
  in
  let* () =
    (* Failing as the block_producer is locked. *)
    let*@? _ = produce_block sequencer in
    unit
  and* () = Evm_node.wait_for_block_producer_locked sequencer in
  let* () =
    (* Repeating won't make the production magically successful. *)
    repeat max_blueprints_ahead (fun () ->
        let*@? _ = produce_block sequencer in
        unit)
  in
  let*@ block_number = Rpc.block_number sequencer in
  Check.((block_number = 6l) int32)
    ~error_msg:"The sequencer should have been locked" ;
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup_address []
  and* () =
    Evm_node.wait_for_rollup_node_follower_connection_acquired sequencer
  in
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in
  let* _ =
    repeat 2 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  let new_blocks = 3l in
  let* () =
    repeat (Int32.to_int new_blocks) (fun () ->
        let*@ _ = produce_block sequencer in
        unit)
  in
  let previous_block_number = block_number in
  let*@ block_number = Rpc.block_number sequencer in
  Check.((block_number = Int32.add previous_block_number new_blocks) int32)
    ~error_msg:
      "The sequencer should have been unlocked (block_number: %L, prev + \
       new_block: %R)" ;
  unit

let test_resilient_to_rollup_node_disconnect =
  (* The objective of this test is to show that the sequencer can deal with
     rollup node outage. The logic of the sequencer at the moment is to
     wait for its advance on the rollup node to be more than [max_blueprints_lag]
     before sending at most [max_blueprints_catchup] blueprints. The sequencer
     waits for [catchup_cooldown] L1 blocks before checking if it needs to push
     new blueprints again. This scenario checks this logic. *)
  let max_blueprints_lag = 10 in
  let max_blueprints_catchup = max_blueprints_lag - 3 in
  let catchup_cooldown = 10 in
  let first_batch_blueprints_count = 5 in
  register_all
    ~__FILE__
    ~max_blueprints_lag
    ~max_blueprints_catchup
    ~catchup_cooldown
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "data"; Tag.flaky]
    ~title:"Sequencer is resilient to rollup node disconnection"
    ~use_dal:ci_enabled_dal_registration
  @@
  fun {sequencer; sc_rollup_node; sc_rollup_address; client; observer; _}
      _protocol
    ->
  (* Produce blueprints *)
  let* _ =
    repeat first_batch_blueprints_count (fun () ->
        let*@ _ = produce_block sequencer in
        unit)
  in
  let* () =
    Evm_node.wait_for_blueprint_injected sequencer first_batch_blueprints_count
  in

  (* Produce some L1 blocks so that the rollup node publishes the blueprints. *)
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

  let*@ rollup_node_head = rollup_level sc_rollup_node in
  Check.((rollup_node_head = Int32.(of_int first_batch_blueprints_count)) int32)
    ~error_msg:
      "The rollup node should have received the first round of lost blueprints \
       (rollup node level: %L, expected level %R)" ;

  (* Check sequencer and rollup consistency *)
  let* () =
    check_rollup_head_consistency ~evm_node:sequencer ~sc_rollup_node ()
  in

  let* () =
    (* bake 2 block so evm_node sees it as finalized in
       `rollup_node_follower` *)
    repeat 2 (fun () ->
        let* _lvl = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in

  (* Kill the rollup node *)
  let* () = Sc_rollup_node.terminate sc_rollup_node in

  (* The sequencer node should keep producing blocks, enough so that
     it cannot catchup in one go. *)
  let* _ =
    repeat (2 * max_blueprints_lag) (fun () ->
        let*@ _ = produce_block sequencer in
        unit)
  in

  let* () =
    Evm_node.wait_for_blueprint_applied
      sequencer
      (first_batch_blueprints_count + (2 * max_blueprints_lag))
  and* () =
    Evm_node.wait_for_blueprint_applied
      observer
      (first_batch_blueprints_count + (2 * max_blueprints_lag))
  in

  (* restart the rollup node to reestablish the connection *)
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup_address []
  and* () = Evm_node.wait_for_rollup_node_follower_connection_acquired sequencer
  and* () =
    Evm_node.wait_for_rollup_node_follower_connection_acquired observer
  in

  (* Produce enough blocks in advance to ensure the sequencer node will catch
     up at the end. *)
  let* _ =
    repeat max_blueprints_lag (fun () ->
        let*@ _ = produce_block sequencer in
        unit)
  in

  let* () =
    Evm_node.wait_for_blueprint_applied
      sequencer
      (first_batch_blueprints_count + (2 * max_blueprints_catchup) + 1)
  and* () =
    Evm_node.wait_for_blueprint_applied
      observer
      (first_batch_blueprints_count + (2 * max_blueprints_catchup) + 1)
  in

  (* Give some time for the sequencer node to inject the first round of
     blueprints *)
  let* _ =
    let expected_level =
      Int32.of_int @@ (first_batch_blueprints_count + max_blueprints_catchup)
    in
    bake_until
      ~__LOC__
      ~bake:(fun () ->
        let* _lvl = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

        unit)
      ~result_f:(fun () ->
        let*@ rollup_node_head = rollup_level sc_rollup_node in
        if rollup_node_head = expected_level then return (Some ())
        else return None)
      ()
  in

  (* Go through several cooldown periods to let the sequencer sends the rest of
     the blueprints. *)
  let* () =
    bake_until_sync
      ~timeout_in_blocks:(catchup_cooldown * 3)
      ~sc_rollup_node
      ~client
      ~sequencer
      ()
  in

  (* Check the consistency again *)
  check_rollup_head_consistency
    ~evm_node:sequencer
    ~sc_rollup_node
    ~error_msg:
      "The head should be the same after the outage. Sequencer: {%L}, rollup \
       node: {%R}"
    ()

let test_can_fetch_blueprint =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "data"]
    ~title:"Sequencer can provide blueprints on demand"
  @@ fun {sequencer; _} _protocol ->
  let number_of_blocks = 5 in
  let* _ =
    repeat number_of_blocks (fun () ->
        let*@ _ = produce_block sequencer in
        unit)
  in

  let* () = Evm_node.wait_for_blueprint_injected ~timeout:5. sequencer 5 in

  let* blueprints =
    fold number_of_blocks [] (fun i acc ->
        let* blueprint =
          Sequencer_rpc.get_blueprint sequencer Int64.(of_int @@ (i + 1))
        in
        return (blueprint :: acc))
  in

  (* Test for uniqueness  *)
  let blueprints_uniq =
    List.sort_uniq
      (fun b1 b2 -> String.compare (JSON.encode b1) (JSON.encode b2))
      blueprints
  in
  if List.length blueprints = List.length blueprints_uniq then unit
  else
    Test.fail
      ~__LOC__
      "At least two blueprints from a different level are equal."

let test_can_fetch_smart_rollup_address =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "rpc"]
    ~title:"Sequencer can return the smart rollup address on demand"
  @@ fun {sequencer; sc_rollup_address; _} _protocol ->
  let* claimed_address = Sequencer_rpc.get_smart_rollup_address sequencer in

  Check.((sc_rollup_address = claimed_address) string)
    ~error_msg:"Returned address is not the expected one" ;

  unit

(** test to initialise a sequencer data dir based on a rollup node data dir *)
let test_init_from_rollup_node_data_dir =
  register_all
    ~__FILE__
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/7285
         Replace by [Any] after the next upgrade
      *)
    ~kernels:[Latest]
    ~time_between_blocks:Nothing
    ~tags:["evm"; "rollup_node"; "init"; "reconstruct"]
    ~title:"Init evm node sequencer data dir from a rollup node data dir"
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/7505
         'Reconstruct from a rollup node data directory that relied on
         signals'. We cannot rely on
         Evm_node.init_from_rollup_node_data_dir until this issue is
         fixed. Enable DAL once it is done. *)
    ~use_dal:Register_without_feature
    ~rollup_history_mode:Archive
  @@ fun {sc_rollup_node; sequencer; observer; client; _} _protocol ->
  (* a sequencer is needed to produce an initial block *)
  let* () =
    repeat 5 (fun () ->
        let*@ _ = produce_block sequencer in
        unit)
  in
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
  let* () = Evm_node.terminate sequencer
  and* () = Evm_node.terminate observer in
  let evm_node' =
    Evm_node.create
      ~node_setup:(Evm_node.make_setup ())
      ~mode:(Evm_node.mode sequencer)
      ()
  in
  let* () = Process.check @@ Evm_node.spawn_init_config evm_node' in
  let* () =
    (* bake 2 blocks so rollup context is for the finalized l1 level
       and can't be reorged. *)
    repeat 2 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  let* () = Evm_node.init_from_rollup_node_data_dir evm_node' sc_rollup_node in
  let* () = Evm_node.run evm_node' in

  let* () =
    check_rollup_head_consistency ~sc_rollup_node ~evm_node:evm_node' ()
  in

  let*@ _ = produce_block evm_node' in
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer:evm_node' () in

  let* () =
    check_rollup_head_consistency ~sc_rollup_node ~evm_node:evm_node' ()
  in

  unit

(* Tests that _all_ blueprint chunks are cleared during a flush, including
   blueprint chunk not yet processed: they will be invalidated by the flush
   anyway.*)
let test_clean_bps =
  let sequencer_account = Constant.bootstrap1 in
  register_all
    ~__FILE__
    ~sequencer:sequencer_account
    ~time_between_blocks:Nothing
    ~delayed_inbox_timeout:0
    ~delayed_inbox_min_levels:1
    ~title:"All blueprints are stale on flush"
    ~tags:["evm"; "flush"; "clean"]
    ~use_dal:Register_without_feature
    ~kernels:[Latest]
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocols
    ->
  let* () = bake_until_sync ~__LOC__ ~sc_rollup_node ~sequencer ~client () in
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
  (* we send a blueprint in the (far) future, to ensure the kernel don't
     execute it after the flush *)
  let* chunks =
    Evm_node.chunk_data
      ~rollup_address:sc_rollup_address
      ~sequencer_key:sequencer_account.alias
      ~client
      ~number:42
      [raw_transfer]
  in
  let send_chunks chunks src =
    let messages =
      `A (List.map (fun c -> `String c) chunks)
      |> JSON.annotate ~origin:"send_message"
      |> JSON.encode
    in
    Client.Sc_rollup.send_message
      ?wait:None
      ~msg:("hex:" ^ messages)
      ~src
      client
  in
  let* () = send_chunks chunks sequencer_account.alias in

  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

  (* check the bp is in storage *)
  let* subkeys =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind:"wasm_2_0_0"
         ~operation:Sc_rollup_rpc.Subkeys
         ~key:"/base/blueprints/42"
         ()
  in
  Check.(
    (subkeys <> [])
      (list string)
      ~error_msg:
        "There should be a list of chunks, otherwise the blueprint was never \
         stored") ;
  (* flush *)
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

  (* check that their generation is old*)
  let* generation =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind:"wasm_2_0_0"
         ~operation:Sc_rollup_rpc.Value
         ~key:"/base/blueprints/42/generation"
         ()
  in
  let generation =
    match generation with
    | Some g -> g
    | None -> Test.fail "The blueprint generation should be an integer"
  in
  let* current_generation =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind:"wasm_2_0_0"
         ~operation:Sc_rollup_rpc.Value
         ~key:"/base/blueprints/generation"
         ()
  in
  let current_generation =
    match current_generation with
    | Some g -> g
    | None -> Test.fail "The current blueprint generation should be an integer"
  in
  Check.(
    (generation <> current_generation)
      string
      ~error_msg:"The blueprint generation should be old, got %L, current is %R") ;
  unit

let test_sequencer_sunset =
  let sequencer_key = Constant.bootstrap1 in
  let new_sequencer_key = Constant.bootstrap2 in
  let timestamp dt = Format.sprintf "2020-01-01T00:00:%02dZ" dt in
  let activation_delay = 30 in
  let genesis_timestamp = timestamp 0 in
  let activation_timestamp = timestamp activation_delay in
  let sunset = 10 in

  register_all
    ~__FILE__
    ~title:
      "The sequencer locks its transaction queue ahead of the sequencer upgrade"
    ~tags:["evm"; "sequencer"; "sequencer_upgrade"; "sunset"; "lock"]
    ~sequencer:sequencer_key
    ~time_between_blocks:Nothing
    ~sequencer_sunset_sec:sunset
    ~genesis_timestamp:Client.(At (Time.of_notation_exn genesis_timestamp))
  @@
  fun {sequencer; sc_rollup_address; l1_contracts; client; sc_rollup_node; _}
      _protocol
    ->
  (* Check we can create a block *)
  let*@ _txns_count = produce_block ~timestamp:(timestamp 1) sequencer in

  let* () =
    sequencer_upgrade
      ~sc_rollup_address
      ~sequencer_admin:Constant.bootstrap2.alias
      ~sequencer_governance_contract:l1_contracts.sequencer_governance
      ~pool_address:Eth_account.bootstrap_accounts.(0).address
      ~client
      ~upgrade_to:new_sequencer_key.alias
      ~activation_timestamp
  in

  let upgrade_info = Evm_node.wait_for_evm_event Sequencer_upgrade sequencer in
  let* () =
    repeat 2 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~client ~sc_rollup_node in
        unit)
  and* _upgrade_info = upgrade_info in

  (* Check we can still create a block before the sunset *)
  let*@ _txns_count =
    produce_block
      ~timestamp:(timestamp (activation_delay - sunset - 1))
      sequencer
  in
  let*@? _err =
    produce_block
      ~timestamp:(timestamp (activation_delay - sunset + 1))
      sequencer
  in
  let*@? _err =
    produce_block
      ~timestamp:(timestamp (activation_delay - sunset + 2))
      sequencer
  in

  unit

(** this test the situation where a sequencer diverged from it
    source. To obtain that we create two sequencers, one is going to
    diverged from the other. *)
let test_sequencer_diverge =
  register_all
    ~__FILE__
    ~genesis_timestamp:Client.(At (Time.of_notation_exn "2020-01-01T00:00:00Z"))
    ~sequencer:Constant.bootstrap1
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "diverge"]
    ~title:"Runs two sequencers, one diverge and stop"
  @@
  fun {sc_rollup_node; client; sequencer; observer; enable_dal; _} _protocol ->
  let* () =
    repeat 4 (fun () ->
        let*@ _l2_level =
          produce_block ~timestamp:"2020-01-01T00:00:00Z" sequencer
        in
        unit)
  in
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in
  let* () =
    (* 2 to make sure it has been finalized *)
    repeat 2 (fun () ->
        let* _l1_level =
          Rollup.next_rollup_node_level ~sc_rollup_node ~client
        in
        unit)
  in
  (* We duplicate the sequencer by creating a snapshot and importing it *)
  let* _ = Evm_node.terminate sequencer in
  let* snapshot_file =
    Runnable.run @@ Evm_node.export_snapshot ~desync:true sequencer
  in
  let sequencer_bis =
    Evm_node.create
      ~node_setup:
        (Evm_node.make_setup
           ?initial_kernel:(Evm_node.initial_kernel sequencer)
           ~preimages_dir:(Evm_node.preimages_dir sequencer)
           ())
      ~mode:(Evm_node.mode sequencer)
      ()
  in
  let* () = Process.check @@ Evm_node.spawn_init_config sequencer_bis in
  let* () =
    Runnable.run
    @@ Evm_node.import_snapshot ~desync:true sequencer_bis ~snapshot_file
  in
  let* () = Evm_node.run sequencer in
  let* () = Evm_node.run sequencer_bis in

  (* We start a new observer for the new sequencer and wait for it to catch-up *)
  let* observer_bis =
    run_new_observer_node ~sc_rollup_node:(Some sc_rollup_node) sequencer_bis
  in
  let* () = Evm_node.wait_for_blueprint_applied observer_bis 4 in

  (* When run in the CI the shutdown event are sometimes handled after the
     sequencer and/or the observer have already been terminated, leading to
     errors while they actually shutdown as expected. *)
  let diverged_and_shutdown sequencer observer =
    let* _ = Evm_node.wait_for_diverged sequencer
    and* _ = Evm_node.wait_for_shutdown_event ~can_terminate:true sequencer
    and* _ = Evm_node.wait_for_reset observer in
    unit
  in
  let* () =
    Lwt.pick
      [
        diverged_and_shutdown sequencer observer;
        diverged_and_shutdown sequencer_bis observer_bis;
      ]
  and* () =
    (* diff timestamp to differ *)
    let* _ = produce_block ~timestamp:"2020-01-01T00:13:00Z" sequencer
    and* _ = produce_block ~timestamp:"2020-01-01T00:12:00Z" sequencer_bis in
    let number_of_blocks = if enable_dal then 20 else 5 in
    repeat number_of_blocks (fun () ->
        let* _ = Rollup.next_rollup_node_level ~client ~sc_rollup_node in
        unit)
  in

  (* The observer has dropped the invalid branch and remembers only
     the finalized state. *)
  let*@ latest = Rpc.get_block_by_number ~block:"latest" observer in
  let*@ block = Rpc.get_block_by_number ~block:"finalized" observer in
  Check.((latest.hash = block.hash) string)
    ~error_msg:"Latest and finalized are different" ;
  unit

(** This test that the sequencer evm node can catchup event from the
    rollup node. *)
let test_sequencer_can_catch_up_on_event =
  register_all
    ~__FILE__
    ~sequencer:Constant.bootstrap1
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "event"]
    ~title:"Evm node can catchup event from the rollup node"
      (* This test relies on the rollup node receiving the blueprint before:
         - the last_produced_block is applied, and
         - the sequencer is shut down.
         When DAL is activated and blueprints are sent using signals,
         the sequencer notifies blueprints only when they can be
         applied by the rollup node. In this test, we stop the
         sequencer to trigger the catch-up mechanism before the
         last_produced_block can be applied. Consequently, the signal
         is not sent, and the rollup node does not apply the blueprint
         corresponding to last_produced_block. *)
    ~use_dal:Register_without_feature
  @@ fun {sc_rollup_node; client; sequencer; observer; _} _protocol ->
  let* () =
    repeat 2 (fun () ->
        let* _ = produce_block sequencer in
        unit)
  in
  let* () = bake_until_sync ~sequencer ~sc_rollup_node ~client () in
  let* _ = produce_block sequencer in
  let*@ last_produced_block = Rpc.block_number sequencer in
  let* () =
    Evm_node.wait_for_blueprint_injected
      sequencer
      ~timeout:5.
      (Int32.to_int last_produced_block)
  in
  let* () = Evm_node.terminate sequencer in
  let* () = Evm_node.terminate observer in
  let* () =
    (* produces some blocks so the rollup node applies latest produced block. *)
    repeat 4 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  let check json =
    let open JSON in
    match as_list (json |-> "event") with
    | [number; hash] ->
        let number = as_int number in
        let hash = as_string hash in
        if number = Int32.to_int last_produced_block then Some (number, hash)
        else None
    | _ ->
        Test.fail
          ~__LOC__
          "invalid json for the evm event kind blueprint applied"
  in
  let* _json = Evm_node.wait_for_evm_event ~check Blueprint_applied sequencer
  and* _json_observer =
    Evm_node.wait_for_evm_event ~check Blueprint_applied observer
  and* () =
    let* () = Evm_node.run sequencer in
    Evm_node.run observer
  in
  let* () =
    check_rollup_head_consistency
      ~evm_node:sequencer
      ~sc_rollup_node
      ~error_msg:
        "The head should be the same after catchup: sequencer is %L while \
         rollup node is %R"
      ()
  in
  unit

let test_sequencer_dont_read_level_twice =
  register_all
    ~__FILE__
    ~sequencer:Constant.bootstrap1
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "event"; Tag.slow]
    ~title:"Evm node don't read the same level twice"
  @@
  fun {sc_rollup_node; client; sequencer; l1_contracts; sc_rollup_address; _}
      _protocol
    ->
  (* We deposit some Tez to the rollup *)
  let deposit_info =
    {
      receiver = EthereumAddr Eth_account.bootstrap_accounts.(1).address;
      chain_id = None;
    }
  in
  (* The deposit reaches the sequencer asynchronously: it is read from
     *finalized* L1 levels through the rollup node and applied to the
     sequencer's local state by a background worker. Set up the watcher
     before sending the deposit. *)
  let delayed_transaction_seen =
    Evm_node.wait_for_evm_event New_delayed_transaction sequencer
  in
  let* () =
    send_deposit_to_delayed_inbox
      ~amount:Tez.(of_int 16)
      ~bridge:l1_contracts.bridge
      ~depositor:Constant.bootstrap5
      ~deposit_info
      ~sc_rollup_node
      ~sc_rollup_address
      client
  in

  (* Bake the 2 extra L1 levels that make the deposit's level final, then
     wait for the sequencer to have seen the deposit, so that the block
     produced below is guaranteed to contain it. *)
  let* _ =
    repeat 2 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  let* _ = delayed_transaction_seen in

  (* We expect the deposit to be in this block. *)
  let* _ = produce_block sequencer in

  let*@ block = Rpc.get_block_by_number ~block:Int.(to_string 1) sequencer in
  let nb_transactions =
    match block.transactions with
    | Empty -> 0
    | Hash l -> List.length l
    | Full l -> List.length l
  in
  Check.((nb_transactions = 1) int)
    ~error_msg:"Expected one transaction (the deposit), got %L" ;

  (* We kill the sequencer and restart it. As a result, its last known L1 level
     is still the L1 level of the deposit. *)
  let* _ = Evm_node.terminate sequencer in
  let* _ = Evm_node.run sequencer in

  (* We produce some empty blocks. *)
  let*@ _ = produce_block sequencer in
  let*@ _ = produce_block sequencer in

  (* If the logic of the sequencer is correct (i.e., it does not process the
     deposit twice), then it is possible for the rollup node to apply them. *)
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

  unit

let test_reset =
  register_all
    ~__FILE__
    ~sequencer:Constant.bootstrap1
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "reset"]
    ~title:"try to reset sequencer and observer state using the command."
  @@
  fun {observer; sequencer; sc_rollup_node; client; sc_rollup_address; _}
      _protocol
    ->
  let reset_level = 5 in
  let after_reset_level = 5 in
  Log.info "Producing %d level then syncing" reset_level ;
  let* () = repeat reset_level (fun () -> next_evm_level ~evm_node:sequencer) in
  let* () = bake_until_sync ~sequencer ~sc_rollup_node ~client () in
  Log.info
    "Stopping the rollup node, then produce %d more blocks "
    (reset_level + after_reset_level) ;
  let* () = Sc_rollup_node.terminate sc_rollup_node in
  let* () =
    repeat after_reset_level (fun () -> next_evm_level ~evm_node:sequencer)
  in
  let*@ sequencer_level = Rpc.block_number sequencer in
  Check.(
    (sequencer_level = Int32.of_int (reset_level + after_reset_level)) int32)
    ~error_msg:
      "The sequencer level %L should be at %R after producing %R blocks" ;
  Log.info "Stopping sequencer and observer" ;
  let* () = Evm_node.terminate observer
  and* () = Evm_node.terminate sequencer in

  Log.info "Reset sequencer and observer state." ;
  let* () = Evm_node.reset observer ~l2_level:reset_level
  and* () = Evm_node.reset sequencer ~l2_level:reset_level in

  Log.info "Rerun rollup node, sequencer and observer." ;
  let* () =
    Sc_rollup_node.run sc_rollup_node sc_rollup_address [Log_kernel_debug]
  in

  (* Recreate the evm node simply to reset the state of the tezt
     instance. *)
  let sequencer =
    Evm_node.create
      ~node_setup:
        (Evm_node.make_setup
           ~data_dir:(Evm_node.data_dir sequencer)
           ?config_file:(Evm_node.config_file sequencer)
           ~rpc_port:(Evm_node.rpc_port sequencer)
           ?private_rpc_port:(Evm_node.private_rpc_port sequencer)
           ?spawn_rpc:(Evm_node.spawn_rpc sequencer)
           ())
      ~mode:(Evm_node.mode sequencer)
      ()
  in
  let* () = Evm_node.run sequencer in
  let observer =
    Evm_node.create
      ~node_setup:
        (Evm_node.make_setup
           ~data_dir:(Evm_node.data_dir observer)
           ?config_file:(Evm_node.config_file observer)
           ~rpc_port:(Evm_node.rpc_port observer)
           ?private_rpc_port:(Evm_node.private_rpc_port observer)
           ())
      ~mode:(Evm_node.mode observer)
      ()
  in

  let* () = Evm_node.run observer in

  Log.info "Check sequencer and observer is at %d level" reset_level ;
  let*@ sequencer_level = Rpc.block_number sequencer in
  let*@ observer_level = Rpc.block_number observer in
  Check.((sequencer_level = Int32.of_int reset_level) int32)
    ~error_msg:
      "The sequencer is at level %L, but should be at the level %R after being \
       reset." ;
  Check.((sequencer_level = observer_level) int32)
    ~error_msg:
      "The sequencer (currently at level %L) and observer (currently at level \
       %R) should be at the same level after both being reset." ;
  let* () =
    repeat after_reset_level (fun () -> next_evm_level ~evm_node:sequencer)
  in
  let* () = bake_until_sync ~sequencer ~sc_rollup_node ~client () in
  (* Check sequencer is at the expected level *)
  let*@ sequencer_level = Rpc.block_number sequencer in
  Check.(
    (sequencer_level = Int32.of_int (reset_level + after_reset_level)) int32)
    ~error_msg:
      "The sequencer level %L should be at %R after producing blocks after the \
       reset." ;
  unit

let test_store_smart_rollup_address =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "store"]
    ~title:"Sequencer checks the smart rollup address"
  @@ fun {sequencer; client; node; _} _protocol ->
  (* Starting the sequencer stores the smart rollup address in the store. *)
  (* Kill the sequencer. *)
  let* () = Evm_node.terminate sequencer in
  (* Originate another rollup. *)
  let* other_rollup_address =
    originate_sc_rollup
      ~keys:[]
      ~kind:"wasm_2_0_0"
      ~alias:"vbot"
      ~src:"bootstrap2"
        (* We cannot use bootstrap1 because it already has an operation in the mempool. *)
      client
  in
  let other_rollup_node =
    Sc_rollup_node.create
      Observer
      node
      ~base_dir:(Client.base_dir client)
      ~kind:"wasm_2_0_0"
  in
  let* () = Sc_rollup_node.run other_rollup_node other_rollup_address [] in
  (* Try to run the sequencer with an invalid smart rollup address. *)
  let process =
    Evm_node.spawn_run
      ~extra_arguments:
        ["--rollup-node-endpoint"; Sc_rollup_node.endpoint other_rollup_node]
      sequencer
  in
  let* () =
    Process.check_error
      ~msg:(rex "The EVM node follows the smart rollup address*")
      process
  in
  unit

let protocols = [Protocol.Alpha]

let () =
  test_remove_sequencer protocols ;
  test_persistent_state () ;
  test_snapshots ~desync:false protocols ;
  test_snapshots ~desync:true protocols ;
  test_snapshots_reexport ~desync:false ~history_mode:Archive protocols ;
  test_publish_blueprints protocols ;
  test_publish_blueprints_signatory protocols ;
  test_blueprint_chunk_oversized_u256_does_not_panic protocols ;
  test_sequencer_too_ahead protocols ;
  test_resilient_to_rollup_node_disconnect protocols ;
  test_can_fetch_smart_rollup_address protocols ;
  test_can_fetch_blueprint protocols ;
  test_init_from_rollup_node_data_dir protocols ;
  test_sequencer_sunset protocols ;
  test_sequencer_diverge protocols ;
  test_sequencer_can_catch_up_on_event protocols ;
  test_sequencer_dont_read_level_twice protocols ;
  test_reset protocols ;
  test_store_smart_rollup_address protocols ;
  test_clean_bps protocols
