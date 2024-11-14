(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Trilitech <contact@trili.tech>                         *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Smart Optimistic Rollups: Etherlink Sequencer
   Requirement:  make -f etherlink.mk build
                 npm install eth-cli solc@0.8.26
                 # Install cast or foundry (see: https://book.getfoundry.sh/getting-started/installation)
                 curl -L https://foundry.paradigm.xyz | bash
                 foundryup
                 make -f etherlink.mk octez-dsn-node
                 ./scripts/install_dal_trusted_setup.sh
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file evm_sequencer.ml
*)

open Sc_rollup_helpers
open Rpc.Syntax
open Contract_path

module Sequencer_rpc = struct
  let get_blueprint sequencer number =
    Runnable.run
    @@ Curl.get
         ~args:["--fail"]
         (Evm_node.endpoint sequencer
         ^ "/evm/blueprint/" ^ Int64.to_string number)

  let get_smart_rollup_address sequencer =
    let* res =
      Runnable.run
      @@ Curl.get
           ~args:["--fail"]
           (Evm_node.endpoint sequencer ^ "/evm/smart_rollup_address")
    in
    return (JSON.as_string res)
end

open Helpers
open Setup

module Delayed_inbox = struct
  type endpoint = Sc_rollup_node of Sc_rollup_node.t | Evm_node of Evm_node.t

  let subkeys path = function
    | Sc_rollup_node sc_rollup_node ->
        Sc_rollup_node.RPC.call sc_rollup_node
        @@ Sc_rollup_rpc.get_global_block_durable_state_value
             ~pvm_kind:"wasm_2_0_0"
             ~operation:Sc_rollup_rpc.Subkeys
             ~key:path
             ()
    | Evm_node evm_node ->
        let*@! list = Rpc.state_subkeys evm_node path in
        return list

  let content = subkeys Durable_storage_path.delayed_inbox

  let data endpoint hash =
    let path = sf "%s/%s/data" Durable_storage_path.delayed_inbox hash in
    match endpoint with
    | Sc_rollup_node sc_rollup_node ->
        Sc_rollup_node.RPC.call sc_rollup_node
        @@ Sc_rollup_rpc.get_global_block_durable_state_value
             ~pvm_kind:"wasm_2_0_0"
             ~operation:Sc_rollup_rpc.Value
             ~key:path
             ()
    | Evm_node evm_node ->
        let*@ res = Rpc.state_value evm_node path in
        return res

  let assert_mem endpoint hash =
    let* delayed_transactions_hashes = content endpoint in
    Check.(list_mem string hash delayed_transactions_hashes)
      ~error_msg:"hash %L should be present in the delayed inbox %R" ;
    unit

  let assert_empty endpoint =
    let* delayed_transactions_hashes = content endpoint in
    Check.is_true
      (List.length delayed_transactions_hashes <= 1)
      ~error_msg:"Expected empty delayed inbox" ;
    unit

  let size endpoint =
    let* delayed_transactions_hashes = content endpoint in
    let size = List.length delayed_transactions_hashes - 1 in
    if size < 0 then
      (* /meta is removed, if the delayed inbox was empty it would be (-1) here. *)
      return 0
    else return size
end

let check_kernel_version ~evm_node ~equal expected =
  let*@ kernel_version = Rpc.tez_kernelVersion evm_node in
  if equal then
    Check.((kernel_version = expected) string)
      ~error_msg:"Expected kernelVersion to be %R, got %L"
  else
    Check.((kernel_version <> expected) string)
      ~error_msg:"Expected kernelVersion to be different than %R" ;
  return kernel_version

let base_fee_for_hardcoded_tx = Wei.to_wei_z @@ Z.of_int 21000

let arb_da_fee_for_delayed_inbox = Wei.of_eth_int 10_000
(* da fee doesn't apply to delayed inbox, set it arbitrarily high
   to prove this *)

let send_raw_transaction_to_delayed_inbox ?(wait_for_next_level = true)
    ?(amount = Tez.one) ?expect_failure ~sc_rollup_node ~client ~l1_contracts
    ~sc_rollup_address ?(sender = Constant.bootstrap2) raw_tx =
  let expected_hash =
    `Hex raw_tx |> Hex.to_bytes |> Tezos_crypto.Hacl.Hash.Keccak_256.digest
    |> Hex.of_bytes |> Hex.show
  in
  let* () =
    Client.transfer
      ~arg:(sf "Pair %S 0x%s" sc_rollup_address raw_tx)
      ~amount
      ~giver:sender.public_key_hash
      ~receiver:l1_contracts.delayed_transaction_bridge
      ~burn_cap:Tez.one
      ?expect_failure
      client
  in
  let* () =
    if wait_for_next_level then
      let* _ = next_rollup_node_level ~sc_rollup_node ~client in
      unit
    else unit
  in
  Lwt.return expected_hash

let send_deposit_to_delayed_inbox ~amount ~l1_contracts ~depositor ~receiver
    ~sc_rollup_node ~sc_rollup_address client =
  let* () =
    Client.transfer
      ~entrypoint:"deposit"
      ~arg:(sf "Pair %S %s" sc_rollup_address receiver)
      ~amount
      ~giver:depositor.Account.public_key_hash
      ~receiver:l1_contracts.bridge
      ~burn_cap:Tez.one
      client
  in
  let* _ = next_rollup_node_level ~sc_rollup_node ~client in
  unit

let send_fa_deposit_to_delayed_inbox ~amount ~l1_contracts ~depositor ~receiver
    ~sc_rollup_node ~sc_rollup_address client =
  let* () =
    Client.transfer
      ~entrypoint:"set"
      ~arg:(sf "Pair %S (Pair (Right (Right %s)) 0)" sc_rollup_address receiver)
      ~amount:Tez.zero
      ~giver:depositor.Account.public_key_hash
      ~receiver:l1_contracts.ticket_router_tester
      ~burn_cap:Tez.one
      client
  in
  let* () = Client.bake_for_and_wait ~keys:[] client in
  let* () =
    Client.transfer
      ~entrypoint:"mint"
      ~arg:(sf "Pair (Pair 0 None) %d" amount)
      ~amount:Tez.zero
      ~giver:depositor.Account.public_key_hash
      ~receiver:l1_contracts.ticket_router_tester
      ~burn_cap:Tez.one
      client
  in
  let* _ = next_rollup_node_level ~sc_rollup_node ~client in
  unit

(* For each feature (threshold encryption, DAL, FA Bridge), tests may
   registered with the feature enabled, with the feature disabled, or both. *)
type feature_test_registration =
  | Register_with_feature
  | Register_without_feature
  | Register_both of {
      extra_tags_with : string list;
      extra_tags_without : string list;
    }
    (* We want at most one variant of the test in the CI, the
       [extra_tags_with] and [extra_tags_without] fields allow to select
       which one by passing [Tag.ci_disabled] or [Tag.slow] to the case
       which should not run in CI. *)
[@@warning "-unused-constructor"]

let default_threshold_encryption_registration =
  Register_both {extra_tags_with = [Tag.ci_disabled]; extra_tags_without = []}

let default_dal_registration =
  Register_both {extra_tags_with = [Tag.slow]; extra_tags_without = []}

let ci_enabled_dal_registration =
  Register_both {extra_tags_with = []; extra_tags_without = []}

(* Register all variants of a test. *)
let register_all ?block_storage_sqlite3 ?sequencer_rpc_port
    ?sequencer_private_rpc_port ?genesis_timestamp ?time_between_blocks
    ?max_blueprints_lag ?max_blueprints_ahead ?max_blueprints_catchup
    ?catchup_cooldown ?delayed_inbox_timeout ?delayed_inbox_min_levels
    ?max_number_of_chunks ?bootstrap_accounts ?sequencer ?sequencer_pool_address
    ?(kernels = Kernel.all) ?da_fee ?minimum_base_fee_per_gas ?preimages_dir
    ?maximum_allowed_ticks ?maximum_gas_per_transaction
    ?max_blueprint_lookahead_in_seconds ?enable_fa_bridge ?history_mode
    ?commitment_period ?challenge_window ?additional_uses
    ?(use_threshold_encryption = default_threshold_encryption_registration)
    ?(use_dal = default_dal_registration) ~title ~tags body protocols =
  let dal_cases =
    match use_dal with
    | Register_both {extra_tags_with; extra_tags_without} ->
        [(false, extra_tags_without); (true, extra_tags_with)]
    | Register_with_feature -> [(true, [])]
    | Register_without_feature -> [(false, [])]
  in
  let threshold_encryption_cases =
    match use_threshold_encryption with
    | Register_both {extra_tags_with; extra_tags_without} ->
        [(false, extra_tags_without); (true, extra_tags_with)]
    | Register_with_feature -> [(true, [])]
    | Register_without_feature -> [(false, [])]
  in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/7367
     Also register the tests with and without FA bridge feature flag. *)
  List.iter
    (fun (threshold_encryption, te_tags) ->
      List.iter
        (fun (enable_dal, dal_tags) ->
          register_test_for_kernels
            ~__FILE__
            ?block_storage_sqlite3
            ?sequencer_rpc_port
            ?sequencer_private_rpc_port
            ?commitment_period
            ?challenge_window
            ?genesis_timestamp
            ?time_between_blocks
            ?max_blueprints_lag
            ?max_blueprints_ahead
            ?max_blueprints_catchup
            ?catchup_cooldown
            ?delayed_inbox_timeout
            ?delayed_inbox_min_levels
            ?max_number_of_chunks
            ?bootstrap_accounts
            ?sequencer
            ?sequencer_pool_address
            ~kernels
            ?da_fee
            ?minimum_base_fee_per_gas
            ?preimages_dir
            ?maximum_allowed_ticks
            ?maximum_gas_per_transaction
            ?max_blueprint_lookahead_in_seconds
            ?enable_fa_bridge
            ?additional_uses
            ~threshold_encryption
            ?history_mode
            ~enable_dal
            ~title
            ~tags:(te_tags @ dal_tags @ tags)
            body
            protocols)
        dal_cases)
    threshold_encryption_cases

let register_upgrade_all ~title ~tags ~genesis_timestamp
    ?(time_between_blocks = Evm_node.Nothing) ?(kernels = Kernel.all)
    ?(upgrade_to = Kernel.upgrade_to) ?(additional_uses = []) scenario protocols
    =
  List.iter
    (fun from ->
      let from_tag, _ = Kernel.to_uses_and_tags from in
      let to_ = upgrade_to from in
      let to_tag, to_use = Kernel.to_uses_and_tags to_ in
      register_all
        ~kernels:[from]
        ~genesis_timestamp
        ~time_between_blocks
        ~tags:("upgrade_scenario" :: to_tag :: tags)
        ~title:Format.(sprintf "%s (%s -> %s)" title from_tag to_tag)
        ~additional_uses:(to_use :: additional_uses)
        (scenario from to_)
        protocols)
    kernels

module Protocol = struct
  include Protocol

  let register_test ~__FILE__ ~title ~tags ?uses ?uses_node ?uses_client
      ?uses_admin_client ?supports _body protocols =
    Protocol.register_test
      ~__FILE__
      ~title
      ~tags
      ?uses
      ?uses_node
      ?uses_client
      ?uses_admin_client
      ?supports
      (fun _protocol ->
        Test.fail
          ~loc:__LOC__
          "Do not call Protocol.register_test directly. Use register_test, or \
           register_both instead.")
      protocols
  [@@warning "-unused-value-declaration"]
end

let test_remove_sequencer =
  register_all
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "admin"]
    ~title:"Remove sequencer via sequencer admin contract"
  @@ fun {
           sequencer;
           proxy;
           sc_rollup_node;
           client;
           sc_rollup_address;
           l1_contracts;
           observer;
           _;
         }
             _protocol ->
  (* Produce blocks to show that both the sequencer and proxy are not
     progressing. *)
  let* _ =
    repeat 5 (fun () ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  (* Both are at genesis *)
  let*@ sequencer_head = Rpc.block_number sequencer in
  let*@ proxy_head = Rpc.block_number proxy in
  Check.((sequencer_head = 0l) int32)
    ~error_msg:"Sequencer should be at genesis" ;
  Check.((sequencer_head = proxy_head) int32)
    ~error_msg:"Sequencer and proxy should have the same block number" ;
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
    (* Produce L1 blocks to show that only the proxy is progressing *)
    repeat 5 (fun () ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  Check.((exit_code = Some 100) (option int))
    ~error_msg:"Expected exit code %R, got %L" ;
  (* Sequencer is at genesis, proxy is at [advance]. *)
  Check.((missing_block_nb = 1) int)
    ~error_msg:"Sequencer should be missing block %L" ;
  let*@ proxy_head = Rpc.block_number proxy in
  Check.((proxy_head > 0l) int32) ~error_msg:"Proxy should have advanced" ;

  unit

let test_patch_state =
  register_test (* It's a node specific test. *)
    ~__FILE__
    ~kernel:Kernel.Latest
    ~enable_dal:false
    ~tags:["evm"; "patch"; "state"]
    ~title:"Patch state via command"
    ~time_between_blocks:Nothing
  @@ fun {sequencer; sc_rollup_node; sc_rollup_address; client; _} _protocol ->
  (* Test patch state for the evm-node. *)
  let path = Durable_storage_path.sequencer in
  let*@! before_patch = Rpc.state_value sequencer path in
  let* () = Evm_node.terminate sequencer in
  let* () = Evm_node.patch_state sequencer ~key:path ~value:"00" in
  let* () = Evm_node.run sequencer in
  let*@! after_patch = Rpc.state_value sequencer path in
  Check.((before_patch <> after_patch) string)
    ~error_msg:"value should have changed" ;
  Check.((after_patch = "00") string) ~error_msg:"value should be 00" ;

  (* Test patch state for the rollup node. *)
  let* before_patch =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind:"wasm_2_0_0"
         ~operation:Sc_rollup_rpc.Value
         ~key:path
         ()
  in
  let* _ = next_rollup_node_level ~sc_rollup_node ~client in
  let* () = Sc_rollup_node.terminate sc_rollup_node in
  let* () =
    Sc_rollup_node.patch_durable_storage sc_rollup_node ~key:path ~value:"00"
  in
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup_address [] in
  let* after_patch =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind:"wasm_2_0_0"
         ~operation:Sc_rollup_rpc.Value
         ~key:path
         ()
  in
  Check.((before_patch <> after_patch) (option string))
    ~error_msg:"value should have changed" ;
  Check.((after_patch = Some "00") (option string))
    ~error_msg:"value should be 00" ;

  unit

let test_persistent_state =
  register_all
    ~tags:["evm"; "sequencer"]
    ~title:"Sequencer state is persistent across runs"
    ~time_between_blocks:Nothing
  @@ fun {sequencer; _} _protocol ->
  (* Force the sequencer to produce a block. *)
  let*@ _ = produce_block sequencer in
  (* Ask for the current block. *)
  let*@ block_number = Rpc.block_number sequencer in
  Check.is_true
    ~__LOC__
    (block_number > 0l)
    ~error_msg:"The sequencer should have produced a block" ;
  (* Terminate the sequencer. *)
  let* () = Evm_node.terminate sequencer in
  (* Restart it. *)
  let* () = Evm_node.run sequencer in
  (* Assert the block number is at least [block_number]. Asserting
     that the block number is exactly the same as {!block_number} can
     be flaky if a block is produced between the restart and the
     RPC. *)
  let*@ new_block_number = Rpc.block_number sequencer in
  Check.is_true
    ~__LOC__
    (new_block_number >= block_number)
    ~error_msg:"The sequencer should have produced a block" ;
  unit

(* Helper to setup snapshot test. This function stops the observer and sequencer
   and exports two snapshots at one block interval. *)
let snapshots_setup {sequencer; observer; _} =
  Log.info "Test setup: Exporting snapshots for test." ;
  Log.info " - Exporting older snapshot from observer." ;
  Log.info "   - Terminate the observer." ;
  let* () = Evm_node.terminate observer in
  Log.info "   - Export observer snapshot." ;
  let*! snapshot_file_before = Evm_node.export_snapshot observer in
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
  let*! snapshot_file = Evm_node.export_snapshot sequencer in
  return (snapshot_file_before, snapshot_file, block_number)

let test_snapshots_lock =
  register_all
    ~tags:["evm"; "sequencer"; "snapshots"]
    ~title:"Sequencer snapshots export require lock"
    ~time_between_blocks:Nothing
  @@ fun {sequencer; _} _protocol ->
  Log.info "Export without stopping evm node." ;
  let*? locked = Evm_node.export_snapshot sequencer in
  let* () =
    Process.check_error
      ~msg:(rex "EVM node is locked by another process")
      locked
  in
  unit

let test_snapshots_import_empty =
  register_all
    ~tags:["evm"; "sequencer"; "snapshots"]
    ~title:"Import sequencer snapshot in empty data dir"
    ~time_between_blocks:Nothing
  @@ fun ({sequencer; sc_rollup_node; _} as setup) _protocol ->
  let* _snapshot_file_before, snapshot_file, block_number =
    snapshots_setup setup
  in
  Log.info "Create new sequencer from snapshot." ;
  let new_sequencer =
    let mode = Evm_node.mode sequencer in
    Evm_node.create ~mode (Sc_rollup_node.endpoint sc_rollup_node)
  in
  let* () = Process.check @@ Evm_node.spawn_init_config new_sequencer in
  let*! () = Evm_node.import_snapshot new_sequencer ~snapshot_file in
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

let test_snapshots_import_populated =
  register_all
    ~tags:["evm"; "sequencer"; "snapshots"]
    ~title:"Import sequencer snapshot in populated data dir"
    ~time_between_blocks:Nothing
  @@ fun ({observer; sequencer; _} as setup) _protocol ->
  let* _snapshot_file_before, snapshot_file, block_number =
    snapshots_setup setup
  in
  Log.info "Should not be able to import snapshot in populated node." ;
  let*? populated = Evm_node.import_snapshot observer ~snapshot_file in
  let* () = Process.check_error ~msg:(rex "is already populated") populated in
  Log.info "Should be able to import snapshot in populated node with --force." ;
  let*! () = Evm_node.import_snapshot observer ~snapshot_file ~force:true in
  Log.info "Restart sequencer." ;
  let* () = Evm_node.run sequencer in
  Log.info "Restart observer." ;
  let* () = Evm_node.run observer in
  Log.info "The new observer should have the current block of the snapshot." ;
  let*@ _block =
    Rpc.get_block_by_number observer ~block:(Int32.to_string block_number)
  in
  unit

let test_snapshots_import_outdated =
  register_all
    ~tags:["evm"; "sequencer"; "snapshots"]
    ~title:"Import outdated sequencer snapshot"
    ~time_between_blocks:Nothing
  @@ fun ({sequencer; _} as setup) _protocol ->
  let* snapshot_file_before, _snapshot_file, _block_number =
    snapshots_setup setup
  in
  Log.info "Cannot import outdated snapshot." ;
  let*? outdated =
    Evm_node.import_snapshot
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
let test_snapshots_reexport =
  register_all
    ~tags:["evm"; "sequencer"; "snapshots"]
    ~title:"Import sequencer snapshot and re-export"
    ~time_between_blocks:Nothing
  @@ fun ({sequencer; sc_rollup_node; _} as setup) _protocol ->
  let* _snapshot_file_before, snapshot_file, _block_number =
    snapshots_setup setup
  in
  Log.info "Create new sequencer from snapshot." ;
  let new_sequencer =
    let mode = Evm_node.mode sequencer in
    Evm_node.create ~mode (Sc_rollup_node.endpoint sc_rollup_node)
  in
  let* () = Process.check @@ Evm_node.spawn_init_config new_sequencer in
  let*! () = Evm_node.import_snapshot new_sequencer ~snapshot_file in
  Log.info "Re-export snapshot from new sequencer." ;
  let*! _file = Evm_node.export_snapshot sequencer in
  unit

let test_snapshots protocols =
  test_snapshots_lock protocols ;
  test_snapshots_import_empty protocols ;
  test_snapshots_import_populated protocols ;
  test_snapshots_import_outdated protocols ;
  test_snapshots_reexport protocols

let test_publish_blueprints =
  register_all
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "data"]
    ~title:"Sequencer publishes the blueprints to L1"
    ~use_dal:ci_enabled_dal_registration
  @@ fun {sequencer; proxy; client; sc_rollup_node; enable_dal; _} _protocol ->
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
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer ~proxy () in

  (* We have unfortunately noticed that the test can be flaky. Sometimes,
     the following RPC is done before the proxy being initialised, even though
     we wait for it. The source of flakiness is unknown but happens very rarely,
     we put a small sleep to make the least flaky possible. *)
  let* () = Lwt_unix.sleep 2. in
  check_head_consistency ~left:sequencer ~right:proxy ()

let test_sequencer_too_ahead =
  let max_blueprints_ahead = 5 in
  register_all
    ~max_blueprints_ahead
    ~time_between_blocks:Nothing
    ~tags:["evm"; "max_blueprint_ahead"]
    ~title:"Sequencer locks production if it's too ahead"
    ~use_dal:ci_enabled_dal_registration
  @@ fun {sequencer; sc_rollup_node; proxy; client; sc_rollup_address; _}
             _protocol ->
  let* () = bake_until_sync ~sc_rollup_node ~proxy ~sequencer ~client () in
  let* () = Sc_rollup_node.terminate sc_rollup_node in
  let* () =
    repeat (max_blueprints_ahead + 1) (fun () ->
        let*@ _ = produce_block sequencer in
        unit)
  in
  let* () =
    let*@ _ = produce_block ~wait_on_blueprint_applied:false sequencer in
    unit
  and* () = Evm_node.wait_for_block_producer_locked sequencer in
  let* () =
    repeat max_blueprints_ahead (fun () ->
        let*@ _ = produce_block ~wait_on_blueprint_applied:false sequencer in
        unit)
  in
  let*@ block_number = Rpc.block_number sequencer in
  Check.((block_number = 6l) int32)
    ~error_msg:"The sequencer should have been locked" ;
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup_address []
  and* () = Evm_node.wait_for_rollup_node_follower_connection_acquired sequencer
  and* () = Evm_node.wait_for_rollup_node_follower_connection_acquired proxy in
  let* () = bake_until_sync ~sc_rollup_node ~proxy ~sequencer ~client () in
  let* _ =
    repeat 2 (fun () ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~client in
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
    ~max_blueprints_lag
    ~max_blueprints_catchup
    ~catchup_cooldown
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "data"; Tag.flaky]
    ~title:"Sequencer is resilient to rollup node disconnection"
    ~use_dal:ci_enabled_dal_registration
  @@ fun {
           sequencer;
           proxy;
           sc_rollup_node;
           sc_rollup_address;
           client;
           observer;
           _;
         }
             _protocol ->
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
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer ~proxy () in

  let*@ rollup_node_head = Rpc.get_block_by_number ~block:"latest" proxy in
  Check.(
    (rollup_node_head.number = Int32.(of_int first_batch_blueprints_count))
      int32)
    ~error_msg:
      "The rollup node should have received the first round of lost blueprints \
       (rollup node level: %L, expected level %R)" ;

  (* Check sequencer and rollup consistency *)
  let* () = check_head_consistency ~left:sequencer ~right:proxy () in

  let* () =
    (* bake 2 block so evm_node sees it as finalized in
       `rollup_node_follower` *)
    repeat 2 (fun () ->
        let* _lvl = next_rollup_node_level ~sc_rollup_node ~client in
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
  and* () = Evm_node.wait_for_rollup_node_follower_connection_acquired observer
  and* () = Evm_node.wait_for_rollup_node_follower_connection_acquired proxy in

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
        let* _lvl = next_rollup_node_level ~sc_rollup_node ~client in

        unit)
      ~result_f:(fun () ->
        let*@ rollup_node_head =
          Rpc.get_block_by_number ~block:"latest" proxy
        in
        if rollup_node_head.number = expected_level then return (Some ())
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
      ~proxy
      ()
  in

  (* Check the consistency again *)
  check_head_consistency
    ~error_msg:
      "The head should be the same after the outage. Sequencer: {%L}, proxy: \
       {%R}"
    ~left:sequencer
    ~right:proxy
    ()

let test_can_fetch_blueprint =
  register_all
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
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "rpc"]
    ~title:"Sequencer can return the smart rollup address on demand"
  @@ fun {sequencer; sc_rollup_address; _} _protocol ->
  let* claimed_address = Sequencer_rpc.get_smart_rollup_address sequencer in

  Check.((sc_rollup_address = claimed_address) string)
    ~error_msg:"Returned address is not the expected one" ;

  unit

let test_send_transaction_to_delayed_inbox =
  register_all
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"]
    ~title:"Send a transaction to the delayed inbox"
  @@ fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; _} _protocol
    ->
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
  let send ~amount ?expect_failure () =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      ~amount
      ?expect_failure
      raw_transfer
  in
  (* Test that paying less than 1XTZ is not allowed. *)
  let* _hash =
    send ~amount:(Tez.parse_floating "0.9") ~expect_failure:true ()
  in
  (* Test the correct case where the user burns 1XTZ to send the transaction. *)
  let* hash = send ~amount:Tez.one ~expect_failure:false () in
  (* Assert that the expected transaction hash is found in the delayed inbox
     durable storage path. *)
  let* () = Delayed_inbox.assert_mem (Sc_rollup_node sc_rollup_node) hash in
  (* Test that paying more than 1XTZ is allowed. *)
  let* _hash =
    send ~amount:(Tez.parse_floating "1.1") ~expect_failure:false ()
  in
  unit

let test_send_deposit_to_delayed_inbox =
  register_all
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "deposit"]
    ~title:"Send a deposit to the delayed inbox"
  @@ fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; _} _protocol
    ->
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
  let* () =
    send_deposit_to_delayed_inbox
      ~amount
      ~l1_contracts
      ~depositor
      ~receiver:receiver.address
      ~sc_rollup_node
      ~sc_rollup_address
      client
  in
  let* delayed_transactions_hashes =
    Delayed_inbox.content (Sc_rollup_node sc_rollup_node)
  in
  let* deposit =
    Delayed_inbox.data
      (Sc_rollup_node sc_rollup_node)
      (List.hd delayed_transactions_hashes)
  in

  let deposit_bytes = Hex.to_bytes (`Hex (Option.get deposit)) in

  match Evm_node_lib_dev_encoding.Rlp.decode deposit_bytes with
  | Ok
      Evm_node_lib_dev_encoding.Rlp.(
        List (List (Value tag :: List (Value amnt :: Value rcvr :: _) :: _) :: _))
    ->
      let deposit_tag = tag |> Hex.of_bytes |> Hex.show in
      Check.((deposit_tag = "02") string)
        ~error_msg:"Expected tag %R, but got: %L" ;
      let receiver = rcvr |> Hex.of_bytes |> Hex.show in
      Check.((receiver = "1074fd1ec02cbeaa5a90450505cf3b48d834f3eb") string)
        ~error_msg:"Expected receiver %R, but got: %L" ;
      let amount = amnt |> Hex.of_bytes |> Hex.show in
      (* 16000000000000000000 big endian *)
      Check.((amount = "de0b6b3a76400000") string)
        ~error_msg:"Expected amount %R, but got: %L" ;
      unit
  | _ -> failwith "invalid delayed inbox item"

let test_rpc_produceBlock =
  register_all
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "produce_block"]
    ~title:"RPC method produceBlock"
  @@ fun {sequencer; _} _protocol ->
  let*@ start_block_number = Rpc.block_number sequencer in
  let*@ _ = produce_block sequencer in
  let*@ new_block_number = Rpc.block_number sequencer in
  Check.((Int32.succ start_block_number = new_block_number) int32)
    ~error_msg:"Expected new block number to be %L, but got: %R" ;
  unit

let wait_for_event ?(timeout = 30.) ?(levels = 10) event_watcher ~sequencer
    ~sc_rollup_node ~client ~error_msg =
  let event_value = ref None in
  let _ =
    let* return_value = event_watcher in
    event_value := Some return_value ;
    unit
  in
  let rec rollup_node_loop n =
    if n = 0 then Test.fail error_msg
    else
      let* _ = next_rollup_node_level ~sc_rollup_node ~client in
      let*@ _ = produce_block sequencer in
      if Option.is_some !event_value then unit else rollup_node_loop (n - 1)
  in
  let* () = Lwt.pick [rollup_node_loop levels; Lwt_unix.sleep timeout] in
  match !event_value with
  | Some value -> return value
  | None -> Test.fail ~loc:__LOC__ "Waiting for event failed"

let wait_for_delayed_inbox_add_tx_and_injected ~sequencer ~sc_rollup_node
    ~client =
  let event_watcher =
    let added = Evm_node.wait_for_evm_event New_delayed_transaction sequencer in
    let injected = Evm_node.wait_for_block_producer_tx_injected sequencer in
    let* (_transaction_kind, added_hash), injected_hash =
      Lwt.both added injected
    in
    Check.((added_hash = injected_hash) string)
      ~error_msg:"Injected hash %R is not the expected one %L" ;
    Lwt.return_unit
  in
  wait_for_event
    event_watcher
    ~sequencer
    ~sc_rollup_node
    ~client
    ~error_msg:
      "Timed out while waiting for transaction to be added to the delayed \
       inbox and injected"

let test_delayed_transfer_is_included =
  register_all
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "inclusion"]
    ~title:"Delayed transaction is included"
    ~time_between_blocks:Nothing
  @@ fun {
           client;
           l1_contracts;
           sc_rollup_address;
           sc_rollup_node;
           sequencer;
           proxy;
           observer;
           _;
         }
             _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
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
  let sender = Eth_account.bootstrap_accounts.(0).address in
  let receiver = Eth_account.bootstrap_accounts.(1).address in
  let* sender_balance_prev = Eth_cli.balance ~account:sender ~endpoint in
  let* receiver_balance_prev = Eth_cli.balance ~account:receiver ~endpoint in
  let* tx_hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      raw_transfer
  in
  let* () =
    wait_for_delayed_inbox_add_tx_and_injected
      ~sequencer
      ~sc_rollup_node
      ~client
  in
  let* () = bake_until_sync ~sc_rollup_node ~proxy ~sequencer ~client () in
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in
  let* sender_balance_next = Eth_cli.balance ~account:sender ~endpoint in
  let* receiver_balance_next = Eth_cli.balance ~account:receiver ~endpoint in
  Check.((sender_balance_prev > sender_balance_next) Wei.typ)
    ~error_msg:"Expected a smaller sender balance (before: %L < after: %R)" ;
  Check.((receiver_balance_next > receiver_balance_prev) Wei.typ)
    ~error_msg:"Expected a bigger receiver balance (before: %L > after: %R)" ;
  let*@! (_receipt : Transaction.transaction_receipt) =
    Rpc.get_transaction_receipt ~tx_hash observer
  in
  unit

let test_largest_delayed_transfer_is_included =
  register_all
    ~kernels:[Kernel.Latest]
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "inclusion"]
    ~title:"Largest possible delayed transaction is included"
    ~time_between_blocks:Nothing
  @@ fun {
           client;
           l1_contracts;
           sc_rollup_address;
           sc_rollup_node;
           sequencer;
           proxy;
           _;
         }
             _protocol ->
  let _endpoint = Evm_node.endpoint sequencer in
  (* This is the largest ethereum transaction we transfer via the bridge contract. *)
  let max_data = String.make 64896 '0' in
  let* transfer_that_fits =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1
      ~nonce:2323
      ~gas_price:10_000_000_000
      ~gas:21_000
      ~value:(Wei.of_eth_int 1)
      ~address:Eth_account.bootstrap_accounts.(1).address
      ~arguments:[max_data]
      ()
  in
  let len_transfer_that_fits = String.length transfer_that_fits / 2 in
  Log.info "Maximum size allowed is %d" len_transfer_that_fits ;
  (* We assert that this is the largest by sending a transaction that is 1 byte
     larger, the protocol refuses it. *)
  let data_too_big = max_data ^ "00" in
  let* transfer_too_big =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1
      ~nonce:2323
      ~gas_price:10_000_000_000
      ~gas:21_000
      ~value:(Wei.of_eth_int 1)
      ~address:Eth_account.bootstrap_accounts.(1).address
      ~arguments:[data_too_big]
      ()
  in
  let len_transfer_too_big = String.length transfer_too_big / 2 in
  assert (len_transfer_that_fits + 1 = len_transfer_too_big) ;
  let* _hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      ~expect_failure:true
      transfer_too_big
  in
  (* Now we check that the largest possible transaction is included by the sequencer. *)
  let* _hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      ~expect_failure:false
      transfer_that_fits
  in
  let* () =
    wait_for_delayed_inbox_add_tx_and_injected
      ~sequencer
      ~sc_rollup_node
      ~client
  in
  let* () = bake_until_sync ~sc_rollup_node ~proxy ~sequencer ~client () in
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in
  unit

let test_delayed_deposit_is_included =
  register_all
    ~time_between_blocks:Nothing
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "inclusion"; "deposit"]
    ~title:"Delayed deposit is included"
    ~use_dal:ci_enabled_dal_registration
  @@ fun {
           client;
           l1_contracts;
           sc_rollup_address;
           sc_rollup_node;
           sequencer;
           proxy;
           _;
         }
             _protocol ->
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
    Eth_cli.balance ~account:receiver.address ~endpoint
  in
  let* () =
    send_deposit_to_delayed_inbox
      ~amount
      ~l1_contracts
      ~depositor
      ~receiver:receiver.address
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
  let* () = bake_until_sync ~sc_rollup_node ~proxy ~sequencer ~client () in
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in
  let* receiver_balance_next =
    Eth_cli.balance ~account:receiver.address ~endpoint
  in
  Check.((receiver_balance_next > receiver_balance_prev) Wei.typ)
    ~error_msg:"Expected a bigger balance" ;
  unit

let encode_data json codec =
  let* hex_string = Codec.encode ~name:codec json in
  let hex = `Hex (Durable_storage_path.no_0x hex_string) in
  return (Hex.to_bytes hex)

let ticket_content token_id =
  let* content_bytes =
    encode_data
      (Ezjsonm.from_string
         (sf
            "{\"prim\": \"Pair\", \"args\": [{\"int\": \"%d\"}, {\"prim\": \
             \"None\"}]}"
            token_id))
      "alpha.script.expr"
  in
  return content_bytes

let ticket_creator ticketer =
  let* ticketer_bytes =
    encode_data (Ezjsonm.string ticketer) "alpha.contract"
  in
  return ticketer_bytes

let ticket_hash ticketer token_id =
  let* ticketer_bytes = ticket_creator ticketer in
  let* content_bytes = ticket_content token_id in
  let payload = Bytes.concat Bytes.empty [ticketer_bytes; content_bytes] in
  let payload_digest = Tezos_crypto.Hacl.Hash.Keccak_256.digest payload in
  return (payload_digest |> Hex.of_bytes |> Hex.show)

let ticket_balance ~ticket_hash ~account endpoint =
  let account =
    account |> Durable_storage_path.no_0x |> String.lowercase_ascii
  in
  let* ticket_balance =
    let key = Durable_storage_path.Ticket_table.balance ~ticket_hash ~account in
    match endpoint with
    | Either.Left sc_rollup_node ->
        Sc_rollup_node.RPC.call
          sc_rollup_node
          ~rpc_hooks:Tezos_regression.rpc_hooks
        @@ Sc_rollup_rpc.get_global_block_durable_state_value
             ~pvm_kind:"wasm_2_0_0"
             ~operation:Sc_rollup_rpc.Value
             ~key
             ()
    | Either.Right evm_node ->
        let*@ v = Rpc.state_value evm_node key in
        return v
  in
  return
  @@ Option.fold
       ~none:0
       ~some:(fun ticket_balance ->
         `Hex ticket_balance |> Hex.to_string |> Z.of_bits |> Z.to_int)
       ticket_balance

let test_delayed_fa_deposit_is_included =
  register_all
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
  @@ fun {
           client;
           l1_contracts;
           sc_rollup_address;
           sc_rollup_node;
           sequencer;
           proxy;
           _;
         }
             _protocol ->
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
  let* () = bake_until_sync ~sc_rollup_node ~proxy ~sequencer ~client () in
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in

  let* zero_ticket_hash = ticket_hash l1_contracts.ticket_router_tester 0 in

  let* ticket_balance_via_rollup_node =
    ticket_balance
      ~ticket_hash:zero_ticket_hash
      ~account:receiver
      (Either.Left sc_rollup_node)
  in
  Check.((amount = ticket_balance_via_rollup_node) int)
    ~error_msg:
      "After deposit we expect %L ticket balance in the rollup node, got %R" ;
  let* ticket_balance_via_sequencer =
    ticket_balance
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
  @@ fun {
           client;
           l1_contracts;
           sc_rollup_address;
           sc_rollup_node;
           sequencer;
           proxy;
           _;
         }
             _protocol ->
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

  let* () = bake_until_sync ~sc_rollup_node ~proxy ~sequencer ~client () in

  let* zero_ticket_hash = ticket_hash l1_contracts.ticket_router_tester 0 in

  let* ticket_balance_via_rollup_node =
    ticket_balance
      ~ticket_hash:zero_ticket_hash
      ~account:receiver
      (Either.Left sc_rollup_node)
  in
  Check.((0 = ticket_balance_via_rollup_node) int)
    ~error_msg:
      "The deposit should have been refused by rollup node, but balance is %R" ;
  let* ticket_balance_via_sequencer =
    ticket_balance
      ~ticket_hash:zero_ticket_hash
      ~account:receiver
      (Either.Right sequencer)
  in
  Check.((0 = ticket_balance_via_sequencer) int)
    ~error_msg:
      "The deposit should have been refused by sequencer, but balance is %R" ;
  unit

let test_delayed_transaction_peeked =
  register_all
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "failure"]
    ~title:"Delayed deposit is removed only when it is applied"
    ~enable_fa_bridge:false
    ~kernels:[Kernel.Latest]
    ~time_between_blocks:Nothing
  @@ fun {
           client;
           l1_contracts;
           sc_rollup_address;
           sc_rollup_node;
           sequencer;
           proxy;
           kernel;
           _;
         }
             _protocol ->
  (* This test act as an non-regression test.

     The unwanted behavior was :
       - A blueprint containing a deposit triggers an upgrade
       - The kernel reboots and upgrade
       - The blueprint is popped again and fails to find the deposit

     To mitigate this problem until the kernel is patched we don't put
     transactions in a block that triggers an upgrade. In this test we
     are going to play with this property to force an upgrade with a
     blueprint that contains a deposit.
  *)
  let* () =
    send_deposit_to_delayed_inbox
      ~amount:Tez.one
      ~l1_contracts
      ~depositor:Constant.bootstrap5
      ~receiver:"0x1074Fd1EC02cbeaa5A90450505cF3B48D834f3EB"
      ~sc_rollup_node
      ~sc_rollup_address
      client
  in
  (* We bake enough blocks for the sequencer to realize there's a deposit. *)
  let* () =
    repeat 2 (fun () ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  (* Send an upgrade to the rollup node, but don't finalize the block, so the
     sequencer doesn't see the upgrade. *)
  let* _root_hash =
    upgrade
      ~sc_rollup_node
      ~sc_rollup_address
      ~admin:Constant.bootstrap2.public_key_hash
      ~admin_contract:l1_contracts.admin
      ~client
      ~upgrade_to:kernel
      ~activation_timestamp:"0"
  in
  (* Produce a block. The sequencer will include the deposit, but won't upgrade.
     The rollup node will upgrade as soon as it receives the block. *)
  let*@ _ = produce_block sequencer in
  bake_until_sync ~sc_rollup_node ~client ~sequencer ~proxy ()

let test_invalid_delayed_transaction =
  register_all
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "invalid"]
    ~title:"Delayed transaction is removed even when invalid"
    ~enable_fa_bridge:false
    ~kernels:[Kernel.Latest]
    ~time_between_blocks:Nothing
  @@ fun {
           client;
           l1_contracts;
           sc_rollup_address;
           sc_rollup_node;
           sequencer;
           proxy;
           _;
         }
             _protocol ->
  (* Produces an invalid transaction by setting an invalid nonce. *)
  let* invalid_nonce =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:16
      ~gas_price:1_000_000_000
      ~gas:23_300
      ~value:(Wei.of_eth_int 1)
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  in
  let* hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      ~amount:Tez.one
      invalid_nonce
  in
  (* Assert that the expected transaction hash is found in the delayed inbox
     durable storage path. *)
  let* () = Delayed_inbox.assert_mem (Sc_rollup_node sc_rollup_node) hash in
  (* We bake enough blocks for the sequencer to see the transaction. *)
  let* () =
    repeat 2 (fun () ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  let*@ _ = produce_block sequencer in
  let* () = bake_until_sync ~sc_rollup_node ~client ~proxy ~sequencer () in

  (* There is no receipt for the transaction because it's invalid. *)
  let*@ receipt_opt = Rpc.get_transaction_receipt ~tx_hash:hash sequencer in
  Check.is_true
    (receipt_opt = None)
    ~error_msg:"The transaction should not have a receipt" ;
  (* The transaction has been cleared from the delayed inbox. *)
  Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node)

let call_fa_withdraw ?expect_failure ~sender ~endpoint ~evm_node ~ticket_owner
    ~routing_info ~amount ~ticketer ~content () =
  let* () =
    Eth_cli.add_abi ~label:"fa_withdrawal" ~abi:(fa_withdrawal_abi_path ()) ()
  in
  send_transaction_to_sequencer
    (Eth_cli.contract_send
       ?expect_failure
       ~source_private_key:sender.Eth_account.private_key
       ~endpoint
       ~abi_label:"fa_withdrawal"
       ~address:"0xff00000000000000000000000000000000000002"
       ~method_call:
         (sf
            {|withdraw("%s", "0x%s", %d, "0x%s", "0x%s")|}
            ticket_owner
            routing_info
            amount
            ticketer
            content))
    evm_node

let test_fa_withdrawal_is_included =
  register_all
    ~da_fee:Wei.one
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
  @@ fun {
           client;
           l1_contracts;
           sc_rollup_address;
           sc_rollup_node;
           sequencer;
           proxy;
           _;
         }
             _protocol ->
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
  let* () = bake_until_sync ~sc_rollup_node ~proxy ~sequencer ~client () in
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in

  (* Check that deposit is successful *)
  let* zero_ticket_hash = ticket_hash l1_contracts.ticket_router_tester 0 in

  let* ticket_balance_after_deposit =
    ticket_balance
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

  let* () = bake_until_sync ~sequencer ~sc_rollup_node ~proxy ~client () in

  (* Check that tickets are gone *)
  let* ticket_balance_after_withdraw =
    ticket_balance
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
      ~evm_node:proxy
      ~sc_rollup_node
      ~sc_rollup_address
      ~client
      ()
  in

  let* _ = next_rollup_node_level ~sc_rollup_node ~client in

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

let test_delayed_deposit_from_init_rollup_node =
  register_all
    ~block_storage_sqlite3:false (* The test uses init from rollup node. *)
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "init"]
    ~title:"Delayed inbox is populated at init from rollup node"
  @@ fun {
           client;
           l1_contracts;
           sc_rollup_address;
           sc_rollup_node;
           sequencer;
           proxy;
           _;
         }
             _protocol ->
  let receiver = "0x1074Fd1EC02cbeaa5A90450505cF3B48D834f3EB" in
  let* receiver_balance_prev =
    Eth_cli.balance ~account:receiver ~endpoint:(Evm_node.endpoint sequencer)
  in
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~proxy ~client () in
  (* We don't care about this sequencer. *)
  let* () = Evm_node.terminate sequencer in
  (* Send the deposit to delayed inbox, no sequencer will be listening to the
     event. *)
  let amount = Tez.of_int 16 in
  let depositor = Constant.bootstrap5 in
  let* () =
    send_deposit_to_delayed_inbox
      ~amount
      ~l1_contracts
      ~depositor
      ~receiver
      ~sc_rollup_node
      ~sc_rollup_address
      client
  in
  (* Bake an extra block for a finalized deposit. *)
  let* _lvl = next_rollup_node_level ~sc_rollup_node ~client in
  let* _lvl = next_rollup_node_level ~sc_rollup_node ~client in

  (* Run a new sequencer that is initialized from a rollup node that has the
     delayed deposit in its state. *)
  let new_sequencer =
    let mode = Evm_node.mode sequencer in
    Evm_node.create ~mode (Sc_rollup_node.endpoint sc_rollup_node)
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
    bake_until_sync ~sc_rollup_node ~proxy ~sequencer:new_sequencer ~client ()
  in
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in
  let* receiver_balance_next =
    Eth_cli.balance
      ~account:receiver
      ~endpoint:(Evm_node.endpoint new_sequencer)
  in
  Check.((receiver_balance_next > receiver_balance_prev) Wei.typ)
    ~error_msg:"Expected a bigger balance" ;
  unit

(** test to initialise a sequencer data dir based on a rollup node data dir *)
let test_init_from_rollup_node_data_dir =
  register_all
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/7285
     Replace by [Any] after the next upgrade
  *)
    ~block_storage_sqlite3:false (* The test uses init from rollup node. *)
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
    ~history_mode:Archive
  @@ fun {sc_rollup_node; sequencer; observer; proxy; client; boot_sector; _}
             _protocol ->
  (* a sequencer is needed to produce an initial block *)
  let* () =
    repeat 5 (fun () ->
        let*@ _ = produce_block sequencer in
        unit)
  in
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer ~proxy () in
  let* () = Evm_node.terminate sequencer
  and* () = Evm_node.terminate observer in
  let evm_node' =
    Evm_node.create
      ~mode:(Evm_node.mode sequencer)
      (Sc_rollup_node.endpoint sc_rollup_node)
  in
  let* () = Process.check @@ Evm_node.spawn_init_config evm_node' in
  let* () =
    (* bake 2 blocks so rollup context is for the finalized l1 level
       and can't be reorged. *)
    repeat 2 (fun () ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in

  let* () =
    Evm_node.reconstruct_from_rollup_node_data_dir
      ~boot_sector
      evm_node'
      sc_rollup_node
  in
  let* () = Evm_node.run evm_node' in

  let* () = check_head_consistency ~left:evm_node' ~right:proxy () in

  let*@ _ = produce_block evm_node' in
  let* () =
    bake_until_sync ~sc_rollup_node ~client ~sequencer:evm_node' ~proxy ()
  in

  let* () = check_head_consistency ~left:evm_node' ~right:proxy () in

  let*@ head = Rpc.block_number evm_node' in

  (* If the sequencer history has been reconstructed during the init, you can
     make requests in the past. *)
  let* () =
    fold (Int32.to_int head) () (fun l () ->
        let*@ _ =
          Rpc.get_balance
            ~address:"0xB7A97043983f24991398E5a82f63F4C58a417185"
            ~block:(Number l)
            evm_node'
        in
        unit)
  in

  unit

let test_init_from_rollup_node_with_delayed_inbox =
  register_all
    ~block_storage_sqlite3:false (* The test uses init from rollup node. *)
    ~time_between_blocks:Nothing
    ~kernels:[Kernel.Latest]
    ~tags:["evm"; "rollup_node"; "init"; "delayed_inbox"; "omit"]
    ~title:
      "Init evm node sequencer data dir from a rollup node data dir with \
       delayed items"
  @@ fun {
           sc_rollup_node;
           sequencer;
           observer;
           proxy;
           client;
           l1_contracts;
           sc_rollup_address;
           _;
         }
             _protocol ->
  (* The sequencer is needed to produce an initial block for the init from
     rollup node to work. *)
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer ~proxy () in
  let* () = Evm_node.terminate sequencer in
  let* () = Evm_node.terminate observer in
  (* Sends a deposit to the delayed inbox. *)
  let* () =
    send_deposit_to_delayed_inbox
      ~amount:Tez.one
      ~l1_contracts
      ~depositor:Constant.bootstrap5
      ~receiver:"0xB7A97043983f24991398E5a82f63F4C58a417185"
      ~sc_rollup_node
      ~sc_rollup_address
      client
  in
  (* Finalize the transaction. *)
  let* _ = next_rollup_node_level ~sc_rollup_node ~client in
  let* _ = next_rollup_node_level ~sc_rollup_node ~client in

  (* Start a new sequencer, the previous sequencer is doomed. *)
  let sequencer =
    Evm_node.create
      ~mode:(Evm_node.mode sequencer)
      (Sc_rollup_node.endpoint sc_rollup_node)
  in
  let* () = Process.check @@ Evm_node.spawn_init_config sequencer in
  let* () = Evm_node.init_from_rollup_node_data_dir sequencer sc_rollup_node in
  let* () = Evm_node.run sequencer in
  (* The sequencer should have items in its delayed inbox. *)
  let* delayed_inbox_size = Delayed_inbox.size (Evm_node sequencer) in
  Check.((delayed_inbox_size = 1) int)
    ~error_msg:"The sequencer should have the delayed inbox in its state" ;
  (* Start a new observer, we will ask it to omit the delayed transactions. *)
  let observer =
    Evm_node.create ~mode:(Evm_node.mode observer) (Evm_node.endpoint sequencer)
  in
  let* () = Process.check @@ Evm_node.spawn_init_config observer in
  let* () =
    Evm_node.init_from_rollup_node_data_dir
      ~omit_delayed_tx_events:true
      observer
      sc_rollup_node
  in
  let* () = Evm_node.run observer in
  let* () = Delayed_inbox.assert_empty (Evm_node observer) in

  (* Finally produce a block to clear the delayed inbox. *)
  let*@ len = produce_block sequencer in
  Check.((len = 1) int) ~error_msg:"Expected one transaction in the block" ;
  let* _ = bake_until_sync ~proxy ~sc_rollup_node ~client ~sequencer () in
  let* () = check_head_consistency ~left:sequencer ~right:observer () in
  unit

let check_applies_blueprint ~timeout sequencer_node observer_node levels_to_wait
    =
  let*@ current_head = Rpc.block_number sequencer_node in
  let targeted_level = Int32.to_int current_head + levels_to_wait in

  let* () =
    repeat levels_to_wait @@ fun () ->
    let*@ _ = Rpc.produce_block sequencer_node in
    unit
  and* _ =
    Evm_node.wait_for_blueprint_applied ~timeout observer_node targeted_level
  and* _ =
    Evm_node.wait_for_blueprint_applied ~timeout sequencer_node targeted_level
  in

  let* () =
    check_block_consistency
      ~left:sequencer_node
      ~right:observer_node
      ~block:(`Level (Int32.of_int levels_to_wait))
      ()
  in
  unit

let test_observer_applies_blueprint =
  let levels_to_wait = 3 in
  let timeout = 30. in
  register_all
    ~time_between_blocks:Nothing
    ~tags:["evm"; "observer"]
    ~title:"Can start an Observer node"
  @@ fun {sequencer = sequencer_node; observer = observer_node; _} _protocol ->
  let* () =
    check_applies_blueprint ~timeout sequencer_node observer_node levels_to_wait
  in

  (* We stop and start the sequencer, to ensure the observer node correctly
     reconnects to it. *)
  let* () = Evm_node.wait_for_retrying_connect observer_node
  and* () =
    let* () = Evm_node.terminate sequencer_node in
    Evm_node.run sequencer_node
  in

  let* () =
    check_applies_blueprint ~timeout sequencer_node observer_node levels_to_wait
  in

  unit

let test_observer_applies_blueprint_dont_track_rollup_node =
  let tbb = 3. in
  register_all
    ~time_between_blocks:(Time_between_blocks tbb)
    ~tags:["evm"; "observer"; "dont_track_rollup_node"]
    ~title:"Can start an Observer node without a rollup node"
  @@ fun {
           sequencer = sequencer_node;
           sc_rollup_node;
           observer = observer_node;
           _;
         }
             _protocols ->
  let levels_to_wait = 3 in
  let timeout = tbb *. float_of_int levels_to_wait *. 2. in
  (* We stop the rollup node. This will allow to demonstrate it is not needed
     to start an observer node when the latter is configure accordingly. *)
  let* () = Sc_rollup_node.terminate sc_rollup_node in

  (* We demonstrate we can start a working observer node with the
     `--dont-track-rollup-node` CLI flag. *)
  let* () = Evm_node.terminate observer_node in
  let* () =
    Evm_node.run ~extra_arguments:["--dont-track-rollup-node"] observer_node
  and* () = Evm_node.wait_for_rollup_node_follower_disabled observer_node in

  let* () =
    check_applies_blueprint ~timeout sequencer_node observer_node levels_to_wait
  in

  (* We demonstrate we can start a working observer node with the
     `observer.rollup_node_tracking` configuration. *)
  let* () = Evm_node.terminate observer_node in
  let* () =
    Evm_node.Config_file.update observer_node (fun json ->
        JSON.update
          "observer"
          (fun json ->
            JSON.put
              ("rollup_node_tracking", JSON.annotate ~origin:"" (`Bool false))
              json)
          json)
  in

  let* () = Evm_node.run observer_node
  and* () = Evm_node.wait_for_rollup_node_follower_disabled observer_node in

  let* () =
    check_applies_blueprint ~timeout sequencer_node observer_node levels_to_wait
  in

  unit

let test_observer_applies_blueprint_from_rpc_node =
  let tbb = 3. in
  register_all
    ~time_between_blocks:(Time_between_blocks tbb)
    ~tags:["evm"; "observer"; "rpc_mode"]
    ~title:"Can start an Observer node tracking a RPC node"
  @@ fun {sequencer = sequencer_node; sc_rollup_node; _} _protocol ->
  (* This is another version of [test_observer_applies_blueprint], but this
     time instead of connecting directly to the sequencer, the observer
     connects to a RPC mode process. *)
  let levels_to_wait = 3 in
  let timeout = tbb *. float_of_int levels_to_wait *. 2. in

  let* rpc_node = run_new_rpc_endpoint sequencer_node in
  let* observer_node = run_new_observer_node ~sc_rollup_node rpc_node in

  let* _ =
    Evm_node.wait_for_blueprint_applied ~timeout observer_node levels_to_wait
  and* _ =
    Evm_node.wait_for_blueprint_applied ~timeout sequencer_node levels_to_wait
  in

  let* () =
    check_block_consistency
      ~left:sequencer_node
      ~right:observer_node
      ~block:(`Level (Int32.of_int levels_to_wait))
      ()
  in

  (* We stop and start the sequencer, to ensure the observer node correctly
     reconnects to it. *)
  let* () = Evm_node.wait_for_retrying_connect observer_node
  and* () =
    let* () = Evm_node.terminate sequencer_node in
    Evm_node.run sequencer_node
  in

  let levels_to_wait = 2 * levels_to_wait in

  let* _ =
    Evm_node.wait_for_blueprint_applied ~timeout observer_node levels_to_wait
  and* _ =
    Evm_node.wait_for_blueprint_applied ~timeout sequencer_node levels_to_wait
  in

  let* () =
    check_block_consistency
      ~left:sequencer_node
      ~right:observer_node
      ~block:(`Level (Int32.of_int levels_to_wait))
      ()
  in

  unit

let test_observer_applies_blueprint_when_restarted =
  register_all
    ~time_between_blocks:Nothing
    ~tags:["evm"; "observer"]
    ~title:"Can restart an Observer node"
  @@ fun {sequencer; observer; _} _protocol ->
  (* We produce a block and check the observer applies it. *)
  let* _ = Evm_node.wait_for_blueprint_applied observer 1
  and* _ = produce_block sequencer in

  (* We restart the observer *)
  let* () = Evm_node.terminate observer in
  let* () = Evm_node.run observer in

  (* We produce a block and check the observer applies it. *)
  let* _ = Evm_node.wait_for_blueprint_applied observer 2
  and* _ = produce_block sequencer in

  unit

let test_get_balance_block_param =
  register_all
    ~block_storage_sqlite3:false (* The test uses init from rollup node. *)
    ~tags:["evm"; "sequencer"; "rpc"; "get_balance"; "block_param"]
    ~title:"RPC method getBalance uses block parameter"
    ~time_between_blocks:Nothing
  @@ fun {sequencer; sc_rollup_node; proxy; client; _} _protocol ->
  (* Transfer funds to a random address. *)
  let address = "0xB7A97043983f24991398E5a82f63F4C58a417185" in
  let* _tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.transaction_send
         ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
         ~to_public_key:address
         ~value:(Wei.of_eth_int 10)
         ~endpoint:(Evm_node.endpoint sequencer))
      sequencer
  in
  (* Check the balance on genesis block and latest block. *)
  let*@ balance_genesis =
    Rpc.get_balance ~address ~block:(Number 0) sequencer
  in
  let*@ balance_now = Rpc.get_balance ~address ~block:Latest sequencer in
  Check.((balance_genesis = Wei.of_eth_int 0) Wei.typ)
    ~error_msg:(sf "%s should have no funds at genesis, but got %%L" address) ;
  Check.((balance_now = Wei.of_eth_int 10) Wei.typ)
    ~error_msg:(sf "Balance of %s expected to be %%R but got %%L" address) ;
  (* Now we will create another observer initialized from the rollup node, in
     order to test the block parameter "earliest". "Earliest" on the current
     sequencer will be block [0] which is not really robust to test. *)
  let* () = bake_until_sync ~sc_rollup_node ~proxy ~sequencer ~client () in
  let* _ =
    repeat 2 (fun _ ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  let observer_partial_history =
    let name = "observer_partial_history" in
    Evm_node.create
      ~name
      ~mode:
        (Observer
           {
             initial_kernel = "evm_kernel.wasm";
             preimages_dir = "/tmp";
             private_rpc_port = None;
             rollup_node_endpoint = Sc_rollup_node.endpoint sc_rollup_node;
           })
      ~data_dir:(Temp.dir name)
      (Evm_node.endpoint sequencer)
  in
  let* () =
    Process.check @@ Evm_node.spawn_init_config observer_partial_history
  in
  let* () =
    Evm_node.init_from_rollup_node_data_dir
      observer_partial_history
      sc_rollup_node
  in
  let* () = Evm_node.run observer_partial_history in
  (* Transfer funds again to the address. *)
  let*@ level = Rpc.block_number sequencer in
  let* _tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.transaction_send
         ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
         ~to_public_key:address
         ~value:(Wei.of_eth_int 10)
         ~endpoint:(Evm_node.endpoint sequencer))
      sequencer
  in
  let* () =
    Evm_node.wait_for_blueprint_applied
      observer_partial_history
      (Int32.to_int level + 1)
  in
  (* Observer does not know block 0. *)
  let*@? (_error : Rpc.error) =
    Rpc.get_balance ~address ~block:(Number 0) observer_partial_history
  in
  let*@ balance_earliest =
    Rpc.get_balance ~address ~block:Earliest observer_partial_history
  in
  let*@ balance_now =
    Rpc.get_balance ~address ~block:Latest observer_partial_history
  in
  Check.((balance_earliest = Wei.of_eth_int 10) Wei.typ)
    ~error_msg:(sf "%s expected to have a balance of %%R but got %%L" address) ;
  Check.((balance_now = Wei.of_eth_int 20) Wei.typ)
    ~error_msg:(sf "%s expected to have a balance of %%R but got %%L" address) ;
  unit

let test_get_block_by_number_block_param =
  register_all
    ~block_storage_sqlite3:false (* The test uses init from rollup node. *)
    ~tags:["evm"; "sequencer"; "rpc"; "get_block_by_number"; "block_param"]
    ~title:"RPC method getBlockByNumber uses block parameter"
    ~time_between_blocks:Nothing
  @@ fun {sequencer; observer; sc_rollup_node; proxy; client; _} _protocols ->
  let observer_offset = 3l in
  let* () =
    repeat Int32.(to_int observer_offset) @@ fun () ->
    next_evm_level ~evm_node:sequencer ~sc_rollup_node ~client
  in
  let* () = bake_until_sync ~sc_rollup_node ~proxy ~sequencer ~client () in
  let* _ =
    repeat 2 (fun _ ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  let observer_partial_history =
    let name = "observer_partial_history" in
    Evm_node.create
      ~name
      ~mode:
        (Observer
           {
             initial_kernel = "evm_kernel.wasm";
             preimages_dir = "/tmp";
             private_rpc_port = None;
             rollup_node_endpoint = Sc_rollup_node.endpoint sc_rollup_node;
           })
      ~data_dir:(Temp.dir name)
      (Evm_node.endpoint sequencer)
  in
  let* () =
    Process.check @@ Evm_node.spawn_init_config observer_partial_history
  in
  let* () =
    Evm_node.init_from_rollup_node_data_dir
      observer_partial_history
      sc_rollup_node
  in
  let* () = Evm_node.run observer_partial_history in
  let* () =
    repeat 2 @@ fun () ->
    next_evm_level ~evm_node:sequencer ~sc_rollup_node ~client
  in

  let*@ earliest_block_sequencer =
    Rpc.get_block_by_number ~block:"earliest" sequencer
  in
  let*@ earliest_block_observer =
    Rpc.get_block_by_number ~block:"earliest" observer
  in
  let*@ earliest_block_observer_partial =
    Rpc.get_block_by_number ~block:"earliest" observer_partial_history
  in

  Check.(
    ((earliest_block_sequencer.number = 0l) int32)
      ~error_msg:"Earliest block of sequencer is %L instead of %R") ;

  Check.(
    ((earliest_block_observer.number = 0l) int32)
      ~error_msg:"Earliest block of observer is %L instead of %R") ;

  Check.(
    ((earliest_block_observer_partial.number = observer_offset) int32)
      ~error_msg:"Earliest block of observer started late is %L instead of %R") ;

  unit

let test_extended_block_param =
  register_all
    ~tags:["evm"; "sequencer"; "rpc"; "block_param"; "counter"]
    ~title:"Supports extended block parameter"
    ~time_between_blocks:Nothing
  @@ fun {sequencer; _} _protocols ->
  (*
     In this test we will deploy a counter contract, increments its counter
     at multiple consecutives blocks, and check the counter using block
     parameter.
     As the counter contract has a view, we can test the block parameter
     both for read only RPCs such as [eth_getStorageAt] and simulation
     such as [eth_call].
  *)
  let* counter_resolved = Solidity_contracts.counter () in
  let* () =
    Eth_cli.add_abi ~label:counter_resolved.label ~abi:counter_resolved.abi ()
  in
  (* Deploy the contract. *)
  let* contract, _tx_hash =
    send_transaction_to_sequencer
      (fun () ->
        Eth_cli.deploy
          ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
          ~endpoint:(Evm_node.endpoint sequencer)
          ~abi:counter_resolved.abi
          ~bin:counter_resolved.bin)
      sequencer
  in
  (* Takes the level where it was originated, the counter value will be 0. *)
  let*@ level = Rpc.block_number sequencer in
  let level = Int32.to_int level in
  (* Increments the counter multiple times. *)
  let* () =
    repeat 2 (fun _ ->
        let* _ =
          send_transaction_to_sequencer
            (Eth_cli.contract_send
               ~source_private_key:
                 Eth_account.bootstrap_accounts.(0).private_key
               ~endpoint:(Evm_node.endpoint sequencer)
               ~abi_label:counter_resolved.label
               ~address:contract
               ~method_call:"incrementCounter()")
            sequencer
        in
        unit)
  in

  let*@ block0 =
    Rpc.get_block_by_number ~block:(string_of_int level) sequencer
  in
  let*@ block1 =
    Rpc.get_block_by_number ~block:(string_of_int (level + 1)) sequencer
  in
  let*@ block2 =
    Rpc.get_block_by_number ~block:(string_of_int (level + 2)) sequencer
  in

  let check_counter_value ~(block : Block.t) expected_value =
    let counter_value ~block =
      let*@ s =
        Rpc.get_storage_at ~address:contract ~pos:"0x0" ~block sequencer
      in
      return (int_of_string s)
    in
    let* counter_via_number =
      counter_value
        ~block:
          (Block_number
             {number = Int32.to_int block.number; require_canonical = false})
    in
    let* counter_via_hash =
      counter_value
        ~block:(Block_hash {hash = block.hash; require_canonical = false})
    in
    let*@ counter_via_call =
      Rpc.(
        call
          ~block:(Number (Int32.to_int block.number))
          ~to_:contract
          ~data:"a87d942c"
          sequencer)
    in
    let counter_via_call = int_of_string counter_via_call in
    Check.((counter_via_number = expected_value) int)
      ~error_msg:"Expected counter to be %R but got %L, using {blockNumber}" ;
    Check.((counter_via_hash = expected_value) int)
      ~error_msg:"Expected counter to be %R but got %L, using {blockHash}" ;
    Check.((counter_via_call = expected_value) int)
      ~error_msg:"Expected counter to be %R but got %L, using eth_call" ;
    unit
  in

  let* () = check_counter_value ~block:block0 0 in
  let* () = check_counter_value ~block:block1 1 in
  let* () = check_counter_value ~block:block2 2 in

  unit

let test_observer_applies_blueprint_when_sequencer_restarted =
  register_all
    ~time_between_blocks:Nothing
    ~tags:["evm"; "observer"]
    ~title:"Can restart the sequencer node"
  @@ fun {sequencer; observer; _} _protocol ->
  (* We produce a block and check the observer applies it. *)
  let* _ = Evm_node.wait_for_blueprint_applied observer 1
  and* _ = produce_block sequencer in

  (* We stop the sequencer and wait for the observer to detect it and emit an
     event accordingly. *)
  let* () = Evm_node.terminate sequencer in
  let* () = Evm_node.wait_for_retrying_connect observer in

  (* We wait for a second event from the observer to witness that its first
     reconnection attempt failed. *)
  let* () = Evm_node.wait_for_retrying_connect observer in

  (* We restart the sequencer. *)
  let* () = Evm_node.run sequencer in

  (* We produce a block and check the observer applies it. *)
  let* _ = Evm_node.wait_for_blueprint_applied observer 2
  and* _ = produce_block sequencer in

  unit

let test_observer_forwards_transaction =
  let tags = ["evm"; "observer"; "transaction"] in
  let title = "Observer forwards transaction" in
  let tbb = 1. in
  register_all ~time_between_blocks:(Time_between_blocks tbb) ~tags ~title
  @@ fun {sequencer = sequencer_node; observer = observer_node; _} _protocol ->
  (* Ensure the sequencer has produced the block. *)
  let* () =
    Evm_node.wait_for_blueprint_applied ~timeout:10.0 sequencer_node 1
  in
  (* Ensure the observer node has a correctly initialized local state. *)
  let* () = Evm_node.wait_for_blueprint_applied ~timeout:50.0 observer_node 1 in

  let* txn =
    Eth_cli.transaction_send
      ~source_private_key:Eth_account.bootstrap_accounts.(1).private_key
      ~to_public_key:Eth_account.bootstrap_accounts.(2).address
      ~value:Wei.one
      ~endpoint:(Evm_node.endpoint observer_node)
      ()
  in

  let* receipt =
    Eth_cli.get_receipt ~endpoint:(Evm_node.endpoint sequencer_node) ~tx:txn
  in

  match receipt with
  | Some receipt when receipt.status -> unit
  | Some _ ->
      Test.fail
        "transaction receipt received from the sequencer, but transaction \
         failed"
  | None ->
      Test.fail
        "Missing receipt in the sequencer node for transaction successfully \
         injected in the observer"

let test_observer_timeout_when_necessary =
  register_all
    ~time_between_blocks:(Time_between_blocks 3.)
    ~tags:["evm"; "observer"; "timeout"]
    ~title:"Observer timeouts when blocks do not arrive quickly enough"
  @@ fun {sequencer; observer; _} _ ->
  (* The sequencer is initially setup to produce a block every 3s, which means
     the observer will expect blocks on this frequency.

     We restart the sequencer to produce block on demand. The observer will not
     request the new time between block, and will keep waiting for new blocks
     to arrive every 3s. *)
  let* () = Evm_node.terminate sequencer in
  let* () =
    Evm_node.run sequencer ~extra_arguments:["--time-between-blocks"; "none"]
  in

  (* After enough time, the observer considers its connection with the
     sequencer is stalled and tries to reconnect. *)
  let* () = Evm_node.wait_for_retrying_connect observer in

  (* We produce a block, and verify that the observer nodes correctly applies
     it. *)
  let* _ = produce_block sequencer in
  let* () = Evm_node.wait_for_blueprint_applied observer 1 in

  unit

let test_sequencer_is_reimbursed =
  let tbb = 1. in
  (* We use an arbitrary address for the pool address, the goal is just to
     verify its balance increases. *)
  let sequencer_pool_address = "0xb7a97043983f24991398e5a82f63f4c58a417185" in
  register_all
    ~da_fee:Wei.one
    ~time_between_blocks:(Time_between_blocks tbb)
    ~sequencer_pool_address
    ~tags:["evm"; "sequencer"; "transaction"]
    ~title:"Sequencer is reimbursed for DA fees"
  @@ fun {sequencer = sequencer_node; _} _protocol ->
  let* balance =
    Eth_cli.balance
      ~account:sequencer_pool_address
      ~endpoint:Evm_node.(endpoint sequencer_node)
  in

  Check.((Wei.zero = balance) Wei.typ)
    ~error_msg:"Balance of the sequencer address pool should be null" ;

  (* Ensure the sequencer has produced the block. *)
  let* () =
    Evm_node.wait_for_blueprint_applied ~timeout:10.0 sequencer_node 1
  in

  let* txn =
    Eth_cli.transaction_send
      ~source_private_key:Eth_account.bootstrap_accounts.(1).private_key
      ~to_public_key:Eth_account.bootstrap_accounts.(2).address
      ~value:Wei.one
      ~endpoint:(Evm_node.endpoint sequencer_node)
      ()
  in

  let* receipt =
    Eth_cli.get_receipt ~endpoint:(Evm_node.endpoint sequencer_node) ~tx:txn
  in

  match receipt with
  | Some receipt when receipt.status ->
      let* balance =
        Eth_cli.balance
          ~account:sequencer_pool_address
          ~endpoint:Evm_node.(endpoint sequencer_node)
      in

      Check.((Wei.zero < balance) Wei.typ)
        ~error_msg:"Balance of the sequencer address pool should not be null" ;
      unit
  | Some _ ->
      Test.fail
        "transaction receipt received from the sequencer, but transaction \
         failed"
  | None ->
      Test.fail
        "Missing receipt in the sequencer node for transaction successfully \
         injected in the observer"

(* TODO: https://gitlab.com/tezos/tezos/-/issues/7215
   This test passes for the threshold encryption sequencer when
   we produce one block or three blocks before upgrading the kernel,
   but not two. This needs to be investigated. *)

(** This tests the situation where the kernel has an upgrade and the
    sequencer upgrade by following the event of the kernel. *)
let test_self_upgrade_kernel =
  (* Add a delay between first block and activation timestamp. *)
  let genesis_timestamp =
    Client.(At (Time.of_notation_exn "2020-01-01T00:00:00Z"))
  in
  let activation_timestamp = "2020-01-01T00:00:10Z" in
  register_all
    ~kernels:[Latest]
    ~genesis_timestamp
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "upgrade"; "self"]
    ~title:"EVM Kernel can upgrade to itself"
  @@ fun {
           sc_rollup_node;
           l1_contracts;
           sc_rollup_address;
           client;
           sequencer;
           proxy;
           observer;
           kernel;
           _;
         }
             _protocol ->
  (* Sends the upgrade to L1, but not to the sequencer. *)
  let* _root_hash =
    upgrade
      ~sc_rollup_node
      ~sc_rollup_address
      ~admin:Constant.bootstrap2.public_key_hash
      ~admin_contract:l1_contracts.admin
      ~client
      ~upgrade_to:kernel
      ~activation_timestamp
  in

  (* Per the activation timestamp, the state will remain synchronised until
     the kernel is upgraded. *)
  let* _ =
    repeat 2 (fun () ->
        let* _ = produce_block ~timestamp:"2020-01-01T00:00:05Z" sequencer in
        unit)
  in

  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer ~proxy ()
  and* _upgrade_info = Evm_node.wait_for_pending_upgrade sequencer in

  let* () =
    check_head_consistency
      ~left:sequencer
      ~right:proxy
      ~error_msg:"The head should be the same before the upgrade"
      ()
  in

  (* Produce a block after activation timestamp, both the rollup
     node and the sequencer will upgrade to itself. *)
  let* _ =
    repeat 2 (fun () ->
        let* _ = produce_block ~timestamp:"2020-01-01T00:00:15Z" sequencer in
        unit)
  and* _ = Evm_node.wait_for_successful_upgrade sequencer
  and* _ = Evm_node.wait_for_successful_upgrade observer in
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer ~proxy () in

  let* () =
    check_head_consistency
      ~left:sequencer
      ~right:proxy
      ~error_msg:"The head should be the same after the upgrade"
      ()
  in

  unit

let test_empty_block_on_upgrade =
  register_all
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "upgrade"; "empty"]
    ~title:"Sequencer produces an empty block in case of upgrade."
  @@ fun {
           client;
           l1_contracts;
           sc_rollup_node;
           sc_rollup_address;
           sequencer;
           kernel;
           _;
         }
             _protocol ->
  (* Send a deposit so the sequencer will in theory add it to its next block. *)
  let* () =
    send_deposit_to_delayed_inbox
      ~amount:(Tez.of_int 1)
      ~l1_contracts
      ~depositor:Constant.bootstrap5
      ~receiver:"0x1074Fd1EC02cbeaa5A90450505cF3B48D834f3EB"
      ~sc_rollup_node
      ~sc_rollup_address
      client
  in
  (* Send an upgrade. *)
  let* _root_hash =
    upgrade
      ~sc_rollup_node
      ~sc_rollup_address
      ~admin:Constant.bootstrap2.public_key_hash
      ~admin_contract:l1_contracts.admin
      ~client
      ~upgrade_to:kernel
      ~activation_timestamp:"2026-01-01T00:00:00Z"
  in

  (* Bake a few blocks to make sure the sequencer sees the deposit and the
     upgrade. *)
  let* () =
    repeat 3 (fun _ ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in

  let check_transactions_in_block expected_number_of_transactions =
    let*@ block = Rpc.get_block_by_number ~block:"latest" sequencer in
    let length_block =
      match block.transactions with
      | Empty -> 0
      | Hash l -> List.length l
      | Full l -> List.length l
    in
    Check.((expected_number_of_transactions = length_block) int)
      ~error_msg:"Expected %L transactions got %R transactions" ;
    unit
  in

  (* Producing the block will create an empty block. As the kernel
     is going to upgrade, the sequencer will not pick the transaction. *)
  let*@ _ = produce_block ~timestamp:"2026-02-02T00:00:00Z" sequencer in
  let* () = check_transactions_in_block 0 in
  (* Next block will have the deposit. *)
  let*@ _ = produce_block ~timestamp:"2026-02-02T00:00:00Z" sequencer in
  let* () = check_transactions_in_block 1 in

  unit

(** This tests the situation where the kernel has an upgrade and the
    sequencer upgrade by following the event of the kernel. *)
let test_upgrade_kernel_auto_sync =
  (* Add a delay between first block and activation timestamp. *)
  let genesis_timestamp =
    Client.(At (Time.of_notation_exn "2020-01-01T00:00:00Z"))
  in
  let activation_timestamp = "2020-01-01T00:00:10Z" in
  register_upgrade_all
    ~kernels:[Mainnet; Ghostnet]
    ~genesis_timestamp
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "upgrade"; "auto"; "sync"]
    ~title:"Rollup-node kernel upgrade is applied to the sequencer state."
  @@ fun from
             to_
             {
               sc_rollup_node;
               l1_contracts;
               sc_rollup_address;
               client;
               sequencer;
               proxy;
               observer;
               _;
             }
             _protocol ->
  let* () =
    match Kernel.commit_of from with
    | Some from_commit ->
        let* _ =
          check_kernel_version ~evm_node:sequencer ~equal:true from_commit
        in
        unit
    | None -> unit
  in

  (* Kill the observer to demonstrate the sequencer propagates the upgrade on
     replay. *)
  let* () = Evm_node.terminate observer in

  (* Sends the upgrade to L1, but not to the sequencer. *)
  let _, to_use = Kernel.to_uses_and_tags to_ in
  let* _root_hash =
    upgrade
      ~sc_rollup_node
      ~sc_rollup_address
      ~admin:Constant.bootstrap2.public_key_hash
      ~admin_contract:l1_contracts.admin
      ~client
      ~upgrade_to:to_use
      ~activation_timestamp
  in

  (* Per the activation timestamp, the state will remain synchronised until
     the kernel is upgraded. *)
  let* _ =
    repeat 2 (fun () ->
        let*@ _ = produce_block ~timestamp:"2020-01-01T00:00:05Z" sequencer in
        unit)
  in
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer ~proxy () in

  let* () =
    check_head_consistency
      ~left:sequencer
      ~right:proxy
      ~error_msg:"The head should be the same before the upgrade"
      ()
  in

  (* Produce a block after activation timestamp, both the rollup
     node and the sequencer will upgrade to debug kernel and
     therefore not produce the block. *)
  let* _ =
    repeat 2 (fun () ->
        let*@ _ = produce_block ~timestamp:"2020-01-01T00:00:15Z" sequencer in
        unit)
  and* _upgrade = Evm_node.wait_for_successful_upgrade sequencer in

  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer ~proxy () in

  let* () =
    check_head_consistency
      ~left:sequencer
      ~right:proxy
      ~error_msg:"The head should be the same after the upgrade"
      ()
  in

  let* () =
    match Kernel.commit_of to_ with
    | Some to_commit ->
        let* _ =
          check_kernel_version ~evm_node:sequencer ~equal:true to_commit
        in
        unit
    | None -> unit
  in

  (* Start the observer again and wait for a successful upgrade *)
  let* () = Evm_node.run observer in
  let* _upgrade = Evm_node.wait_for_successful_upgrade observer in

  let* () = Evm_node.wait_for_blueprint_applied observer 4 in

  let* () =
    check_head_consistency
      ~left:sequencer
      ~right:observer
      ~error_msg:"The head should be the same after the upgrade"
      ()
  in

  unit

let test_legacy_deposits_dispatched_after_kernel_upgrade =
  let genesis_timestamp =
    Client.(At (Time.of_notation_exn "2020-01-01T00:00:00Z"))
  in
  let activation_timestamp = "2020-01-01T00:00:01Z" in
  register_upgrade_all
    ~kernels:[Mainnet; Ghostnet]
    ~upgrade_to:(fun _ -> Latest)
    ~genesis_timestamp
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "upgrade"; "deposit"; "legacy"; "delayed"]
    ~title:
      "After kernel upgrade a legacy deposit from delayed inbox can be decoded \
       and processed."
  @@ fun from
             to_
             {
               sc_rollup_node;
               l1_contracts;
               sc_rollup_address;
               client;
               sequencer;
               proxy;
               _;
             }
             _protocol ->
  let* () =
    match Kernel.commit_of from with
    | Some from_commit ->
        let* _ =
          check_kernel_version ~evm_node:sequencer ~equal:true from_commit
        in
        unit
    | None -> unit
  in

  let* receiver_balance_prev =
    Eth_cli.balance
      ~account:Eth_account.bootstrap_accounts.(1).address
      ~endpoint:(Evm_node.endpoint sequencer)
  in

  let* () =
    send_deposit_to_delayed_inbox
      ~amount:Tez.(of_int 16)
      ~l1_contracts
      ~depositor:Constant.bootstrap5
      ~receiver:Eth_account.bootstrap_accounts.(1).address
      ~sc_rollup_node
      ~sc_rollup_address
      client
  in

  let _, to_use = Kernel.to_uses_and_tags to_ in
  let* _root_hash =
    upgrade
      ~sc_rollup_node
      ~sc_rollup_address
      ~admin:Constant.bootstrap2.public_key_hash
      ~admin_contract:l1_contracts.admin
      ~client
      ~upgrade_to:to_use
      ~activation_timestamp
  in

  (* The sequencer follows finalized levels of the octez-node,
     so we need to have 2 tezos levels before the sequencer sees the upgrade *)
  let* _ =
    repeat 2 (fun () ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in

  (* Produce a block where the upgrade would happen *)
  let*@ _ = produce_block ~timestamp:"2020-01-01T00:00:10Z" sequencer in
  let* () = bake_until_sync ~sc_rollup_node ~proxy ~sequencer ~client () in

  (* Ensure the kernel is upgraded *)
  let* () =
    match Kernel.commit_of from with
    | Some from_commit ->
        let* _ =
          check_kernel_version ~evm_node:sequencer ~equal:false from_commit
        in
        unit
    | None -> unit
  in

  (* Produce a block where deposit would be handled *)
  let*@ _ = produce_block ~timestamp:"2020-01-01T00:00:20Z" sequencer in
  let* () = bake_until_sync ~sc_rollup_node ~proxy ~sequencer ~client () in

  (* Deposit must be applied by now *)
  let* receiver_balance_next =
    Eth_cli.balance
      ~account:Eth_account.bootstrap_accounts.(1).address
      ~endpoint:(Evm_node.endpoint sequencer)
  in
  Check.((receiver_balance_next > receiver_balance_prev) Wei.typ)
    ~error_msg:"Expected a bigger balance" ;

  (* Ensure delayed inbox is empty now *)
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in

  unit

let test_delayed_transfer_timeout =
  register_all
    ~delayed_inbox_timeout:3
    ~delayed_inbox_min_levels:1
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "timeout"]
    ~title:"Delayed transaction timeout"
    ~use_dal:ci_enabled_dal_registration
  @@ fun {
           client;
           node = _;
           l1_contracts;
           sc_rollup_address;
           sc_rollup_node;
           sequencer;
           proxy;
           _;
         }
             _protocol ->
  (* Kill the sequencer *)
  let* () = Evm_node.terminate sequencer in
  let endpoint = Evm_node.endpoint proxy in
  let* _ = next_rollup_node_level ~sc_rollup_node ~client in
  let sender = Eth_account.bootstrap_accounts.(0).address in
  let _ = Rpc.block_number proxy in
  let receiver = Eth_account.bootstrap_accounts.(1).address in
  let* sender_balance_prev = Eth_cli.balance ~account:sender ~endpoint in
  let* receiver_balance_prev = Eth_cli.balance ~account:receiver ~endpoint in
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
  let* _hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      raw_transfer
  in
  (* Bake a few blocks, should be enough for the tx to time out and be
     forced *)
  let* _ =
    repeat 5 (fun () ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  let* sender_balance_next = Eth_cli.balance ~account:sender ~endpoint in
  let* receiver_balance_next = Eth_cli.balance ~account:receiver ~endpoint in
  Check.((sender_balance_prev <> sender_balance_next) Wei.typ)
    ~error_msg:"Balance should be updated" ;
  Check.((receiver_balance_prev <> receiver_balance_next) Wei.typ)
    ~error_msg:"Balance should be updated" ;
  Check.((sender_balance_prev > sender_balance_next) Wei.typ)
    ~error_msg:"Expected a smaller balance" ;
  Check.((receiver_balance_next > receiver_balance_prev) Wei.typ)
    ~error_msg:"Expected a bigger balance" ;
  unit

let test_forced_blueprint_takes_pred_timestamp =
  register_all
    ~kernels:[Kernel.Latest]
    ~time_between_blocks:Nothing
    ~genesis_timestamp:Client.(At (Time.of_notation_exn "2020-01-01T00:00:00Z"))
    ~delayed_inbox_timeout:1
    ~delayed_inbox_min_levels:1
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "timeout"]
    ~title:"Forced blueprint can take predecessor timestamp"
  @@ fun {
           client;
           l1_contracts;
           sc_rollup_address;
           sc_rollup_node;
           sequencer;
           proxy;
           _;
         }
             _protocol ->
  (* The head timestamp will be high enough that we don't use the L1 timestamp
     for the forced blueprint and just take the same timestamp. *)
  let*@ (_ : int) = produce_block ~timestamp:"2020-01-01T00:04:00Z" sequencer in
  let* () = bake_until_sync ~sc_rollup_node ~client ~proxy ~sequencer () in
  (* Make a delayed transaction and force it by creating L1 blocks. *)
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
  let* _ = next_rollup_node_level ~sc_rollup_node ~client in

  let*@ proxy_head = Rpc.get_block_by_number ~block:"latest" proxy in
  Check.(
    (Tezos_base.Time.Protocol.to_notation proxy_head.timestamp
    = "2020-01-01T00:04:00Z")
      string)
    ~error_msg:
      "The forced blueprint should have the same timestamp as its predecessor" ;
  let* l1_timestamp = l1_timestamp client in
  Check.(
    (Tezos_base.Time.Protocol.to_seconds proxy_head.timestamp
    > Tezos_base.Time.Protocol.to_seconds l1_timestamp)
      int64)
    ~error_msg:"The proxy should have taken a timestamp greater than L1 one" ;
  unit

let test_forced_blueprint_takes_l1_timestamp =
  register_all
    ~kernels:[Kernel.Latest]
    ~time_between_blocks:Nothing
    ~genesis_timestamp:Client.(At (Time.of_notation_exn "2020-01-01T00:00:00Z"))
    ~delayed_inbox_timeout:1
    ~delayed_inbox_min_levels:1
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "timeout"]
    ~title:"Forced blueprint can take l1 timestamp"
  @@ fun {
           client;
           l1_contracts;
           sc_rollup_address;
           sc_rollup_node;
           sequencer;
           proxy;
           _;
         }
             _protocol ->
  let*@ (_ : int) = produce_block ~timestamp:"2020-01-01T00:00:00Z" sequencer in
  let* () = bake_until_sync ~sc_rollup_node ~client ~proxy ~sequencer () in
  (* Make a delayed transaction and force it by creating L1 blocks. *)
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
  let* l1_timestamp = l1_timestamp client in
  let* _ = next_rollup_node_level ~sc_rollup_node ~client in

  let*@ proxy_head = Rpc.get_block_by_number ~block:"latest" proxy in
  (* The forced block will have a timestamp of l1_timestamp. *)
  Check.(
    (Tezos_base.Time.Protocol.to_notation proxy_head.timestamp
    = Tezos_base.Time.Protocol.to_notation l1_timestamp)
      string)
    ~error_msg:
      "Forced blueprint should have timestamp of L1. (proxy has %L, l1 has %R)" ;
  unit

let test_delayed_transfer_timeout_fails_l1_levels =
  register_all
    ~delayed_inbox_timeout:3
    ~delayed_inbox_min_levels:20
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "timeout"; "min_levels"]
    ~title:"Delayed transaction timeout considers l1 level"
    ~use_dal:ci_enabled_dal_registration
  @@ fun {
           client;
           node = _;
           l1_contracts;
           sc_rollup_address;
           sc_rollup_node;
           sequencer;
           proxy;
           _;
         }
             _protocol ->
  (* Kill the sequencer *)
  let* () = Evm_node.terminate sequencer in
  let endpoint = Evm_node.endpoint proxy in
  let* _ = next_rollup_node_level ~sc_rollup_node ~client in
  let sender = Eth_account.bootstrap_accounts.(0).address in
  let _ = Rpc.block_number proxy in
  let receiver = Eth_account.bootstrap_accounts.(1).address in
  let* sender_balance_prev = Eth_cli.balance ~account:sender ~endpoint in
  let* receiver_balance_prev = Eth_cli.balance ~account:receiver ~endpoint in
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
  let* _hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      raw_transfer
  in
  (* Bake a few blocks, should be enough for the tx to time out in terms
     of wall time, but not in terms of L1 levels.
     Note that this test is almost the same as the one where the tx
     times out, only difference being the value of [delayed_inbox_min_levels].
  *)
  let* _ =
    repeat 5 (fun () ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  let* sender_balance_next = Eth_cli.balance ~account:sender ~endpoint in
  let* receiver_balance_next = Eth_cli.balance ~account:receiver ~endpoint in
  Check.((sender_balance_prev = sender_balance_next) Wei.typ)
    ~error_msg:"Sender balance should be the same (prev %L = next %R)" ;
  Check.((receiver_balance_prev = receiver_balance_next) Wei.typ)
    ~error_msg:"Receiver balance should be the same (prev %L = next %R)" ;
  (* Wait until it's forced *)
  let* _ =
    repeat 15 (fun () ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  let* sender_balance_next = Eth_cli.balance ~account:sender ~endpoint in
  let* receiver_balance_next = Eth_cli.balance ~account:receiver ~endpoint in
  Check.((sender_balance_prev > sender_balance_next) Wei.typ)
    ~error_msg:"Expected a smaller sender balance (prev %L > next %R)" ;
  Check.((receiver_balance_next > receiver_balance_prev) Wei.typ)
    ~error_msg:"Expected a bigger receiver balance (next %L > prev %R)" ;
  unit

(** This tests the situation where force kernel upgrade happens too soon. *)
let test_force_kernel_upgrade_too_early =
  let genesis_timestamp =
    Client.(At (Time.of_notation_exn "2020-01-10T00:00:00Z"))
  in
  register_upgrade_all
    ~kernels:[Latest]
    ~upgrade_to:(fun _ -> Ghostnet)
    ~genesis_timestamp
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "upgrade"; "force"; Tag.ci_disabled]
    ~title:"Force kernel upgrade fail too early"
  @@ fun _from
             to_
             {
               sc_rollup_node;
               l1_contracts;
               sc_rollup_address;
               client;
               sequencer;
               proxy;
               _;
             }
             _protocol ->
  (* Wait for the sequencer to publish its genesis block. *)
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer ~proxy () in
  let* proxy =
    Evm_node.init ~mode:Proxy (Sc_rollup_node.endpoint sc_rollup_node)
  in

  (* Assert the kernel version is the same at start up. *)
  let*@ sequencer_kernelVersion = Rpc.tez_kernelVersion sequencer in
  let*@ proxy_kernelVersion = Rpc.tez_kernelVersion proxy in
  Check.((sequencer_kernelVersion = proxy_kernelVersion) string)
    ~error_msg:"Kernel versions should be the same at start up" ;

  (* Activation timestamp is 1 day after the genesis. Therefore, it cannot
     be forced now. *)
  let activation_timestamp = "2020-01-11T00:00:00Z" in
  (* Sends the upgrade to L1 and sequencer. *)
  let _, to_use = Kernel.to_uses_and_tags to_ in
  let* _root_hash =
    upgrade
      ~sc_rollup_node
      ~sc_rollup_address
      ~admin:Constant.bootstrap2.public_key_hash
      ~admin_contract:l1_contracts.admin
      ~client
      ~upgrade_to:to_use
      ~activation_timestamp
  in

  (* Now we try force the kernel upgrade via an external message. *)
  let* () = force_kernel_upgrade ~sc_rollup_address ~sc_rollup_node ~client in

  (* Assert the kernel version are still the same. *)
  let*@ sequencer_kernelVersion = Rpc.tez_kernelVersion sequencer in
  let*@ new_proxy_kernelVersion = Rpc.tez_kernelVersion proxy in
  Check.((sequencer_kernelVersion = new_proxy_kernelVersion) string)
    ~error_msg:"The force kernel ugprade should have failed" ;
  unit

(** This tests the situation where the kernel does not produce blocks but
    still can be forced to upgrade via an external message. *)
let test_force_kernel_upgrade =
  let genesis_timestamp =
    Client.(At (Time.of_notation_exn "2020-01-10T00:00:00Z"))
  in
  register_upgrade_all
    ~kernels:[Latest]
    ~upgrade_to:(fun _ -> Ghostnet)
    ~tags:["evm"; "sequencer"; "upgrade"; "force"; Tag.ci_disabled]
    ~genesis_timestamp
    ~title:"Force kernel upgrade"
  @@ fun _from
             to_
             {
               sc_rollup_node;
               l1_contracts;
               sc_rollup_address;
               client;
               sequencer;
               proxy;
               _;
             }
             _protocol ->
  (* Wait for the sequencer to publish its genesis block. *)
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer ~proxy () in
  let* proxy =
    Evm_node.init ~mode:Proxy (Sc_rollup_node.endpoint sc_rollup_node)
  in

  (* Assert the kernel version is the same at start up. *)
  let*@ sequencer_kernelVersion = Rpc.tez_kernelVersion sequencer in
  let*@ proxy_kernelVersion = Rpc.tez_kernelVersion proxy in
  Check.((sequencer_kernelVersion = proxy_kernelVersion) string)
    ~error_msg:"Kernel versions should be the same at start up" ;

  (* Activation timestamp is 1 day before the genesis. Therefore, it can
     be forced immediatly. *)
  let activation_timestamp = "2020-01-09T00:00:00Z" in
  (* Sends the upgrade to L1 and sequencer. *)
  let _, to_use = Kernel.to_uses_and_tags to_ in
  let* _root_hash =
    upgrade
      ~sc_rollup_node
      ~sc_rollup_address
      ~admin:Constant.bootstrap2.public_key_hash
      ~admin_contract:l1_contracts.admin
      ~client
      ~upgrade_to:to_use
      ~activation_timestamp
  in

  (* We bake a few blocks. As the sequencer is not producing anything, the
     kernel will not upgrade. *)
  let* () =
    repeat 5 (fun () ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  (* Assert the kernel version is the same, it proves the upgrade did not
      happen. *)
  let*@ sequencer_kernelVersion = Rpc.tez_kernelVersion sequencer in
  let*@ proxy_kernelVersion = Rpc.tez_kernelVersion proxy in
  Check.((sequencer_kernelVersion = proxy_kernelVersion) string)
    ~error_msg:"Kernel versions should be the same even after the message" ;

  (* Now we force the kernel upgrade via an external message. They will
     become unsynchronised. *)
  let* () = force_kernel_upgrade ~sc_rollup_address ~sc_rollup_node ~client in

  (* Assert the kernel version are now different, it shows that only the rollup
     node upgraded. *)
  let*@ sequencer_kernelVersion = Rpc.tez_kernelVersion sequencer in
  let*@ new_proxy_kernelVersion = Rpc.tez_kernelVersion proxy in
  Check.((sequencer_kernelVersion <> new_proxy_kernelVersion) string)
    ~error_msg:"Kernel versions should be different after forced upgrade" ;
  Check.((sequencer_kernelVersion = proxy_kernelVersion) string)
    ~error_msg:"Sequencer should be on the previous version" ;
  unit

let test_external_transaction_to_delayed_inbox_fails =
  (* We have a da_fee set to zero here. This is because the proxy will perform
     validation on the tx before adding it the transaction pool. This will fail
     due to 'gas limit too low' if the da fee is set.

     Since we want to test what happens when the tx is actually submitted, we
     bypass the da fee check here. *)
  register_all
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
    ~time_between_blocks:Nothing
    ~bootstrap_accounts:Eth_account.lots_of_address
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "external"]
    ~title:"Sending an external transaction to the delayed inbox fails"
  @@ fun {client; sequencer; proxy; sc_rollup_node; _} _protocol ->
  let* () = Evm_node.wait_for_blueprint_injected ~timeout:5. sequencer 0 in
  (* Bake a couple more levels for the blueprint to be final *)
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer ~proxy () in
  let raw_tx, _ = read_tx_from_file () |> List.hd in
  let*@ tx_hash = Rpc.send_raw_transaction ~raw_tx proxy in
  (* Bake enough levels to make sure the transaction would be processed
     if added *)
  let* () =
    repeat 10 (fun () ->
        let*@ _ = produce_block sequencer in
        let* _ = next_rollup_node_level ~client ~sc_rollup_node in
        unit)
  in
  (* Response should be none *)
  let*@ response = Rpc.get_transaction_receipt ~tx_hash proxy in
  assert (Option.is_none response) ;
  let*@ response = Rpc.get_transaction_receipt ~tx_hash sequencer in
  assert (Option.is_none response) ;
  unit

let test_proxy_node_can_forward_to_evm_endpoint =
  register_all
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
    ~time_between_blocks:Nothing
    ~bootstrap_accounts:Eth_account.lots_of_address
    ~tags:["proxy"; "evm_node_endpoint"]
    ~title:"Proxy node can forward transactions to another EVM node"
  @@ fun {sequencer; proxy; sc_rollup_node; client; _} _protocol ->
  let* _ = next_rollup_node_level ~sc_rollup_node ~client in
  (* We restart the proxy node with a companion EVM node (the sequencer, in
     this case). It is expected that, in this configuration, it will forward
     its transactions to the sequencer, which means said transactions will be
     included in blocks. *)
  let* () = Evm_node.terminate proxy in
  let* () =
    Evm_node.run
      ~extra_arguments:["--evm-node-endpoint"; Evm_node.endpoint sequencer]
      proxy
  in
  let raw_tx, _ = read_tx_from_file () |> List.hd in

  (* We send a transaction through the proxy. *)
  let*@ tx_hash = Rpc.send_raw_transaction ~raw_tx proxy in

  (* We bake a few blocks using the sequencer. *)
  let* () =
    repeat 2 @@ fun () ->
    let*@ _ = produce_block sequencer in
    unit
  in

  (* We verify the transaction sent to the proxy is known to the sequencer. *)
  let*@ response_seq = Rpc.get_transaction_receipt ~tx_hash sequencer in
  assert (Option.is_some response_seq) ;

  (* We bake enough L1 blocks so that the proxy node becomes aware of all
     blocks created by the sequencer. *)
  let* () = bake_until_sync ~sc_rollup_node ~proxy ~sequencer ~client () in

  (* And now we check that the proxy is also aware that the transaction has
     been included in a block. *)
  let*@ response_proxy = Rpc.get_transaction_receipt ~tx_hash sequencer in
  assert (Option.is_some response_proxy) ;
  unit

let test_delayed_inbox_flushing =
  (* Setup with a short wall time timeout but a significant lower bound of
     L1 levels needed for timeout.
     The idea is to send 2 transactions to the delayed inbox, having one
     time out and check that the second is also forced.
     We set [delayed_inbox_min_levels] to a value that is large enough
     to give us time to send the second one while the first one is not
     timed out yet.
  *)
  register_all
    ~delayed_inbox_timeout:1
    ~delayed_inbox_min_levels:20
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "timeout"]
    ~title:"Delayed inbox flushing"
  @@ fun {
           client;
           node = _;
           l1_contracts;
           sc_rollup_address;
           sc_rollup_node;
           sequencer;
           proxy;
           _;
         }
             _protocol ->
  (* Kill the sequencer *)
  let* () = Evm_node.terminate sequencer in
  let endpoint = Evm_node.endpoint proxy in
  let* _ = next_rollup_node_level ~sc_rollup_node ~client in
  let sender = Eth_account.bootstrap_accounts.(0).address in
  let _ = Rpc.block_number proxy in
  let receiver = Eth_account.bootstrap_accounts.(1).address in
  let* sender_balance_prev = Eth_cli.balance ~account:sender ~endpoint in
  let* receiver_balance_prev = Eth_cli.balance ~account:receiver ~endpoint in
  (* Thix crafted tx comes from [100-inputs-for-proxy] (the first one),
     but the signer is not the same so the tx is different on the signature *)
  let* tx1 =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:21_000
      ~gas:21_000
      ~value:(Wei.of_string "10")
      ~address:"0x0000000000000000000000000000000000000000"
      ()
  in
  let* _hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      tx1
  in
  (* Bake a few blocks but not enough for the first tx to be forced! *)
  let* _ =
    repeat 10 (fun () ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  (* Send the second transaction, a transfer from
     Eth_account.bootstrap_accounts.(0) to Eth_account.bootstrap_accounts.(1).
  *)
  let* tx2 =
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
      tx2
  in
  (* Bake a few more blocks to make sure the first tx times out, but not
     the second one. However, the latter should also be included. *)
  let* _ =
    repeat 10 (fun () ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  let* sender_balance_next = Eth_cli.balance ~account:sender ~endpoint in
  let* receiver_balance_next = Eth_cli.balance ~account:receiver ~endpoint in
  Check.((sender_balance_prev <> sender_balance_next) Wei.typ)
    ~error_msg:"Balance should be updated" ;
  Check.((receiver_balance_prev <> receiver_balance_next) Wei.typ)
    ~error_msg:"Balance should be updated" ;
  Check.((sender_balance_prev > sender_balance_next) Wei.typ)
    ~error_msg:"Expected a smaller balance" ;
  Check.((receiver_balance_next > receiver_balance_prev) Wei.typ)
    ~error_msg:"Expected a bigger balance" ;
  unit

let test_no_automatic_block_production =
  register_all
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "block"]
    ~title:"No automatic block production"
  @@ fun {sequencer; _} _protocol ->
  let*@ before_head = Rpc.get_block_by_number ~block:"latest" sequencer in
  let transfer =
    let* tx_hash =
      Eth_cli.transaction_send
        ~source_private_key:Eth_account.(bootstrap_accounts.(0).private_key)
        ~to_public_key:Eth_account.(bootstrap_accounts.(0).address)
        ~value:(Wei.of_eth_int 1)
        ~endpoint:(Evm_node.endpoint sequencer)
        ()
    in
    return (Some tx_hash)
  in
  let timeout =
    let* () = Lwt_unix.sleep 15. in
    return None
  in
  let* tx_hash = Lwt.pick [transfer; timeout] in

  let*@ after_head = Rpc.get_block_by_number ~block:"latest" sequencer in
  (* As the time between blocks is "none", the sequencer should not produce a block
     even if we send a transaction. *)
  Check.((before_head.number = after_head.number) int32)
    ~error_msg:"No block production expected" ;
  (* The transaction hash is not returned as no receipt is produced, and eth-cli
     awaits for the receipt. *)
  Check.is_true
    (Option.is_none tx_hash)
    ~error_msg:"No transaction hash expected" ;
  unit

let test_non_increasing_timestamp =
  register_all
    ~genesis_timestamp:Client.(At (Time.of_notation_exn "2020-01-01T00:00:00Z"))
    ~kernels:[Kernel.Latest]
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "block"; "timestamp"]
    ~title:"Non increasing timestamp are forbidden"
  @@ fun {sequencer; _} _protocol ->
  let*@ (_ : int) = produce_block ~timestamp:"2020-01-01T00:00:05Z" sequencer in
  (* This produce block will fail as the timestamp is before the previous block. *)
  let* () =
    match Evm_node.mode sequencer with
    | Sequencer _ ->
        let*@? _err =
          Rpc.produce_block ~timestamp:"2020-01-01T00:00:00Z" sequencer
        in
        unit
    | Threshold_encryption_sequencer _ ->
        let wait_for_invalid = Evm_node.wait_for_blueprint_invalid sequencer in
        let* _ntx =
          Rpc.produce_proposal ~timestamp:"2020-01-01T00:00:00Z" sequencer
        and* () = wait_for_invalid in
        unit
    | _ -> assert false (* impossible case as it's a sequencer. *)
  in
  (* However the same timestamp is accepted. *)
  let*@ (_ : int) = produce_block ~timestamp:"2020-01-01T00:00:05Z" sequencer in
  unit

let test_timestamp_from_the_future =
  register_all
    ~max_blueprint_lookahead_in_seconds:300L
    ~genesis_timestamp:Client.(At (Time.of_notation_exn "2020-01-01T00:00:00Z"))
    ~kernels:[Kernel.Latest]
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "block"; "timestamp"]
    ~title:"Timestamp from the future are refused"
    ~use_dal:ci_enabled_dal_registration
  @@ fun {sequencer; proxy; sc_rollup_node; client; enable_dal; _} _protocol ->
  (* In this test the time between blocks is 1 second. *)

  (* Producing a block 4:50 minutes after the L1 timestamp will be accepted. We
     do not check precisely 4:59 minutes to avoid flakiness w.r.t to blueprint
     inclusion. *)
  let* current_l1_timestamp = l1_timestamp client in
  let accepted_timestamp =
    Tezos_base.Time.Protocol.(add current_l1_timestamp 270L |> to_notation)
  in
  (* The sequencer will accept it anyway, but we need to check that the rollup
     node accepts it. *)
  let*@ (_ : int) = produce_block ~timestamp:accepted_timestamp sequencer in
  let* () = bake_until_sync ~sc_rollup_node ~client ~proxy ~sequencer () in

  (* Producing a block 5:30 minutes after the L1 timetamp will be accepted by
     the sequencer and not the rollup node. *)
  let* current_l1_timestamp = l1_timestamp client in
  let refused_timestamp =
    Tezos_base.Time.Protocol.(add current_l1_timestamp 330L |> to_notation)
  in
  let*@ (_ : int) = produce_block ~timestamp:refused_timestamp sequencer in
  (* We wait more in case of DAL because 5 blocks are not enough to
     send the blueprint through the DAL. *)
  let number_of_blocks_to_wait = if enable_dal then 20 else 5 in
  let* _ =
    repeat number_of_blocks_to_wait (fun () ->
        let* _l1_lvl = next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in

  let*@ proxy_head = Rpc.block_number proxy in
  let*@ sequencer_head = Rpc.block_number sequencer in

  Check.((sequencer_head = Int32.succ proxy_head) int32)
    ~error_msg:"The proxy was supposed to refuse the block" ;

  let kernel_log =
    let path = Sc_rollup_node.data_dir sc_rollup_node // "kernel.log" in
    read_file path
  in

  Check.(
    kernel_log
    =~ rex
         "Deleting invalid blueprint at path /evm/blueprints/2, error: \
          TimestampFromFuture")
    ~error_msg:"The blueprint should have been refused by TimestampFromFuture" ;

  unit

(** This tests the situation where the kernel has an upgrade and the
    sequencer upgrade by following the event of the kernel. *)
let test_sequencer_upgrade =
  register_all
    ~sequencer:Constant.bootstrap1
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "sequencer_upgrade"; "auto"; "sync"; Tag.flaky]
    ~title:
      "Rollup-node sequencer upgrade is applied to the sequencer local state."
  @@ fun {
           sc_rollup_node;
           l1_contracts;
           sc_rollup_address;
           client;
           sequencer;
           proxy;
           observer;
           enable_dal;
           _;
         }
             _protocol ->
  (* produce an initial block *)
  let*@ _lvl = produce_block sequencer in
  let* () = bake_until_sync ~proxy ~sequencer ~sc_rollup_node ~client () in
  let* () =
    check_head_consistency
      ~left:proxy
      ~right:sequencer
      ~error_msg:"The head should be the same before the upgrade"
      ()
  in
  let*@ previous_proxy_head = Rpc.get_block_by_number ~block:"latest" proxy in
  (* Sends the upgrade to L1. *)
  Log.info "Sending the sequencer upgrade to the L1 contract" ;
  let new_sequencer_key = Constant.bootstrap2.alias in
  let* _upgrade_info = Evm_node.wait_for_evm_event Sequencer_upgrade sequencer
  and* _upgrade_info_observer =
    Evm_node.wait_for_evm_event Sequencer_upgrade observer
  and* () =
    let* () =
      sequencer_upgrade
        ~sc_rollup_address
        ~sequencer_admin:Constant.bootstrap2.alias
        ~sequencer_governance_contract:l1_contracts.sequencer_governance
        ~pool_address:Eth_account.bootstrap_accounts.(0).address
        ~client
        ~upgrade_to:new_sequencer_key
        ~activation_timestamp:"0"
    in
    (* 2 block so the sequencer sees the event from the rollup
       node. *)
    repeat 2 (fun () ->
        let* _ = next_rollup_node_level ~client ~sc_rollup_node in
        unit)
  in
  let* () =
    check_head_consistency
      ~left:proxy
      ~right:sequencer
      ~error_msg:"The head should be the same after the upgrade"
      ()
  in
  let nb_block = 4l in
  (* apply the upgrade in the kernel  *)
  let* _ = next_rollup_node_level ~client ~sc_rollup_node in
  (* produce_block fails because sequencer changed *)
  let* () =
    match Evm_node.mode sequencer with
    | Sequencer _ ->
        let*@? _err = Rpc.produce_block sequencer in
        unit
    | Threshold_encryption_sequencer _ ->
        let wait_for_invalid = Evm_node.wait_for_blueprint_invalid sequencer in
        let* _ntx = Rpc.produce_proposal sequencer and* () = wait_for_invalid in
        unit
    | _ -> assert false (* impossible case as it's a sequencer. *)
  in
  let* () =
    repeat 5 (fun () ->
        let* _ = next_rollup_node_level ~client ~sc_rollup_node in
        unit)
  in
  let*@ proxy_head = Rpc.get_block_by_number ~block:"latest" proxy in
  Check.((previous_proxy_head.hash = proxy_head.hash) string)
    ~error_msg:
      "The proxy should not have progessed because no block have been produced \
       by the current sequencer." ;
  (* Check that even the evm-node sequencer itself refuses the blocks as they do
     not respect the sequencer's signature. *)
  let* () =
    check_head_consistency
      ~left:proxy
      ~right:sequencer
      ~error_msg:
        "The head should be the same after the sequencer tried to produce \
         blocks, they are are disregarded."
      ()
  in
  Log.info
    "Stopping current sequencer and starting a new one with new sequencer key" ;
  let new_sequencer =
    let mode =
      match Evm_node.mode sequencer with
      | Sequencer config ->
          Evm_node.Sequencer
            {
              config with
              sequencer = new_sequencer_key;
              private_rpc_port = Some (Port.fresh ());
            }
      | Threshold_encryption_sequencer config ->
          Evm_node.Threshold_encryption_sequencer
            {
              config with
              sequencer = new_sequencer_key;
              private_rpc_port = Some (Port.fresh ());
            }
      | _ -> Test.fail "impossible case, it's a sequencer"
    in
    Evm_node.create ~mode (Sc_rollup_node.endpoint sc_rollup_node)
  in
  let* () = Process.check @@ Evm_node.spawn_init_config new_sequencer in

  let* _ = Evm_node.wait_for_shutdown_event sequencer
  and* () =
    let* () =
      Evm_node.init_from_rollup_node_data_dir new_sequencer sc_rollup_node
    in
    let* () = Evm_node.run new_sequencer in
    let* () =
      repeat (Int32.to_int nb_block) (fun () ->
          let* _ = produce_block new_sequencer in
          unit)
    in
    let* () =
      let number_of_blocks = if enable_dal then 20 else 5 in
      repeat number_of_blocks (fun () ->
          let* _ = next_rollup_node_level ~client ~sc_rollup_node in
          unit)
    in
    let previous_proxy_head = proxy_head in
    let* () =
      check_head_consistency
        ~left:proxy
        ~right:new_sequencer
        ~error_msg:
          "The head should be the same after blocks produced by the new \
           sequencer"
        ()
    in
    let*@ proxy_head = Rpc.get_block_by_number ~block:"latest" proxy in
    Check.(
      (Int32.add previous_proxy_head.number nb_block = proxy_head.number) int32)
      ~error_msg:
        "The block number should have incremented (previous: %L, current: %R)" ;
    unit
  in
  unit

(** this test the situation where a sequencer diverged from it
    source. To obtain that we create two sequencers, one is going to
    diverged from the other. *)
let test_sequencer_diverge =
  register_all
    ~genesis_timestamp:Client.(At (Time.of_notation_exn "2020-01-01T00:00:00Z"))
    ~sequencer:Constant.bootstrap1
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "diverge"]
    ~title:"Runs two sequencers, one diverge and stop"
  @@ fun {sc_rollup_node; client; sequencer; observer; proxy; enable_dal; _}
             _protocol ->
  let* () =
    repeat 4 (fun () ->
        let*@ _l2_level =
          produce_block ~timestamp:"2020-01-01T00:00:00Z" sequencer
        in
        unit)
  in
  let* () = bake_until_sync ~sc_rollup_node ~proxy ~sequencer ~client () in
  let* () =
    (* 2 to make sure it has been finalized *)
    repeat 2 (fun () ->
        let* _l1_level = next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  (* We duplicate the sequencer by creating a snapshot and importing it *)
  let* _ = Evm_node.terminate sequencer in
  let* snapshot_file = Runnable.run @@ Evm_node.export_snapshot sequencer in
  let* sequencer_bis =
    let* mode =
      match Evm_node.mode sequencer with
      | Sequencer config ->
          return
          @@ Evm_node.Sequencer
               {config with private_rpc_port = Some (Port.fresh ())}
      | Threshold_encryption_sequencer config ->
          let sequencer_sidecar = Dsn_node.sequencer () in
          let* () = Dsn_node.start sequencer_sidecar in
          return
          @@ Evm_node.Threshold_encryption_sequencer
               {
                 config with
                 private_rpc_port = Some (Port.fresh ());
                 sequencer_sidecar_endpoint =
                   Dsn_node.endpoint sequencer_sidecar;
               }
      | _ -> Test.fail "impossible case, it's a sequencer"
    in
    return @@ Evm_node.create ~mode (Sc_rollup_node.endpoint sc_rollup_node)
  in
  let* () = Process.check @@ Evm_node.spawn_init_config sequencer_bis in
  let* () =
    Runnable.run @@ Evm_node.import_snapshot sequencer_bis ~snapshot_file
  in
  let* () = Evm_node.run sequencer in
  let* () = Evm_node.run sequencer_bis in

  (* We start a new observer for the new sequencer and wait for it to catch-up *)
  let* observer_bis = run_new_observer_node ~sc_rollup_node sequencer_bis in
  let* () = Evm_node.wait_for_blueprint_applied observer_bis 4 in

  (* When run in the CI the shutdown event are sometimes handled after the
     sequencer and/or the observer have already been terminated, leading to
     errors while they actually shutdown as expected. *)
  let diverged_and_shutdown sequencer observer =
    let* _ = Evm_node.wait_for_diverged sequencer
    and* _ = Evm_node.wait_for_shutdown_event ~can_terminate:true sequencer
    and* _ = Evm_node.wait_for_diverged observer
    and* _ = Evm_node.wait_for_shutdown_event ~can_terminate:true observer in
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
        let* _ = next_rollup_node_level ~client ~sc_rollup_node in
        unit)
  in
  unit

(** This test that the sequencer evm node can catchup event from the
    rollup node. *)
let test_sequencer_can_catch_up_on_event =
  register_all
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
  @@ fun {sc_rollup_node; client; sequencer; proxy; observer; _} _protocol ->
  let* () =
    repeat 2 (fun () ->
        let* _ = produce_block sequencer in
        unit)
  in
  let* () = bake_until_sync ~sequencer ~sc_rollup_node ~proxy ~client () in
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
        let* _ = next_rollup_node_level ~sc_rollup_node ~client in
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
  let* () = check_head_consistency ~left:proxy ~right:sequencer () in
  unit

let test_sequencer_dont_read_level_twice =
  register_all
    ~sequencer:Constant.bootstrap1
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "event"; Tag.slow]
    ~title:"Evm node don't read the same level twice"
  @@ fun {
           sc_rollup_node;
           client;
           sequencer;
           proxy;
           l1_contracts;
           sc_rollup_address;
           _;
         }
             _protocol ->
  (* We deposit some Tez to the rollup *)
  let* () =
    send_deposit_to_delayed_inbox
      ~amount:Tez.(of_int 16)
      ~l1_contracts
      ~depositor:Constant.bootstrap5
      ~receiver:Eth_account.bootstrap_accounts.(1).address
      ~sc_rollup_node
      ~sc_rollup_address
      client
  in

  (* We bake two blocks, so thet the EVM node can process the deposit and
     create a blueprint with it. *)
  let* _ = next_rollup_node_level ~sc_rollup_node ~client in
  let* _ = next_rollup_node_level ~sc_rollup_node ~client in

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
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer ~proxy () in

  unit

(** Test that the kernel can handle more than 100 withdrawals per level,
    which is currently the limit of outbox messages in the L1. *)
let test_outbox_size_limit_resilience ~slow =
  let commitment_period = 5 and challenge_window = 5 in
  let slow_str = if slow then "slow" else "fast" in
  register_all
    ~sequencer:Constant.bootstrap1
    ~time_between_blocks:Nothing
    ~tags:
      (["evm"; "withdraw"; "outbox"; "spam"] @ if slow then [Tag.slow] else [])
    ~title:(sf "Outbox size limit resilience (%s)" slow_str)
    ~commitment_period
    ~challenge_window
  @@ fun {
           client;
           l1_contracts;
           sc_rollup_address;
           sc_rollup_node;
           sequencer;
           proxy;
           _;
         }
             _protocol ->
  let endpoint = Evm_node.endpoint sequencer in

  (* Make a tez deposit *)
  let amount = Tez.of_int 1000 in
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
    Eth_cli.balance ~account:receiver.address ~endpoint
  in
  let* () =
    send_deposit_to_delayed_inbox
      ~amount
      ~l1_contracts
      ~depositor
      ~receiver:receiver.address
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
  let* () = bake_until_sync ~sc_rollup_node ~proxy ~sequencer ~client () in
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in
  let* receiver_balance_next =
    Eth_cli.balance ~account:receiver.address ~endpoint
  in
  Check.((receiver_balance_next > receiver_balance_prev) Wei.typ)
    ~error_msg:"Expected a bigger balance" ;

  (* Deploy and top up batch withdrawal contract *)
  let* spammer_resolved = Solidity_contracts.spam_withdrawal () in
  let* () =
    Eth_cli.add_abi ~label:spammer_resolved.label ~abi:spammer_resolved.abi ()
  in
  let* spammer_contract, _tx_hash =
    send_transaction_to_sequencer
      (fun () ->
        Eth_cli.deploy
          ~source_private_key:receiver.private_key
          ~endpoint:(Evm_node.endpoint sequencer)
          ~abi:spammer_resolved.abi
          ~bin:spammer_resolved.bin)
      sequencer
  in
  let* _ =
    send_transaction_to_sequencer
      (Eth_cli.contract_send
         ~source_private_key:receiver.private_key
         ~endpoint
         ~abi_label:spammer_resolved.label
         ~address:spammer_contract
         ~method_call:"giveFunds()"
         ~value:(Wei.of_eth_int 990))
      sequencer
  in

  (* Create and topup many accounts (to overcome single account per block limitation) *)
  let* wallets = Cast.gen_wallets ~number:150 () in
  let addresses =
    List.map (fun (w : Cast.wallet) -> sf "\"%s\"" w.address) wallets
  in
  let* _ =
    send_transaction_to_sequencer
      (Eth_cli.contract_send
         ~source_private_key:receiver.private_key
         ~endpoint
         ~abi_label:spammer_resolved.label
         ~address:spammer_contract
         ~method_call:(sf "batchTopUp([%s])" (String.concat "," addresses))
         ~value:(Wei.of_eth_int 0))
      sequencer
  in

  (* Create 150 withdrawals *)
  let* withdrawal_level = Client.level client in
  let withdraw_tx ~(wallet : Cast.wallet) =
    Cast.craft_tx
      ~source_private_key:wallet.private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:30_000_000
      ~value:(Wei.of_eth_int 0)
      ~address:spammer_contract
      ~signature:"doWithdrawals(uint256)"
      ~arguments:["1"]
  in
  let* withdraw_txs =
    List.map (fun w -> withdraw_tx ~wallet:w ()) wallets |> Lwt.all
  in
  let* _ = batch_n_transactions ~evm_node:sequencer withdraw_txs in
  let* _ = produce_block sequencer in

  (* At this point the outbox queue must contain 150 messages *)
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer ~proxy () in

  (* 100 messages flushed at this point *)
  let* _ = produce_block sequencer in
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer ~proxy () in

  (* +50 messages flushed at this point *)
  if slow then (
    (* Execute the first 100 withdrawals *)
    let* actual_withdrawal_level =
      find_and_execute_withdrawal
        ~withdrawal_level
        ~commitment_period
        ~challenge_window
        ~evm_node:proxy
        ~sc_rollup_node
        ~sc_rollup_address
        ~client
        ()
    in
    let* balance =
      Client.get_balance_for
        ~account:"tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j"
        client
    in
    Check.((balance = Tez.of_int 100) Tez.typ)
      ~error_msg:"Expected balance of %R, got %L" ;
    (* Execute the next 50 withdrawals *)
    let* _ =
      find_and_execute_withdrawal
        ~withdrawal_level:(actual_withdrawal_level + 1)
        ~commitment_period
        ~challenge_window
        ~evm_node:proxy
        ~sc_rollup_node
        ~sc_rollup_address
        ~client
        ()
    in
    let* balance =
      Client.get_balance_for
        ~account:"tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j"
        client
    in
    Check.((balance = Tez.of_int 150) Tez.typ)
      ~error_msg:"Expected balance of %R, got %L" ;
    unit)
  else unit

let test_stage_one_reboot =
  register_all
    ~sequencer:Constant.bootstrap1
    ~time_between_blocks:Nothing
    ~maximum_allowed_ticks:9_000_000_000L
    ~tags:["evm"; "sequencer"; "reboot"; Tag.slow]
    ~title:
      "Checks the stage one reboots when reading too much chunks in a single \
       L1 level"
  @@ fun {sc_rollup_node; client; sc_rollup_address; _} _protocol ->
  let* chunks =
    Lwt_list.map_s (fun i ->
        Evm_node.chunk_data
          ~rollup_address:sc_rollup_address
          ~sequencer_key:Constant.bootstrap1.alias
          ~client
          ~number:i
          [])
    @@ List.init 400 Fun.id
  in
  let chunks = List.flatten chunks in
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
  let rec split_chunks acc chunks =
    match chunks with
    | [] -> acc
    | _ ->
        let messages, rem = Tezos_stdlib.TzList.split_n 100 chunks in
        split_chunks (messages :: acc) rem
  in
  let splitted_messages = split_chunks [] chunks in
  let* () =
    Lwt_list.iteri_s
      (fun i messages -> send_chunks messages Account.Bootstrap.keys.(i).alias)
      splitted_messages
  in
  let* total_tick_number_before_expected_reboots =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_total_ticks ()
  in
  let* _ = next_rollup_node_level ~client ~sc_rollup_node in
  let* total_tick_number_with_expected_reboots =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_total_ticks ()
  in
  let ticks_after_expected_reboot =
    total_tick_number_with_expected_reboots
    - total_tick_number_before_expected_reboots
  in

  (* The PVM takes 11G ticks for collecting inputs, 11G for a kernel_run. As such,
     an L1 level is at least 22G ticks. *)
  let min_ticks_per_l1_level = ticks_per_snapshot * 2 in
  (* If the inbox is not empty, the kernel enforces a reboot after reading it,
     to give the maximum ticks available for the first block production. *)
  let min_ticks_when_inbox_is_not_empty =
    min_ticks_per_l1_level + ticks_per_snapshot
  in
  Check.((ticks_after_expected_reboot > min_ticks_when_inbox_is_not_empty) int)
    ~error_msg:
      "The number of ticks spent during the period should be higher than %R, \
       but got %L, which implies there have been no reboot, contrary to what \
       was expected." ;
  unit

let test_blueprint_is_limited_in_size =
  register_all
    ~sequencer:Constant.bootstrap1
    ~time_between_blocks:Nothing
    ~max_number_of_chunks:2
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
    ~bootstrap_accounts:Eth_account.lots_of_address
    ~tags:["evm"; "sequencer"; "blueprint"; "limit"]
    ~title:
      "Checks the sequencer doesn't produce blueprint bigger than the given \
       maximum number of chunks"
    ~use_dal:ci_enabled_dal_registration
  @@ fun {sc_rollup_node; client; sequencer; _} _protocol ->
  let txs = read_tx_from_file () |> List.map (fun (tx, _hash) -> tx) in
  let* requests, hashes =
    Helpers.batch_n_transactions ~evm_node:sequencer txs
  in
  (* Each transaction is about 114 bytes, hence 100 * 114 = 11400 bytes, which
     will fit in two blueprints of two chunks each. *)
  let* () = next_evm_level ~evm_node:sequencer ~sc_rollup_node ~client in
  let* () = next_evm_level ~evm_node:sequencer ~sc_rollup_node ~client in
  let first_hash = List.hd hashes in
  let* level_of_first_transaction =
    let*@ receipt = Rpc.get_transaction_receipt ~tx_hash:first_hash sequencer in
    match receipt with
    | None -> Test.fail "Delayed transaction hasn't be included"
    | Some receipt -> return receipt.blockNumber
  in
  let*@ block_with_first_transaction =
    Rpc.get_block_by_number
      ~block:(Int32.to_string level_of_first_transaction)
      sequencer
  in
  (* The block containing the first transaction of the batch cannot contain the
     100 transactions of the batch, as it doesn't fit in two chunks. *)
  let block_size_of_first_transaction =
    match block_with_first_transaction.Block.transactions with
    | Block.Empty -> Test.fail "Expected a non empty block"
    | Block.Full _ ->
        Test.fail "Block is supposed to contain only transaction hashes"
    | Block.Hash hashes ->
        Check.((List.length hashes < List.length requests) int)
          ~error_msg:"Expected less than %R transactions in the block, got %L" ;
        List.length hashes
  in

  let* () = next_evm_level ~evm_node:sequencer ~sc_rollup_node ~client in
  (* It's not clear the first transaction of the batch is applied in the first
     blueprint or the second, as it depends how the tx_pool sorts the
     transactions (by caller address). We need to check that either the previous
     block or the next block contains transactions, which puts in evidence that
     the batch has been splitted into two consecutive blueprints.
  *)
  let check_block_size block_number =
    let*@ block =
      Rpc.get_block_by_number ~block:(Int32.to_string block_number) sequencer
    in
    match block.Block.transactions with
    | Block.Empty -> return 0
    | Block.Full _ ->
        Test.fail "Block is supposed to contain only transaction hashes"
    | Block.Hash hashes -> return (List.length hashes)
  in
  let* next_block_size =
    check_block_size (Int32.succ level_of_first_transaction)
  in
  let* previous_block_size =
    check_block_size (Int32.pred level_of_first_transaction)
  in
  if next_block_size = 0 && previous_block_size = 0 then
    Test.fail
      "The sequencer didn't apply the 100 transactions in two consecutive \
       blueprints" ;
  Check.(
    (block_size_of_first_transaction + previous_block_size + next_block_size
    = List.length hashes)
      int
      ~error_msg:
        "Not all the transactions have been injected, only %L, while %R was \
         expected.") ;
  unit

let test_blueprint_limit_with_delayed_inbox =
  register_all
    ~bootstrap_accounts:Eth_account.lots_of_address
    ~sequencer:Constant.bootstrap1
    ~time_between_blocks:Nothing
    ~max_number_of_chunks:2
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
    ~tags:["evm"; "sequencer"; "blueprint"; "limit"; "delayed"]
    ~title:
      "Checks the sequencer doesn't produce blueprint bigger than the given \
       maximum number of chunks and count delayed transactions size in the \
       blueprint"
    ~use_dal:ci_enabled_dal_registration
  @@ fun {sc_rollup_node; client; sequencer; sc_rollup_address; l1_contracts; _}
             _protocol ->
  let txs = read_tx_from_file () |> List.map (fun (tx, _hash) -> tx) in
  (* The first 3 transactions will be sent to the delayed inbox *)
  let delayed_txs, direct_txs = Tezos_base.TzPervasives.TzList.split_n 3 txs in
  let send_to_delayed_inbox (sender, raw_tx) =
    send_raw_transaction_to_delayed_inbox
      ~wait_for_next_level:false
      ~sender
      ~sc_rollup_node
      ~sc_rollup_address
      ~client
      ~l1_contracts
      raw_tx
  in
  let* delayed_hashes =
    Lwt_list.map_s send_to_delayed_inbox
    @@ List.combine
         [Constant.bootstrap2; Constant.bootstrap3; Constant.bootstrap4]
         delayed_txs
  in
  (* Ensures the transactions are added to the rollup delayed inbox and picked
     by the sequencer *)
  let* () =
    repeat 4 (fun () ->
        let* _l1_level = next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  let* _requests, _hashes =
    Helpers.batch_n_transactions ~evm_node:sequencer direct_txs
  in
  (* Due to the overapproximation of 4096 bytes per delayed transactions, there
     should be only a single delayed transaction per blueprints with 2 chunks. *)
  let* _ = next_evm_level ~evm_node:sequencer ~sc_rollup_node ~client in
  let* _ = next_evm_level ~evm_node:sequencer ~sc_rollup_node ~client in
  let* _ = next_evm_level ~evm_node:sequencer ~sc_rollup_node ~client in
  (* Checks the delayed transactions and at least the first transaction from the
     batch have been applied *)
  let* block_numbers =
    Lwt_list.map_s
      (fun tx_hash ->
        let*@ receipt = Rpc.get_transaction_receipt ~tx_hash sequencer in
        match receipt with
        | None -> Test.fail "Delayed transaction hasn't be included"
        | Some receipt -> return receipt.blockNumber)
      delayed_hashes
  in
  let check_block_contains_delayed_transaction_and_transactions
      (delayed_hash, block_number) =
    let*@ block =
      Rpc.get_block_by_number ~block:(Int32.to_string block_number) sequencer
    in
    match block.Block.transactions with
    | Block.Empty -> Test.fail "Block shouldn't be empty"
    | Block.Full _ ->
        Test.fail "Block is supposed to contain only transaction hashes"
    | Block.Hash hashes ->
        if not (List.mem ("0x" ^ delayed_hash) hashes && 2 < List.length hashes)
        then
          Test.fail
            "The delayed transaction %s hasn't been included in the expected \
             block along other transactions from the pool"
            delayed_hash ;
        unit
  in
  Lwt_list.iter_s check_block_contains_delayed_transaction_and_transactions
  @@ List.combine delayed_hashes block_numbers

let test_reset =
  register_all
    ~sequencer:Constant.bootstrap1
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "reset"]
    ~title:"try to reset sequencer and observer state using the command."
  @@ fun {
           proxy;
           observer;
           sequencer;
           sc_rollup_node;
           client;
           sc_rollup_address;
           _;
         }
             _protocol ->
  let reset_level = 5 in
  let after_reset_level = 5 in
  Log.info "Producing %d level then syncing" reset_level ;
  let* () =
    repeat reset_level (fun () ->
        next_evm_level ~evm_node:sequencer ~sc_rollup_node ~client)
  in
  let* () = bake_until_sync ~sequencer ~sc_rollup_node ~proxy ~client () in
  Log.info
    "Stopping the rollup node, then produce %d more blocks "
    (reset_level + after_reset_level) ;
  let* () = Sc_rollup_node.terminate sc_rollup_node in
  let* () =
    repeat after_reset_level (fun () ->
        next_evm_level ~evm_node:sequencer ~sc_rollup_node ~client)
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
      ~mode:(Evm_node.mode sequencer)
      ~data_dir:(Evm_node.data_dir sequencer)
      ~rpc_port:(Evm_node.rpc_port sequencer)
      (Sc_rollup_node.endpoint sc_rollup_node)
  in
  let* () = Evm_node.run sequencer in
  let observer =
    Evm_node.create
      ~mode:(Evm_node.mode observer)
      ~data_dir:(Evm_node.data_dir observer)
      ~rpc_port:(Evm_node.rpc_port observer)
      (Evm_node.endpoint sequencer)
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
    repeat after_reset_level (fun () ->
        next_evm_level ~evm_node:sequencer ~sc_rollup_node ~client)
  in
  let* () = bake_until_sync ~sequencer ~sc_rollup_node ~proxy ~client () in
  (* Check sequencer is at the expected level *)
  let*@ sequencer_level = Rpc.block_number sequencer in
  Check.(
    (sequencer_level = Int32.of_int (reset_level + after_reset_level)) int32)
    ~error_msg:
      "The sequencer level %L should be at %R after producing blocks after the \
       reset." ;
  unit

let test_preimages_endpoint =
  (* Add a delay between first block and activation timestamp. *)
  let genesis_timestamp =
    Client.(At (Time.of_notation_exn "2020-01-01T00:00:00Z"))
  in
  let activation_timestamp = "2020-01-01T00:00:10Z" in
  register_all
    ~block_storage_sqlite3:false (* The test uses init from rollup node. *)
    ~sequencer:Constant.bootstrap1
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "preimages_endpoint"]
    ~title:"Sequencer use remote server to get preimages"
    ~kernels:[Latest]
    ~additional_uses:[Constant.WASM.ghostnet_evm_kernel]
    ~genesis_timestamp
  @@ fun {
           sc_rollup_node;
           l1_contracts;
           sc_rollup_address;
           client;
           sequencer;
           proxy;
           _;
         }
             _protocol ->
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer ~proxy () in
  let* () = Evm_node.terminate sequencer in
  (* Prepares the sequencer without [preimages-dir], to force the use of
     preimages endpoint. *)
  let sequencer_mode_without_preimages_dir =
    match Evm_node.mode sequencer with
    | Evm_node.Sequencer mode ->
        Evm_node.Sequencer {mode with preimage_dir = None}
    | Evm_node.Threshold_encryption_sequencer mode ->
        Evm_node.Threshold_encryption_sequencer {mode with preimage_dir = None}
    | _ -> assert false
  in
  let new_sequencer =
    Evm_node.create
      ~mode:sequencer_mode_without_preimages_dir
      (Sc_rollup_node.endpoint sc_rollup_node)
  in
  let* () = Process.check @@ Evm_node.spawn_init_config new_sequencer in
  let* () =
    repeat 2 (fun () ->
        let* _ = next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  let* () =
    Evm_node.init_from_rollup_node_data_dir new_sequencer sc_rollup_node
  in
  (* Sends an upgrade with new preimages. *)
  let* root_hash =
    upgrade
      ~sc_rollup_node
      ~sc_rollup_address
      ~admin:Constant.bootstrap2.public_key_hash
      ~admin_contract:l1_contracts.admin
      ~client
      ~upgrade_to:Constant.WASM.ghostnet_evm_kernel
      ~activation_timestamp
  in
  let* _ =
    repeat 2 (fun () ->
        let* _ = next_rollup_node_level ~client ~sc_rollup_node in
        unit)
  in
  (* Create a file server that serves the preimages. *)
  let provider_port = Port.fresh () in
  let served = ref false in
  Sc_rollup_helpers.serve_files
    ~name:"preimages_server"
    ~port:provider_port
    ~root:(Sc_rollup_node.data_dir sc_rollup_node // "wasm_2_0_0")
    ~on_request:(fun _ -> served := true)
  @@ fun () ->
  let preimages_endpoint =
    sf "http://%s:%d" Constant.default_host provider_port
  in
  let* () =
    Evm_node.run
      ~extra_arguments:["--preimages-endpoint"; preimages_endpoint]
      new_sequencer
  in
  (* Produce a block so the sequencer sees the event. *)
  let kernel_downloaded =
    Evm_node.wait_for_predownload_kernel new_sequencer ~root_hash
  in
  let* _ = next_rollup_node_level ~sc_rollup_node ~client
  and* () = kernel_downloaded in
  let*@ _ = produce_block ~timestamp:"2020-01-01T00:00:15Z" new_sequencer in
  Check.is_true
    !served
    ~error_msg:"The sequencer should have used the file server" ;
  unit

let test_store_smart_rollup_address =
  register_all
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "store"]
    ~title:"Sequencer checks the smart rollup address"
  @@ fun {sequencer; client; node; _} _protocol ->
  (* Starting the sequencer stores the smart rollup address in the store. *)
  (* Kill the sequencer. *)
  let* () = Evm_node.terminate sequencer in
  (* Originate another rollup. *)
  let* other_rollup_address =
    originate_sc_rollup ~keys:[] ~kind:"wasm_2_0_0" ~alias:"vbot" client
  in
  let other_rollup_node =
    Sc_rollup_node.create Observer node ~base_dir:(Client.base_dir client)
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

let test_replay_rpc =
  register_all
    ~tags:["evm"; "rpc"; "replay"]
    ~title:"Sequencer can replay a block"
    ~time_between_blocks:Nothing
  @@ fun {sc_rollup_node; sequencer; client; proxy; _} _protocol ->
  (* Transfer funds to a random address. *)
  let address = "0xB7A97043983f24991398E5a82f63F4C58a417185" in
  let* transaction_hash =
    send_transaction_to_sequencer
      (Eth_cli.transaction_send
         ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
         ~to_public_key:address
         ~value:(Wei.of_eth_int 10)
         ~endpoint:(Evm_node.endpoint sequencer))
      sequencer
  in
  let*@! {Transaction.blockNumber; _} =
    Rpc.get_transaction_by_hash ~transaction_hash sequencer
  in
  (* Block few levels to ensure we are replaying on an old block. *)
  let* () =
    repeat 2 (fun () ->
        next_evm_level ~evm_node:sequencer ~sc_rollup_node ~client)
  in
  let* () = bake_until_sync ~sequencer ~sc_rollup_node ~proxy ~client () in
  let blockNumber =
    match blockNumber with
    | Some blockNumber -> blockNumber
    | None -> Test.fail "Missing block number"
  in
  let*@ original_block =
    Rpc.get_block_by_number
      ~full_tx_objects:false
      ~block:(Int32.to_string blockNumber)
      sequencer
  in
  let*@ replayed_block =
    Rpc.replay_block (Int32.to_int blockNumber) sequencer
  in
  (* Checks the block hash is the same. If so, we can assume they have the same
     state hash and transactions. *)
  Check.(
    (original_block.hash = replayed_block.hash)
      string
      ~error_msg:"Replayed block hash is %R, but the original block has %L") ;
  unit

let test_trace_transaction =
  let check_trace_result ~sequencer:trace_result ~rpc:trace_result_rpc =
    match (trace_result, trace_result_rpc) with
    | Ok t, Ok t' -> assert (JSON.equal t t')
    | Error _, _ ->
        Test.fail "Trace transaction shouldn't have failed (sequencer)"
    | _, Error _ -> Test.fail "Trace transaction shouldn't have failed (rpc)"
  in
  register_all
    ~kernels:Kernel.[Latest; Ghostnet]
      (* Re-enable on [Mainnet] when the next upgrade happens. *)
    ~tags:["evm"; "rpc"; "run"; "trace"]
    ~title:"Sequencer can run debug_traceTransaction"
    ~time_between_blocks:Nothing
  @@ fun {sequencer; _} _protocol ->
  (* Start a RPC node, as we also want to test that the sequencer and its RPC
     node return the same thing. *)
  let* rpc_node = run_new_rpc_endpoint sequencer in
  (* Transfer funds to a random address. *)
  let address = "0xB7A97043983f24991398E5a82f63F4C58a417185" in
  let* transaction_hash =
    send_transaction_to_sequencer
      (Eth_cli.transaction_send
         ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
         ~to_public_key:address
         ~value:(Wei.of_eth_int 10)
         ~endpoint:(Evm_node.endpoint sequencer))
      sequencer
  in
  let* _ = produce_block sequencer in
  (* Check tracing without options works *)
  let* trace_result = Rpc.trace_transaction ~transaction_hash sequencer in
  let* trace_result_rpc = Rpc.trace_transaction ~transaction_hash rpc_node in
  check_trace_result ~sequencer:trace_result ~rpc:trace_result_rpc ;
  (* Check tracing with a tracer without config *)
  let* trace_result =
    Rpc.trace_transaction ~transaction_hash ~tracer:"structLogger" sequencer
  in
  (match trace_result with
  | Ok _ -> ()
  | Error _ -> Test.fail "Trace transaction shouldn't have failed") ;
  (* Check tracing with a tracer and a config *)
  let* trace_result =
    Rpc.trace_transaction
      ~transaction_hash
      ~tracer:"structLogger"
      ~tracer_config:[("enableMemory", `Bool true)]
      sequencer
  in
  let* trace_result_rpc =
    Rpc.trace_transaction
      ~transaction_hash
      ~tracer:"structLogger"
      ~tracer_config:[("enableMemory", `Bool true)]
      rpc_node
  in
  check_trace_result ~sequencer:trace_result ~rpc:trace_result_rpc ;
  (* Check tracing without a tracer and a config *)
  let* trace_result =
    Rpc.trace_transaction
      ~transaction_hash
      ~tracer_config:
        [
          ("enableMemory", `Bool true);
          ("enableReturnData", `Bool true);
          ("disableStorage", `Bool true);
          ("disableStack", `Bool true);
        ]
      sequencer
  in
  let* trace_result_rpc =
    Rpc.trace_transaction
      ~transaction_hash
      ~tracer_config:
        [
          ("enableMemory", `Bool true);
          ("enableReturnData", `Bool true);
          ("disableStorage", `Bool true);
          ("disableStack", `Bool true);
        ]
      rpc_node
  in
  check_trace_result ~sequencer:trace_result ~rpc:trace_result_rpc ;
  unit

let test_overwrite_simulation_tick_limit =
  register_all
    ~kernels:Kernel.all
    ~tags:["evm"; "rpc"; "overwrite_tick_limit"; "eth_call"]
    ~title:"Can overwrite tick limit in eth_call with the experimental feature"
    ~time_between_blocks:Nothing
    ~maximum_allowed_ticks:
      (* We set a smaller tick limit to make it easier to trigger the
         OutOfTick error *)
      500_000_000L
  @@ fun {sequencer; sc_rollup_node; _} _protocol ->
  let* observer =
    run_new_observer_node
      ~sc_rollup_node
      ~patch_config:
        JSON.(
          update
            "experimental_features"
            (put
               ( "overwrite_simulation_tick_limit",
                 annotate ~origin:"" (`Bool true) )))
      sequencer
  in

  let* recursive = Solidity_contracts.recursive () in
  let* () = Eth_cli.add_abi ~label:recursive.label ~abi:recursive.abi () in
  (* Deploy the contract. *)
  let* recursive_address, _tx_hash =
    send_transaction_to_sequencer
      (fun () ->
        Eth_cli.deploy
          ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
          ~endpoint:(Evm_node.endpoint sequencer)
          ~abi:recursive.abi
          ~bin:recursive.bin)
      sequencer
  in

  let* has_failed =
    Lwt.catch
      (fun () ->
        let* _ =
          Eth_cli.contract_call
            ~endpoint:Evm_node.(endpoint sequencer)
            ~abi_label:recursive.label
            ~address:recursive_address
            ~method_call:"call(200)"
            ()
        in
        return false)
      (fun _ -> return true)
  in

  if not has_failed then
    Test.fail "The sequencer should have refused the `eth_call`" ;

  let* has_failed =
    Lwt.catch
      (fun () ->
        let* _ =
          Eth_cli.contract_call
            ~endpoint:Evm_node.(endpoint observer)
            ~abi_label:recursive.label
            ~address:recursive_address
            ~method_call:"call(200)"
            ()
        in
        return false)
      (fun _ -> return true)
  in

  if has_failed then
    Test.fail "The observer shouldn’t have refused the `eth_call`" ;

  (* `eth_call` is supposed to work, but not gas estimation. *)
  let* data = Cast.calldata ~args:["200"] "call(uint256)" in
  let*@? (_ : Rpc.error) =
    Rpc.estimate_gas
      [("to", `String recursive_address); ("data", `String data)]
      observer
  in

  unit

let test_trace_transaction_on_invalid_transaction =
  register_all
    ~kernels:Kernel.all
    ~tags:["evm"; "rpc"; "trace"; "fail"]
    ~title:"debug_traceTransaction fails on invalid transactions"
    ~time_between_blocks:Nothing
  @@ fun {sequencer; _} _protocol ->
  let* _ = produce_block sequencer in
  (* Check tracing without options works *)
  let* trace_result =
    Rpc.trace_transaction
      ~transaction_hash:("0x" ^ String.make 64 'f')
      sequencer
  in
  (match trace_result with
  | Ok _ -> Test.fail "Trace transaction should have failed"
  | Error {message; _} ->
      Check.(
        (message =~ rex "not found")
          ~error_msg:"traceTransaction failed with the wrong error")) ;
  unit

(* Checks that each opcode log are either all empty or non empty, considering
   the configuration. *)
let check_struct_logs expect_null log =
  let check_field field =
    if expect_null then
      Check.(
        (JSON.is_null log = false)
          bool
          ~error_msg:
            (Format.sprintf
               "Field %s was expected to be null, but got %%L instead"
               field))
    else
      Check.(
        (JSON.is_null log = false)
          bool
          ~error_msg:
            (Format.sprintf "Field %s wasn't expected to be null" field))
  in
  check_field "memory" ;
  check_field "storage" ;
  check_field "memSize" ;
  check_field "stack" ;
  check_field "returnData"

let check_trace expect_null expected_returned_value receipt trace =
  let failed = JSON.(trace |-> "failed" |> as_bool) in
  let gas_used = JSON.(trace |-> "gas" |> as_int64) in
  let returned_value =
    JSON.(trace |-> "returnValue" |> as_string |> Durable_storage_path.no_0x)
  in
  let logs = JSON.(trace |-> "structLogs" |> as_list) in
  Check.(
    (failed <> receipt.Transaction.status)
      bool
      ~error_msg:"The trace has a different status than in the receipt") ;
  Check.(
    (gas_used = receipt.gasUsed)
      int64
      ~error_msg:"Trace reported %L gas used, but the trace reported %R") ;

  (* Whether we don't expect a value and we get "0x", or we expect a value and
     it is encoded into a H256. *)
  (match expected_returned_value with
  | Some value ->
      let expected_value = Helpers.hex_256_of_int value in
      Check.(
        (returned_value = expected_value)
          string
          ~error_msg:
            "The transaction returned the value %L, but %R was expected")
  | None ->
      Check.(
        (returned_value = "")
          string
          ~error_msg:"The transaction shouldn't return a value, but returned %L")) ;

  (* Checks the logs are consistent with the configuration (its an all in or
     all out). *)
  Check.((logs <> []) (list json) ~error_msg:"Logs shouldn't be empty") ;
  List.iter
    (check_struct_logs expect_null)
    JSON.(trace |-> "structLogs" |> as_list) ;
  unit

let test_trace_transaction_call =
  register_all
    ~kernels:Kernel.[Latest; Ghostnet]
      (* Re-enable on [Mainnet] when the next upgrade happens. *)
    ~tags:["evm"; "rpc"; "trace"; "call"]
    ~title:"Sequencer can run debug_traceTransaction and return a valid log"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; _} _protocol ->
  (* Transfer funds to a random address. *)
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* simple_storage_resolved = Solidity_contracts.simple_storage () in

  (* deploy contract *)
  let* () =
    Eth_cli.add_abi
      ~label:simple_storage_resolved.label
      ~abi:simple_storage_resolved.abi
      ()
  in
  let* contract_address, _tx_deployment =
    send_transaction_to_sequencer
      (fun () ->
        Eth_cli.deploy
          ~source_private_key:sender.Eth_account.private_key
          ~endpoint
          ~abi:simple_storage_resolved.label
          ~bin:simple_storage_resolved.bin)
      sequencer
  in
  let* _ = produce_block sequencer in
  (* We will first trace with every options enabled, and check that we have the
     logs as complete as possible. We call the function `set` from the contract,
     which isn't expected to fail and doesn't return any result. *)
  let value_in_storage = 10 in
  let* transaction_hash =
    send_transaction_to_sequencer
      (Eth_cli.contract_send
         ~source_private_key:sender.private_key
         ~endpoint
         ~abi_label:simple_storage_resolved.label
         ~address:contract_address
         ~method_call:(Format.sprintf "set(%d)" value_in_storage))
      sequencer
  in
  let* _ = produce_block sequencer in
  (* We will use the receipt to check that the results from the trace are
     consistent with the result from the transaction once applied in the
     block. *)
  let*@ transaction_receipt =
    Rpc.get_transaction_receipt ~tx_hash:transaction_hash sequencer
  in
  let transaction_receipt = Option.get transaction_receipt in
  let* trace_result =
    Rpc.trace_transaction
      ~transaction_hash
      ~tracer_config:
        [("enableMemory", `Bool true); ("enableReturnData", `Bool true)]
      sequencer
  in
  let* () =
    match trace_result with
    | Ok trace -> check_trace false None transaction_receipt trace
    | Error _ -> Test.fail "Trace transaction shouldn't have failed"
  in
  (* The second test will disable every tracing options and call `get`, thats
     returns a value, so that we can check that it returns the correct value in
     the end. *)
  let* transaction_hash =
    send_transaction_to_sequencer
      (Eth_cli.contract_send
         ~source_private_key:sender.private_key
         ~endpoint
         ~abi_label:simple_storage_resolved.label
         ~address:contract_address
         ~method_call:"get()")
      sequencer
  in
  let* _ = produce_block sequencer in
  let*@ transaction_receipt =
    Rpc.get_transaction_receipt ~tx_hash:transaction_hash sequencer
  in
  let transaction_receipt = Option.get transaction_receipt in
  let* trace_result =
    Rpc.trace_transaction
      ~transaction_hash
      ~tracer_config:
        [("disableStack", `Bool true); ("disableStorage", `Bool true)]
      sequencer
  in
  match trace_result with
  | Ok trace ->
      check_trace true (Some value_in_storage) transaction_receipt trace
  | Error _ -> Test.fail "Trace transaction shouldn't have failed"

let test_trace_transaction_call_trace =
  register_all
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call_trace"]
    ~title:"Sequencer can run debug_traceTransaction with calltracer"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; _} _protocol ->
  (* Transfer funds to a random address. *)
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* simple_storage_resolved = Solidity_contracts.simple_storage () in
  (* deploy contract *)
  let* () =
    Eth_cli.add_abi
      ~label:simple_storage_resolved.label
      ~abi:simple_storage_resolved.abi
      ()
  in
  let* contract_address, _tx_deployment =
    send_transaction_to_sequencer
      (fun () ->
        Eth_cli.deploy
          ~source_private_key:sender.Eth_account.private_key
          ~endpoint
          ~abi:simple_storage_resolved.label
          ~bin:simple_storage_resolved.bin)
      sequencer
  in
  let* _ = produce_block sequencer in
  (* We will first trace with every options enabled, and check that we have the
     logs as complete as possible. We call the function `set` from the contract,
     which isn't expected to fail and doesn't return any result. *)
  let value_in_storage = 10 in
  let* transaction_hash =
    send_transaction_to_sequencer
      (Eth_cli.contract_send
         ~source_private_key:sender.private_key
         ~endpoint
         ~abi_label:simple_storage_resolved.label
         ~address:contract_address
         ~method_call:(Format.sprintf "set(%d)" value_in_storage))
      sequencer
  in
  let* _ = produce_block sequencer in
  (* We will use the receipt to check that the results from the trace are
     consistent with the result from the transaction once applied in the
     block. *)
  let*@ transaction_receipt =
    Rpc.get_transaction_receipt ~tx_hash:transaction_hash sequencer
  in
  let transaction_receipt = Option.get transaction_receipt in
  let*@ trace_result =
    Rpc.trace_transaction
      ~tracer:"callTracer"
      ~transaction_hash
      ~tracer_config:[("withLog", `Bool true); ("onlyTopCall", `Bool true)]
      sequencer
  in
  let*@! {Transaction.input; gas; _} =
    Rpc.get_transaction_by_hash ~transaction_hash sequencer
  in
  Check.(
    (JSON.(trace_result |-> "calls" |> as_list) = [])
      (list json)
      ~error_msg:"Wrong calls, expected %R but got %L") ;
  Check.(
    (JSON.(trace_result |-> "type" |> as_string) = "CALL")
      string
      ~error_msg:"Wrong type, expected %R but got %L") ;
  Check.(
    (JSON.(trace_result |-> "from" |> as_string) = transaction_receipt.from)
      string
      ~error_msg:"Wrong from, expected %R but got %L") ;
  Check.(
    (JSON.(trace_result |-> "to" |> as_string)
    = Option.get transaction_receipt.to_)
      string
      ~error_msg:"Wrong to, expected %R but got %L") ;
  Check.(
    (JSON.(trace_result |-> "value" |> as_string) = "0x00")
      string
      ~error_msg:"Wrong value, expected %R but got %L") ;
  Check.(
    (JSON.(trace_result |-> "gas_used" |> as_string)
    = Format.sprintf "0x%02x" (Int64.to_int transaction_receipt.gasUsed))
      string
      ~error_msg:"Wrong gas_used, expected %R but got %L") ;
  Check.(
    (JSON.(trace_result |-> "output" |> as_string) = "0x")
      string
      ~error_msg:"Wrong output, expected %R but got %L") ;
  Check.(
    (JSON.(trace_result |-> "input" |> as_string)
    = Option.value ~default:"" input)
      string
      ~error_msg:"Wrong input, expected %R but got %L") ;
  Check.(
    (JSON.(trace_result |-> "gas" |> as_string)
    = Format.sprintf "0x%02x" @@ Int64.to_int gas)
      string
      ~error_msg:"Wrong gas, expected %R but got %L") ;
  unit

let test_trace_transaction_calltracer_failed_create =
  register_all
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call_trace"; "fail"]
    ~title:
      "debug_traceTransaction with calltracer returns an error when contract \
       creation reverts but is included"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* call_types = Solidity_contracts.call_types () in
  let* () = Eth_cli.add_abi ~label:call_types.label ~abi:call_types.abi () in
  let* _contract_address, create_tx =
    send_transaction_to_sequencer
      (fun () ->
        Eth_cli.deploy
          ~source_private_key:sender.Eth_account.private_key
          ~endpoint
          ~abi:call_types.label
          ~bin:call_types.bin)
      sequencer
  in
  let* _ = produce_block sequencer in
  let*@! {Transaction.gas; _} =
    Rpc.get_transaction_by_hash ~transaction_hash:create_tx sequencer
  in
  let gas = Int64.to_int gas in
  let not_enough_gas = gas - (gas / 10) in
  let data = read_file call_types.bin in
  let* raw_tx =
    Cast.craft_deploy_tx
      ~chain_id:1337
      ~gas_price:1_000_000_000
      ~nonce:1
      ~source_private_key:sender.Eth_account.private_key
      ~gas:not_enough_gas
      ~data
      ()
  in
  let*@ tx_hash = Rpc.send_raw_transaction ~raw_tx sequencer in
  let*@ size = produce_block sequencer in
  Check.((size = 1) int)
    ~error_msg:"Expected %R transactions in the block, got %L" ;
  let*@ trace_result =
    Rpc.trace_transaction
      ~tracer:"callTracer"
      ~transaction_hash:tx_hash
      sequencer
  in
  Check.(
    (JSON.(trace_result |-> "error" |> as_string) = "OutOfGas")
      string
      ~error_msg:"Trace should report an error. Expected %R but got %L") ;
  unit

let test_trace_transaction_calltracer_all_types =
  register_all
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call_trace"; "types"]
    ~title:
      "debug_traceTransaction with calltracer can produce all the call types"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* call_types = Solidity_contracts.call_types () in
  let* () = Eth_cli.add_abi ~label:call_types.label ~abi:call_types.abi () in
  let* contract_address, _ =
    send_transaction_to_sequencer
      (fun () ->
        Eth_cli.deploy
          ~source_private_key:sender.Eth_account.private_key
          ~endpoint
          ~abi:call_types.label
          ~bin:call_types.bin)
      sequencer
  in
  let* _ = produce_block sequencer in
  let* tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.contract_send
         ~source_private_key:sender.private_key
         ~endpoint
         ~abi_label:call_types.label
         ~address:contract_address
         ~method_call:"testProduceOpcodes()")
      sequencer
  in
  let* _ = produce_block sequencer in
  let*@ trace_result =
    Rpc.trace_transaction
      ~tracer:"callTracer"
      ~transaction_hash:tx_hash
      ~tracer_config:[("withLog", `Bool false); ("onlyTopCall", `Bool false)]
      sequencer
  in
  let call_list = JSON.(trace_result |-> "calls" |> as_list) in
  (* Initial call is of type CALL *)
  Check.(
    (JSON.(trace_result |-> "type" |> as_string) = "CALL")
      string
      ~error_msg:"Wrong type, expected %R but got %L") ;
  (* The call list should be of size 6, we produce the following opcodes:
     CREATE / CREATE2 / CALL / DELEGATECALL / STATICCALL / CALLCODE *)
  Check.(
    (List.length call_list = 6)
      int
      ~error_msg:"Wrong call list size, expected %R but got %L") ;
  List.iteri
    (fun position call ->
      match position with
      | 0 ->
          Check.(
            (JSON.(call |-> "type" |> as_string) = "CREATE")
              string
              ~error_msg:"Wrong type, expected %R but got %L")
      | 1 ->
          Check.(
            (JSON.(call |-> "type" |> as_string) = "CREATE2")
              string
              ~error_msg:"Wrong type, expected %R but got %L")
      | 2 ->
          Check.(
            (JSON.(call |-> "type" |> as_string) = "CALL")
              string
              ~error_msg:"Wrong type, expected %R but got %L")
      | 3 ->
          Check.(
            (JSON.(call |-> "type" |> as_string) = "DELEGATECALL")
              string
              ~error_msg:"Wrong type, expected %R but got %L")
      | 4 ->
          Check.(
            (JSON.(call |-> "type" |> as_string) = "STATICCALL")
              string
              ~error_msg:"Wrong type, expected %R but got %L")
      | 5 ->
          Check.(
            (JSON.(call |-> "type" |> as_string) = "CALLCODE")
              string
              ~error_msg:"Wrong type, expected %R but got %L")
      | _ -> failwith "Impossible case, call list's size should be 6")
    call_list ;
  unit

let test_trace_transaction_call_tracer_with_logs =
  register_all
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call_trace"; "with_logs"]
    ~title:"debug_traceTransaction with calltracer can produce a call with logs"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* simple_logger = Solidity_contracts.simple_logger () in
  let* () =
    Eth_cli.add_abi ~label:simple_logger.label ~abi:simple_logger.abi ()
  in
  let* contract_address, _ =
    send_transaction_to_sequencer
      (fun () ->
        Eth_cli.deploy
          ~source_private_key:sender.Eth_account.private_key
          ~endpoint
          ~abi:simple_logger.label
          ~bin:simple_logger.bin)
      sequencer
  in
  let* _ = produce_block sequencer in
  let value = 251197 in
  let* tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.contract_send
         ~source_private_key:sender.private_key
         ~endpoint
         ~abi_label:simple_logger.label
         ~address:contract_address
         ~method_call:(Format.sprintf "logValue(%d)" value))
      sequencer
  in
  let* _ = produce_block sequencer in
  let*@ trace_result =
    Rpc.trace_transaction
      ~tracer:"callTracer"
      ~transaction_hash:tx_hash
      ~tracer_config:[("withLog", `Bool true)]
      sequencer
  in
  let logs = JSON.(trace_result |-> "logs" |> as_list) in
  Check.(
    (List.length @@ logs = 1)
      int
      ~error_msg:"Wrong logs size, expected %R but got %L") ;
  let log = List.hd logs in
  let topics = JSON.(log |-> "topics" |> as_list) in
  (* The first topic is the selector of the function which isn't relevant for the test.
     We will match the second topic which is the value that was emitted, i.e: 251197. *)
  let topic_value = JSON.(List.nth topics 1 |> as_string) in
  Check.(
    (topic_value = add_0x @@ hex_256_of_int value)
      string
      ~error_msg:"Wrong topic value, expected %R but got %L") ;
  unit

let test_trace_transaction_call_trace_certain_depth =
  register_all
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call_trace"; "depth"]
    ~title:"debug_traceTransaction with calltracer to see diffuclt depth"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* call_tracer_depth = Solidity_contracts.call_tracer_depth () in
  let* () =
    Eth_cli.add_abi ~label:call_tracer_depth.label ~abi:call_tracer_depth.abi ()
  in
  let* contract_address, _ =
    send_transaction_to_sequencer
      (fun () ->
        Eth_cli.deploy
          ~source_private_key:sender.Eth_account.private_key
          ~endpoint
          ~abi:call_tracer_depth.label
          ~bin:call_tracer_depth.bin)
      sequencer
  in
  let* _ = produce_block sequencer in
  let* tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.contract_send
         ~source_private_key:sender.private_key
         ~endpoint
         ~abi_label:call_tracer_depth.label
         ~address:contract_address
         ~method_call:"startDepth()")
      sequencer
  in
  let* _ = produce_block sequencer in
  let*@ trace_result =
    Rpc.trace_transaction
      ~tracer:"callTracer"
      ~transaction_hash:tx_hash
      ~tracer_config:[("withLog", `Bool false); ("onlyTopCall", `Bool false)]
      sequencer
  in
  let check_and_get_subcalls opcode size json_value =
    Check.(
      (JSON.(json_value |-> "type" |> as_string) = opcode)
        string
        ~error_msg:"Wrong type, expected %R but got %L") ;

    let call_list = JSON.(json_value |-> "calls" |> as_list) in
    Check.(
      (List.length call_list = size)
        int
        ~error_msg:"Wrong call list size, expected %R but got %L") ;
    return call_list
  in
  (* The call list should be of size 4, 3 create and one call in depth *)
  let* depth_call_list = check_and_get_subcalls "CALL" 4 trace_result in
  (* Check that in a CREATE we can make a CALL *)
  let first_contract = List.nth depth_call_list 1 in
  let* create_call_list = check_and_get_subcalls "CREATE" 1 first_contract in
  let call_in_create = List.hd create_call_list in
  let* _ = check_and_get_subcalls "CALL" 0 call_in_create in
  (* Now check the last CALL of the first list that makes multiple calls
     in depth *)
  let depth_0 = List.nth depth_call_list 3 in
  let* depth_1 = check_and_get_subcalls "CALL" 1 depth_0 in
  let* depth_2 = check_and_get_subcalls "CALL" 2 (List.hd depth_1) in
  let depth_2_1, depth_2_2 = (List.nth depth_2 0, List.nth depth_2 0) in
  (* first branch *)
  let* depth_3_1 = check_and_get_subcalls "CALL" 1 depth_2_1 in
  let* _ = check_and_get_subcalls "CALL" 0 (List.hd depth_3_1) in
  (* second branch *)
  let* depth_3_2 = check_and_get_subcalls "CALL" 1 depth_2_2 in
  let* _ = check_and_get_subcalls "CALL" 0 (List.hd depth_3_2) in
  unit

let test_trace_transaction_call_trace_revert =
  register_all
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call_trace"; "revert"]
    ~title:"debug_traceTransaction with calltracer to see how revert is handled"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* call_tracer_revert = Solidity_contracts.call_tracer_revert () in
  let* () =
    Eth_cli.add_abi
      ~label:call_tracer_revert.label
      ~abi:call_tracer_revert.abi
      ()
  in
  let* contract_address, _ =
    send_transaction_to_sequencer
      (fun () ->
        Eth_cli.deploy
          ~source_private_key:sender.Eth_account.private_key
          ~endpoint
          ~abi:call_tracer_revert.label
          ~bin:call_tracer_revert.bin)
      sequencer
  in
  let* _ = produce_block sequencer in
  let* tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.contract_send
         ~source_private_key:sender.private_key
         ~endpoint
         ~abi_label:call_tracer_revert.label
         ~address:contract_address
         ~method_call:"startTest()")
      sequencer
  in
  let* _ = produce_block sequencer in
  let*@ trace_result =
    Rpc.trace_transaction
      ~tracer:"callTracer"
      ~transaction_hash:tx_hash
      ~tracer_config:[("withLog", `Bool false); ("onlyTopCall", `Bool false)]
      sequencer
  in
  let check_error_and_revert_reason ~error ?revert_reason call =
    Check.(
      (JSON.(call |-> "type" |> as_string) = "CALL")
        string
        ~error_msg:"Wrong type, expected %R but got %L") ;
    let error_call = JSON.(call |-> "error" |> as_string) in
    Check.(
      (error_call = error)
        string
        ~error_msg:"Wrong error for call, expected %R but got %L") ;
    match revert_reason with
    | None -> ()
    | Some revert_reason ->
        let revert_reason_call = JSON.(call |-> "revert_reason" |> as_string) in
        Check.(
          (revert_reason_call = revert_reason)
            string
            ~error_msg:"Wrong revert_reason for call, expected %R but got %L")
  in
  let call_list = JSON.(trace_result |-> "calls" |> as_list) in
  (* Initial call is of type CALL *)
  Check.(
    (JSON.(trace_result |-> "type" |> as_string) = "CALL")
      string
      ~error_msg:"Wrong type, expected %R but got %L") ;
  (* The call list should be of size 5, 4 call and one create *)
  Check.(
    (List.length call_list = 5)
      int
      ~error_msg:"Wrong call list size, expected %R but got %L") ;
  let call = List.nth call_list 0 in
  check_error_and_revert_reason
    ~error:"execution reverted"
    ~revert_reason:"Expected message for explicit revert"
    call ;
  (* Skip item 1 in call_list, it's a create *)
  let call = List.nth call_list 2 in
  let call = List.hd JSON.(call |-> "calls" |> as_list) in
  check_error_and_revert_reason ~error:"OutOfGas" call ;
  let call = List.nth call_list 3 in
  check_error_and_revert_reason ~error:"InvalidCode(Opcode(254))" call ;
  let call = List.nth call_list 4 in
  check_error_and_revert_reason
    ~error:"execution reverted"
    ~revert_reason:"Expected message for explicit assert false"
    call ;
  unit

(* The test checks that each call is traced separately, not causing an error
   having multiple top calls. *)
let test_trace_transaction_calltracer_multiple_txs =
  register_all
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call_trace"; "multi_txs"]
    ~title:
      "debug_traceTransaction handles blocks containing several transactions"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let sender_0 = Eth_account.bootstrap_accounts.(0) in
  let sender_1 = Eth_account.bootstrap_accounts.(1) in
  let* call_types = Solidity_contracts.call_types () in
  let* () = Eth_cli.add_abi ~label:call_types.label ~abi:call_types.abi () in
  let* address, _ =
    send_transaction_to_sequencer
      (fun () ->
        Eth_cli.deploy
          ~source_private_key:sender_0.Eth_account.private_key
          ~endpoint
          ~abi:call_types.label
          ~bin:call_types.bin)
      sequencer
  in
  let* raw_tx_0 =
    Cast.craft_tx
      ~chain_id:1337
      ~source_private_key:sender_0.private_key
      ~nonce:1
      ~gas_price:1_000_000_000
      ~gas:300_000
      ~value:Wei.zero
      ~address
      ~signature:"testProduceOpcodes()"
      ()
  in
  let* raw_tx_1 =
    Cast.craft_tx
      ~chain_id:1337
      ~source_private_key:sender_1.private_key
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:300_000
      ~value:Wei.zero
      ~address
      ~signature:"testProduceOpcodes()"
      ()
  in
  let*@ transaction_hash_0 =
    Rpc.send_raw_transaction ~raw_tx:raw_tx_0 sequencer
  in
  let*@ transaction_hash_1 =
    Rpc.send_raw_transaction ~raw_tx:raw_tx_1 sequencer
  in
  let*@ size = produce_block sequencer in
  Check.((size = 2) int)
    ~error_msg:"Expected 2 transactions in the block, got %L" ;
  let*@ _ =
    Rpc.trace_transaction
      ~tracer:"callTracer"
      ~transaction_hash:transaction_hash_0
      ~tracer_config:[("withLog", `Bool false); ("onlyTopCall", `Bool false)]
      sequencer
  in
  let*@ _ =
    Rpc.trace_transaction
      ~tracer:"callTracer"
      ~transaction_hash:transaction_hash_1
      ~tracer_config:[("withLog", `Bool false); ("onlyTopCall", `Bool false)]
      sequencer
  in
  (* At this point, the two RPCs succeeded otherwise we'd get an error about having
     multiple top calls. *)
  unit

let test_trace_transaction_calltracer_on_simple_transfer =
  register_all
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call_trace"; "simple_transfer"]
    ~title:"debug_traceTransaction can trace a simple transfer"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let receiver = Eth_account.bootstrap_accounts.(1) in
  let* transaction_hash =
    send_transaction_to_sequencer
      (Eth_cli.transaction_send
         ~source_private_key:sender.private_key
         ~to_public_key:receiver.address
         ~value:(Wei.of_eth_int 10)
         ~endpoint)
      sequencer
  in
  let* _ = produce_block sequencer in
  let*@ _ =
    Rpc.trace_transaction
      ~tracer:"callTracer"
      ~transaction_hash
      ~tracer_config:[("withLog", `Bool false); ("onlyTopCall", `Bool false)]
      sequencer
  in
  unit

let test_trace_transaction_calltracer_precompiles =
  register_all
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call_trace"; "precompiles"]
    ~title:"debug_traceTransaction with calltracer can trace precompiles"
    ~da_fee:Wei.zero
    ~maximum_allowed_ticks:100_000_000_000_000L
    ~time_between_blocks:Nothing
  @@ fun {sequencer; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* precompiles = Solidity_contracts.precompiles () in
  let* () = Eth_cli.add_abi ~label:precompiles.label ~abi:precompiles.abi () in
  let* contract_address, _ =
    send_transaction_to_sequencer
      (fun () ->
        Eth_cli.deploy
          ~source_private_key:sender.Eth_account.private_key
          ~endpoint
          ~abi:precompiles.label
          ~bin:precompiles.bin)
      sequencer
  in
  let* _ = produce_block sequencer in
  let* transaction_hash =
    send_transaction_to_sequencer
      (Eth_cli.contract_send
         ~source_private_key:sender.private_key
         ~endpoint
         ~abi_label:precompiles.label
         ~address:contract_address
         ~method_call:"callPrecompiles()")
      sequencer
  in
  let* _ = produce_block sequencer in
  let*@ trace_result =
    Rpc.trace_transaction
      ~tracer:"callTracer"
      ~transaction_hash
      ~tracer_config:[("withLog", `Bool false); ("onlyTopCall", `Bool false)]
      sequencer
  in
  let call_list = JSON.(trace_result |-> "calls" |> as_list) in
  Check.(
    (JSON.(trace_result |-> "type" |> as_string) = "CALL")
      string
      ~error_msg:"Wrong type, expected %R but got %L") ;
  (* All precompiles from 0x00..01 to 0x00..09. *)
  Check.(
    (List.length call_list = 9)
      int
      ~error_msg:"Wrong call list size, expected %R but got %L") ;
  List.iteri
    (fun position call ->
      match position with
      | 0 ->
          Check.(
            (JSON.(call |-> "to" |> as_string)
            = "0x0000000000000000000000000000000000000001")
              string
              ~error_msg:"Wrong precompiled contract, expected %R but got %L")
      | 1 ->
          Check.(
            (JSON.(call |-> "to" |> as_string)
            = "0x0000000000000000000000000000000000000002")
              string
              ~error_msg:"Wrong precompiled contract, expected %R but got %L")
      | 2 ->
          Check.(
            (JSON.(call |-> "to" |> as_string)
            = "0x0000000000000000000000000000000000000003")
              string
              ~error_msg:"Wrong precompiled contract, expected %R but got %L")
      | 3 ->
          Check.(
            (JSON.(call |-> "to" |> as_string)
            = "0x0000000000000000000000000000000000000004")
              string
              ~error_msg:"Wrong precompiled contract, expected %R but got %L")
      | 4 ->
          Check.(
            (JSON.(call |-> "to" |> as_string)
            = "0x0000000000000000000000000000000000000005")
              string
              ~error_msg:"Wrong precompiled contract, expected %R but got %L")
      | 5 ->
          Check.(
            (JSON.(call |-> "to" |> as_string)
            = "0x0000000000000000000000000000000000000006")
              string
              ~error_msg:"Wrong precompiled contract, expected %R but got %L")
      | 6 ->
          Check.(
            (JSON.(call |-> "to" |> as_string)
            = "0x0000000000000000000000000000000000000007")
              string
              ~error_msg:"Wrong precompiled contract, expected %R but got %L")
      | 7 ->
          Check.(
            (JSON.(call |-> "to" |> as_string)
            = "0x0000000000000000000000000000000000000008")
              string
              ~error_msg:"Wrong precompiled contract, expected %R but got %L")
      | 8 ->
          Check.(
            (JSON.(call |-> "to" |> as_string)
            = "0x0000000000000000000000000000000000000009")
              string
              ~error_msg:"Wrong precompiled contract, expected %R but got %L")
      | _ -> failwith "Impossible case, call list's size should be 9")
    call_list ;
  unit

let test_trace_transaction_calltracer_deposit =
  register_all
    ~kernels:[Latest]
    ~time_between_blocks:Nothing
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "rpc"; "trace"; "call_trace"; "deposit"]
    ~title:"debug_traceTransaction with calltracer can trace deposits"
  @@ fun {
           client;
           l1_contracts;
           sc_rollup_address;
           sc_rollup_node;
           sequencer;
           proxy;
           _;
         }
             _protocol ->
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
    Eth_cli.balance ~account:receiver.address ~endpoint
  in
  let* () =
    send_deposit_to_delayed_inbox
      ~amount
      ~l1_contracts
      ~depositor
      ~receiver:receiver.address
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
  let*@ block = Rpc.get_block_by_number ~block:"latest" sequencer in
  let transaction_hash =
    match block.transactions with
    | Hash txs -> List.hd txs
    | Full _ | Empty ->
        failwith "Block should contain at least one simple transaction"
  in
  let* () = bake_until_sync ~sc_rollup_node ~proxy ~sequencer ~client () in
  let* receiver_balance_next =
    Eth_cli.balance ~account:receiver.address ~endpoint
  in
  Check.((receiver_balance_next > receiver_balance_prev) Wei.typ)
    ~error_msg:"Expected a bigger balance" ;
  (* The following tracing should succeed. *)
  let*@ _ =
    Rpc.trace_transaction
      ~tracer:"callTracer"
      ~transaction_hash
      ~tracer_config:[("withLog", `Bool false); ("onlyTopCall", `Bool false)]
      sequencer
  in
  unit

let test_miner =
  let sequencer_pool_address =
    String.lowercase_ascii "0x8aaD6553Cf769Aa7b89174bE824ED0e53768ed70"
  in
  register_all
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/7285
     Replace by [Any] after the next upgrade *)
    ~kernels:[Latest]
    ~tags:["evm"; "miner"; "coinbase"]
    ~title:"Sequencer pool address is the block's miner"
    ~sequencer_pool_address
  @@ fun {sequencer; _} _protocol ->
  let*@ block = Rpc.get_block_by_number ~block:"latest" sequencer in
  Check.((String.lowercase_ascii block.miner = sequencer_pool_address) string)
    ~error_msg:
      "Block miner should be the sequencer pool address, expected %R got %L" ;
  (* We deploy a contract that stores the block coinbase in its storage, and
     also has a view to get the block coinbase. *)
  let* coinbase_resolved = Solidity_contracts.coinbase () in
  let* () =
    Eth_cli.add_abi ~label:coinbase_resolved.label ~abi:coinbase_resolved.abi ()
  in
  let* contract, _tx_hash =
    send_transaction_to_sequencer
      (fun () ->
        Eth_cli.deploy
          ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
          ~endpoint:(Evm_node.endpoint sequencer)
          ~abi:coinbase_resolved.abi
          ~bin:coinbase_resolved.bin)
      sequencer
  in
  let* storage_coinbase =
    Eth_cli.contract_call
      ~endpoint:(Evm_node.endpoint sequencer)
      ~abi_label:coinbase_resolved.label
      ~address:contract
      ~method_call:"getStorageCoinbase()"
      ()
  in
  Check.(
    (String.lowercase_ascii @@ String.trim storage_coinbase
    = sequencer_pool_address)
      string)
    ~error_msg:
      "Stored coinbase should be the sequencer pool address, expected %R got %L" ;
  let* view_coinbase =
    Eth_cli.contract_call
      ~endpoint:(Evm_node.endpoint sequencer)
      ~abi_label:coinbase_resolved.label
      ~address:contract
      ~method_call:"getStorageCoinbase()"
      ()
  in
  Check.(
    (String.lowercase_ascii @@ String.trim view_coinbase
    = sequencer_pool_address)
      string)
    ~error_msg:
      "Viewed coinbase should be the sequencer pool address, expected %R got %L" ;

  let*@ rpc_coinbase = Rpc.coinbase sequencer in
  Check.((rpc_coinbase = sequencer_pool_address) string)
    ~error_msg:
      "eth_coinbase should be the sequencer pool address, expected %R got %L" ;

  unit

let test_fa_bridge_feature_flag =
  register_all
    ~tags:["fa_bridge"; "feature_flag"]
    ~title:"FA bridge feature is set in storage"
    ~enable_fa_bridge:true
  @@ fun {sequencer; _} _protocol ->
  (* We simply check that the flag is set in the storage. *)
  let*@ flag =
    Rpc.state_value sequencer Durable_storage_path.enable_fa_bridge
  in
  Check.is_true
    (Option.is_some flag)
    ~error_msg:
      (sf
         "Expected to have a value at %s"
         Durable_storage_path.enable_fa_bridge) ;
  unit

let test_trace_call =
  register_all
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call"]
    ~title:"Sequencer can run debug_traceCall and return a valid log"
    ~da_fee:Wei.zero
  @@ fun {sc_rollup_node; sequencer; client; proxy = _; _} _protocol ->
  (* Start a RPC node as well, since we will want to check it returns the
     same result as the sequencer *)
  let* rpc_node = run_new_rpc_endpoint sequencer in
  (* Transfer funds to a random address. *)
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* simple_storage_resolved = Solidity_contracts.simple_storage () in

  (* deploy contract *)
  let* () =
    Eth_cli.add_abi
      ~label:simple_storage_resolved.label
      ~abi:simple_storage_resolved.abi
      ()
  in
  let* contract_address, _tx_deployment =
    send_transaction_to_sequencer
      (fun () ->
        Eth_cli.deploy
          ~source_private_key:sender.Eth_account.private_key
          ~endpoint
          ~abi:simple_storage_resolved.label
          ~bin:simple_storage_resolved.bin)
      sequencer
  in
  let* () =
    repeat 2 (fun () ->
        next_evm_level ~evm_node:sequencer ~sc_rollup_node ~client)
  in

  let value_in_storage = 10 in
  let* _ =
    send_transaction_to_sequencer
      (Eth_cli.contract_send
         ~source_private_key:sender.private_key
         ~endpoint
         ~abi_label:simple_storage_resolved.label
         ~address:contract_address
         ~method_call:(Format.sprintf "set(%d)" value_in_storage))
      sequencer
  in
  let* () =
    repeat 2 (fun () ->
        next_evm_level ~evm_node:sequencer ~sc_rollup_node ~client)
  in

  let* abi_string =
    Eth_cli.encode_method
      ~abi_label:simple_storage_resolved.label
      ~method_:"get()"
  in

  let*@ trace =
    Rpc.trace_call
      ~block:Latest
      ~to_:contract_address
      ~data:abi_string
      ~tracer_config:
        [("enableMemory", `Bool true); ("enableReturnData", `Bool true)]
      sequencer
  in
  let*@ trace_rpc =
    Rpc.trace_call
      ~block:Latest
      ~to_:contract_address
      ~data:abi_string
      ~tracer_config:
        [("enableMemory", `Bool true); ("enableReturnData", `Bool true)]
      rpc_node
  in

  assert (JSON.equal trace trace_rpc) ;

  let logs = JSON.(trace |-> "structLogs" |> as_list) in
  let returned_value =
    JSON.(trace |-> "returnValue" |> as_string |> Durable_storage_path.no_0x)
  in
  Check.((logs <> []) (list json) ~error_msg:"Logs shouldn't be empty") ;
  Check.(
    (returned_value
   = "000000000000000000000000000000000000000000000000000000000000000a")
      string
      ~error_msg:"Expect return value to be %R, got %L") ;
  List.iter (check_struct_logs false) logs ;

  let*@ trace_failed_transaction =
    Rpc.trace_call
      ~block:(Number 0)
      ~to_:contract_address
      ~data:abi_string
      ~tracer_config:
        [("enableMemory", `Bool true); ("enableReturnData", `Bool true)]
      sequencer
  in
  Check.(
    JSON.(
      trace_failed_transaction |-> "returnValue" |> as_string
      |> Durable_storage_path.no_0x = "")
      string
      ~error_msg:"Expect return value to be empty, got %L") ;

  unit

let test_patch_kernel =
  register_all
    ~kernels:[Mainnet]
    ~tags:["evm"; "patch_kernel"; "experimental"]
    ~title:"Can patch the kernel of an existing node"
    ~additional_uses:[Constant.WASM.ghostnet_evm_kernel]
    ~da_fee:Wei.zero
  @@ fun {sequencer; _} _protocol ->
  let* _ =
    check_kernel_version
      ~evm_node:sequencer
      ~equal:true
      Constant.WASM.mainnet_evm_commit
  in
  let* () = Evm_node.terminate sequencer in
  let* () =
    Evm_node.patch_kernel
      sequencer
      Uses.(path Constant.WASM.ghostnet_evm_kernel)
  in
  let* () = Evm_node.run sequencer in
  (* Produce a block so that the migration code is executed *)
  let* _ = produce_block sequencer in
  let* _ =
    check_kernel_version
      ~evm_node:sequencer
      ~equal:true
      Constant.WASM.ghostnet_evm_commit
  in
  unit

let test_finalized_view =
  register_all
    ~kernels:
      [
        Latest
        (* This test focuses on a feature that purely relies on the node, it does not makes sense to register it for every protocols *);
      ]
    ~tags:["evm"; "finalized_view"]
    ~time_between_blocks:Nothing
    ~title:"--finalized-view returns the latest final block of the sequencer"
    ~da_fee:Wei.zero
      (* This test depends on the number of blocks produced until the
         rollup node synchronizes with the sequencer and proxy. When
         DAL is activated, bake_until_sync would need to produce more
         blocks than usual. Consequently, this would finalize the
         blocks posted on-chain. The finalized proxy does have a head,
         and the RPC does not fail, which is not the expected result
         when running this test without DAL activation. *)
    ~use_dal:Register_without_feature
  @@ fun {sc_rollup_node; client; sequencer; proxy; _} _protocol ->
  (* Start a proxy node with --finalized-view enabled *)
  let* finalized_proxy =
    Evm_node.init
      ~patch_config:
        JSON.(
          fun json ->
            put ("finalized_view", annotate ~origin:"" (`Bool true)) json)
      ~mode:Proxy
      (Sc_rollup_node.endpoint sc_rollup_node)
  in
  let* finalized_observer =
    run_new_observer_node
      ~patch_config:
        JSON.(
          fun json ->
            put ("finalized_view", annotate ~origin:"" (`Bool true)) json)
      ~sc_rollup_node
      sequencer
  in
  (* Produce a few EVM blocks *)
  let* _ =
    repeat 4 @@ fun () ->
    let* _ = produce_block sequencer in
    unit
  in
  (* Produces two L1 blocks to ensure the L2 blocks are posted onchain by the sequencer *)
  let* () =
    bake_until_sync ~__LOC__ ~sc_rollup_node ~proxy ~sequencer ~client ()
  in
  (* Check the heads of the various nodes *)
  let*@ sequencer_head = Rpc.block_number sequencer in
  let*@ proxy_head = Rpc.block_number proxy in

  Check.((sequencer_head = 4l) int32)
    ~error_msg:"Sequencer head should be %R, but is %L instead" ;
  Check.((proxy_head = sequencer_head) int32)
    ~error_msg:
      "Regular proxy head should be equal to sequencer head (%R), but is %L \
       instead" ;
  (* While the blocks were posted onchain, they are not final wrt. the
     consensus algorithm, so the finalized proxy does not have a head yet. *)
  let*@? _ = Rpc.block_number finalized_proxy in

  (* By default the stateful node assume the block 0 is finalized *)
  let*@ finalized_block = Rpc.block_number finalized_observer in
  Check.((finalized_block = 0l) int32)
    ~error_msg:
      "The stateful node should assume the block 0 is finalized, but %L is \
       returned" ;

  (* We produce more L1 blocks to finalize the L2 blocks and to give some time
     to the EVM nodes to catch-up. *)
  let* _ =
    Evm_node.wait_for_blueprint_finalized
      finalized_observer
      (Int32.to_int sequencer_head)
  and* _ =
    repeat 3 @@ fun () ->
    let* _ = next_rollup_node_level ~sc_rollup_node ~client in
    unit
  in
  let*@ finalized_proxy_block = Rpc.block_number finalized_proxy in
  let*@ finalized_observer_block = Rpc.block_number finalized_observer in
  Check.((finalized_proxy_block = sequencer_head) int32)
    ~error_msg:
      "Finalized proxy head should be equal to sequencer head (%R), but is %L \
       instead" ;
  Check.((finalized_observer_block = sequencer_head) int32)
    ~error_msg:
      "Finalized observer head should be equal to sequencer head (%R), but is \
       %L instead" ;
  unit

let test_finalized_block_param =
  register_all
    ~time_between_blocks:Nothing
    ~kernels:
      [
        Latest
        (* This test focuses on a feature that purely relies on the node, it does not makes sense to register it for every protocols *);
      ]
    ~tags:["evm"; "finalized_block_param"]
    ~title:
      "The finalized block parameter is correctly interpreted by the EVM node"
    ~da_fee:Wei.zero
  @@ fun {sc_rollup_node; client; sequencer; proxy; _} _protocol ->
  let* rpc = run_new_rpc_endpoint sequencer in
  (* Produce a few EVM blocks *)
  let* () =
    repeat 4 @@ fun () ->
    let* _ = produce_block sequencer in
    unit
  in
  let* () =
    bake_until_sync ~__LOC__ ~sc_rollup_node ~client ~sequencer ~proxy ()
  in
  (* Check that the L2 blocks where indeed posted onchain. *)
  let*@ sequencer_head = Rpc.get_block_by_number ~block:"latest" sequencer in
  Check.((sequencer_head.number = 4l) int32)
    ~error_msg:"Sequencer head should be %R, but is %L instead" ;
  let* () =
    check_head_consistency
      ~left:proxy
      ~right:sequencer
      ~error_msg:"Sequencer and proxy should have the same head"
      ()
  in
  (* While the blocks were posted onchain, they are not final wrt. the
     consensus algorithm, so the finalized proxy does not have a head yet.

     We produce both two more L2 blocks and two L1 blocks; the latter will
     allow to finalized the first four blocks posted earlier. *)
  let* () =
    repeat 2 @@ fun () ->
    let* _ = next_rollup_node_level ~sc_rollup_node ~client in
    let* _ = produce_block sequencer in
    unit
  in
  (* Produces two L1 blocks to ensure the L2 blocks are posted onchain by the sequencer *)
  let* () =
    bake_until_sync ~__LOC__ ~sc_rollup_node ~client ~sequencer ~proxy ()
  in
  (* We can check the consistency of the various nodes. *)
  let*@ sequencer_new_head =
    Rpc.get_block_by_number ~block:"latest" sequencer
  in
  let*@ sequencer_finalized_head =
    Rpc.get_block_by_number ~block:"finalized" sequencer
  in

  Check.((sequencer_new_head.number = 6l) int32)
    ~error_msg:"Sequencer head should be %R, but is %L instead" ;
  Check.((sequencer_finalized_head.number = 4l) int32)
    ~error_msg:"Sequencer finalized head should be %R, but is %L instead" ;

  let* () =
    check_head_consistency
      ~left:proxy
      ~right:sequencer
      ~error_msg:"Sequencer and proxy should have the same head"
      ()
  in
  let* () =
    check_block_consistency
      ~block:`Finalized
      ~left:proxy
      ~right:sequencer
      ~error_msg:"Sequencer and proxy should have the same last finalized"
      ()
  in
  let* () =
    check_block_consistency
      ~block:`Finalized
      ~left:rpc
      ~right:sequencer
      ~error_msg:"Sequencer and rpc should have the same last finalized"
      ()
  in

  (* Terminate the sequencer. *)
  let* () = Evm_node.terminate sequencer in
  (* Restart it. *)
  let* () = Evm_node.run sequencer in

  let* () =
    check_block_consistency
      ~block:`Finalized
      ~left:proxy
      ~right:sequencer
      ~error_msg:
        "Sequencer and proxy should have the same last finalized after \
         sequencer reboot"
      ()
  in

  unit

let test_regression_block_hash_gen =
  (* This test is created because of bug in blockConstant in simulation,
     which caused the simulation to return a wrong estimate of gas limit,
     leading to failed contract deployment for block_hash_gen.
     This test checks regression for the fix *)
  let timestamp = "2020-01-01T00:00:05Z" in
  register_all
    ~time_between_blocks:Nothing
    ~tags:["evm"; "l2_call"; "block_hash"; "timestamp"]
    ~title:"Random generation based on block hash and timestamp"
    ~genesis_timestamp:Client.(At (Time.of_notation_exn timestamp))
  @@ fun {sequencer; _} _protocol ->
  let* () =
    repeat 3 (fun _ ->
        let*@ _ = produce_block sequencer ~timestamp in
        unit)
  in
  let* {abi; bin; _} = Solidity_contracts.block_hash_gen () in
  let* _contract, _tx_hash =
    send_transaction_to_sequencer
      (fun () ->
        Eth_cli.deploy
          ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
          ~endpoint:(Evm_node.endpoint sequencer)
          ~abi
          ~bin)
      sequencer
  in
  unit

(* Self test for sequencer in sandbox mode. *)
let test_sequencer_sandbox () =
  Test.register
    ~__FILE__
    ~title:"Sequencer in sandbox mode"
    ~tags:["sequencer"; "sandbox"]
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
  let* sequencer = init_sequencer_sandbox () in
  let*@ _ = produce_block sequencer in
  let* tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.transaction_send
         ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
         ~to_public_key:"0xB7A97043983f24991398E5a82f63F4C58a417185"
         ~value:(Wei.of_eth_int 10)
         ~endpoint:(Evm_node.endpoint sequencer))
      sequencer
  in
  let*@! _receipt = Rpc.get_transaction_receipt ~tx_hash sequencer in
  unit

let test_rpc_mode_while_block_are_produced =
  register_all
    ~title:"rpc node can respond to rpcs without disturbing the sequencer."
    ~tags:["rpc_mode"; Tag.ci_disabled]
    ~time_between_blocks:Nothing
  @@ fun {sequencer; _} _protocol ->
  (* The goal of this test is to prove that a RPC node running alongside a
     read-write node like a sequencer works as expected. To that end, we
     simulate calling various RPCs while blocks are being produced by the
     sequencer.

     In the very first versions of the RPC node, when the EVM node was not
     ready at all to accept concurrent read-only access to the SQLite and Irmin
     stores, the test was failing, demonstrating that it was capturing the
     expected behavior. *)
  let* rpc_node = run_new_rpc_endpoint sequencer in

  let latest_block_number = ref 0l in

  let* () =
    repeat 5 @@ fun () ->
    let* _ =
      Lwt.all
        [
          (* The idea is to show that we can create a block (in our case, with
             one transaction) while we do a bunch of read-only RPCs calls. *)
          (let* _tx_hash =
             send_transaction_to_sequencer
               (Eth_cli.transaction_send
                  ~source_private_key:
                    Eth_account.bootstrap_accounts.(0).private_key
                  ~to_public_key:Eth_account.bootstrap_accounts.(1).address
                  ~value:(Wei.of_eth_int 10)
                  ~endpoint:(Evm_node.endpoint sequencer))
               sequencer
           in
           unit);
          (* For instance, we show it is possible to read the current kernel
             version (simple read in the EVM state of the head). *)
          ( repeat 5 @@ fun () ->
            let*@ _kernel_version = Rpc.tez_kernelVersion rpc_node in
            Lwt_unix.sleep 0.1 );
          (* Similarly, we fetch the block number of the current head several
             time. *)
          ( repeat 5 @@ fun () ->
            let*@ block_number = Rpc.block_number rpc_node in
            latest_block_number := block_number ;
            Lwt_unix.sleep 0.1 );
          (* And fetch the contents of the block before the head. *)
          ( repeat 5 @@ fun () ->
            let requested_block =
              Int32.max 0l (Int32.sub !latest_block_number 1l)
            in
            let*@ _ =
              Rpc.get_block_by_number
                ~full_tx_objects:true
                ~block:(Int32.to_string requested_block)
                rpc_node
            in
            Lwt_unix.sleep 0.1 );
        ]
    in
    unit
  in
  unit

let test_batch_limit_size_rpc =
  register_all
    ~bootstrap_accounts:Eth_account.lots_of_address
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
  (* Considering the choosen limit (5), we prepare two batches of transactions.
     The first one with the first three transactions of our tx file, the second
     one with the 10 ones that comes after.*)
  let below_limit_txs =
    List.filteri (fun i _ -> i < 3) all_txs |> List.map fst
  in
  let above_limit_txs =
    List.filteri (fun i _ -> 3 <= i && i < 13) all_txs |> List.map fst
  in

  (* We first check that we can inject the first batch. *)
  let* _ = Helpers.batch_n_transactions ~evm_node:sequencer below_limit_txs in

  (* We then demonstrate that we cannot inject the second batch. *)
  let* has_failed =
    Lwt.catch
      (fun () ->
        let* _ =
          Helpers.batch_n_transactions ~evm_node:sequencer above_limit_txs
        in
        return false)
      (fun _ -> return true)
  in

  if not has_failed then
    Test.fail "Large batch exceeding the limit should have failed." ;

  (* We restart the sequencer, and explicitely lift the limit. *)
  let* () = Evm_node.terminate sequencer in
  let* () =
    Evm_node.run ~extra_arguments:["--rpc-batch-limit"; "unlimited"] sequencer
  in

  (* We demonstrate it is now possible to inject the batch. *)
  let* _ = Helpers.batch_n_transactions ~evm_node:sequencer above_limit_txs in

  unit

let test_debug_print_store_schemas () =
  Regression.register
    ~__FILE__
    ~title:"EVM Node: debug print store schemas"
    ~tags:["evm"; "store"; "schemas"]
    ~uses:[Constant.octez_evm_node]
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun () ->
  let hooks = Tezos_regression.hooks in
  Evm_node.debug_print_store_schemas ~hooks ()

let test_man () =
  Regression.register
    ~__FILE__
    ~title:"EVM Node: man"
    ~tags:["evm"; "man"]
    ~uses:[Constant.octez_evm_node]
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun () ->
  let hooks = Tezos_regression.hooks in
  Evm_node.man ~hooks ()

let test_describe_config () =
  Regression.register
    ~__FILE__
    ~title:"EVM Node: describe config"
    ~tags:["evm"; "config"]
    ~uses:[Constant.octez_evm_node]
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun () ->
  let hooks = Tezos_regression.hooks in
  Evm_node.describe_config ~hooks ()

let test_relay_restricted_rpcs =
  register_all
    ~bootstrap_accounts:Eth_account.lots_of_address
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
    ~time_between_blocks:Nothing
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "relay"]
    ~title:"Relay restricted RPC only accept eth_sendRawTransaction"
    ~da_fee:Wei.zero
  @@ fun {sequencer; _} _protocol ->
  let* () = Evm_node.terminate sequencer in
  let* () =
    Evm_node.run
      ~extra_arguments:["--whitelisted-rpcs"; "eth_sendRawTransaction"]
      sequencer
  in
  let raw_tx, _ = read_tx_from_file () |> List.hd in
  let*@ _ = Rpc.send_raw_transaction ~raw_tx sequencer in
  let*@? _ = Rpc.get_block_by_number ~block:"0x0" sequencer in
  let*@? _ = Rpc.tez_kernelVersion sequencer in
  unit

let test_tx_pool_replacing_transactions =
  register_all
    ~time_between_blocks:Nothing
    ~kernels:[Latest] (* Not a kernel specific test. *)
    ~tags:["evm"; "tx_pool"]
    ~title:"Transactions can be replaced"
  @@ fun {sequencer; _} _protocol ->
  let* gas_price = Rpc.get_gas_price sequencer in
  let gas_price = Int32.to_int gas_price in
  let* tx_a =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price
      ~gas:21_000
      ~value:(Wei.of_eth_int 10)
      ~address:"0x0000000000000000000000000000000000000000"
      ()
  in
  let* tx_b =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:(gas_price + 1)
      ~gas:21_000
      ~value:(Wei.of_eth_int 5)
      ~address:"0x0000000000000000000000000000000000000000"
      ()
  in
  (* Send the transactions to the proxy*)
  let*@ tx_a_hash = Rpc.send_raw_transaction ~raw_tx:tx_a sequencer in
  let*@ tx_b_hash = Rpc.send_raw_transaction ~raw_tx:tx_b sequencer in
  (* Get the transaction objects, if it has been replaced, [tx_a_hash] will
     be missing. *)
  let*@ tx_a =
    Rpc.get_transaction_by_hash ~transaction_hash:tx_a_hash sequencer
  in
  let*@ tx_b =
    Rpc.get_transaction_by_hash ~transaction_hash:tx_b_hash sequencer
  in
  Check.is_true
    (Option.is_none tx_a)
    ~error_msg:"First transaction should have been replaced" ;
  Check.is_true
    (Option.is_some tx_b)
    ~error_msg:"Second transaction should have replaced the previous one" ;
  unit

let test_da_fees_after_execution =
  register_all
    ~time_between_blocks:Nothing
    ~tags:["evm"; "da_fees"; "foo"]
    ~da_fee:(Wei.of_string "4_000_000_000_000")
    ~title:"da fees test"
  @@ fun {sequencer; _} _protocol ->
  (* This is a non-regression test of the situation where the source of
     a transaction is able to prepay for the gas, but after the transfer
     is executed it cannot pay for the DA fees.

     Because obviously DA fees are accounted AFTER the execution.
  *)
  let account =
    Eth_account.
      {
        address = "0xA257edC8ad1D8f8f463aC0D947cc381000b3c863";
        private_key =
          "0xb80e5dd2ba9281e482589973600609bb0f10f6a075e6c733e4472d4dd2df238a";
      }
  in
  let* _ =
    send_transaction_to_sequencer
      (Eth_cli.transaction_send
         ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
         ~to_public_key:account.address
         ~value:(Wei.of_eth_int 1)
         ~endpoint:(Evm_node.endpoint sequencer))
      sequencer
  in

  let* res =
    Lwt.catch
      (fun () ->
        Eth_cli.transaction_send
          ~source_private_key:account.private_key
          ~to_public_key:"0x1074Fd1EC02cbeaa5A90450505cF3B48D834f3EB"
          ~value:(Wei.of_eth_string "0.9999")
          ~endpoint:(Evm_node.endpoint sequencer)
          ())
      (fun _exn -> Lwt.return "")
  in

  Check.((res = "") string) ~error_msg:"The transaction should have failed" ;

  unit

let test_configuration_service =
  Protocol.register_regression_test
    ~__FILE__
    ~tags:["evm"; "rpc"; "configuration"]
    ~title:"Configuration RPC"
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
      ])
  @@ fun protocol ->
  let* {sequencer; proxy; observer; _} =
    Setup.setup_sequencer ~mainnet_compat:false ~enable_dal:false protocol
  in
  let* proxy_config = Rpc.configuration proxy in
  let* sequencer_config = Rpc.configuration sequencer in
  let* observer_config = Rpc.configuration observer in

  let remove_public_rpc_port json =
    JSON.update
      "public_rpc"
      (fun json ->
        JSON.update
          "port"
          (fun _ ->
            JSON.annotate
              ~origin:"remove_fresh_ports"
              (`String "hidden-for-regression-only"))
          json)
      json
  in

  Regression.capture (JSON.encode (remove_public_rpc_port proxy_config)) ;
  Regression.capture (JSON.encode (remove_public_rpc_port @@ sequencer_config)) ;
  Regression.capture (JSON.encode (remove_public_rpc_port @@ observer_config)) ;

  unit

let protocols = Protocol.all

let () =
  test_remove_sequencer protocols ;
  test_persistent_state protocols ;
  test_snapshots protocols ;
  test_patch_state [Protocol.Alpha] ;
  test_publish_blueprints protocols ;
  test_sequencer_too_ahead protocols ;
  test_resilient_to_rollup_node_disconnect protocols ;
  test_can_fetch_smart_rollup_address protocols ;
  test_can_fetch_blueprint protocols ;
  test_send_transaction_to_delayed_inbox protocols ;
  test_send_deposit_to_delayed_inbox protocols ;
  test_rpc_produceBlock protocols ;
  test_get_balance_block_param protocols ;
  test_get_block_by_number_block_param protocols ;
  test_extended_block_param protocols ;
  test_delayed_transfer_is_included protocols ;
  test_delayed_deposit_is_included protocols ;
  test_delayed_fa_deposit_is_included protocols ;
  test_delayed_fa_deposit_is_ignored_if_feature_disabled protocols ;
  test_delayed_transaction_peeked protocols ;
  test_invalid_delayed_transaction protocols ;
  test_fa_withdrawal_is_included protocols ;
  test_largest_delayed_transfer_is_included protocols ;
  test_delayed_deposit_from_init_rollup_node protocols ;
  test_legacy_deposits_dispatched_after_kernel_upgrade protocols ;
  test_init_from_rollup_node_data_dir protocols ;
  test_init_from_rollup_node_with_delayed_inbox protocols ;
  test_observer_applies_blueprint protocols ;
  test_observer_applies_blueprint_dont_track_rollup_node protocols ;
  test_observer_applies_blueprint_from_rpc_node protocols ;
  test_observer_applies_blueprint_when_restarted protocols ;
  test_observer_applies_blueprint_when_sequencer_restarted protocols ;
  test_observer_forwards_transaction protocols ;
  test_observer_timeout_when_necessary protocols ;
  test_sequencer_is_reimbursed protocols ;
  test_empty_block_on_upgrade protocols ;
  test_upgrade_kernel_auto_sync protocols ;
  test_self_upgrade_kernel protocols ;
  test_force_kernel_upgrade protocols ;
  test_force_kernel_upgrade_too_early protocols ;
  test_external_transaction_to_delayed_inbox_fails protocols ;
  test_delayed_transfer_timeout protocols ;
  test_delayed_transfer_timeout_fails_l1_levels protocols ;
  test_forced_blueprint_takes_pred_timestamp protocols ;
  test_forced_blueprint_takes_l1_timestamp protocols ;
  test_delayed_inbox_flushing protocols ;
  test_no_automatic_block_production protocols ;
  test_non_increasing_timestamp protocols ;
  test_timestamp_from_the_future protocols ;
  test_sequencer_upgrade protocols ;
  test_sequencer_diverge protocols ;
  test_sequencer_can_catch_up_on_event protocols ;
  test_sequencer_dont_read_level_twice protocols ;
  test_stage_one_reboot protocols ;
  test_blueprint_is_limited_in_size protocols ;
  test_blueprint_limit_with_delayed_inbox protocols ;
  test_reset protocols ;
  test_preimages_endpoint protocols ;
  test_store_smart_rollup_address protocols ;
  test_replay_rpc protocols ;
  test_trace_transaction protocols ;
  test_trace_transaction_on_invalid_transaction protocols ;
  test_trace_transaction_call protocols ;
  test_miner protocols ;
  test_fa_bridge_feature_flag protocols ;
  test_trace_call protocols ;
  test_patch_kernel protocols ;
  test_finalized_view protocols ;
  test_finalized_block_param protocols ;
  test_regression_block_hash_gen protocols ;
  test_sequencer_sandbox () ;
  test_rpc_mode_while_block_are_produced protocols ;
  test_trace_transaction_call_trace protocols ;
  test_relay_restricted_rpcs protocols ;
  test_batch_limit_size_rpc protocols ;
  test_trace_transaction_calltracer_all_types protocols ;
  test_trace_transaction_call_tracer_with_logs protocols ;
  test_trace_transaction_call_trace_certain_depth protocols ;
  test_trace_transaction_call_trace_revert protocols ;
  test_trace_transaction_calltracer_multiple_txs protocols ;
  test_trace_transaction_calltracer_on_simple_transfer protocols ;
  test_trace_transaction_calltracer_precompiles protocols ;
  test_trace_transaction_calltracer_deposit protocols ;
  test_debug_print_store_schemas () ;
  test_man () ;
  test_describe_config () ;
  test_outbox_size_limit_resilience ~slow:true protocols ;
  test_outbox_size_limit_resilience ~slow:false protocols ;
  test_proxy_node_can_forward_to_evm_endpoint protocols ;
  test_tx_pool_replacing_transactions protocols ;
  test_da_fees_after_execution protocols ;
  test_trace_transaction_calltracer_failed_create protocols ;
  test_configuration_service [Protocol.Alpha] ;
  test_overwrite_simulation_tick_limit protocols
