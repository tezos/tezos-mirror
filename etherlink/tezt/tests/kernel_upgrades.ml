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
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file kernel_upgrades.ml
   Subject:      Kernel upgrades: self, auto-sync, forced, and kernel version checks.
*)

open Sc_rollup_helpers
open Rpc.Syntax
open Delayed_inbox
open Test_helpers
open Setup

let register_upgrade_all ~title ~tags ~genesis_timestamp
    ?(time_between_blocks = Evm_node.Nothing) ?(kernels = Kernel.etherlink_all)
    ?(upgrade_to = Kernel.upgrade_to) ?(additional_uses = []) scenario protocols
    =
  List.iter
    (fun from ->
      let from_tag, _ = Kernel.to_uses_and_tags from in
      let to_ = upgrade_to from in
      let to_tag, to_use = Kernel.to_uses_and_tags to_ in
      register_all
        ~__FILE__
        ~kernels:[from]
        ~genesis_timestamp
        ~time_between_blocks
        ~tags:("upgrade_scenario" :: to_tag :: tags)
        ~title:Format.(sprintf "%s (%s -> %s)" title from_tag to_tag)
        ~additional_uses:(to_use :: additional_uses)
        (scenario from to_)
        protocols)
    kernels

(* Guards against the [Kernel.name_of Mainnet] string drifting out of sync
   with the WASM kernel we actually ship as [Constant.WASM.mainnet_kernel].
   The name is consumed by the EVM node's [--kernel-compat] flag to pick
   the right storage-path / feature gates in [kernel_config.ml]; a
   mismatch is silent at parse time but breaks installer configs the
   moment a gate keyed on the wrong kernel version is introduced. *)
let test_mainnet_kernel_name_matches_root_hash () =
  Test.register
    ~__FILE__
    ~title:"Kernel.name_of Mainnet matches mainnet_kernel.wasm root hash"
    ~tags:["kernel"; "mainnet"; "compat"]
    ~uses_admin_client:false
    ~uses_client:false
    ~uses_node:false
    ~uses:[Constant.WASM.mainnet_kernel; Constant.smart_rollup_installer]
  @@ fun () ->
  let name =
    match Kernel.name_of Mainnet with
    | Some n -> n
    | None -> Test.fail "Kernel.name_of Mainnet returned None"
  in
  let kernel_enum =
    match Evm_node_lib_dev.Constants.kernel_from_string name with
    | Some k -> k
    | None ->
        Test.fail
          "Kernel.name_of Mainnet returned %S, which \
           Constants.kernel_from_string does not recognize"
          name
  in
  let expected_hash =
    match
      Evm_node_lib_dev.Constants.root_hash_from_released_kernel kernel_enum
    with
    | Some (`Hex h) -> h
    | None ->
        Test.fail
          "No released root hash known for kernel %S — Kernel.name_of Mainnet \
           must point to a released kernel"
          name
  in
  let preimages_dir = Temp.dir "wasm_2_0_0" in
  let* {root_hash; _} =
    prepare_installer_kernel ~preimages_dir Constant.WASM.mainnet_kernel
  in
  Check.((root_hash = expected_hash) string)
    ~error_msg:
      (Printf.sprintf
         "mainnet_kernel.wasm has root hash %%L, but Kernel.name_of Mainnet = \
          %S points to a kernel whose released root hash is %%R — update \
          Kernel.name_of in etherlink/tezt/lib/kernel.ml"
         name) ;
  unit

module Protocol = Test_helpers.Protocol_no_direct_register

let test_patch_state =
  register_test (* It's a node specific test. *)
    ~__FILE__
    ~kernel:Kernel.Latest
    ~enable_dal:false
    ~tags:["evm"; "patch"; "state"]
    ~title:"Patch state via command"
    ~time_between_blocks:Nothing
  @@
  fun {sequencer; sc_rollup_node; sc_rollup_address; client; kernel; _}
      _protocol
    ->
  (* Test patch state for the evm-node. *)
  let path = Durable_storage_path.sequencer kernel in
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
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
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
    ~__FILE__
    ~kernels:[Latest]
    ~genesis_timestamp
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "upgrade"; "self"]
    ~title:"EVM Kernel can upgrade to itself"
  @@
  fun {
        sc_rollup_node;
        l1_contracts;
        sc_rollup_address;
        client;
        sequencer;
        observer;
        kernel;
        _;
      }
      _protocol
    ->
  (* Sends the upgrade to L1, but not to the sequencer. *)
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

  (* Per the activation timestamp, the state will remain synchronised until
     the kernel is upgraded. *)
  let* _ =
    repeat 2 (fun () ->
        let* _ = produce_block ~timestamp:"2020-01-01T00:00:05Z" sequencer in
        unit)
  in

  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer ()
  and* _upgrade_info = Evm_node.wait_for_pending_upgrade sequencer in

  let* () =
    check_rollup_head_consistency
      ~sc_rollup_node
      ~evm_node:sequencer
      ~error_msg:
        "The rollup head should be the same before the upgrade: sequencer is \
         %%L while rollup node is %%R"
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
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

  let* () =
    check_rollup_head_consistency
      ~sc_rollup_node
      ~evm_node:sequencer
      ~error_msg:
        "The rollup head should be the same after the upgrade: sequencer is \
         %%L while rollup node is %%R"
      ()
  in

  unit

let test_empty_block_on_upgrade =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "upgrade"; "empty"]
    ~title:"Sequencer produces an empty block in case of upgrade."
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_node;
        sc_rollup_address;
        sequencer;
        kernel;
        _;
      }
      _protocol
    ->
  (* Send a deposit so the sequencer will in theory add it to its next block. *)
  let deposit_info =
    {
      receiver = EthereumAddr "0x1074Fd1EC02cbeaa5A90450505cF3B48D834f3EB";
      chain_id = None;
    }
  in
  let* () =
    send_deposit_to_delayed_inbox
      ~amount:(Tez.of_int 1)
      ~bridge:l1_contracts.bridge
      ~depositor:Constant.bootstrap5
      ~deposit_info
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
      ~upgrade_to:(Kernel.to_uses kernel)
      ~activation_timestamp:"2077-01-01T00:00:00Z"
  in

  (* Bake a few blocks to make sure the sequencer sees the deposit and the
     upgrade. *)
  let* () =
    repeat 3 (fun _ ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
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
  let*@ _ = produce_block ~timestamp:"2077-02-02T00:00:00Z" sequencer in
  let* () = check_transactions_in_block 0 in
  (* Next block will have the deposit. *)
  let*@ _ = produce_block ~timestamp:"2077-02-02T00:00:00Z" sequencer in
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
    ~kernels:[Mainnet]
    ~genesis_timestamp
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "upgrade"; "auto"; "sync"]
    ~title:"Rollup-node kernel upgrade is applied to the sequencer state."
  @@
  fun from
      to_
      {
        sc_rollup_node;
        l1_contracts;
        sc_rollup_address;
        client;
        sequencer;
        observer;
        _;
      }
      _protocol
    ->
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
  let* to_root_hash =
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
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

  let* () =
    check_rollup_head_consistency
      ~sc_rollup_node
      ~evm_node:sequencer
      ~error_msg:
        "The rollup head should be the same before the upgrade: sequencer is \
         %%L while rollup node is %%R"
      ()
  in

  (* Snapshot the pre-upgrade head: its block number and the kernel root
     hash exposed via [tez_kernelRootHash] without a block parameter. The
     kernel only writes [KERNEL_ROOT_HASH] when processing an upgrade
     (see [etherlink/kernel_latest/kernel/src/upgrade.rs]); at boot the
     installer config does not seed it, so this snapshot is expected to
     be [None]. After the upgrade we use this snapshot to assert that
     the historical RPC at this block faithfully returns [None] rather
     than the post-upgrade root hash. *)
  let*@ pre_upgrade_block = Rpc.block_number sequencer in
  let*@ from_root_hash_opt = Rpc.tez_kernelRootHash sequencer in
  Check.((from_root_hash_opt = None) (option string))
    ~error_msg:
      "Expected tez_kernelRootHash to be None before the first upgrade (the \
       kernel only writes it on upgrade), got %L" ;

  (* Produce a block after activation timestamp, both the rollup
     node and the sequencer will upgrade to debug kernel and
     therefore not produce the block. *)
  let* _ =
    let*@ _ = produce_block ~timestamp:"2020-01-01T00:00:15Z" sequencer in
    unit
  and* _upgrade = Evm_node.wait_for_successful_upgrade sequencer in

  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

  let* () =
    check_rollup_head_consistency
      ~sc_rollup_node
      ~evm_node:sequencer
      ~error_msg:
        "The rollup head should be the same after the upgrade: sequencer is \
         %%L while rollup node is %%R"
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

  (* Historical RPC checks: querying [tez_kernelVersion] and
     [tez_kernelRootHash] at the pre-upgrade block must return the [from]
     kernel state, while querying at [Latest] must return the [to_]
     kernel state. *)
  let pre_upgrade_block_param = Rpc.Number (Int32.to_int pre_upgrade_block) in
  let*@ latest_root_hash_opt = Rpc.tez_kernelRootHash sequencer in
  Check.((latest_root_hash_opt = Some to_root_hash) (option string))
    ~error_msg:
      "Expected tez_kernelRootHash at latest to be %R after the upgrade, got %L" ;
  let*@ historical_root_hash_opt =
    Rpc.tez_kernelRootHash ~block:pre_upgrade_block_param sequencer
  in
  Check.((historical_root_hash_opt = from_root_hash_opt) (option string))
    ~error_msg:
      "Expected tez_kernelRootHash at the pre-upgrade block to be %R, got %L" ;
  let* () =
    match Kernel.commit_of from with
    | Some from_commit ->
        let*@ historical_kernel_version =
          Rpc.tez_kernelVersion ~block:pre_upgrade_block_param sequencer
        in
        Check.((historical_kernel_version = from_commit) string)
          ~error_msg:
            "Expected tez_kernelVersion at the pre-upgrade block to be %R, got \
             %L" ;
        let* _ =
          check_kernel_version ~evm_node:sequencer ~equal:false from_commit
        in
        unit
    | None -> unit
  in

  (* Start the observer again and wait for a successful upgrade *)
  let* () = Evm_node.run observer in
  let* _upgrade = Evm_node.wait_for_successful_upgrade observer in

  let* () = Evm_node.wait_for_blueprint_applied observer 3 in

  let* () =
    check_head_consistency
      ~left:sequencer
      ~right:observer
      ~error_msg:
        "The head should be the same after the upgrade: sequencer is %%L while \
         observer is %%R"
      ()
  in

  unit

let test_legacy_deposits_dispatched_after_kernel_upgrade =
  let genesis_timestamp =
    Client.(At (Time.of_notation_exn "2020-01-01T00:00:00Z"))
  in
  let activation_timestamp = "2020-01-01T00:00:01Z" in
  register_upgrade_all
    ~kernels:[Mainnet]
    ~upgrade_to:(fun _ -> Latest)
    ~genesis_timestamp
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "upgrade"; "deposit"; "legacy"; "delayed"]
    ~title:
      "After kernel upgrade a legacy deposit from delayed inbox can be decoded \
       and processed."
  @@
  fun from
      to_
      {sc_rollup_node; l1_contracts; sc_rollup_address; client; sequencer; _}
      _protocol
    ->
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
      ()
  in
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
     wait for the sequencer to have seen the deposit, so that it is
     guaranteed to be part of its local delayed inbox before the
     post-upgrade block is produced below. Only L1 levels are baked (no
     sequencer block production): the deposit must not be consumed by the
     pre-upgrade kernel, since the very point of the test is to decode it
     with the post-upgrade kernel. *)
  let* _ =
    repeat 2 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  let* _ = delayed_transaction_seen in

  let _, to_use = Kernel.to_uses_and_tags to_ in
  let pending_upgrade = Evm_node.wait_for_pending_upgrade sequencer in
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
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  let* _ = pending_upgrade in

  (* Produce a block where the upgrade would happen *)
  let*@ _ = produce_block ~timestamp:"2020-01-01T00:00:10Z" sequencer in
  let* () =
    bake_until_sync ~timeout:120. ~sc_rollup_node ~sequencer ~client ()
  in

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
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in

  (* Deposit must be applied by now *)
  let* receiver_balance_next =
    Eth_cli.balance
      ~account:Eth_account.bootstrap_accounts.(1).address
      ~endpoint:(Evm_node.endpoint sequencer)
      ()
  in
  Check.((receiver_balance_next > receiver_balance_prev) Wei.typ)
    ~error_msg:"Expected a bigger balance" ;

  (* Ensure delayed inbox is empty now *)
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in

  unit

(** This tests the situation where force kernel upgrade happens too soon. *)
let test_force_kernel_upgrade_too_early =
  let genesis_timestamp =
    Client.(At (Time.of_notation_exn "2020-01-10T00:00:00Z"))
  in
  register_upgrade_all
    ~kernels:[Latest]
    ~upgrade_to:(fun _ -> Mainnet)
    ~genesis_timestamp
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "upgrade"; "force"]
    ~title:"Force kernel upgrade fail too early"
  @@
  fun _from
      to_
      {sc_rollup_node; l1_contracts; sc_rollup_address; client; sequencer; _}
      _protocol
    ->
  (* Wait for the sequencer to publish its genesis block. *)
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

  (* Assert the kernel version is the same at start up. *)
  let*@ sequencer_kernelVersion = Rpc.tez_kernelVersion sequencer in
  let* rollup_kernelVersion = get_rollup_kernel_version ~sc_rollup_node () in
  Check.((sequencer_kernelVersion = rollup_kernelVersion) string)
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
  let* new_rollup_kernelVersion =
    get_rollup_kernel_version ~sc_rollup_node ()
  in
  Check.((sequencer_kernelVersion = new_rollup_kernelVersion) string)
    ~error_msg:"The force kernel upgrade should have failed" ;
  unit

(** This tests the situation where the kernel does not produce blocks but
    still can be forced to upgrade via an external message. *)
let test_force_kernel_upgrade =
  let genesis_timestamp =
    Client.(At (Time.of_notation_exn "2020-01-10T00:00:00Z"))
  in
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~kernels:[Latest]
    ~tags:["evm"; "sequencer"; "upgrade"; "force"]
    ~genesis_timestamp
    ~title:"Force kernel upgrade"
    ~additional_uses:[Constant.WASM.debug_kernel]
  @@
  fun {sc_rollup_node; l1_contracts; sc_rollup_address; client; sequencer; _}
      _protocol
    ->
  (* Wait for the sequencer to publish its genesis block. *)
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
  (* Assert the kernel version is the same at start up. *)
  let*@ sequencer_kernelVersion = Rpc.tez_kernelVersion sequencer in
  let* rollup_kernelVersion = get_rollup_kernel_version ~sc_rollup_node () in
  Check.((sequencer_kernelVersion = rollup_kernelVersion) string)
    ~error_msg:"Kernel versions should be the same at start up" ;

  (* Activation timestamp is 1 day before the genesis. Therefore, it can
     be forced immediately. *)
  let activation_timestamp = "2020-01-09T00:00:00Z" in
  (* Sends the upgrade to L1 and sequencer. *)
  let* _root_hash =
    upgrade
      ~sc_rollup_node
      ~sc_rollup_address
      ~admin:Constant.bootstrap2.public_key_hash
      ~admin_contract:l1_contracts.admin
      ~client
      ~upgrade_to:Constant.WASM.debug_kernel
      ~activation_timestamp
  in

  (* We bake a few blocks. As the sequencer is not producing anything, the
     kernel will not upgrade. *)
  let* () =
    repeat 5 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  (* Assert the kernel version is the same, it proves the upgrade did not
      happen. *)
  let*@ sequencer_kernelVersion = Rpc.tez_kernelVersion sequencer in
  let* rollup_kernelVersion = get_rollup_kernel_version ~sc_rollup_node () in
  Check.((sequencer_kernelVersion = rollup_kernelVersion) string)
    ~error_msg:"Kernel versions should be the same even after the message" ;

  (* Now we force the kernel upgrade via an external message. They will
     become unsynchronised. *)
  let* () = force_kernel_upgrade ~sc_rollup_address ~sc_rollup_node ~client in

  (* Assert the kernel version are now different, it shows that only the rollup
     node upgraded. *)
  let*@ sequencer_kernel =
    Rpc.state_value sequencer Durable_storage_path.kernel_boot_wasm
  in
  let* rollup_node_kernel =
    Sc_rollup_node.RPC.call sc_rollup_node ~rpc_hooks:Tezos_regression.rpc_hooks
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind:"wasm_2_0_0"
         ~operation:Sc_rollup_rpc.Value
         ~key:Durable_storage_path.kernel_boot_wasm
         ()
  in
  Check.((sequencer_kernel <> rollup_node_kernel) (option string))
    ~error_msg:"Rollup node should have forced the upgrade" ;
  unit

let test_patch_kernel =
  register_all
    ~__FILE__
    ~kernels:[Mainnet]
    ~tags:["evm"; "patch_kernel"; "experimental"]
    ~title:"Can patch the kernel of an existing node"
    ~additional_uses:[Constant.WASM.mainnet_kernel]
    ~da_fee:Wei.zero
  @@ fun {sequencer; _} _protocol ->
  let* _ =
    check_kernel_version
      ~evm_node:sequencer
      ~equal:true
      Constant.WASM.mainnet_commit
  in
  let* () = Evm_node.terminate sequencer in
  let* () = Evm_node.patch_kernel sequencer (Kernel.path Mainnet) in
  let* () = Evm_node.run sequencer in
  (* Produce a block so that the migration code is executed *)
  let* _ = produce_block sequencer in
  let* _ =
    check_kernel_version
      ~evm_node:sequencer
      ~equal:true
      Constant.WASM.mainnet_commit
  in
  unit

(* Guard against [Kernel.storage_version] in [etherlink/tezt/lib/kernel.ml]
   drifting from the [STORAGE_VERSION] constant baked into each kernel binary.
   Boot the kernel, then assert the value the kernel itself wrote to its
   durable-storage [storage_version] path matches what [Kernel.storage_version]
   reports for that kernel. If a kernel is rebaked with a new [STORAGE_VERSION]
   and tezt's table isn't bumped, this test fails with a pointer to the file to
   fix.

   Coverage here is limited to [etherlink_all] (Mainnet + Latest) — Previewnet
   cannot be booted by the default [register_all] fixture and is covered by
   [test_previewnet_storage_version_matches_constant] in [evm_rollup.ml],
   which calls [setup_evm_kernel] directly with the V49 [/evm/storage_version]
   pre-seed required to make the previewnet kernel run its V50–V56 migration
   ladder. *)
let test_kernel_storage_version_matches_constant =
  register_all
    ~__FILE__
    ~kernels:Kernel.[Latest; Mainnet]
    ~tags:["durable_storage"; "storage_version"; "kernel_constant"]
    ~time_between_blocks:Nothing
    ~title:"Kernel.storage_version matches each kernel's baked STORAGE_VERSION"
  @@ fun {sc_rollup_node; kernel; _} _protocol ->
  let key = Durable_storage_path.storage_version kernel in
  let* raw_opt =
    Sc_rollup_node.RPC.call sc_rollup_node ~rpc_hooks:Tezos_regression.rpc_hooks
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind:"wasm_2_0_0"
         ~operation:Sc_rollup_rpc.Value
         ~key
         ()
  in
  let actual =
    match raw_opt with
    | None ->
        Test.fail
          ~__LOC__
          "No value found in durable storage at path %S — the kernel did not \
           initialize its storage version"
          key
    | Some hex ->
        let bytes = Hex.to_bytes (`Hex hex) in
        Bytes.get_int64_le bytes 0 |> Int64.to_int
  in
  let expected = Kernel.storage_version kernel in
  Check.(
    (actual = expected)
      int
      ~error_msg:
        (Printf.sprintf
           "Kernel %s wrote V%%L to %s but Kernel.storage_version reports V%%R \
            — update [storage_version] in etherlink/tezt/lib/kernel.ml to \
            match the rebaked kernel."
           (Kernel.to_tag kernel)
           key)) ;
  unit

let protocols = [Protocol.Alpha]

let () =
  test_patch_state [Protocol.Alpha] ;
  test_legacy_deposits_dispatched_after_kernel_upgrade protocols ;
  test_empty_block_on_upgrade protocols ;
  test_upgrade_kernel_auto_sync protocols ;
  test_self_upgrade_kernel protocols ;
  test_force_kernel_upgrade protocols ;
  test_force_kernel_upgrade_too_early protocols ;
  test_mainnet_kernel_name_matches_root_hash () ;
  test_patch_kernel protocols ;
  test_kernel_storage_version_matches_constant [Alpha]
