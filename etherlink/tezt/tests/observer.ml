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
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file observer.ml
   Subject:      Observer nodes, finalized views, and RPC-only nodes.
*)

open Sc_rollup_helpers
open Rpc.Syntax
open Transaction
open Test_helpers
open Setup

(* The test uses a very specific setup, so it doesn't use general  *)
let test_observer_reset =
  Protocol.register_test
    ~__FILE__
    ~title:"Observer resets on finalized state"
    ~tags:["observer"; "reset"; "divergence"]
    ~uses_admin_client:true
    ~uses_client:true
    ~uses_node:true
    ~uses:(fun _protocol ->
      [
        Constant.octez_evm_node;
        Constant.octez_smart_rollup_node;
        Constant.WASM.evm_kernel;
        Constant.smart_rollup_installer;
      ])
  @@ fun protocol ->
  (* The setup is a bit verbose but hang in there, it's not that complicated.

     It creates 2 evm-node sequencer, one that has the correct key and one
     that has an invalid one.

     If an observer follows the invalid sequencer (and agrees with the invalid
     key) it can accept blocks on an invalid branch.

     If the valid sequencer starts to publish blueprints with the valid key,
     they will see a divergence.
  *)
  let kernel = Constant.WASM.evm_kernel in
  let valid_sequencer = Constant.bootstrap1 in
  let invalid_sequencer = Constant.bootstrap2 in
  let* node, client = setup_l1 protocol in
  let sc_rollup_node =
    Sc_rollup_node.create
      ~default_operator:Constant.bootstrap1.public_key_hash
      Batcher
      node
      ~base_dir:(Client.base_dir client)
      ~kind:"wasm_2_0_0"
  in
  let preimages_dir = Sc_rollup_node.data_dir sc_rollup_node // "wasm_2_0_0" in
  let valid_config = Temp.file "valid_config.yaml" in
  let invalid_config = Temp.file "invalid_config.yaml" in
  let*! () =
    Evm_node.make_kernel_installer_config
      (Evm_node.make_kernel_setup ~sequencer:valid_sequencer.public_key ())
      ~output:valid_config
      ()
  in
  let*! () =
    Evm_node.make_kernel_installer_config
      (Evm_node.make_kernel_setup ~sequencer:invalid_sequencer.public_key ())
      ~output:invalid_config
      ()
  in
  let* {output = valid_kernel; _} =
    prepare_installer_kernel
      ~output:(Temp.file "valid_kernel.hex")
      ~preimages_dir
      ~config:(`Path valid_config)
      kernel
  in
  let* {output = invalid_kernel; _} =
    prepare_installer_kernel
      ~output:(Temp.file "invalid_kernel.hex")
      ~preimages_dir
      ~config:(`Path invalid_config)
      kernel
  in
  let* sc_rollup_address =
    originate_sc_rollup
      ~keys:[]
      ~kind:"wasm_2_0_0"
      ~boot_sector:("file:" ^ valid_kernel)
      ~parameters_ty:Rollup.evm_type
      client
  in
  let* () =
    Sc_rollup_node.run sc_rollup_node sc_rollup_address [Log_kernel_debug]
  in
  (* Run a valid sequencer. *)
  let sequencer_config : Evm_node.sequencer_config =
    {
      time_between_blocks = Some Nothing;
      genesis_timestamp = None;
      max_number_of_chunks = None;
      wallet_dir = Some (Client.base_dir client);
    }
  in

  let* valid_sequencer =
    Evm_node.init
      ~node_setup:
        (Evm_node.make_setup
           ~name:"valid"
           ~initial_kernel:valid_kernel
           ~preimages_dir
           ())
      ~mode:
        (Evm_node.Sequencer
           {
             rollup_node_endpoint = Sc_rollup_node.endpoint sc_rollup_node;
             sequencer_config;
             sequencer_keys = [valid_sequencer.alias];
             max_blueprints_lag = None;
             max_blueprints_ahead = None;
             max_blueprints_catchup = None;
             catchup_cooldown = None;
             dal_slots = None;
             sequencer_sunset_sec = None;
           })
      ()
  in
  (* We start a sequencer with an invalid key, but from its perspective the
     key is valid.

     We connect it to a temporary rollup node, which is is terminated right after
     so the sequencer doesn't detect the divergence and keep producing blocks.
  *)
  let temp_sc_rollup_node =
    Sc_rollup_node.create
      ~name:"temp_sc_rollup_node"
      ~default_operator:Constant.bootstrap1.public_key_hash
      Batcher
      node
      ~base_dir:(Client.base_dir client)
      ~kind:"wasm_2_0_0"
  in
  let* () =
    Sc_rollup_node.run temp_sc_rollup_node sc_rollup_address [Log_kernel_debug]
  in

  let* invalid_sequencer =
    Evm_node.init
      ~node_setup:
        (Evm_node.make_setup
           ~name:"invalid"
           ~initial_kernel:invalid_kernel
           ~preimages_dir
           ())
      ~mode:
        (Evm_node.Sequencer
           {
             rollup_node_endpoint = Sc_rollup_node.endpoint temp_sc_rollup_node;
             sequencer_config;
             sequencer_keys = [invalid_sequencer.alias];
             max_blueprints_lag = None;
             max_blueprints_ahead = None;
             max_blueprints_catchup = None;
             catchup_cooldown = None;
             dal_slots = None;
             sequencer_sunset_sec = None;
           })
      ()
  in
  (* Preparing two observers, they will be the victim of our tests in 2 different
     scenarios.

     The first observer will detect the divergence after the fact. The second
     observer will detect the divergence when it applies blueprints after
     getting the confirmations from the rollup node.
  *)
  let* observer_victim1 =
    Evm_node.init
      ~node_setup:
        (Evm_node.make_setup
           ~name:"victim1"
           ~initial_kernel:invalid_kernel
           ~preimages_dir
           ())
      ~mode:
        (Evm_node.Observer
           {
             rollup_node_endpoint = Some (Sc_rollup_node.endpoint sc_rollup_node);
             evm_node_endpoint = Evm_node.endpoint invalid_sequencer;
           })
      ()
  in

  let* observer_victim2 =
    Evm_node.init
      ~node_setup:
        (Evm_node.make_setup
           ~name:"victim2"
           ~rpc_port:(Port.fresh ())
           ~initial_kernel:invalid_kernel
           ~preimages_dir
           ())
      ~mode:
        (Evm_node.Observer
           {
             rollup_node_endpoint = Some (Sc_rollup_node.endpoint sc_rollup_node);
             evm_node_endpoint = Evm_node.endpoint invalid_sequencer;
           })
      ()
  in
  (* We want this observer to detect the divergence after seeing the L1 events,
     so it does not track blueprints for now. *)
  let* () = Evm_node.terminate observer_victim2 in
  let* () = Evm_node.run ~extra_arguments:["--no-sync"] observer_victim2 in

  (* Initialization is complete, let us now start the test. *)

  (* Produce 5 blocks with the invalid sequencer. *)
  let* _ =
    repeat 5 (fun () ->
        let*@ _ = Rpc.produce_block invalid_sequencer in
        unit)
  and* () = Evm_node.wait_for_blueprint_applied invalid_sequencer 5
  and* () = Evm_node.wait_for_blueprint_applied observer_victim1 5 in

  (* Produce 1 block with the valid sequencer, {!observer_victim1} will detect
     a divergence.
     We terminate the invalid sequencer rollup node so it doesn't see the event.
  *)
  let* () = Sc_rollup_node.terminate temp_sc_rollup_node in
  let*@ _ = Rpc.produce_block valid_sequencer in
  let* () =
    let* () =
      bake_until_sync ~sc_rollup_node ~sequencer:valid_sequencer ~client ()
    in
    (* Make the published blueprint final. *)
    let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
    let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
    unit
  and* () = Evm_node.wait_for_reset observer_victim1 in
  (* The observer has reset because of the divergence. Let's see how it
     refuses blocks now. *)
  let* _ =
    let*@ _ = Rpc.produce_block invalid_sequencer in
    unit
  and* () = Evm_node.wait_for_blueprint_invalid_applied observer_victim1 in

  (* Now let's test the second observer that will detect the divergence
     when synchronizing. *)
  let* () = Evm_node.terminate observer_victim2 in
  let* () = Evm_node.run observer_victim2
  and* () = Evm_node.wait_for_reset observer_victim2 in
  (* It will also refuse blocks provided by the invalid sequencer. *)
  let* _ = Evm_node.wait_for_diverged observer_victim2 in

  unit

module Protocol = Test_helpers.Protocol_no_direct_register

let check_applies_blueprint ?timeout sequencer_node observer_node levels_to_wait
    =
  let*@ current_head = Rpc.block_number sequencer_node in
  let targeted_level = Int32.to_int current_head + levels_to_wait in

  let* () =
    repeat levels_to_wait @@ fun () ->
    let*@ _ = Rpc.produce_block sequencer_node in
    unit
  and* _ =
    Evm_node.wait_for_blueprint_applied ?timeout observer_node targeted_level
  and* _ =
    Evm_node.wait_for_blueprint_applied ?timeout sequencer_node targeted_level
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
    ~__FILE__
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
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~tags:["evm"; "observer"; "dont_track_rollup_node"]
    ~title:"Can start an Observer node without a rollup node"
  @@
  fun {sequencer = sequencer_node; sc_rollup_node; observer = observer_node; _}
      _protocols
    ->
  let levels_to_wait = 3 in
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
    check_applies_blueprint sequencer_node observer_node levels_to_wait
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
    check_applies_blueprint sequencer_node observer_node levels_to_wait
  in

  unit

let test_observer_applies_blueprint_from_rpc_node =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~tags:["evm"; "observer"; "rpc_mode"]
    ~title:"Can start an Observer node tracking a RPC node"
  @@ fun {sequencer = sequencer_node; sc_rollup_node; _} _protocol ->
  (* This is another version of [test_observer_applies_blueprint], but this
     time instead of connecting directly to the sequencer, the observer
     connects to a RPC mode process. *)
  let levels_to_wait = 3 in

  let* rpc_node = run_new_rpc_endpoint sequencer_node in
  let* observer_node =
    run_new_observer_node ~sc_rollup_node:(Some sc_rollup_node) rpc_node
  in

  let* _ = Evm_node.wait_for_blueprint_applied observer_node levels_to_wait
  and* _ = Evm_node.wait_for_blueprint_applied sequencer_node levels_to_wait
  and* _ =
    repeat levels_to_wait @@ fun () ->
    let*@ _ = produce_block sequencer_node in
    unit
  in

  let* () =
    check_block_consistency
      ~left:sequencer_node
      ~right:observer_node
      ~block:(`Level (Int32.of_int levels_to_wait))
      ()
  in

  (* We stop and start the sequencer, to ensure the rpc node correctly
     reconnects to it. *)
  let* () = Evm_node.wait_for_retrying_connect rpc_node
  and* () =
    let* () = Evm_node.terminate sequencer_node in
    Evm_node.run sequencer_node
  in

  (* We stop and start the rpc node, to ensure the observer node correctly
     reconnects to it. *)
  let* () = Evm_node.wait_for_retrying_connect observer_node
  and* () =
    let* () = Evm_node.terminate rpc_node in
    Evm_node.run rpc_node
  in

  let* _ = Evm_node.wait_for_blueprint_applied observer_node (2 * levels_to_wait)
  and* _ =
    Evm_node.wait_for_blueprint_applied sequencer_node (2 * levels_to_wait)
  and* _ =
    repeat levels_to_wait @@ fun () ->
    let*@ _ = produce_block sequencer_node in
    unit
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
    ~__FILE__
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

let test_observer_applies_blueprint_when_sequencer_restarted =
  register_all
    ~__FILE__
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
  register_all
    ~__FILE__
    ~time_between_blocks:(Time_between_blocks tbb)
    ~tags
    ~title
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
    Eth_cli.get_receipt ~endpoint:(Evm_node.endpoint sequencer_node) ~tx:txn ()
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
    ~__FILE__
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
  (* After enough time, the observer considers its connection with the
     sequencer is stalled and tries to reconnect. *)
  let* () = Evm_node.wait_for_retrying_connect ~timeout:5. observer
  and* () =
    Evm_node.run sequencer ~extra_arguments:["--time-between-blocks"; "none"]
  in
  (* We produce a block, and verify that the observer nodes correctly applies
     it. *)
  let* _ = produce_block sequencer in
  let* () = Evm_node.wait_for_blueprint_applied observer 1 in

  unit

let test_observer_finalized_view =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "observer"; "finalized"; "dont_track_rollup_node"]
    ~title:
      "observer has finalized block parameter without tracking the rollup node"
    ~time_between_blocks:Nothing
    ~use_dal:
      (* The logic tested here is orthogonal with the DAL, and the test itself is written with constants
         for the case DAL is not enabled. *)
      Register_without_feature
  @@ fun {sc_rollup_node; observer; client; sequencer; _} _protocol ->
  let* () = Evm_node.terminate observer in
  let* () =
    Evm_node.run ~extra_arguments:["--dont-track-rollup-node"] observer
  in
  (* Create blueprints for level 1 and 2 *)
  let* _ = produce_block sequencer in
  let* _ = produce_block sequencer in

  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

  let target_finalized = 2l in

  (* Arm both event listeners before baking to avoid races. *)
  let wait_observer_finalized =
    Evm_node.wait_for_observer_finalized_level
      observer
      (Int32.to_int target_finalized)
  in
  (* Bake L1 blocks until the sequencer emits the blueprint-finalized event.
     Running baking and waiting concurrently avoids the race between the rollup
     node processing an L1 block and the sequencer updating its finalized level. *)
  let rec bake_until_sequencer_finalized ?(remaining_attempts = 6) () =
    if remaining_attempts <= 0 then
      Test.fail "Sequencer did not finalize blueprint %ld" target_finalized
    else
      Lwt.catch
        (fun () ->
          let* _ =
            Evm_node.wait_for_blueprint_finalized
              ~timeout:5.
              sequencer
              (Int32.to_int target_finalized)
          and* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
          unit)
        (fun _ ->
          bake_until_sequencer_finalized
            ~remaining_attempts:(remaining_attempts - 1)
            ())
  in
  let* () = bake_until_sequencer_finalized () in
  (* Wait for the observer to finish processing the Finalized_levels broadcast
     via the sequencer's /evm/messages stream. *)
  let* () = wait_observer_finalized in
  (* Check the finalized blueprint is the same for the observer and
     the sequencer. *)
  let* () =
    check_block_consistency ~block:`Finalized ~left:sequencer ~right:observer ()
  in
  unit

let test_finalized_persistent =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "finalized"]
    ~title:"Persistent finalized block parameter"
    ~time_between_blocks:Nothing
    ~use_dal:
      (* The logic tested here is orthogonal with the DAL, and the test itself is written with constants
         for the case DAL is not enabled. *)
      Register_without_feature
  @@ fun {sc_rollup_node; client; sequencer; _} _protocol ->
  (* Helper to finalize a L2 block by baking the minimum number of L1 block required *)
  let rec bake_until_blueprint_finalized ?(remaining_attempts = 6) number =
    if remaining_attempts <= 0 then
      Test.fail "Could not finalize blueprint %ld" number
    else
      Lwt.catch
        (fun () ->
          let* _ =
            Evm_node.wait_for_blueprint_finalized
              ~timeout:5.
              sequencer
              Int32.(to_int number)
          and* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
          unit)
        (fun _ ->
          bake_until_blueprint_finalized
            ~remaining_attempts:Int.(pred remaining_attempts)
            number)
  in

  (* Finalized blueprint 0 *)
  let* () = bake_until_blueprint_finalized 0l in

  (* Create blueprints for level 1 and 2 *)
  let* _ = produce_block sequencer in
  let* _ = produce_block sequencer in

  let target_finalized = 2l in

  (* Create enough L1 blocks to finalized the blueprint 2 *)
  let* () = bake_until_blueprint_finalized target_finalized in

  (* Check the finalized blueprint is indeed the expected one *)
  let*@ finalized_head = Rpc.get_block_by_number ~block:"finalized" sequencer in
  Check.(
    (finalized_head.number = target_finalized)
      int32
      ~error_msg:"Expected finalized level %R, got %L") ;

  (* Restart the sequencer *)
  let* () = Evm_node.terminate sequencer in
  let* () = Evm_node.run sequencer in

  (* Check the finalized block is still the one we are expecting *)
  let*@ finalized_head = Rpc.get_block_by_number ~block:"finalized" sequencer in
  Check.(
    (finalized_head.number = target_finalized)
      int32
      ~error_msg:"Expected finalized level %R, got %L") ;
  unit

let test_finalized_view =
  register_all
    ~__FILE__
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
  @@ fun {sc_rollup_node; client; sequencer; _} _protocol ->
  let* finalized_observer =
    run_new_observer_node
      ~finalized_view:true
      ~sc_rollup_node:(Some sc_rollup_node)
      sequencer
  in
  let p = Evm_node.wait_for_blueprint_applied finalized_observer 4 in
  (* Produce a few EVM blocks *)
  let* _ =
    repeat 4 @@ fun () ->
    let* _ = produce_block sequencer in
    unit
  and* () = p in
  (* Produces two L1 blocks to ensure the L2 blocks are posted onchain by the sequencer *)
  let* () = bake_until_sync ~__LOC__ ~sc_rollup_node ~sequencer ~client () in
  (* Check the heads of the various nodes *)
  let*@ sequencer_head = Rpc.block_number sequencer in

  Check.((sequencer_head = 4l) int32)
    ~error_msg:"Sequencer head should be %R, but is %L instead" ;

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
    let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
    unit
  in
  let*@ finalized_observer_block = Rpc.block_number finalized_observer in
  Check.((finalized_observer_block = sequencer_head) int32)
    ~error_msg:
      "Finalized observer head should be equal to sequencer head (%R), but is \
       %L instead" ;
  unit

let test_finalized_view_forward_txn =
  let raw_transfer nonce =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce
      ~gas_price:1_000_000_000
      ~gas:23_300
      ~value:(Wei.of_eth_int 1)
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  in
  let nb_transactions = 3 in
  register_all
    ~__FILE__
    ~kernels:
      [
        Latest
        (* This test focuses on a feature that purely relies on the node, it does not makes sense to register it for every protocols *);
      ]
    ~tags:["evm"; "finalized_view"; "forward"]
    ~time_between_blocks:Nothing
    ~title:
      "--finalized-view forwards transactions to the sequencer without waiting"
    ~da_fee:Wei.zero
      (* This test depends on the number of blocks produced until the
         rollup node synchronizes with the sequencer. When
         DAL is activated, bake_until_sync would need to produce more
         blocks than usual. Consequently, this would finalize the
         blocks posted on-chain. The finalized rollup does have a head,
         and the RPC does not fail, which is not the expected result
         when running this test without DAL activation. *)
    ~use_dal:Register_without_feature
  @@ fun {sc_rollup_node; client; sequencer; _} _protocol ->
  (* Start a observer node with --finalized-view enabled *)
  let* finalized_observer =
    run_new_observer_node
      ~finalized_view:true
      ~sc_rollup_node:(Some sc_rollup_node)
      sequencer
  in
  let* () = Evm_node.wait_for_blueprint_applied finalized_observer 0 in

  (* Produce a few EVM blocks *)
  let* _ =
    repeat 4 @@ fun () ->
    let*@ _ = produce_block sequencer in
    unit
  in
  (* Produces two L1 blocks to ensure the L2 blocks are posted onchain by the sequencer *)
  let* () = bake_until_sync ~__LOC__ ~sc_rollup_node ~sequencer ~client () in

  (* Craft a given number of consecutive transactions. We will prove they are
     injected in blocks without the need to create new L1 blocks. *)
  let* txns =
    fold nb_transactions [] (fun i txns ->
        let* tx = raw_transfer i in
        return (tx :: txns))
  in
  let txns = (* respect transaction order *) List.rev txns in

  let* _ =
    Lwt_list.map_s
      (fun txn ->
        let add_txn = Evm_node.wait_for_tx_queue_add_transaction sequencer in
        let* _ = batch_n_transactions ~evm_node:finalized_observer [txn]
        and* _ = add_txn in
        unit)
      txns
  in

  (* Create a block as we have created transactions, check they all
     contain one transaction. This
     should not be flaky because the `eth_sendRawTransaction`
     implementation of the observer (in finalized view) is blocked
     until the sequencer accepts the transaction. *)
  let*@ txns_in_block = produce_block sequencer in
  Check.(
    (txns_in_block = nb_transactions)
      int
      ~error_msg:
        "Expected %R transactions even without producing L1 blocks, found %L") ;

  unit

let test_finalized_block_param =
  register_all
    ~__FILE__
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
  @@ fun {sc_rollup_node; client; sequencer; _} _protocol ->
  let* rpc = run_new_rpc_endpoint sequencer in
  (* Produce a few EVM blocks *)
  let* () =
    repeat 4 @@ fun () ->
    let* _ = produce_block sequencer in
    unit
  in
  let* () = bake_until_sync ~__LOC__ ~sc_rollup_node ~client ~sequencer () in
  (* Check that the L2 blocks where indeed posted onchain. *)
  let*@ sequencer_head = Rpc.get_block_by_number ~block:"latest" sequencer in
  Check.((sequencer_head.number = 4l) int32)
    ~error_msg:"Sequencer head should be %R, but is %L instead" ;
  let* () =
    check_rollup_head_consistency ~evm_node:sequencer ~sc_rollup_node ()
  in
  (* While the blocks were posted onchain, they are not final wrt. the
     consensus algorithm, so the finalized rollup does not have a head yet.

     We produce both two more L2 blocks and two L1 blocks; the latter will
     allow to finalized the first four blocks posted earlier. *)
  let* () =
    repeat 2 @@ fun () ->
    let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
    let* _ = produce_block sequencer in
    unit
  in
  (* Produces two L1 blocks to ensure the L2 blocks are posted onchain by the sequencer *)
  let* () = bake_until_sync ~__LOC__ ~sc_rollup_node ~client ~sequencer () in
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
    check_rollup_head_consistency ~evm_node:sequencer ~sc_rollup_node ()
  in
  let* () =
    check_block_consistency ~block:`Finalized ~left:rpc ~right:sequencer ()
  in

  (* Terminate the sequencer. *)
  let* () = Evm_node.terminate sequencer in
  (* Restart it. *)
  let* () = Evm_node.run sequencer in

  unit

let test_rpc_mode_while_block_are_produced =
  register_all
    ~__FILE__
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

let test_relay_restricted_rpcs =
  register_all
    ~__FILE__
    ~eth_bootstrap_accounts:Eth_account.lots_of_address
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

let test_spawn_rpc =
  let fresh_port = Port.fresh () in
  register_all
    ~__FILE__
    ~spawn_rpc:fresh_port
    ~tags:["sequencer"; "spawn"; "rpc"; Tag.flaky]
    ~title:
      "The spawned RPC successfully functions as an intermediate node for \
       public requests"
  @@ fun {sequencer; observer; _} _protocol ->
  Log.info "experimental_features.spawn_rpc = %d" fresh_port ;
  let* raw_tx =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:300_000
      ~value:Wei.zero
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  in
  let*@ transaction_hash = Rpc.send_raw_transaction ~raw_tx sequencer in
  let*@ size = Rpc.produce_block sequencer in
  Check.((size = 1) int) ~error_msg:"block size should be 1" ;
  let* () = Evm_node.wait_for_blueprint_applied sequencer 1 in
  let* () = Evm_node.wait_for_blueprint_applied observer 1 in
  let*@ _seq_block = Rpc.get_block_by_number ~block:"1" sequencer in
  let*@ _obs_block = Rpc.get_block_by_number ~block:"1" observer in
  let*@ tx_obj = Rpc.get_transaction_by_hash ~transaction_hash sequencer in
  Check.is_true (Option.is_some tx_obj) ~error_msg:"tx is missing" ;
  unit

let test_observer_init_from_snapshot =
  register_all
    ~__FILE__
    ~tags:["observer"; "init"; "from"; "snapshot"]
    ~title:"Observer successfully inits from a rolling snapshot"
  @@ fun {sequencer; sc_rollup_node; _} _protocol ->
  (* Restart the sequencer in rolling:3 *)
  let* () = Evm_node.terminate sequencer in
  let*! () = Evm_node.switch_history_mode sequencer (Rolling 2) in
  let* () = Evm_node.run sequencer
  and* _ =
    Evm_node.wait_for_start_history_mode ~history_mode:"rolling:2" sequencer
  in

  (* Produce 1 block, stop, export the snapshot, start *)
  let*@ _size = Rpc.produce_block sequencer in
  let* () = Evm_node.wait_for_blueprint_applied sequencer 1 in
  let* () = Evm_node.terminate sequencer in
  let*! snapshot_file = Evm_node.export_snapshot ~desync:false sequencer in
  let* () = Evm_node.run sequencer in

  (* Init observer from snapshot but with history mode set to rolling:5 *)
  let observer =
    Evm_node.create
      ~node_setup:
        (Evm_node.make_setup
           ~name:"init_from_snapshot"
           ?initial_kernel:(Evm_node.initial_kernel sequencer)
           ~preimages_dir:"/tmp"
           ())
      ~mode:
        (Observer
           {
             rollup_node_endpoint = Some (Sc_rollup_node.endpoint sc_rollup_node);
             evm_node_endpoint = Evm_node.endpoint sequencer;
           })
      ()
  in
  let* () = Process.check @@ Evm_node.spawn_init_config observer in
  let extra_arguments =
    ["--init-from-snapshot"; snapshot_file; "--history"; "rolling:5"]
  in
  let* () = Evm_node.run observer ~extra_arguments
  and* _ =
    Evm_node.wait_for_start_history_mode ~history_mode:"rolling:5" observer
  and* _ = Evm_node.wait_for_import_finished observer in

  (* Retrieve first block *)
  let*@ _block = Rpc.get_block_by_number ~block:"1" observer in
  unit

let test_observer_periodic_snapshot =
  let gc = 2 in
  register_all
    ~__FILE__
    ~periodic_snapshot_path:"periodic_snapshot/evm-%h-snapshot-%r-%l"
    ~history_mode:(Rolling gc)
    ~genesis_timestamp
    ~time_between_blocks:Nothing
    ~tags:["evm"; "observer"; "periodic"; "snapshot"]
    ~title:"Can export periodic snapshots"
    ~use_dal:Register_without_feature
    ~websockets:false
    ~kernels:[Latest]
  @@ fun {sequencer; observer; sc_rollup_node = _; _} _protocol ->
  let* () = Evm_node.wait_for_blueprint_applied observer 0 in
  let* snapshot_file =
    let res = ref None in
    let _ =
      let* _ = Evm_node.wait_for_gc_finished observer in
      let* file = Evm_node.wait_for_finished_exporting_snapshot observer in
      res := Some file ;
      unit
    in
    let result_f () = return !res in
    Log.info "Next block will trigger gc and periodic snapshot export." ;
    let*@ _ = Rpc.produce_block ~timestamp:(get_timestamp gc) sequencer in
    let* () = Evm_node.wait_for_blueprint_applied observer 1 in
    Log.info "Produce blocks while snapshot is exporting." ;
    (* There were cases in the CI where the default [timeout_in_blocks] was reached.
       So we double both timeouts. *)
    bake_until
      ~__LOC__
      ~timeout_in_blocks:40
      ~timeout:60.
      ~bake:(fun () ->
        let*@ _ = produce_block sequencer in
        unit)
      ~result_f
      ()
  in

  let* () = Evm_node.terminate sequencer in
  let new_sequencer = Evm_node.create ~mode:(Evm_node.mode sequencer) () in
  let*? import_process =
    Evm_node.import_snapshot new_sequencer ~desync:false ~snapshot_file
  in
  let* () = Process.check @@ import_process in
  let* () = Process.check @@ Evm_node.spawn_init_config new_sequencer in
  let* () = Evm_node.run new_sequencer in
  let*@ _ = produce_block new_sequencer in
  unit

let test_observer_divergence_fallback_on_instant_confirmations () =
  register_sandbox_with_observer
    ~fail_on_divergence:false
    ~patch_config:
      (Evm_node.patch_config_with_experimental_feature
         ~preconfirmation_stream_enabled:true
         ())
    ~__FILE__
    ~tags:["observer"; "divergence"; "fallback"; "instant_confirmations"]
    ~title:
      "Observer divergence fallback recovers correctly on instant confirmations"
  @@ fun {sandbox; observer} ->
  (*
    1. Start sandbox (sequencer) + observer with fail_on_divergence:false
    2. Send a transaction to the observer, both nodes are aware of this one
    3. Call executeSingleTransaction private RPC on observer with a different transaction,
       this adds an extra transaction to the observer's BIP that the sequencer does not have
    4. Produce block on sandbox, it only includes the first transaction
    5. Observer detects divergence, its BIP has 2 transactions but the sequencer's block only has 1
    6. Observer recovers by discarding its BIP state and re-applying the sequencer's blueprint from scratch
    7. Verify observer continues working correctly for subsequent blocks
  *)

  (* 1 *)
  let* _ = produce_block sandbox
  and* () = Evm_node.wait_for_blueprint_applied observer 1 in

  (* 2 *)
  let* raw_tx =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~value:(Wei.of_eth_int 1)
      ~gas:21000
      ~gas_price:1_000_000_000
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  in
  let*@ _hash = Rpc.send_raw_transaction ~raw_tx observer in

  (* 3 *)
  let* raw_tx =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(1).private_key
      ~chain_id:1337
      ~nonce:0
      ~value:(Wei.of_eth_int 1)
      ~gas:21000
      ~gas_price:1_000_000_000
      ~address:Eth_account.bootstrap_accounts.(0).address
      ()
  in
  let execution_done_promise =
    Evm_node.wait_for_single_tx_execution_done observer
  in
  let* () = Evm_node.execute_single_transaction observer ~raw_tx in
  let* _tx_hash = execution_done_promise in
  Log.info "Correctly executed faulty single transaction on observer" ;

  (* 4, 5, 6 *)
  let divergence_p = Evm_node.wait_for_assemble_block_diverged observer in
  let bp_application_p = Evm_node.wait_for_blueprint_applied observer 2 in
  let*@ _ = produce_block sandbox in
  let* divergence_level = divergence_p in
  Log.info
    "Divergence detected at level %d, observer is recovering by re-executing \
     the full blueprint"
    divergence_level ;
  let* () = bp_application_p in
  Log.info "Observer has recovered and applied the block after divergence." ;

  (* 7 *)
  let* _ = produce_block sandbox
  and* () = Evm_node.wait_for_blueprint_applied observer 3 in
  let*@ final_observer_block_number = Rpc.block_number observer in
  Check.(
    (final_observer_block_number >= 3l)
      int32
      ~error_msg:"observer should have recovered and be at block >= 3, got %L") ;
  unit

let protocols = [Protocol.Alpha]

let () =
  test_observer_applies_blueprint protocols ;
  test_observer_applies_blueprint_dont_track_rollup_node protocols ;
  test_observer_applies_blueprint_from_rpc_node protocols ;
  test_observer_applies_blueprint_when_restarted protocols ;
  test_observer_applies_blueprint_when_sequencer_restarted protocols ;
  test_observer_forwards_transaction protocols ;
  test_observer_timeout_when_necessary protocols ;
  test_observer_finalized_view protocols ;
  test_finalized_persistent protocols ;
  test_finalized_view protocols ;
  test_finalized_view_forward_txn protocols ;
  test_finalized_block_param protocols ;
  test_rpc_mode_while_block_are_produced protocols ;
  test_relay_restricted_rpcs protocols ;
  test_observer_reset [Protocol.Alpha] ;
  test_spawn_rpc protocols ;
  test_observer_init_from_snapshot protocols ;
  test_observer_periodic_snapshot [Alpha] ;
  test_observer_divergence_fallback_on_instant_confirmations ()
