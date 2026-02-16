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
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file evm_sequencer.ml
*)

open Sc_rollup_helpers
open Rpc.Syntax
open Contract_path
open Transaction
open Delayed_inbox

module Sequencer_rpc = struct
  let get_blueprint sequencer number =
    Runnable.run
    @@ Curl.get
         ~name:("curl#" ^ Evm_node.name sequencer)
         ~args:["--fail"]
         (Evm_node.endpoint sequencer
         ^ "/evm/blueprint/" ^ Int64.to_string number)

  let get_smart_rollup_address sequencer =
    let* res =
      Runnable.run
      @@ Curl.get
           ~name:("curl#" ^ Evm_node.name sequencer)
           ~args:["--fail"]
           (Evm_node.endpoint sequencer ^ "/evm/smart_rollup_address")
    in
    return (JSON.as_string res)
end

open Test_helpers
open Setup

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
      let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
      unit
    else unit
  in
  Lwt.return expected_hash

let send_fa_deposit_to_delayed_inbox ?(proxy = "") ~amount ~l1_contracts
    ~depositor ~receiver ~sc_rollup_node ~sc_rollup_address client =
  let* () =
    Client.transfer
      ~entrypoint:"set"
      ~arg:
        (sf
           "Pair %S (Pair (Right (Right %s%s)) 0)"
           sc_rollup_address
           receiver
           (remove_0x proxy))
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
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  unit

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

let test_make_l2_kernel_installer_config chain_family =
  Protocol.register_test
    ~__FILE__
    ~title:
      (Printf.sprintf
         "Test that the command make_l2_kernel_installer setup bootstrap \
          account at the right path with %s family"
         chain_family)
    ~tags:["evm"; "multichain"; "installer"]
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
  let* node, client = setup_l1 protocol in

  (* Random chain id, let's not take one that could have been set by default (1, 42, 1337) *)
  let chain_id_1 = 2988 in
  let chain_id_2 = 4571 in

  (* Configuration files for the two l2 chains and for the rollup *)
  let l2_config_1 = Temp.file "l2-chain-1-config.yaml" in
  let l2_config_2 = Temp.file "l2-chain-2-config.yaml" in
  let rollup_config = Temp.file "rollup-chain-config.yaml" in

  (* Argument for the l2 chain, a bootstrap account and a new world_state_path *)
  let world_state_path = "/test/chain/" in
  let address = Eth_account.bootstrap_accounts.(0).address in
  let*! () =
    Evm_node.make_l2_kernel_installer_config
      ~chain_id:chain_id_1
      ~chain_family
      ~world_state_path
      ~eth_bootstrap_accounts:[address]
      ~output:l2_config_1
      ()
  in
  let*! () =
    Evm_node.make_l2_kernel_installer_config
      ~chain_id:chain_id_2
      ~chain_family
      ~eth_bootstrap_accounts:[address]
      ~output:l2_config_2
      ()
  in
  let*! () =
    Evm_node.make_kernel_installer_config
    (* No need for a real sequencer governance *)
      ~sequencer_governance:"KT1"
      ~l2_chain_ids:[chain_id_1; chain_id_2]
      ~output:rollup_config
      ()
  in

  (* Setup the rollup (Origination and Start a rollup node) *)
  let sc_rollup_node =
    Sc_rollup_node.create
      ~default_operator:Constant.bootstrap1.public_key_hash
      Batcher
      node
      ~base_dir:(Client.base_dir client)
      ~kind:"wasm_2_0_0"
  in

  let preimages_dir = Sc_rollup_node.data_dir sc_rollup_node // "wasm_2_0_0" in
  let kernel = Constant.WASM.evm_kernel in
  let* {output = kernel; _} =
    prepare_installer_kernel_with_multiple_setup_file
      ~output:(Temp.file "kernel.hex")
      ~preimages_dir
      ~configs:[rollup_config; l2_config_1; l2_config_2]
      (Uses.path kernel)
  in
  let* sc_rollup_address =
    originate_sc_rollup
      ~keys:[]
      ~kind:"wasm_2_0_0"
      ~boot_sector:("file:" ^ kernel)
      ~parameters_ty:Rollup.evm_type
      client
  in
  let* () =
    Sc_rollup_node.run sc_rollup_node sc_rollup_address [Log_kernel_debug]
  in

  (* Setup a sequencer with a private rpc port to verify the durable storage. *)
  let sequencer_config : Evm_node.sequencer_config =
    {
      time_between_blocks = Some Nothing;
      genesis_timestamp = None;
      max_number_of_chunks = None;
      wallet_dir = Some (Client.base_dir client);
    }
  in
  let sequencer_mode =
    Evm_node.Sequencer
      {
        rollup_node_endpoint = Sc_rollup_node.endpoint sc_rollup_node;
        sequencer_config;
        sequencer_keys = [Constant.bootstrap1.alias];
        max_blueprints_lag = None;
        max_blueprints_ahead = None;
        max_blueprints_catchup = None;
        catchup_cooldown = None;
        dal_slots = None;
        sequencer_sunset_sec = None;
      }
  in
  let* sequencer =
    Evm_node.init
      ~node_setup:(Evm_node.make_setup ~initial_kernel:kernel ~preimages_dir ())
      ~mode:sequencer_mode
      ()
  in

  (* Verify the chain_family is properly set *)
  let*@ family_value = Rpc.get_chain_family sequencer chain_id_1 in
  Check.((family_value = chain_family) string)
    ~error_msg:"Expected chain_family to be %R, got %L" ;

  (* Verify the chain_family is properly set *)
  let*@ family_value = Rpc.get_chain_family sequencer chain_id_2 in
  Check.((family_value = chain_family) string)
    ~error_msg:"Expected chain_family to be %R, got %L" ;

  (* Verify that the balance of the bootstrap account is set by the command
     `make_l2_kernel_installer_config` *)
  let address_in_durable = remove_0x (String.lowercase_ascii address) in
  let address_balance =
    world_state_path ^ "eth_accounts/" ^ address_in_durable ^ "/balance"
  in
  let*@ rpc_balance = Rpc.state_value sequencer address_balance in
  let* _balance =
    match rpc_balance with
    | None ->
        Test.fail
          ~__LOC__
          "There should be a value at %s setup by the \
           make_l2_kernel_installer_config"
          address_balance
    | Some balance -> return balance
  in
  let chain_ids = "/evm/chain_ids" in
  let*@ rpc_chain_ids = Rpc.state_value sequencer chain_ids in
  let* chain_ids =
    match rpc_chain_ids with
    | None ->
        Test.fail
          ~__LOC__
          "There should be a value at %s setup by the \
           make_kernel_installer_config"
          chain_ids
    | Some chain_ids -> return chain_ids
  in
  let chain_ids_bytes = Hex.to_bytes (`Hex chain_ids) in

  match Evm_node_lib_dev_encoding.Rlp.decode chain_ids_bytes with
  | Ok
      Evm_node_lib_dev_encoding.Rlp.(
        List [Value rlp_chain_id_1; Value rlp_chain_id_2]) ->
      let str_chain_1 = Bytes.to_string rlp_chain_id_1 in
      Check.(string_of_int chain_id_1 = str_chain_1)
        Check.string
        ~error_msg:
          "Unexpected chain id from durable storage, expected: %L, got %R." ;
      let str_chain_2 = Bytes.to_string rlp_chain_id_2 in
      Check.(string_of_int chain_id_2 = str_chain_2)
        Check.string
        ~error_msg:
          "Unexpected chain id from durable storage, expected: %L, got %R." ;
      return ()
  | _ ->
      Test.fail
        ~__LOC__
        "Unexpected value decoded at path %s in durable storage"
        chain_ids

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
      ~sequencer:valid_sequencer.public_key
      ~output:valid_config
      ()
  in
  let*! () =
    Evm_node.make_kernel_installer_config
      ~sequencer:invalid_sequencer.public_key
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

let test_patch_state =
  register_test (* It's a node specific test. *)
    ~__FILE__
    ~kernel:Kernel.Latest
    ~enable_dal:false
    ~enable_multichain:false
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
  @@
  fun ({sequencer; sc_rollup_node = _; l2_chains; enable_multichain; _} as setup)
      _protocol
    ->
  let* _snapshot_file_before, snapshot_file, block_number =
    snapshots_setup ~desync setup
  in
  Log.info "Create new sequencer from snapshot." ;
  (* patch sequencer config if multichain *)
  let spawn_rpc = if enable_multichain then Some (Port.fresh ()) else None in
  let new_sequencer =
    let mode = Evm_node.mode sequencer in
    Evm_node.create ~node_setup:(Evm_node.make_setup ?spawn_rpc ()) ~mode ()
  in
  let* () = Process.check @@ Evm_node.spawn_init_config new_sequencer in
  let* () =
    match enable_multichain with
    | true ->
        let patch_config =
          Evm_node.patch_config_with_experimental_feature
            ~l2_chains
            ?spawn_rpc
            ()
        in
        Evm_node.Config_file.update new_sequencer patch_config
    | false -> unit
  in
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
let test_snapshots_reexport ~desync =
  register_all
    ~__FILE__
    ~tags:(["evm"; "sequencer"; "snapshots"] |> add_desync_tag desync)
    ~title:
      (sf
         "Import %ssequencer snapshot and re-export"
         (if desync then "desync " else ""))
    ~time_between_blocks:Nothing
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

let test_send_transaction_to_delayed_inbox =
  register_all
    ~__FILE__
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"]
    ~title:"Send a transaction to the delayed inbox"
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; _} _protocol ->
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
    ~__FILE__
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "deposit"]
    ~title:"Send a deposit to the delayed inbox"
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; _} _protocol ->
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
  let deposit_info =
    {receiver = EthereumAddr receiver.address; chain_id = None}
  in
  let* () =
    send_deposit_to_delayed_inbox
      ~amount
      ~bridge:l1_contracts.bridge
      ~depositor
      ~deposit_info
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
    ~__FILE__
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

let test_delayed_transfer_is_included =
  register_all
    ~__FILE__
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "inclusion"]
    ~title:"Delayed transaction is included"
    ~time_between_blocks:Nothing
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
  let* sender_balance_prev = Eth_cli.balance ~account:sender ~endpoint () in
  let* receiver_balance_prev = Eth_cli.balance ~account:receiver ~endpoint () in
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
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in
  let* sender_balance_next = Eth_cli.balance ~account:sender ~endpoint () in
  let* receiver_balance_next = Eth_cli.balance ~account:receiver ~endpoint () in
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
    ~__FILE__
    ~kernels:[Kernel.Latest]
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "inclusion"]
    ~title:"Largest possible delayed transaction is included"
    ~time_between_blocks:Nothing
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
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
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in
  unit

let test_delayed_deposit_is_included =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "inclusion"; "deposit"]
    ~title:"Delayed deposit is included"
    ~use_dal:ci_enabled_dal_registration
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
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
    Eth_cli.balance ~account:receiver.address ~endpoint ()
  in
  let deposit_info =
    {receiver = EthereumAddr receiver.address; chain_id = None}
  in
  let* () =
    send_deposit_to_delayed_inbox
      ~amount
      ~bridge:l1_contracts.bridge
      ~depositor
      ~deposit_info
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
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in
  let* receiver_balance_next =
    Eth_cli.balance ~account:receiver.address ~endpoint ()
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
    ~__FILE__
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
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
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
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in
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
    ~__FILE__
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
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
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

  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in

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
    ~__FILE__
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "failure"]
    ~title:"Delayed deposit is removed only when it is applied"
    ~enable_fa_bridge:false
    ~kernels:[Kernel.Latest]
    ~time_between_blocks:Nothing
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        kernel;
        _;
      }
      _protocol
    ->
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
  let deposit_info =
    {
      receiver = EthereumAddr "0x1074Fd1EC02cbeaa5A90450505cF3B48D834f3EB";
      chain_id = None;
    }
  in
  let* () =
    send_deposit_to_delayed_inbox
      ~amount:Tez.one
      ~bridge:l1_contracts.bridge
      ~depositor:Constant.bootstrap5
      ~deposit_info
      ~sc_rollup_node
      ~sc_rollup_address
      client
  in
  (* We bake enough blocks for the sequencer to realize there's a deposit. *)
  let* () =
    repeat 2 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
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
  bake_until_sync ~sc_rollup_node ~client ~sequencer ()

let test_invalid_delayed_transaction =
  register_all
    ~__FILE__
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "invalid"]
    ~title:"Delayed transaction is removed even when invalid"
    ~enable_fa_bridge:false
    ~kernels:[Kernel.Latest]
    ~time_between_blocks:Nothing
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
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

  (* We bake enough blocks for the sequencer to see the transaction and inject
     it. *)
  let* () =
    bake_until
      ~bake:(fun () ->
        let*@ _ = produce_block sequencer in
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
      ~result_f:(fun () ->
        let* size = Delayed_inbox.size (Sc_rollup_node sc_rollup_node) in
        if size = 0 then return (Some ()) else return None)
      ()
  in
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in

  (* There is no receipt for the transaction because it's invalid. *)
  let*@ receipt_opt = Rpc.get_transaction_receipt ~tx_hash:hash sequencer in
  Check.is_true
    (receipt_opt = None)
    ~error_msg:"The transaction should not have a receipt" ;
  unit

let call_fa_withdraw ?timestamp ?expect_failure ~sender ~endpoint ~evm_node
    ~ticket_owner ~routing_info ~amount ~ticketer ~content () =
  let* () =
    Eth_cli.add_abi
      ~label:"fa_withdrawal"
      ~abi:(predep_fa_bridge_abi_path ())
      ()
  in
  send_transaction_to_sequencer
    ?timestamp
    (Eth_cli.contract_send
       ?expect_failure
       ~source_private_key:sender.Eth_account.private_key
       ~endpoint
       ~abi_label:"fa_withdrawal"
       ~address:Solidity_contracts.Precompile.fa_bridge
       ~method_call:
         (sf
            {|withdraw("%s", "0x%s", %d, "0x%s", "0x%s")|}
            ticket_owner
            routing_info
            amount
            ticketer
            content))
    evm_node

let call_fa_fast_withdraw ?expect_failure ?timestamp ~sender ~sequencer
    ~ticket_owner ~receiver ~amount ~ticketer ~content
    ~fast_withdrawal_contract_address () =
  let endpoint = Evm_node.endpoint sequencer in
  let produce_block () = produce_block sequencer in
  let* receiver = ticket_creator receiver in
  let routing_info =
    String.concat "" [receiver |> Hex.of_bytes |> Hex.show; ticketer]
  in
  let* () =
    Eth_cli.add_abi
      ~label:"fa_fast_withdraw"
      ~abi:(predep_fa_bridge_abi_path ())
      ()
  in
  let call_fast_withdraw () =
    send_transaction_to_sequencer
      ?timestamp
      (Eth_cli.contract_send
         ?expect_failure
         ~source_private_key:sender.Eth_account.private_key
         ~endpoint
         ~abi_label:"fa_fast_withdraw"
         ~address:Solidity_contracts.Precompile.fa_bridge
         ~method_call:
           (sf
              {|fa_fast_withdraw("%s", "0x%s", %d, "0x%s", "0x%s", "%s", "%s")|}
              ticket_owner
              routing_info
              amount
              ticketer
              content
              fast_withdrawal_contract_address
              "0x0000000000000000000000000000000000000000000000000000000000000001"))
      sequencer
  in
  wait_for_application ~produce_block call_fast_withdraw

let test_fa_withdrawal_is_included =
  register_all
    ~__FILE__
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
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
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
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in
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

  let* () = bake_until_sync ~sequencer ~sc_rollup_node ~client () in

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
      ~evm_node:sequencer
      ~sc_rollup_node
      ~sc_rollup_address
      ~client
      ()
  in

  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

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

let test_fa_reentrant_deposit_reverts =
  register_all
    ~__FILE__
    ~da_fee:Wei.one
    ~tags:
      [
        "evm";
        "sequencer";
        "delayed_outbox";
        "fa_deposit";
        "enabled";
        "reentrancy";
      ]
    ~title:"FA Deposit cannot do reentrancy"
    ~enable_fa_bridge:true
    ~kernels:[Kernel.Latest]
    ~time_between_blocks:Nothing
    ~additional_uses:[Constant.octez_codec]
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        evm_version;
        _;
      }
      _protocol
    ->
  let* reentrancy = Solidity_contracts.reentrancy_test evm_version in
  let* () = Eth_cli.add_abi ~label:reentrancy.label ~abi:reentrancy.abi () in

  let sender = Eth_account.bootstrap_accounts.(0) in
  let* reentrancy, _create_tx =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~args:
           (sf
              {|["%s", "0x", 0, "0x00000000000000000000000000000000000000000000", "0x", 5]|}
              Solidity_contracts.Precompile.xtz_bridge)
         ~source_private_key:sender.private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi:reentrancy.label
         ~bin:reentrancy.bin)
      sequencer
  in

  let amount = 42 in
  let depositor = Constant.bootstrap5 in
  let receiver = Eth_account.bootstrap_accounts.(0).address in

  let* () =
    send_fa_deposit_to_delayed_inbox
      ~proxy:reentrancy
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
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in

  let* () =
    Eth_cli.add_abi ~label:"claim" ~abi:(predep_fa_bridge_abi_path ()) ()
  in
  let claim =
    Eth_cli.contract_send
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~endpoint:(Evm_node.endpoint sequencer)
      ~abi_label:"claim"
      ~address:Solidity_contracts.Precompile.fa_bridge
      ~method_call:(sf {|claim(%d)|} 0)
  in
  let produce_block () = Rpc.produce_block sequencer in
  let* _res = wait_for_application ~produce_block claim in

  (* If the call to the proxy fails, the owner of the ticket is the receiver,
     not the proxy.

     As the proxy is doing reeantrancy, the inner call should revert, therefore
     the receiver should be the owner.
  *)
  let* zero_ticket_hash = ticket_hash l1_contracts.ticket_router_tester 0 in
  let* receiver_balance =
    ticket_balance
      ~ticket_hash:zero_ticket_hash
      ~account:receiver
      (Either.Right sequencer)
  in
  Check.((receiver_balance > 0) int)
    ~error_msg:"As the deposit fails, the receiver should own the ticket" ;
  let* proxy_balance =
    ticket_balance
      ~ticket_hash:zero_ticket_hash
      ~account:reentrancy
      (Either.Right sequencer)
  in
  Check.((proxy_balance = 0) int)
    ~error_msg:"As the deposit fails, the proxy must not own the ticket" ;

  unit

let test_delayed_deposit_from_init_rollup_node =
  register_all
    ~__FILE__
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "init"]
    ~title:"Delayed inbox is populated at init from rollup node"
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        l2_chains;
        enable_multichain;
        _;
      }
      _protocol
    ->
  let receiver = "0x1074Fd1EC02cbeaa5A90450505cF3B48D834f3EB" in
  let* receiver_balance_prev =
    Eth_cli.balance ~account:receiver ~endpoint:(Evm_node.endpoint sequencer) ()
  in
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in
  (* We don't care about this sequencer. *)
  let* () = Evm_node.terminate sequencer in
  (* Send the deposit to delayed inbox, no sequencer will be listening to the
     event. *)
  let amount = Tez.of_int 16 in
  let depositor = Constant.bootstrap5 in
  let deposit_info = {receiver = EthereumAddr receiver; chain_id = None} in
  let* () =
    send_deposit_to_delayed_inbox
      ~amount
      ~bridge:l1_contracts.bridge
      ~depositor
      ~deposit_info
      ~sc_rollup_node
      ~sc_rollup_address
      client
  in
  (* Bake an extra block for a finalized deposit. *)
  let* _lvl = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* _lvl = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

  (* Run a new sequencer that is initialized from a rollup node that has the
     delayed deposit in its state. *)
  let spawn_rpc = if enable_multichain then Some (Port.fresh ()) else None in
  let new_sequencer =
    Evm_node.create
      ~node_setup:(Evm_node.make_setup ?spawn_rpc ())
      ~mode:(Evm_node.mode sequencer)
      ()
  in
  let* () = Process.check @@ Evm_node.spawn_init_config new_sequencer in
  let* () =
    match enable_multichain with
    | true ->
        let patch_config =
          Evm_node.patch_config_with_experimental_feature
            ~l2_chains
            ?spawn_rpc
            ()
        in
        Evm_node.Config_file.update new_sequencer patch_config
    | false -> unit
  in
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
    bake_until_sync ~sc_rollup_node ~sequencer:new_sequencer ~client ()
  in
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in
  let* receiver_balance_next =
    Eth_cli.balance
      ~account:receiver
      ~endpoint:(Evm_node.endpoint new_sequencer)
      ()
  in
  Check.((receiver_balance_next > receiver_balance_prev) Wei.typ)
    ~error_msg:"Expected a bigger balance" ;
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
  @@
  fun {
        sc_rollup_node;
        sequencer;
        observer;
        client;
        l2_chains;
        enable_multichain;
        _;
      }
      _protocol
    ->
  (* a sequencer is needed to produce an initial block *)
  let* () =
    repeat 5 (fun () ->
        let*@ _ = produce_block sequencer in
        unit)
  in
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
  let* () = Evm_node.terminate sequencer
  and* () = Evm_node.terminate observer in
  let spawn_rpc = if enable_multichain then Some (Port.fresh ()) else None in
  let evm_node' =
    Evm_node.create
      ~node_setup:(Evm_node.make_setup ?spawn_rpc ())
      ~mode:(Evm_node.mode sequencer)
      ()
  in
  let* () = Process.check @@ Evm_node.spawn_init_config evm_node' in
  let* () =
    match enable_multichain with
    | true ->
        let patch_config =
          Evm_node.patch_config_with_experimental_feature
            ~l2_chains
            ?spawn_rpc
            ()
        in
        Evm_node.Config_file.update evm_node' patch_config
    | false -> unit
  in
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

let test_init_from_rollup_node_with_delayed_inbox =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~kernels:[Kernel.Latest]
    ~tags:["evm"; "rollup_node"; "init"; "delayed_inbox"; "omit"]
    ~title:
      "Init evm node sequencer data dir from a rollup node data dir with \
       delayed items"
  @@
  fun {
        sc_rollup_node;
        sequencer;
        observer;
        client;
        l1_contracts;
        sc_rollup_address;
        l2_chains;
        enable_multichain;
        _;
      }
      _protocol
    ->
  (* The sequencer is needed to produce an initial block for the init from
     rollup node to work. *)
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
  let* () = Evm_node.terminate sequencer in
  let* () = Evm_node.terminate observer in
  (* Sends a deposit to the delayed inbox. *)
  let deposit_info =
    {
      receiver = EthereumAddr "0xB7A97043983f24991398E5a82f63F4C58a417185";
      chain_id = None;
    }
  in
  let* () =
    send_deposit_to_delayed_inbox
      ~amount:Tez.one
      ~bridge:l1_contracts.bridge
      ~depositor:Constant.bootstrap5
      ~deposit_info
      ~sc_rollup_node
      ~sc_rollup_address
      client
  in
  (* Finalize the transaction. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

  (* Start a new sequencer, the previous sequencer is doomed. *)
  let spawn_rpc = if enable_multichain then Some (Port.fresh ()) else None in
  let sequencer =
    Evm_node.create
      ~node_setup:(Evm_node.make_setup ?spawn_rpc ())
      ~mode:(Evm_node.mode sequencer)
      ()
  in
  let* () = Process.check @@ Evm_node.spawn_init_config sequencer in
  let* () =
    match enable_multichain with
    | true ->
        let patch_config =
          Evm_node.patch_config_with_experimental_feature
            ~l2_chains
            ?spawn_rpc
            ()
        in
        Evm_node.Config_file.update sequencer patch_config
    | false -> unit
  in
  let* () = Evm_node.init_from_rollup_node_data_dir sequencer sc_rollup_node in
  let* () = Evm_node.run sequencer in
  (* The sequencer should have items in its delayed inbox. *)
  let* delayed_inbox_size = Delayed_inbox.size (Evm_node sequencer) in
  Check.((delayed_inbox_size = 1) int)
    ~error_msg:"The sequencer should have the delayed inbox in its state" ;
  (* Start a new observer, we will ask it to omit the delayed transactions. *)
  let observer =
    Evm_node.create
      ~mode:
        (Evm_node.Observer
           {
             rollup_node_endpoint = Some (Sc_rollup_node.endpoint sc_rollup_node);
             evm_node_endpoint = Evm_node.endpoint sequencer;
           })
      ()
  in
  let* () = Process.check @@ Evm_node.spawn_init_config observer in
  let* () =
    match enable_multichain with
    | true ->
        let patch_config =
          Evm_node.patch_config_with_experimental_feature ~l2_chains ()
        in
        Evm_node.Config_file.update observer patch_config
    | false -> unit
  in
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
  let* _ = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
  let* () = check_head_consistency ~left:sequencer ~right:observer () in
  unit

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

let test_get_balance_block_param =
  register_all
    ~__FILE__
    ~tags:["evm"; "sequencer"; "rpc"; "get_balance"; "block_param"]
    ~title:"RPC method getBalance uses block parameter"
    ~time_between_blocks:Nothing
  @@
  fun {sequencer; sc_rollup_node; client; enable_multichain; l2_chains; _}
      _protocol
    ->
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
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in
  let* _ =
    repeat 2 (fun _ ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  let observer_partial_history =
    Evm_node.create
      ~node_setup:
        (Evm_node.make_setup
           ~name:"partial_history"
           ~initial_kernel:"evm_kernel.wasm"
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
  let* () =
    Process.check @@ Evm_node.spawn_init_config observer_partial_history
  in
  let* () =
    match enable_multichain with
    | true ->
        let patch_config =
          Evm_node.patch_config_with_experimental_feature ~l2_chains ()
        in
        Evm_node.Config_file.update observer_partial_history patch_config
    | false -> unit
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
    ~__FILE__
    ~tags:["evm"; "sequencer"; "rpc"; "get_block_by_number"; "block_param"]
    ~title:"RPC method getBlockByNumber uses block parameter"
    ~time_between_blocks:Nothing
  @@
  fun {
        sequencer;
        observer;
        sc_rollup_node;
        client;
        enable_multichain;
        l2_chains;
        _;
      }
      _protocols
    ->
  let observer_offset = 3l in
  let* () =
    repeat Int32.(to_int observer_offset) @@ fun () ->
    next_evm_level ~evm_node:sequencer ~sc_rollup_node ~client
  in
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in
  let* _ =
    repeat 2 (fun _ ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  let observer_partial_history =
    Evm_node.create
      ~node_setup:
        (Evm_node.make_setup
           ~name:"partial_history"
           ?initial_kernel:(Evm_node.initial_kernel observer)
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
  let* () =
    Process.check @@ Evm_node.spawn_init_config observer_partial_history
  in
  let* () =
    match enable_multichain with
    | true ->
        let patch_config =
          Evm_node.patch_config_with_experimental_feature ~l2_chains ()
        in
        Evm_node.Config_file.update observer_partial_history patch_config
    | false -> unit
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
    ~__FILE__
    ~tags:["evm"; "sequencer"; "rpc"; "block_param"; "counter"]
    ~title:"Supports extended block parameter"
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocols ->
  (*
     In this test we will deploy a counter contract, increments its counter
     at multiple consecutives blocks, and check the counter using block
     parameter.
     As the counter contract has a view, we can test the block parameter
     both for read only RPCs such as [eth_getStorageAt] and simulation
     such as [eth_call].
  *)
  let* counter_resolved = Solidity_contracts.counter evm_version in
  let* () =
    Eth_cli.add_abi ~label:counter_resolved.label ~abi:counter_resolved.abi ()
  in
  (* Deploy the contract. *)
  let* contract, _tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.deploy
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

let test_sequencer_is_reimbursed =
  let tbb = 1. in
  (* We use an arbitrary address for the pool address, the goal is just to
     verify its balance increases. *)
  let sequencer_pool_address = "0xb7a97043983f24991398e5a82f63f4c58a417185" in
  register_all
    ~__FILE__
    ~da_fee:Wei.one
    ~time_between_blocks:(Time_between_blocks tbb)
    ~sequencer_pool_address
    ~tags:["evm"; "sequencer"; "transaction"; "reimbursed"]
    ~title:"Sequencer is reimbursed for DA fees"
  @@ fun {sequencer = sequencer_node; _} _protocol ->
  let* balance =
    Eth_cli.balance
      ~account:sequencer_pool_address
      ~endpoint:Evm_node.(endpoint sequencer_node)
      ()
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
    Eth_cli.get_receipt ~endpoint:(Evm_node.endpoint sequencer_node) ~tx:txn ()
  in

  match receipt with
  | Some receipt when receipt.status ->
      let* balance =
        Eth_cli.balance
          ~account:sequencer_pool_address
          ~endpoint:Evm_node.(endpoint sequencer_node)
          ()
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
      ~upgrade_to:kernel
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
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in

  (* Produce a block where the upgrade would happen *)
  let*@ _ = produce_block ~timestamp:"2020-01-01T00:00:10Z" sequencer in
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in

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
         ~key:"/evm/blueprints/42"
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
         ~key:"/evm/blueprints/42/generation"
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
         ~key:"/evm/blueprints/generation"
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

let test_delayed_inbox_flushing_event =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~delayed_inbox_timeout:0
    ~delayed_inbox_min_levels:1
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "timeout"; "flush"]
    ~title:"Flush delayed inbox event"
    ~use_dal:Register_without_feature
    ~use_multichain:Register_without_feature
    ~kernels:[Latest]
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
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

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
  let* l1_level = Client.level client in
  (* Wait for the events and finalize the level. *)
  let wait_for_processed_l1_level =
    Evm_node.wait_for_processed_l1_level ~level:l1_level sequencer
  in
  let wait_for_flush = Evm_node.wait_for_flush_delayed_inbox sequencer in

  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

  (* Wait until the event is completely processed. Head of sequencer and rollup node
     should be in sync. *)
  let* _ = wait_for_flush and* _ = wait_for_processed_l1_level in
  let* () =
    check_rollup_head_consistency ~sc_rollup_node ~evm_node:sequencer ()
  in

  unit

let test_flushed_blueprint_reorg =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~delayed_inbox_timeout:0
    ~delayed_inbox_min_levels:1
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "timeout"; "flush"; "reorg"]
    ~title:"Flush delayed inbox event leads to reorg"
    ~use_dal:Register_without_feature
    ~use_multichain:
      (* TODO #7843: Adapt this test to multichain context *)
      Register_without_feature
    ~kernels:[Latest]
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
  (* Observer does not track rollup node events, therefore it will blindly
     follow the sequencer then reorganizes when the sequencer will push the
     flushed blueprint. *)
  let* () =
    Evm_node.run ~extra_arguments:["--dont-track-rollup-node"] observer
  in
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

  let craft_raw_transfer account =
    Cast.craft_tx
      ~source_private_key:account.Eth_account.private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:23_300
      ~value:(Wei.of_eth_int 1)
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  in
  let* raw_transfer = craft_raw_transfer Eth_account.bootstrap_accounts.(0) in
  let* raw_transfer_2 = craft_raw_transfer Eth_account.bootstrap_accounts.(1) in
  (* Add a transaction to the delayed inbox. *)
  let* _hash =
    send_raw_transaction_to_delayed_inbox
      ~wait_for_next_level:false
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      raw_transfer
  in
  let* _hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      ~sender:Constant.bootstrap3
      raw_transfer_2
  in

  (* We mark at which level the delayed inbox item was added. *)
  let* add_delayed_tx_level = Client.level client in
  let wait_for_add_delayed_inbox =
    Evm_node.wait_for_processed_l1_level ~level:add_delayed_tx_level sequencer
  in
  (* We mark at which level the delayed inbox was flushed. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* flushed_delayed_inbox_level = Client.level client in
  let wait_for_processed_l1_level =
    Evm_node.wait_for_processed_l1_level
      ~level:flushed_delayed_inbox_level
      sequencer
  in
  let wait_for_flush = Evm_node.wait_for_flush_delayed_inbox sequencer in
  (* A new block will make the {!add_delayed_tx_level} final. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* _ = wait_for_add_delayed_inbox in

  (* Produce a bunch of L2 blocks. The sequencer is aware of the delayed inbox
     item but refuses to include it. *)
  let* () =
    repeat 5 (fun _ ->
        let*@ _ =
          Rpc.produce_block ~with_delayed_transactions:false sequencer
        in
        unit)
  in
  let*@ speculative_head = Rpc.get_block_by_number ~block:"latest" sequencer in

  (* A new block will make the {!flushed_delayed_inbox_levle} final. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

  (* Wait until the event is completely processed. Head of sequencer and rollup node
     should be in sync. *)
  let* _ = wait_for_flush and* _ = wait_for_processed_l1_level in
  let* () =
    check_rollup_head_consistency ~sc_rollup_node ~evm_node:sequencer ()
  in

  (* Speculative head has been removed as the sequencer was forced to
     reorganize the blocks because of the flushed blueprint. *)
  let*@? (err : Rpc.error) =
    Rpc.get_block_by_hash ~block:speculative_head.hash sequencer
  in
  Check.(err.message =~ rex "Block .* not found")
    ~error_msg:"Expected error to match %R, got %L" ;

  (* Sequencer resumes block production. *)
  let* () =
    repeat 5 (fun _ ->
        let*@ _ = Rpc.produce_block sequencer in
        unit)
  in
  let*@ sequencer_head = Rpc.block_number sequencer in

  (* As the observer was down during the reorganization, restarting it
     will be enough, it'll apply the new branch as expected. *)
  let* () =
    Evm_node.wait_for_blueprint_applied observer (Int32.to_int sequencer_head)
  in

  unit

let test_multiple_flushed_blueprints =
  register_all
    ~__FILE__
    ~max_delayed_inbox_blueprint_length:1
    ~time_between_blocks:Nothing
    ~delayed_inbox_timeout:0
    ~delayed_inbox_min_levels:1
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:
      [
        "evm";
        "sequencer";
        "delayed_inbox";
        "timeout";
        "flush";
        "reorg";
        "multiple";
      ]
    ~title:"Multiple flushed blueprints"
    ~use_dal:Register_without_feature
    ~kernels:[Latest]
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
  (* Observer does not track rollup node events, therefore it will blindly
     follow the sequencer then reorganizes when the sequencer will push the
     flushed blueprint. *)
  let* () =
    Evm_node.run ~extra_arguments:["--dont-track-rollup-node"] observer
  in
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

  (* Add 2 transactions to the delayed inbox. *)
  let* tx1 =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:23_300
      ~value:(Wei.of_eth_int 1)
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  and* tx2 =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:1
      ~gas_price:1_000_000_000
      ~gas:23_300
      ~value:(Wei.of_eth_int 1)
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  in

  let* _hash1 =
    send_raw_transaction_to_delayed_inbox
      ~wait_for_next_level:false
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      ~sender:Constant.bootstrap2
      tx1
  and* _hash2 =
    send_raw_transaction_to_delayed_inbox
      ~wait_for_next_level:false
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      ~sender:Constant.bootstrap3
      tx2
  in

  let*@ rollup_head_before_flush = rollup_level sc_rollup_node in

  (* Both delayed transaction added *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  (* Flush of delayed inbox in kernel *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  (* We mark at which level the delayed inbox was flushed. *)
  let* flushed_delayed_inbox_level = Client.level client in

  let wait_for_processed_l1_level =
    Evm_node.wait_for_processed_l1_level
      ~level:flushed_delayed_inbox_level
      sequencer
  in
  let wait_for_first_flush =
    Evm_node.wait_for_flush_delayed_inbox ~level:1 sequencer
  in
  let wait_for_second_flush =
    Evm_node.wait_for_flush_delayed_inbox ~level:2 sequencer
  in

  (* Make the {!flushed_delayed_inbox_level} final and ait until the
     event is completely processed. Head of sequencer and rollup node should
     be in sync. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client
  and* _ = wait_for_first_flush
  and* _ = wait_for_second_flush
  and* _ = wait_for_processed_l1_level in

  let* () =
    check_rollup_head_consistency ~sc_rollup_node ~evm_node:sequencer ()
  in

  let*@ rollup_head_after_flush = rollup_level sc_rollup_node in
  Check.(
    (Int32.to_int rollup_head_before_flush + 2
    = Int32.to_int rollup_head_after_flush)
      int)
    ~error_msg:
      (sf
         "Expected 2 flushed blueprints, rollup level before flush is %ld, \
          after is %%R"
         rollup_head_before_flush) ;

  (* Sequencer resumes block production. *)
  let* () =
    repeat 5 (fun _ ->
        let*@ _ = Rpc.produce_block sequencer in
        unit)
  in
  let*@ sequencer_head = Rpc.block_number sequencer in

  let* () =
    Evm_node.wait_for_blueprint_applied observer (Int32.to_int sequencer_head)
  in

  unit

let test_observer_reorg_on_blueprint_stream =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~delayed_inbox_timeout:0
    ~delayed_inbox_min_levels:1
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "timeout"; "flush"; "reorg"]
    ~title:
      "Observer tracking a rollup node reorganizes after blueprint on stream"
    ~use_dal:Register_without_feature
    ~use_multichain:
      (* TODO #7843: Adapt this test to multichain context *)
      Register_without_feature
    ~kernels:[Latest]
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
  (*
     This test focuses on the case where an observer is connected to a rollup
     node.

     The observer will start by detecting a divergence and reset to the state
     prior to the delayed inbox flushing.

     It will then reset itself (again) and start accepting the new blueprints
     from the sequencer.
  *)
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

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
  (* Add a transaction to the delayed inbox. *)
  let* _hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      raw_transfer
  in
  (* We mark at which level the delayed inbox item was added. *)
  let* add_delayed_inbox_level = Client.level client in
  let wait_for_add_delayed_inbox =
    Evm_node.wait_for_processed_l1_level
      ~level:add_delayed_inbox_level
      sequencer
  in
  (* We mark at which level the delayed inbox was flushed. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* flushed_delayed_inbox_level = Client.level client in
  let wait_for_processed_l1_level =
    Evm_node.wait_for_processed_l1_level
      ~level:flushed_delayed_inbox_level
      sequencer
  in
  let wait_for_flush = Evm_node.wait_for_flush_delayed_inbox sequencer in
  let wait_for_observer_reset = Evm_node.wait_for_reset observer in
  (* A new block will make the {!add_delayed_inbox_level} final. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* _ = wait_for_add_delayed_inbox in

  (* Produce a bunch of L2 blocks. The sequencer is aware of the delayed inbox
     item but refuses to include it. *)
  let* () =
    repeat 5 (fun _ ->
        let*@ _ =
          Rpc.produce_block ~with_delayed_transactions:false sequencer
        in
        unit)
  in

  (* A new block will make the {!flushed_delayed_inbox_level} final. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

  (* Wait until the event is completely processed. Head of sequencer and rollup
     should be in sync. *)
  let* _ = wait_for_flush
  and* _ = wait_for_processed_l1_level
  and* () = wait_for_observer_reset in

  let* () =
    check_rollup_head_consistency ~sc_rollup_node ~evm_node:sequencer ()
  in

  (* Sequencer resumes block production. *)
  let* () =
    repeat 5 (fun _ ->
        let*@ _ = Rpc.produce_block sequencer in
        unit)
  in
  let*@ sequencer_head = Rpc.block_number sequencer in
  (* Observer accepts the new blocks. *)
  let* () =
    Evm_node.wait_for_blueprint_applied observer (Int32.to_int sequencer_head)
  in

  unit

let test_observer_reorg_on_blueprint_catchup =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~delayed_inbox_timeout:0
    ~delayed_inbox_min_levels:1
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "timeout"; "flush"; "reorg"]
    ~title:"Observer reorganizes after blueprint on catchup"
    ~use_dal:Register_without_feature
    ~kernels:[Latest]
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
  (*
     This test focuses on the case where an observer detects a divergence
     and reset to the state prior to the delayed inbox flushing, when the
     blueprint is seen during a catchup.
  *)
  let* () = Evm_node.terminate observer in
  let* () =
    Evm_node.run ~extra_arguments:["--dont-track-rollup-node"] observer
  in
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

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
  (* Add a transaction to the delayed inbox. *)
  let* _hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      raw_transfer
  in
  (* We mark at which level the delayed inbox item was added. *)
  let* add_delayed_inbox_level = Client.level client in
  let wait_for_add_delayed_inbox =
    Evm_node.wait_for_processed_l1_level
      ~level:add_delayed_inbox_level
      sequencer
  in
  (* We mark at which level the delayed inbox was flushed. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* flushed_delayed_inbox_level = Client.level client in
  let wait_for_processed_l1_level =
    Evm_node.wait_for_processed_l1_level
      ~level:flushed_delayed_inbox_level
      sequencer
  in
  let wait_for_flush = Evm_node.wait_for_flush_delayed_inbox sequencer in
  (* A new block will make the {!add_delayed_inbox_level} final. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* _ = wait_for_add_delayed_inbox in

  (* Produce a bunch of L2 blocks. The sequencer is aware of the delayed inbox
     item but refuses to include it. *)
  let* () =
    repeat 2 (fun _ ->
        let*@ _ =
          Rpc.produce_block ~with_delayed_transactions:false sequencer
        in
        unit)
  in
  let*@ sequencer_head = Rpc.block_number sequencer in
  (* Observer accepts these blocks and shutdowns. *)
  let* () =
    Evm_node.wait_for_blueprint_applied observer (Int32.to_int sequencer_head)
  in
  let* () = Evm_node.terminate observer in

  (* A new block will make the {!flushed_delayed_inbox_level} final. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

  (* Wait until the event is completely processed. Head of sequencer and rollup
     should be in sync. *)
  let* _ = wait_for_flush and* _ = wait_for_processed_l1_level in

  let* () =
    check_rollup_head_consistency ~sc_rollup_node ~evm_node:sequencer ()
  in

  (* Sequencer resumes block production. *)
  let* () =
    repeat 5 (fun _ ->
        let*@ _ = Rpc.produce_block sequencer in
        unit)
  in
  let*@ sequencer_head = Rpc.block_number sequencer in

  (* Restart the observer, observer will be at level N. The flushed delayed
     blueprint happened before level N, therefore it will have to realize
     something is wrong and traverse the chain backward. *)
  let* () =
    Evm_node.run ~extra_arguments:["--dont-track-rollup-node"] observer
  in

  (* Observer accepts the new blocks. *)
  let* () =
    Evm_node.wait_for_blueprint_applied observer (Int32.to_int sequencer_head)
  in

  unit

(* Same test as reorg, but the delayed transaction event is applied at the same
   L2 level as the flush event. *)
let test_flushed_blueprint_reorg_late =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~delayed_inbox_timeout:0
    ~delayed_inbox_min_levels:1
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:
      ["evm"; "sequencer"; "delayed_inbox"; "timeout"; "flush"; "reorg"; "late"]
    ~title:"Flush delayed inbox event leads to reorg, including late delayed tx"
    ~use_dal:Register_without_feature
    ~use_multichain:
      (* TODO #7843: Adapt this test to multichain context *)
      Register_without_feature
    ~kernels:[Latest]
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
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

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
  (* Add a transaction to the delayed inbox. *)
  let* _hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      raw_transfer
  in
  (* We mark at which level the delayed inbox item was added. *)
  let* add_delayed_tx_level = Client.level client in

  (* the sequencer produces blocks before seeing the delayed transactions
     (speculative execution). *)
  let* () =
    repeat 4 (fun _ ->
        let*@ _ =
          Rpc.produce_block ~with_delayed_transactions:false sequencer
        in
        unit)
  in

  (* We mark at which level the delayed inbox was flushed. *)
  let* flushed_delayed_inbox_level =
    Rollup.next_rollup_node_level ~sc_rollup_node ~client
  in

  (* A new block will make the add delayed inbox event final. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client
  and* _ =
    Evm_node.wait_for_processed_l1_level ~level:add_delayed_tx_level sequencer
  in

  (* Produce a few l2 blocks (speculative execution) after seeing delayed tx *)
  let* () =
    repeat 4 (fun _ ->
        let*@ _ =
          Rpc.produce_block ~with_delayed_transactions:false sequencer
        in
        unit)
  in

  let*@ speculative_head = Rpc.get_block_by_number ~block:"latest" sequencer in

  (* A new block will make the {!flushed_delayed_inbox_level} final. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client
  and* _ =
    (* Wait until the event is completely processed. Head of sequencer and rollup
       should be in sync. *)
    Evm_node.wait_for_processed_l1_level
      ~level:flushed_delayed_inbox_level
      sequencer
  and* _ = Evm_node.wait_for_flush_delayed_inbox sequencer in
  let* () =
    check_rollup_head_consistency ~sc_rollup_node ~evm_node:sequencer ()
  in

  (* Speculative head has been removed as the sequencer was forced to
     reorganize the blocks because of the flushed blueprint. *)
  let*@? (err : Rpc.error) =
    Rpc.get_block_by_hash ~block:speculative_head.hash sequencer
  in

  Check.(err.message =~ rex "Block .* not found")
    ~error_msg:"Expected error to match %R, got %L" ;

  (* Sequencer resumes block production. *)
  let* () =
    repeat 4 (fun _ ->
        let*@ _ = Rpc.produce_block sequencer in
        unit)
  in
  let*@ sequencer_head = Rpc.block_number sequencer in

  (* As the observer was done during the reorganization, restarting it
     will be enough, it'll apply the new branch as expected. *)
  let* () =
    Evm_node.wait_for_blueprint_applied observer (Int32.to_int sequencer_head)
  in
  let* () = check_head_consistency ~left:observer ~right:sequencer () in

  unit

(* Same test as reorg, but the delayed transaction event is applied at the same
   L2 level as the flush event, and transactions were included but too late. *)
let test_flushed_blueprint_reorg_done_late =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~delayed_inbox_timeout:0
    ~delayed_inbox_min_levels:1
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:
      [
        "evm";
        "sequencer";
        "delayed_inbox";
        "timeout";
        "flush";
        "reorg";
        "late";
        "reinject";
      ]
    ~title:
      "Flush delayed inbox event leads to reorg, including delayed tx included \
       too late"
    ~use_dal:Register_without_feature
    ~use_multichain:
      (* TODO #7843: Adapt this test to multichain context *)
      Register_without_feature
    ~kernels:[Latest]
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
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
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
  (* Add a transaction to the delayed inbox. *)
  let* _hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      raw_transfer
  in
  (* We mark at which level the delayed inbox item was added. *)
  let* add_delayed_tx_level = Client.level client in

  (* The sequencer produces blocks before seeing the delayed transactions
     (speculative execution). *)
  let* () =
    repeat 4 (fun _ ->
        let*@ _ =
          Rpc.produce_block ~with_delayed_transactions:false sequencer
        in
        unit)
  in

  (* We mark at which level the delayed inbox was flushed. *)
  let* flushed_delayed_inbox_level =
    Rollup.next_rollup_node_level ~sc_rollup_node ~client
  in

  (* A new block will make the add delayed event. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client
  and* _ =
    Evm_node.wait_for_processed_l1_level ~level:add_delayed_tx_level sequencer
  in

  (* Produce a few l2 blocks (speculative execution) after seeing delayed tx *)
  let* () =
    repeat 4 (fun _ ->
        let*@ _ =
          Rpc.produce_block ~with_delayed_transactions:false sequencer
        in
        unit)
  in
  (* Produce blocks **with** delayed transactions, but too late. *)
  let* () =
    repeat 4 (fun _ ->
        let*@ _ = Rpc.produce_block ~with_delayed_transactions:true sequencer in
        unit)
  in
  let*@ speculative_head = Rpc.get_block_by_number ~block:"latest" sequencer in

  (* A new block will make the {!flushed_delayed_inbox_level} final. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client
  and* _ =
    (* Wait until the event is completely processed. Head of sequencer and rollup node
       should be in sync. *)
    Evm_node.wait_for_processed_l1_level
      ~level:flushed_delayed_inbox_level
      sequencer
  and* _ = Evm_node.wait_for_flush_delayed_inbox sequencer in

  (* Wait until the event is completely processed. Head of sequencer and rollup node
     should be in sync. *)
  let* () =
    check_rollup_head_consistency ~sc_rollup_node ~evm_node:sequencer ()
  in

  (* Speculative head has been removed as the sequencer was forced to
     reorganize the blocks because of the flushed blueprint. *)
  let*@? (err : Rpc.error) =
    Rpc.get_block_by_hash ~block:speculative_head.hash sequencer
  in

  Check.(err.message =~ rex "Block .* not found")
    ~error_msg:"Expected error to match %R, got %L" ;

  (* Sequencer resumes block production. *)
  let* () =
    repeat 4 (fun _ ->
        let*@ _ = Rpc.produce_block sequencer in
        unit)
  in
  let*@ sequencer_head = Rpc.block_number sequencer in

  (* As the observer was done during the reorganization, restarting it
     will be enough, it'll apply the new branch as expected. *)
  let* () =
    Evm_node.wait_for_blueprint_applied observer (Int32.to_int sequencer_head)
  in
  let* () = check_head_consistency ~left:observer ~right:sequencer () in

  unit

let test_upgrade_injected_before_flush_level =
  let genesis_timestamp = "2020-01-01T00:00:00Z" in
  let activation_timestamp = "2020-01-01T00:01:00Z" in
  let after_timestamp = "2020-01-01T00:02:00Z" in
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~time_between_blocks:Nothing
    ~genesis_timestamp:Client.(At (Time.of_notation_exn genesis_timestamp))
    ~delayed_inbox_timeout:0
    ~delayed_inbox_min_levels:1
    ~tags:
      [
        "evm";
        "sequencer";
        "delayed_inbox";
        "timeout";
        "flush";
        "reorg";
        "upgrade";
        "before";
      ]
    ~title:"Upgrade injected before flushed level"
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        observer;
        kernel;
        _;
      }
      _protocol
    ->
  (* This test the situation where the injected before of the upgrade
     event is before the flushed level.

     It can be a bit tedious to read the test, here infinitys what happens:

     (l2_level)           0  <- Upgrade event is seen at head = 0.
                          1  <- Upgrade event is at injected_before here.
                          2  <- Delayed inbox item event is seen.
                          3' <- Flushed blueprint

     The flushed blueprint logic will not be able to find the upgrade event
     in the invalid branch, but the "generic" logic to reset to finalized level
     is supposed to find the latest unapplied upgrade that was seen
     before the flushed level.

     To ensure everything is OK after the flush we trigger the update and check
     that both the sequencer and observer are consistent with the rollup node.
  *)
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
  (* Set a pending upgrade before the flush level. *)
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
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

  (* Produces 2 valid blocks and wait until they are synchronized. *)
  let*@ _ = produce_block ~timestamp:genesis_timestamp sequencer in
  let*@ _ = produce_block ~timestamp:genesis_timestamp sequencer in
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

  (* Send a delayed transaction. *)
  let* _hash =
    let* tx =
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
    send_raw_transaction_to_delayed_inbox
      ~wait_for_next_level:true
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      tx
  in
  (* Flush the delayed inbox. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* flush_l1_level = Client.level client in

  (* Make the flush final. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

  let* _ = Evm_node.wait_for_processed_l1_level ~level:flush_l1_level sequencer
  and* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

  (* The sequencer should trigger the upgrade *)
  let* _ =
    repeat 2 (fun () ->
        let* _ = produce_block ~timestamp:after_timestamp sequencer in
        unit)
  and* rh_seq, lvl_seq = Evm_node.wait_for_successful_upgrade sequencer in

  let*@ sequencer_head = Rpc.block_number sequencer in

  (* The observer should follow the reorg and reach the same head *)
  let* () =
    Evm_node.wait_for_blueprint_applied observer (Int32.to_int sequencer_head)
  and* rh_obs, lvl_obs = Evm_node.wait_for_successful_upgrade observer in

  Check.((rh_seq = rh_obs) string) ~error_msg:"Root hash should be the same" ;
  Check.((lvl_seq = lvl_obs) int)
    ~error_msg:"Level of upgrade should be the same" ;

  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

  let* () =
    check_rollup_head_consistency ~sc_rollup_node ~evm_node:sequencer ()
  in
  unit

let test_upgrade_activated_after_flush_level =
  let genesis_timestamp = "2020-01-01T00:00:00Z" in
  let activation_timestamp = "2020-01-01T00:01:00Z" in
  let after_timestamp = "2020-01-01T00:02:00Z" in
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~time_between_blocks:Nothing
    ~genesis_timestamp:Client.(At (Time.of_notation_exn genesis_timestamp))
    ~delayed_inbox_timeout:0
    ~delayed_inbox_min_levels:1
    ~tags:
      [
        "evm";
        "sequencer";
        "delayed_inbox";
        "timeout";
        "flush";
        "reorg";
        "upgrade";
        "after";
        "reactivate";
      ]
    ~title:
      "Upgrade injected and activated after flushed level (on invalid branch)"
    ~use_multichain:
      (* TODO #7843: Adapt this test to multichain context *)
      Register_without_feature
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        observer;
        kernel;
        _;
      }
      _protocol
    ->
  (* This test the situation where the injected before of the upgrade
     event is AT the flushed level and activated in invalid branch.

     It can be a bit tedious to read the test, here is what happens:

     (l2_level)             0
                            1
                            2
                           /
                          /
        Upgrade event -> 3
                         5
        Activation    -> 6
                         7

     Then a flush happens at level 3, invalidating the branch and creating a new
     one.

     (l2_level)             0
                            1
                            2
                           / \
                          /   \
                         X     3' <- Flushed blueprint, upgrade event
                               4' <- activation

     The upgrade event must be retrieved from the invalid branch before
     resetting to level 2 and applying level 3'.
  *)
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
  (* Set a pending upgrade at the flush level. *)
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
  (* Produces 2 valid blocks and wait until they are synchronized. *)
  let*@ _ = produce_block ~timestamp:genesis_timestamp sequencer in
  let*@ _ = produce_block ~timestamp:genesis_timestamp sequencer in
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

  (* Shutdown the rollup, restart it without Batcher mode, we will be able
     to create the invalid branch without publishing it. *)
  let* () = Sc_rollup_node.terminate sc_rollup_node in
  Sc_rollup_node.change_node_mode sc_rollup_node Observer ;
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup_address [] in
  let*@ _ = produce_block ~timestamp:genesis_timestamp sequencer in

  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client
  and* _ =
    repeat 2 (fun () ->
        let* _ = produce_block ~timestamp:after_timestamp sequencer in
        unit)
  (* make sure the upgrade is done on invalid branch *)
  and* _ = Evm_node.wait_for_successful_upgrade sequencer in

  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  (* Send a delayed transaction. *)
  let* _hash =
    let* tx =
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
    send_raw_transaction_to_delayed_inbox
      ~wait_for_next_level:true
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      tx
  in
  (* Flush the delayed inbox. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* flush_l1_level = Client.level client in

  (* Make the flush final. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

  let* _ = Evm_node.wait_for_processed_l1_level ~level:flush_l1_level sequencer
  and* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* () =
    check_rollup_head_consistency ~sc_rollup_node ~evm_node:sequencer ()
  in

  let* _ =
    repeat 2 (fun () ->
        let* _ = produce_block ~timestamp:after_timestamp sequencer in
        unit)
  (* Make sure the upgrade still happens *)
  and* rh_seq, lvl_seq = Evm_node.wait_for_successful_upgrade sequencer in
  let*@ sequencer_head = Rpc.block_number sequencer in

  (* The observer should follow the reorg and reach the same head *)
  let* () =
    Evm_node.wait_for_blueprint_applied observer (Int32.to_int sequencer_head)
  and* rh_obs, lvl_obs = Evm_node.wait_for_successful_upgrade observer in
  Check.((rh_seq = rh_obs) string) ~error_msg:"Root hash should be the same" ;
  Check.((lvl_seq = lvl_obs) int)
    ~error_msg:"Level of upgrade should be the same" ;

  let* () = check_head_consistency ~left:observer ~right:sequencer () in
  unit

let test_upgrade_injected_after_flush_level =
  let genesis_timestamp = "2020-01-01T00:00:00Z" in
  let activation_timestamp = "2020-01-01T00:01:00Z" in
  let after_timestamp = "2020-01-01T00:02:00Z" in
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~time_between_blocks:Nothing
    ~genesis_timestamp:Client.(At (Time.of_notation_exn genesis_timestamp))
    ~delayed_inbox_timeout:0
    ~delayed_inbox_min_levels:1
    ~tags:
      [
        "evm";
        "sequencer";
        "delayed_inbox";
        "timeout";
        "flush";
        "reorg";
        "upgrade";
        "after";
      ]
    ~title:"Upgrade injected after flushed level (on invalid branch)"
    ~use_multichain:
      (* TODO #7843: Adapt this test to multichain context *)
      Register_without_feature
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        observer;
        kernel;
        _;
      }
      _protocol
    ->
  (* This test the situation where the injected before of the upgrade
     event is after the flushed level.

     It can be a bit tedious to read the test, here is what happens:

     (l2_level)             0
                            1
                            2
                           /
                          /
                         3
                         4
        Upgrade event -> 5

     Then a flush happens at level 3, invalidating the branch and creating a new
     one.

     (l2_level)             0
                            1
                            2
                           / \
                          /   \
                         X     3' <- Flushed blueprint

     The upgrade event must be retrieved from the invalid branch before
     resetting to level 2 and applying level 3'.
  *)
  (* Produces 2 valid blocks and wait until they are synchronized. *)
  let*@ _ = produce_block ~timestamp:genesis_timestamp sequencer in
  let*@ _ = produce_block ~timestamp:genesis_timestamp sequencer in
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

  (* Shutdown the rollup, restart it without Batcher mode, we will be able
     to create the invalid branch without publishing it. *)
  let* () = Sc_rollup_node.terminate sc_rollup_node in
  Sc_rollup_node.change_node_mode sc_rollup_node Observer ;
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup_address [] in

  let*@ _ = produce_block ~timestamp:genesis_timestamp sequencer in
  let*@ _ = produce_block ~timestamp:genesis_timestamp sequencer in
  let*@ _ = produce_block ~timestamp:genesis_timestamp sequencer in

  (* Set a pending upgrade. *)
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
  let* _ =
    repeat 2 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  and* _ = Evm_node.wait_for_pending_upgrade sequencer in

  (* Send a delayed transaction. *)
  let* _hash =
    let* tx =
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
    send_raw_transaction_to_delayed_inbox
      ~wait_for_next_level:true
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      tx
  in
  (* Flush the delayed inbox. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* flush_l1_level = Client.level client in

  (* Make the flush final. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

  let* _ = Evm_node.wait_for_processed_l1_level ~level:flush_l1_level sequencer
  and* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

  let* _ =
    repeat 2 (fun () ->
        let* _ = produce_block ~timestamp:after_timestamp sequencer in
        unit)
  and* rh_seq, lvl_seq = Evm_node.wait_for_successful_upgrade sequencer in
  let*@ sequencer_head = Rpc.block_number sequencer in

  (* The observer should follow the reorg and reach the same head *)
  let* () =
    Evm_node.wait_for_blueprint_applied observer (Int32.to_int sequencer_head)
  and* rh_obs, lvl_obs = Evm_node.wait_for_successful_upgrade observer in
  Check.((rh_seq = rh_obs) string) ~error_msg:"Root hash should be the same" ;
  Check.((lvl_seq = lvl_obs) int)
    ~error_msg:"Level of upgrade should be the same" ;
  let* () = check_head_consistency ~left:observer ~right:sequencer () in
  unit

(* test of reorg: the sequencer sees the "upgrade to do" event at the same
   time as the "flushed blueprint" event*)
let test_flushed_blueprint_reorg_upgrade =
  (* Add a delay between first block and activation timestamp. *)
  let genesis_timestamp =
    Client.(At (Time.of_notation_exn "2020-01-01T00:00:00Z"))
  in
  let before_timestamp = "2020-01-01T00:00:05Z" in
  let activation_timestamp = "2020-01-01T00:00:10Z" in
  let after_timestamp = "2020-01-01T00:00:15Z" in
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~time_between_blocks:Nothing
    ~genesis_timestamp
    ~delayed_inbox_timeout:0
    ~delayed_inbox_min_levels:1
    ~tags:
      [
        "evm";
        "sequencer";
        "delayed_inbox";
        "timeout";
        "flush";
        "reorg";
        "upgrade";
        "at";
      ]
    ~title:"Flush delayed inbox event leads to reorg, including upgrade"
    ~use_multichain:
      (* TODO #7843: Adapt this test to multichain context *)
      Register_without_feature
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        observer;
        kernel;
        _;
      }
      _protocol
    ->
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

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
  (* We mark at which level the delayed inbox item was added. *)
  let* add_delayed_inbox_level = Client.level client in

  (* The sequencer produces a few blocks before seeing the delayed transaction
     (speculative execution) *)
  let* () =
    repeat 4 (fun _ ->
        let*@ _ =
          Rpc.produce_block
            ~timestamp:before_timestamp
            ~with_delayed_transactions:false
            sequencer
        in
        unit)
  in

  (* upgrade message will also trigger flush*)
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

  (* We mark at which level the delayed inbox was flushed. *)
  let* flushed_delayed_inbox_level = Client.level client in

  (* produce a few l2 blocks (speculative execution) after seeing delayed tx *)
  let* () =
    repeat 4 (fun _ ->
        let*@ _ =
          Rpc.produce_block
            ~timestamp:after_timestamp
            ~with_delayed_transactions:false
            sequencer
        in
        unit)
  in

  let* _ =
    Evm_node.wait_for_processed_l1_level
      ~level:add_delayed_inbox_level
      sequencer
  and* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let*@ speculative_head = Rpc.get_block_by_number ~block:"latest" sequencer in

  (* Wait until the event is completely processed. Head of sequencer and rollup
     should be in sync. *)
  let* _ =
    Evm_node.wait_for_processed_l1_level
      ~level:flushed_delayed_inbox_level
      sequencer
  and* _ = Evm_node.wait_for_flush_delayed_inbox sequencer
  and* _ = Evm_node.wait_for_pending_upgrade sequencer
  (* A new block will make the {!flushed_delayed_inbox_level} final. *)
  and* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

  let* () =
    check_rollup_head_consistency ~sc_rollup_node ~evm_node:sequencer ()
  in

  (* Speculative head has been removed as the sequencer was forced to
     reorganize the blocks because of the flushed blueprint. *)
  let*@? (err : Rpc.error) =
    Rpc.get_block_by_hash ~block:speculative_head.hash sequencer
  in

  Check.(err.message =~ rex "Block .* not found")
    ~error_msg:"Expected error to match %R, got %L" ;

  (* The sequencer should trigger the upgrade *)
  let* rh_seq, lvl_seq = Evm_node.wait_for_successful_upgrade sequencer
  and* _ =
    repeat 2 (fun () ->
        let* _ = produce_block ~timestamp:after_timestamp sequencer in
        unit)
  in

  let*@ sequencer_head = Rpc.block_number sequencer in

  (* The observer should follow the reorg and reach the same head *)
  let* () =
    Evm_node.wait_for_blueprint_applied observer (Int32.to_int sequencer_head)
  and* rh_obs, lvl_obs = Evm_node.wait_for_successful_upgrade observer in
  Check.((rh_seq = rh_obs) string) ~error_msg:"Root hash should be the same" ;
  Check.((lvl_seq = lvl_obs) int)
    ~error_msg:"Level of upgrade should be the same" ;
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
  let* () = check_head_consistency ~left:observer ~right:sequencer () in
  unit

let test_delayed_transfer_timeout =
  register_all
    ~__FILE__
    ~delayed_inbox_timeout:3
    ~delayed_inbox_min_levels:1
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "timeout"]
    ~title:"Delayed transaction timeout"
    ~use_dal:ci_enabled_dal_registration
  @@
  fun {
        client;
        node = _;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        proxy;
        _;
      }
      _protocol
    ->
  (* Kill the sequencer *)
  let* () = Evm_node.terminate sequencer in
  let endpoint = Evm_node.endpoint proxy in
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let sender = Eth_account.bootstrap_accounts.(0).address in
  let _ = Rpc.block_number proxy in
  let receiver = Eth_account.bootstrap_accounts.(1).address in
  let* sender_balance_prev = Eth_cli.balance ~account:sender ~endpoint () in
  let* receiver_balance_prev = Eth_cli.balance ~account:receiver ~endpoint () in
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
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  let* sender_balance_next = Eth_cli.balance ~account:sender ~endpoint () in
  let* receiver_balance_next = Eth_cli.balance ~account:receiver ~endpoint () in
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
    ~__FILE__
    ~kernels:[Kernel.Latest]
    ~time_between_blocks:Nothing
    ~genesis_timestamp:Client.(At (Time.of_notation_exn "2020-01-01T00:00:00Z"))
    ~delayed_inbox_timeout:1
    ~delayed_inbox_min_levels:1
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "timeout"]
    ~title:"Forced blueprint can take predecessor timestamp"
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        proxy;
        _;
      }
      _protocol
    ->
  (* The head timestamp will be high enough that we don't use the L1 timestamp
     for the forced blueprint and just take the same timestamp. *)
  let*@ (_ : int) = produce_block ~timestamp:"2020-01-01T00:04:00Z" sequencer in
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
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
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

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
    ~__FILE__
    ~kernels:[Kernel.Latest]
    ~time_between_blocks:Nothing
    ~genesis_timestamp:Client.(At (Time.of_notation_exn "2020-01-01T00:00:00Z"))
    ~delayed_inbox_timeout:1
    ~delayed_inbox_min_levels:1
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "timeout"]
    ~title:"Forced blueprint can take l1 timestamp"
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        proxy;
        _;
      }
      _protocol
    ->
  let*@ (_ : int) = produce_block ~timestamp:"2020-01-01T00:00:00Z" sequencer in
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
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
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

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
    ~__FILE__
    ~delayed_inbox_timeout:3
    ~delayed_inbox_min_levels:20
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "timeout"; "min_levels"]
    ~title:"Delayed transaction timeout considers l1 level"
    ~use_dal:ci_enabled_dal_registration
  @@
  fun {
        client;
        node = _;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        proxy;
        _;
      }
      _protocol
    ->
  (* Kill the sequencer *)
  let* () = Evm_node.terminate sequencer in
  let endpoint = Evm_node.endpoint proxy in
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let sender = Eth_account.bootstrap_accounts.(0).address in
  let _ = Rpc.block_number proxy in
  let receiver = Eth_account.bootstrap_accounts.(1).address in
  let* sender_balance_prev = Eth_cli.balance ~account:sender ~endpoint () in
  let* receiver_balance_prev = Eth_cli.balance ~account:receiver ~endpoint () in
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
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  let* sender_balance_next = Eth_cli.balance ~account:sender ~endpoint () in
  let* receiver_balance_next = Eth_cli.balance ~account:receiver ~endpoint () in
  Check.((sender_balance_prev = sender_balance_next) Wei.typ)
    ~error_msg:"Sender balance should be the same (prev %L = next %R)" ;
  Check.((receiver_balance_prev = receiver_balance_next) Wei.typ)
    ~error_msg:"Receiver balance should be the same (prev %L = next %R)" ;
  (* Wait until it's forced *)
  let* _ =
    repeat 15 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  let* sender_balance_next = Eth_cli.balance ~account:sender ~endpoint () in
  let* receiver_balance_next = Eth_cli.balance ~account:receiver ~endpoint () in
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
    ~upgrade_to:(fun _ -> Mainnet)
    ~genesis_timestamp
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "upgrade"; "force"]
    ~title:"Force kernel upgrade fail too early"
  @@
  fun _from
      to_
      {
        sc_rollup_node;
        l1_contracts;
        sc_rollup_address;
        client;
        sequencer;
        l2_chains;
        enable_multichain;
        _;
      }
      _protocol
    ->
  (* Wait for the sequencer to publish its genesis block. *)
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
  let patch_config =
    Evm_node.patch_config_with_experimental_feature
      ?l2_chains:(if enable_multichain then Some l2_chains else None)
      ()
  in
  let* proxy =
    Evm_node.init
      ~patch_config
      ~mode:(Proxy (Sc_rollup_node.endpoint sc_rollup_node))
      ()
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
  fun {
        sc_rollup_node;
        l1_contracts;
        sc_rollup_address;
        client;
        sequencer;
        l2_chains;
        enable_multichain;
        _;
      }
      _protocol
    ->
  (* Wait for the sequencer to publish its genesis block. *)
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
  let patch_config =
    Evm_node.patch_config_with_experimental_feature
      ?l2_chains:(if enable_multichain then Some l2_chains else None)
      ()
  in
  let* proxy =
    Evm_node.init
      ~patch_config
      ~mode:(Proxy (Sc_rollup_node.endpoint sc_rollup_node))
      ()
  in

  (* Assert the kernel version is the same at start up. *)
  let*@ sequencer_kernelVersion = Rpc.tez_kernelVersion sequencer in
  let*@ proxy_kernelVersion = Rpc.tez_kernelVersion proxy in
  Check.((sequencer_kernelVersion = proxy_kernelVersion) string)
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
  let*@ proxy_kernelVersion = Rpc.tez_kernelVersion proxy in
  Check.((sequencer_kernelVersion = proxy_kernelVersion) string)
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

let test_external_transaction_to_delayed_inbox_fails =
  (* We have a da_fee set to zero here. This is because the proxy will perform
     validation on the tx before adding it the transaction pool. This will fail
     due to 'gas limit too low' if the da fee is set.

     Since we want to test what happens when the tx is actually submitted, we
     bypass the da fee check here. *)
  register_all
    ~__FILE__
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
    ~time_between_blocks:Nothing
    ~eth_bootstrap_accounts:Eth_account.lots_of_address
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "external"]
    ~title:"Sending an external transaction to the delayed inbox fails"
  @@ fun {client; sequencer; proxy; sc_rollup_node; _} _protocol ->
  let* () = Evm_node.wait_for_blueprint_injected ~timeout:5. sequencer 0 in
  (* Bake a couple more levels for the blueprint to be final *)
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
  let raw_tx, _ = read_tx_from_file () |> List.hd in
  let*@ tx_hash = Rpc.send_raw_transaction ~raw_tx proxy in
  (* Bake enough levels to make sure the transaction would be processed
     if added *)
  let* () =
    repeat 10 (fun () ->
        let*@ _ = produce_block sequencer in
        let* _ = Rollup.next_rollup_node_level ~client ~sc_rollup_node in
        unit)
  in
  (* Response should be none *)
  let*@ response = Rpc.get_transaction_receipt ~tx_hash proxy in
  assert (Option.is_none response) ;
  let*@ response = Rpc.get_transaction_receipt ~tx_hash sequencer in
  assert (Option.is_none response) ;
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
    ~__FILE__
    ~delayed_inbox_timeout:1
    ~delayed_inbox_min_levels:20
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "timeout"]
    ~title:"Delayed inbox flushing"
  @@
  fun {
        client;
        node = _;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        proxy;
        _;
      }
      _protocol
    ->
  (* Kill the sequencer *)
  let* () = Evm_node.terminate sequencer in
  let endpoint = Evm_node.endpoint proxy in
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let sender = Eth_account.bootstrap_accounts.(0).address in
  let _ = Rpc.block_number proxy in
  let receiver = Eth_account.bootstrap_accounts.(1).address in
  let* sender_balance_prev = Eth_cli.balance ~account:sender ~endpoint () in
  let* receiver_balance_prev = Eth_cli.balance ~account:receiver ~endpoint () in
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
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
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
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  let* sender_balance_next = Eth_cli.balance ~account:sender ~endpoint () in
  let* receiver_balance_next = Eth_cli.balance ~account:receiver ~endpoint () in
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
    ~__FILE__
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
    ~__FILE__
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
    | _ -> assert false (* impossible case as it's a sequencer. *)
  in
  (* However the same timestamp is accepted. *)
  let*@ (_ : int) = produce_block ~timestamp:"2020-01-01T00:00:05Z" sequencer in
  unit

let test_timestamp_from_the_future =
  register_all
    ~__FILE__
    ~max_blueprint_lookahead_in_seconds:300L
    ~genesis_timestamp:Client.(At (Time.of_notation_exn "2020-01-01T00:00:00Z"))
    ~kernels:[Kernel.Latest]
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "block"; "timestamp"]
    ~title:"Timestamp from the future are refused"
    ~use_dal:ci_enabled_dal_registration
  @@ fun {sequencer; sc_rollup_node; client; enable_dal; _} _protocol ->
  (* In this test the time between blocks is 1 second. *)

  (* Producing a block 4:25 minutes after the L1 timestamp will be accepted. We
     do not check precisely 4:59 minutes to avoid flakiness w.r.t to blueprint
     inclusion. *)
  let* current_l1_timestamp = l1_timestamp client in
  let accepted_timestamp =
    Tezos_base.Time.Protocol.(add current_l1_timestamp 265L |> to_notation)
  in
  (* The sequencer will accept it anyway, but we need to check that the rollup
     node accepts it. *)
  let*@ (_ : int) = produce_block ~timestamp:accepted_timestamp sequencer in
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

  (* Producing a block 5:30 minutes after the L1 timestamp will be accepted by
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
        let* _l1_lvl = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in

  let*@ rollup_head = rollup_level sc_rollup_node in
  let*@ sequencer_head = Rpc.block_number sequencer in

  Check.((sequencer_head = Int32.succ rollup_head) int32)
    ~error_msg:"The rollup was supposed to refuse the block" ;

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
    ~use_multichain:
      (* TODO #7843: Adapt this test to multichain context *)
      Register_without_feature
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

(** This tests the situation where the kernel has an upgrade and the
    sequencer upgrade by following the event of the kernel. Observer
    must upgrade as well, even the one that is not connected to a
    rollup node. *)
let test_sequencer_upgrade =
  let check_sequencer ~evm_node ~expected_sequencer =
    let*@! current_sequencer =
      Rpc.state_value evm_node Durable_storage_path.sequencer
    in
    Check.(
      (Hex.to_string (`Hex current_sequencer) = expected_sequencer)
        ~__LOC__
        string
        ~error_msg:"Sequencer in state is %L but expected %R") ;
    return ()
  in

  let genesis_timestamp =
    "2020-01-01T00:00:00Z"
    (* timestamp used for the genesis of the l1 and the l2 *)
  in
  (* 30 sec later. *)
  let activation_timestamp = "2020-01-01T00:00:30Z" in

  let sequencer_key = Constant.bootstrap1 in
  let new_sequencer_key = Constant.bootstrap2 in

  register_all
    ~__FILE__
    ~sequencer:sequencer_key
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "sequencer_upgrade"; "auto"; "sync"]
    ~title:
      "Rollup-node sequencer upgrade is applied to the sequencer local state."
    ~use_multichain:
      (* TODO #7843: Adapt this test to multichain context *)
      Register_without_feature
    ~genesis_timestamp:Client.(At (Time.of_notation_exn genesis_timestamp))
  @@
  fun {
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
  let* observer_dont_track_rollup =
    Evm_node.init
      ~node_setup:
        (Evm_node.make_setup
           ?initial_kernel:(Evm_node.initial_kernel observer)
           ~preimages_dir:(Evm_node.preimages_dir observer)
           ())
      ~mode:
        (Observer
           {
             rollup_node_endpoint = None;
             evm_node_endpoint = Evm_node.endpoint sequencer;
           })
      ()
  in

  let*@ _ = produce_block ~timestamp:genesis_timestamp sequencer in
  let* () = bake_until_sync ~sequencer ~sc_rollup_node ~client () in

  let* () =
    check_rollup_head_consistency
      ~evm_node:sequencer
      ~sc_rollup_node
      ~error_msg:
        "The head should be the same before the upgrade: observer is %R while \
         rollup node is %L"
      ()
  in

  (* dummy check, upgrade is not submitted yet *)
  let* () =
    check_sequencer
      ~evm_node:sequencer
      ~expected_sequencer:sequencer_key.public_key
  in
  let* () =
    check_sequencer
      ~evm_node:observer
      ~expected_sequencer:sequencer_key.public_key
  in

  Log.info "Sending the sequencer upgrade to the L1 contract" ;

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

  Log.info
    "Baking 2 block to make the event final in the l1 so the upgrade is seen \
     by EVM node following a rollup node." ;
  let upgrade_info = Evm_node.wait_for_evm_event Sequencer_upgrade sequencer
  and upgrade_info_observer =
    Evm_node.wait_for_evm_event Sequencer_upgrade observer
  in
  let* () =
    repeat 2 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~client ~sc_rollup_node in
        unit)
  and* _upgrade_info = upgrade_info
  and* _upgrade_info_observer = upgrade_info_observer in

  Log.info
    "Producing a block from the current sequencer so the event is propagated \
     to observer not following a rollup node." ;
  let upgrade_info_observer_no_rollup_node =
    Evm_node.wait_for_evm_event Sequencer_upgrade observer_dont_track_rollup
  in
  let* () =
    let*@ _ = produce_block ~timestamp:genesis_timestamp sequencer in
    unit
  and* _upgrade_info_observer_no_rollup_node =
    upgrade_info_observer_no_rollup_node
  in

  let* () = bake_until_sync ~sequencer ~sc_rollup_node ~client () in

  Log.info
    "The upgrade is not yet triggered, head should be equivalent with the \
     block just produced" ;
  let* () =
    check_rollup_head_consistency
      ~evm_node:sequencer
      ~sc_rollup_node
      ~error_msg:
        "The head should be the same before the upgrade: observer is %R while \
         rollup node is %L"
      ()
  in

  let*@ previous_rollup_head = rollup_level sc_rollup_node in

  Log.info "Baking until the sequencer upgrade is triggered in the l1" ;
  let has_sequencer_changed () =
    let* current_sequencer_in_rollup_hex =
      Sc_rollup_node.RPC.call
        sc_rollup_node
        ~rpc_hooks:Tezos_regression.rpc_hooks
      @@ Sc_rollup_rpc.get_global_block_durable_state_value
           ~pvm_kind:"wasm_2_0_0"
           ~operation:Sc_rollup_rpc.Value
           ~key:Durable_storage_path.sequencer
           ()
    in
    let current_sequencer_in_rollup =
      match current_sequencer_in_rollup_hex with
      | Some s -> Hex.to_string (`Hex s)
      | None -> Test.fail "missing sequencer"
    in
    if String.equal current_sequencer_in_rollup new_sequencer_key.public_key
    then return (Some ())
    else return None
  in
  let* () =
    bake_until
      ~__LOC__
      ~timeout_in_blocks:100
      ~timeout:60.
      ~bake:(fun () -> Rollup.next_rollup_node_level ~client ~sc_rollup_node)
      ~result_f:has_sequencer_changed
      ()
  in

  (* upgrade is not triggered until a blueprint with a superior
     timestamp is applied *)
  let* () =
    check_sequencer
      ~evm_node:sequencer
      ~expected_sequencer:sequencer_key.public_key
  in
  let* () =
    check_sequencer
      ~evm_node:observer
      ~expected_sequencer:sequencer_key.public_key
  in

  Log.info
    "Sequencer fails to produce a block because blueprint timestamp is above \
     the activation timestamp." ;
  let*@? _ = produce_block ~timestamp:activation_timestamp sequencer in

  (* Check that the evm-node sequencer refuses the blocks as they do
     not respect the sequencer's signature. *)
  let* () =
    check_rollup_head_consistency
      ~evm_node:sequencer
      ~sc_rollup_node
      ~error_msg:
        "The head should be the same after the sequencer tried to produce \
         blocks, they are are disregarded: sequencer is %%L while rollup node \
         is %%R"
      ()
  in
  let* () =
    check_rollup_head_consistency
      ~evm_node:observer
      ~sc_rollup_node
      ~error_msg:
        "The head should be the same after the sequencer tried to produce \
         blocks, they are are disregarded: observer is %%L while rollup node \
         is %%R"
      ()
  in

  Log.info
    "Baking couple of blocks to check the block is not propagated nor accepted \
     by the rollup node." ;
  (* maybe unnecessary check here, we already not the block production failed. *)
  let* () =
    repeat 5 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~client ~sc_rollup_node in
        unit)
  in
  let*@ rollup_head = rollup_level sc_rollup_node in
  Check.((previous_rollup_head = rollup_head) int32)
    ~error_msg:
      "The rollup should not have progressed because no block have been \
       produced by the current sequencer." ;

  Log.info
    "Stopping current sequencer and starting a new one with new sequencer key" ;
  let* () = Evm_node.terminate sequencer
  and* () = Evm_node.wait_termination sequencer in
  let* snapshot_file =
    Runnable.run @@ Evm_node.export_snapshot ~desync:true sequencer
  in

  let* new_sequencer =
    let mode =
      match Evm_node.mode sequencer with
      | Sequencer config ->
          Evm_node.Sequencer
            {config with sequencer_keys = [new_sequencer_key.alias]}
      | _ -> Test.fail "impossible case, it's a sequencer"
    in
    let new_sequencer =
      Evm_node.create
        ~node_setup:
          (Evm_node.make_setup
             ?initial_kernel:(Evm_node.initial_kernel sequencer)
             ~preimages_dir:(Evm_node.preimages_dir sequencer)
             ())
        ~mode
        ()
    in
    let* () = Process.check @@ Evm_node.spawn_init_config new_sequencer in
    let* () =
      Runnable.run
      @@ Evm_node.import_snapshot ~desync:true new_sequencer ~snapshot_file
    in
    let* () = Evm_node.run new_sequencer in
    return new_sequencer
  in

  Log.info "Producing a block with the new sequencer." ;
  let*@ _ = produce_block ~timestamp:activation_timestamp new_sequencer in
  Log.info "Baking to check the block is accepted by the rollup node." ;
  let* () =
    bake_until_sync ~sequencer:new_sequencer ~sc_rollup_node ~client ()
  in
  let* () =
    check_rollup_head_consistency
      ~evm_node:new_sequencer
      ~sc_rollup_node
      ~error_msg:
        "The head should be the same after blocks produced by the new \
         sequencer: new_sequencer is %L while rollup node is %R"
      ()
  in
  let*@ rollup_head = rollup_level sc_rollup_node in
  let current_head_number = rollup_head in
  Check.((Int32.add previous_rollup_head 1l = current_head_number) int32)
    ~error_msg:
      "The block number should have incremented (previous: %L, current: %R)" ;

  Log.info
    "Transforming the old sequencer to a observer so it should receive and \
     apply the blueprint. The connected observers should also receive it." ;
  let* observer_sequencer =
    let observer_sequencer =
      Evm_node.switch_sequencer_to_observer
        ~old_sequencer:sequencer
        ~new_sequencer
    in
    let* () =
      Process.check
      @@ Evm_node.spawn_init_config
           ~extra_arguments:["--force"]
           observer_sequencer
    in
    let* () = Evm_node.run observer_sequencer in
    return observer_sequencer
  in
  let current_head_number = Int32.to_int rollup_head in
  let* () =
    Evm_node.wait_for_blueprint_applied observer_sequencer current_head_number
  in
  let* () = Evm_node.wait_for_blueprint_applied observer current_head_number in
  let* () =
    check_head_consistency
      ~left:new_sequencer
      ~right:observer_sequencer
      ~error_msg:
        "The head should be the same after blocks produced by the new \
         sequencer: new_sequencer is %L while observer_sequencer is %R"
      ()
  in
  let* () =
    check_head_consistency
      ~left:new_sequencer
      ~right:observer
      ~error_msg:
        "The head should be the same after blocks produced by the new \
         sequencer: new_sequencer is %L while observer is %R"
      ()
  in

  let* () =
    Evm_node.wait_for_blueprint_applied
      observer_dont_track_rollup
      current_head_number
  in
  let* () =
    check_head_consistency
      ~left:new_sequencer
      ~right:observer_dont_track_rollup
      ~error_msg:
        "The head should be the same after blocks produced by the new \
         sequencer: new_sequencer is %L while observer is %R"
      ()
  in

  Log.info
    "Bootstrapping an observer to make sure it applied the sequencer upgrade \
     as well." ;
  let* observer_bootstrap =
    match Evm_node.mode observer with
    | Observer mode ->
        Evm_node.init
          ~node_setup:
            (Evm_node.make_setup
               ?initial_kernel:(Evm_node.initial_kernel observer)
               ~preimages_dir:(Evm_node.preimages_dir observer)
               ())
          ~mode:(Observer mode)
          ()
    | _ -> Test.fail "impossible, it's an observer"
  in
  let* _ = Evm_node.wait_for_evm_event Sequencer_upgrade observer_bootstrap in
  let* () =
    Evm_node.wait_for_blueprint_applied observer_bootstrap current_head_number
  in
  let* () =
    check_head_consistency
      ~left:new_sequencer
      ~right:observer_bootstrap
      ~error_msg:
        "The head should be the same after blocks produced by the new \
         sequencer: new_sequencer is %L while observer_bootstrap is %R"
      ()
  in
  unit

(** This tests the scenario where two identical sequencer upgrade
    transactions are sent to the L1 contract. The test verifies that
    the system handles duplicate upgrades correctly without duplicated
    insertions. This is mostly [test_sequencer_upgrade] with less
    checks but two identical sequencer upgrades. *)
let test_duplicate_sequencer_upgrade =
  let genesis_timestamp =
    "2020-01-01T00:00:00Z"
    (* timestamp used for the genesis of the l1 and the l2 *)
  in
  (* 30 sec later. *)
  let activation_timestamp = "2020-01-01T00:00:30Z" in

  let sequencer_key = Constant.bootstrap1 in
  let new_sequencer_key = Constant.bootstrap2 in

  register_all
    ~__FILE__
    ~sequencer:sequencer_key
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "sequencer_upgrade"; "auto"; "sync"]
    ~title:"Duplicated sequencer upgrade."
    ~use_multichain:Register_without_feature
    ~genesis_timestamp:Client.(At (Time.of_notation_exn genesis_timestamp))
  @@
  fun {
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
  let*@ _ = produce_block ~timestamp:genesis_timestamp sequencer in
  let* () = bake_until_sync ~sequencer ~sc_rollup_node ~client () in

  Log.info "Sending two sequencer upgrades to the L1 contract" ;

  let send_sequencer_upgrade () =
    sequencer_upgrade
      ~sc_rollup_address
      ~sequencer_admin:Constant.bootstrap2.alias
      ~sequencer_governance_contract:l1_contracts.sequencer_governance
      ~pool_address:Eth_account.bootstrap_accounts.(0).address
      ~client
      ~upgrade_to:new_sequencer_key.alias
      ~activation_timestamp
  in

  Log.info
    "Send two identical sequencer upgrade transactions to the L1 contract" ;
  let* () = send_sequencer_upgrade () in
  let* () = send_sequencer_upgrade () in

  Log.info "Baking until the sequencer upgrade is triggered in the l1" ;
  let has_sequencer_changed () =
    let* current_sequencer_in_rollup_hex =
      Sc_rollup_node.RPC.call
        sc_rollup_node
        ~rpc_hooks:Tezos_regression.rpc_hooks
      @@ Sc_rollup_rpc.get_global_block_durable_state_value
           ~pvm_kind:"wasm_2_0_0"
           ~operation:Sc_rollup_rpc.Value
           ~key:Durable_storage_path.sequencer
           ()
    in
    let current_sequencer_in_rollup =
      match current_sequencer_in_rollup_hex with
      | Some s -> Hex.to_string (`Hex s)
      | None -> Test.fail "missing sequencer"
    in
    if String.equal current_sequencer_in_rollup new_sequencer_key.public_key
    then return (Some ())
    else return None
  in
  let* () =
    bake_until
      ~__LOC__
      ~timeout_in_blocks:100
      ~timeout:60.
      ~bake:(fun () -> Rollup.next_rollup_node_level ~client ~sc_rollup_node)
      ~result_f:has_sequencer_changed
      ()
  in

  Log.info
    "Stopping current sequencer and starting a new one with new sequencer key" ;
  let* () = Evm_node.terminate sequencer
  and* () = Evm_node.wait_termination sequencer in
  let* snapshot_file =
    Runnable.run @@ Evm_node.export_snapshot ~desync:true sequencer
  in

  let* new_sequencer =
    let mode =
      match Evm_node.mode sequencer with
      | Sequencer config ->
          Evm_node.Sequencer
            {config with sequencer_keys = [new_sequencer_key.alias]}
      | _ -> Test.fail "impossible case, it's a sequencer"
    in
    let new_sequencer = Evm_node.create ~mode () in
    let* () = Process.check @@ Evm_node.spawn_init_config new_sequencer in
    let* () =
      Runnable.run
      @@ Evm_node.import_snapshot new_sequencer ~desync:true ~snapshot_file
    in
    let* () = Evm_node.run new_sequencer in
    return new_sequencer
  in

  Log.info "Producing a block with the new sequencer." ;
  let*@ _ = produce_block ~timestamp:activation_timestamp new_sequencer in
  Log.info "Baking to check the block is accepted by the rollup node." ;
  let* () =
    bake_until_sync ~sequencer:new_sequencer ~sc_rollup_node ~client ()
  in

  Log.info
    "Bootstrapping an observer to make sure it applied the sequencer upgrade \
     as well." ;
  let* observer_bootstrap =
    Evm_node.init
      ~node_setup:
        (Evm_node.make_setup
           ~preimages_dir:(Evm_node.preimages_dir observer)
           ?initial_kernel:(Evm_node.initial_kernel observer)
           ())
      ~mode:
        (Observer
           {
             rollup_node_endpoint = None;
             evm_node_endpoint = Evm_node.endpoint new_sequencer;
           })
      ()
  in
  let* _ = Evm_node.wait_for_evm_event Sequencer_upgrade observer_bootstrap in
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
  fun {
        sc_rollup_node;
        client;
        sequencer;
        observer;
        enable_dal;
        enable_multichain;
        l2_chains;
        _;
      }
      _protocol
    ->
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
  let spawn_rpc = if enable_multichain then Some (Port.fresh ()) else None in
  let sequencer_bis =
    Evm_node.create
      ~node_setup:
        (Evm_node.make_setup
           ?spawn_rpc
           ?initial_kernel:(Evm_node.initial_kernel sequencer)
           ~preimages_dir:(Evm_node.preimages_dir sequencer)
           ())
      ~mode:(Evm_node.mode sequencer)
      ()
  in
  let* () = Process.check @@ Evm_node.spawn_init_config sequencer_bis in
  let* () =
    match enable_multichain with
    | true ->
        let patch_config =
          Evm_node.patch_config_with_experimental_feature
            ~l2_chains
            ?spawn_rpc
            ()
        in
        Evm_node.Config_file.update sequencer_bis patch_config
    | false -> unit
  in
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

  (* We bake two blocks, so that the EVM node can process the deposit and
     create a blueprint with it. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

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

(** Test that the kernel can handle more than 100 withdrawals per level,
    which is currently the limit of outbox messages in the L1. *)
let test_outbox_size_limit_resilience ~slow =
  let commitment_period = 5 and challenge_window = 5 in
  let slow_str = if slow then "slow" else "fast" in
  register_all
    ~__FILE__
    ~sequencer:Constant.bootstrap1
    ~time_between_blocks:Nothing
    ~tags:
      (["evm"; "withdraw"; "outbox"; "spam"] @ if slow then [Tag.slow] else [])
    ~title:(sf "Outbox size limit resilience (%s)" slow_str)
    ~commitment_period
    ~challenge_window
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        proxy;
        evm_version;
        _;
      }
      _protocol
    ->
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
    Eth_cli.balance ~account:receiver.address ~endpoint ()
  in
  let deposit_info =
    {receiver = EthereumAddr receiver.address; chain_id = None}
  in
  let* () =
    send_deposit_to_delayed_inbox
      ~amount
      ~bridge:l1_contracts.bridge
      ~depositor
      ~deposit_info
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
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in
  let* receiver_balance_next =
    Eth_cli.balance ~account:receiver.address ~endpoint ()
  in
  Check.((receiver_balance_next > receiver_balance_prev) Wei.typ)
    ~error_msg:"Expected a bigger balance" ;

  (* Deploy and top up batch withdrawal contract *)
  let* spammer_resolved = Solidity_contracts.spam_withdrawal evm_version in
  let* () =
    Eth_cli.add_abi ~label:spammer_resolved.label ~abi:spammer_resolved.abi ()
  in
  let* spammer_contract, _tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.deploy
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
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

  (* 100 messages flushed at this point *)
  let* _ = produce_block sequencer in
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in

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
    ~__FILE__
    ~sequencer:Constant.bootstrap1
    ~time_between_blocks:Nothing
    ~maximum_allowed_ticks:9_000_000_000L
    ~tags:["evm"; "sequencer"; "reboot"; Tag.slow]
    ~title:
      "Checks the stage one reboots when reading too much chunks in a single \
       L1 level"
    ~use_multichain:
      (* TODO #7843: Adapt this test to multichain context *)
      Register_without_feature
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
  let* _ = Rollup.next_rollup_node_level ~client ~sc_rollup_node in
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
    ~__FILE__
    ~sequencer:Constant.bootstrap1
    ~time_between_blocks:Nothing
    ~max_number_of_chunks:2
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
    ~eth_bootstrap_accounts:Eth_account.lots_of_address
    ~tags:["evm"; "sequencer"; "blueprint"; "limit"]
    ~title:
      "Checks the sequencer doesn't produce blueprint bigger than the given \
       maximum number of chunks"
    ~use_dal:ci_enabled_dal_registration
      (* IC set to false because transaction dropping mechanism from IC workflow invalidates it:
      Remaining transactions are not included in the last block *)
    ~instant_confirmations:false
  @@ fun {sc_rollup_node; client; sequencer; _} _protocol ->
  let txs = read_tx_from_file () |> List.map (fun (tx, _hash) -> tx) in
  let* requests, hashes = batch_n_transactions ~evm_node:sequencer txs in
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
     the batch has been split into two consecutive blueprints.
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
    ~__FILE__
    ~eth_bootstrap_accounts:Eth_account.lots_of_address
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
      (* IC set to false because transaction dropping mechanism from IC workflow invalidates it:
      Delayed transactions are included by remaining common transaction are not *)
    ~instant_confirmations:false
  @@
  fun {sc_rollup_node; client; sequencer; sc_rollup_address; l1_contracts; _}
      _protocol
    ->
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
        let* _l1_level =
          Rollup.next_rollup_node_level ~sc_rollup_node ~client
        in
        unit)
  in
  let* _requests, _hashes =
    batch_n_transactions ~evm_node:sequencer direct_txs
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
  let* () =
    repeat reset_level (fun () ->
        next_evm_level ~evm_node:sequencer ~sc_rollup_node ~client)
  in
  let* () = bake_until_sync ~sequencer ~sc_rollup_node ~client () in
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
    repeat after_reset_level (fun () ->
        next_evm_level ~evm_node:sequencer ~sc_rollup_node ~client)
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

let test_preimages_endpoint =
  (* Add a delay between first block and activation timestamp. *)
  let genesis_timestamp =
    Client.(At (Time.of_notation_exn "2020-01-01T00:00:00Z"))
  in
  let activation_timestamp = "2020-01-01T00:00:10Z" in
  register_all
    ~__FILE__
    ~sequencer:Constant.bootstrap1
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "preimages_endpoint"]
    ~title:"Sequencer use remote server to get preimages"
    ~kernels:[Latest]
    ~additional_uses:[Constant.WASM.mainnet_kernel]
    ~genesis_timestamp
  @@
  fun {
        sc_rollup_node;
        l1_contracts;
        sc_rollup_address;
        client;
        sequencer;
        observer = _;
        l2_chains;
        enable_multichain;
        _;
      }
      _protocol
    ->
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
  let* () = Evm_node.terminate sequencer in
  (* Prepares the sequencer without [preimages-dir], to force the use of
     preimages endpoint. Not passing ~preimages_dir to create means it will be None. *)
  let sequencer_mode =
    match Evm_node.mode sequencer with
    | Evm_node.Sequencer config -> Evm_node.Sequencer config
    | _ -> assert false
  in
  let spawn_rpc = if enable_multichain then Some (Port.fresh ()) else None in
  let new_sequencer =
    Evm_node.create
      ~node_setup:(Evm_node.make_setup ?spawn_rpc ())
      ~mode:sequencer_mode
      ()
  in
  let* () = Process.check @@ Evm_node.spawn_init_config new_sequencer in
  let* () =
    match enable_multichain with
    | true ->
        let patch_config =
          Evm_node.patch_config_with_experimental_feature
            ~l2_chains
            ?spawn_rpc
            ()
        in
        Evm_node.Config_file.update new_sequencer patch_config
    | false -> unit
  in
  (* Prepares the observer without [preimages-dir], to force the use of
     preimages endpoint. *)
  let observer_mode evm_node_endpoint =
    Evm_node.Observer {rollup_node_endpoint = None; evm_node_endpoint}
  in
  let new_observer =
    Evm_node.create ~mode:(observer_mode (Evm_node.endpoint new_sequencer)) ()
  in
  let new_observer2 =
    Evm_node.create ~mode:(observer_mode (Evm_node.endpoint new_observer)) ()
  in

  let* () = Process.check @@ Evm_node.spawn_init_config new_observer in
  let* () = Process.check @@ Evm_node.spawn_init_config new_observer2 in

  let* () =
    match enable_multichain with
    | true ->
        let patch_config =
          Evm_node.patch_config_with_experimental_feature ~l2_chains ()
        in
        let* () = Evm_node.Config_file.update new_observer patch_config in
        let* () = Evm_node.Config_file.update new_observer2 patch_config in
        Lwt.return_unit
    | false -> Lwt.return_unit
  in

  let* () =
    repeat 2 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  let* () = Evm_node.init_from_rollup_node_data_dir new_sequencer sc_rollup_node
  and* () = Evm_node.init_from_rollup_node_data_dir new_observer sc_rollup_node
  and* () =
    Evm_node.init_from_rollup_node_data_dir new_observer2 sc_rollup_node
  in
  (* Sends an upgrade with new preimages. *)
  let* root_hash =
    upgrade
      ~sc_rollup_node
      ~sc_rollup_address
      ~admin:Constant.bootstrap2.public_key_hash
      ~admin_contract:l1_contracts.admin
      ~client
      ~upgrade_to:Constant.WASM.mainnet_kernel
      ~activation_timestamp
  in
  let* _ =
    repeat 2 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~client ~sc_rollup_node in
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
  let kernel_downloaded =
    Lwt.join
      [
        Evm_node.wait_for_predownload_kernel new_sequencer ~root_hash;
        Evm_node.wait_for_predownload_kernel new_observer ~root_hash;
        Evm_node.wait_for_predownload_kernel new_observer2 ~root_hash;
      ]
  in
  let* () =
    Evm_node.run
      ~extra_arguments:["--preimages-endpoint"; preimages_endpoint]
      new_sequencer
  in
  let* () =
    Evm_node.run
      ~extra_arguments:["--preimages-endpoint"; preimages_endpoint]
      new_observer
  in
  let* () =
    Evm_node.run
      ~extra_arguments:["--preimages-endpoint"; preimages_endpoint]
      new_observer2
  in

  (* It won't upgrade, but both will download the preimages. *)
  let*@ _ = produce_block ~timestamp:"2020-01-01T00:00:09Z" new_sequencer in
  let* () = kernel_downloaded in
  (* Finally, do the upgrade. *)
  let*@ _ = produce_block ~timestamp:"2020-01-01T00:00:15Z" new_sequencer in
  Check.is_true
    !served
    ~error_msg:"The sequencer should have used the file server" ;
  unit

let test_preimages_endpoint_retry =
  (* Add a delay between first block and activation timestamp. *)
  let genesis_timestamp =
    Client.(At (Time.of_notation_exn "2020-01-01T00:00:00Z"))
  in
  let activation_timestamp = "2020-01-01T00:00:10Z" in
  register_all
    ~__FILE__
    ~sequencer:Constant.bootstrap1
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "preimages_endpoint"; "retry"; Tag.slow]
    ~title:"Sequencer use remote server to get preimages with retries"
    ~kernels:[Latest]
    ~additional_uses:[Constant.WASM.mainnet_kernel]
    ~genesis_timestamp
    ~use_multichain:
      (* TODO #7843: Adapt this test to multichain context *)
      Register_without_feature
  @@
  fun {
        sc_rollup_node;
        l1_contracts;
        sc_rollup_address;
        client;
        sequencer;
        l2_chains;
        enable_multichain;
        _;
      }
      _protocol
    ->
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
  let* () = Evm_node.terminate sequencer in
  let finalizeL1 () =
    repeat 2 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  let sequencer_mode =
    match Evm_node.mode sequencer with
    | Evm_node.Sequencer config -> Evm_node.Sequencer config
    | _ -> assert false
  in
  let spawn_rpc = if enable_multichain then Some (Port.fresh ()) else None in
  let new_sequencer =
    Evm_node.create
      ~node_setup:(Evm_node.make_setup ?spawn_rpc ())
      ~mode:sequencer_mode
      ()
  in
  let* () = Process.check @@ Evm_node.spawn_init_config new_sequencer in
  let* () =
    match enable_multichain with
    | true ->
        let patch_config =
          Evm_node.patch_config_with_experimental_feature
            ~l2_chains
            ?spawn_rpc
            ()
        in
        Evm_node.Config_file.update new_sequencer patch_config
    | false -> unit
  in
  let* () = finalizeL1 () in
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
      ~upgrade_to:Constant.WASM.mainnet_kernel
      ~activation_timestamp
  in
  let* () = finalizeL1 () in
  let provider_port = Port.fresh () in
  let preimages_endpoint =
    sf "http://%s:%d" Constant.default_host provider_port
  in
  let* () =
    Evm_node.run
      ~extra_arguments:["--preimages-endpoint"; preimages_endpoint]
      new_sequencer
  in
  let* () =
    Evm_node.wait_for_predownload_kernel_failed new_sequencer ~root_hash
  in
  (* Create a file server that serves the preimages. *)
  let served = ref false in
  Sc_rollup_helpers.serve_files
    ~name:"preimages_server"
    ~port:provider_port
    ~root:(Sc_rollup_node.data_dir sc_rollup_node // "wasm_2_0_0")
    ~on_request:(fun _ -> served := true)
  @@ fun () ->
  let* _ =
    Evm_node.wait_for_predownload_kernel ~timeout:90. new_sequencer ~root_hash
  in
  Check.is_true
    !served
    ~error_msg:"The sequencer should have used the file server" ;
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

let test_replay_rpc =
  register_all
    ~__FILE__
    ~tags:["evm"; "rpc"; "replay"]
    ~title:"Sequencer can replay a block"
    ~time_between_blocks:Nothing
  @@ fun {sc_rollup_node; sequencer; client; _} _protocol ->
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
  let* () = bake_until_sync ~sequencer ~sc_rollup_node ~client () in
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
    ~__FILE__
    ~kernels:Kernel.etherlink_all
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

let test_trace_transaction_on_invalid_transaction =
  register_all
    ~__FILE__
    ~kernels:Kernel.etherlink_all
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
      let expected_value = hex_256_of_int value in
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
    ~__FILE__
    ~kernels:Kernel.etherlink_all
    ~tags:["evm"; "rpc"; "trace"; "call"]
    ~title:"Sequencer can run debug_traceTransaction and return a valid log"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  (* Transfer funds to a random address. *)
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* simple_storage_resolved =
    Solidity_contracts.simple_storage evm_version
  in

  (* deploy contract *)
  let* () =
    Eth_cli.add_abi
      ~label:simple_storage_resolved.label
      ~abi:simple_storage_resolved.abi
      ()
  in
  let* contract_address, _tx_deployment =
    send_transaction_to_sequencer
      (Eth_cli.deploy
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
  (* The second test will disable every tracing options and call `get`, which
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
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call_trace"]
    ~title:"Sequencer can run debug_traceTransaction with calltracer"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  (* Transfer funds to a random address. *)
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* simple_storage_resolved =
    Solidity_contracts.simple_storage evm_version
  in
  (* deploy contract *)
  let* () =
    Eth_cli.add_abi
      ~label:simple_storage_resolved.label
      ~abi:simple_storage_resolved.abi
      ()
  in
  let* contract_address, _tx_deployment =
    send_transaction_to_sequencer
      (Eth_cli.deploy
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
    (JSON.(trace_result |-> "gasUsed" |> as_string)
    = Format.sprintf "0x%02x" (Int64.to_int transaction_receipt.gasUsed))
      string
      ~error_msg:"Wrong gasUsed, expected %R but got %L") ;
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
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call_trace"; "fail"]
    ~title:
      "debug_traceTransaction with calltracer returns an error when contract \
       creation reverts but is included"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* call_types = Solidity_contracts.call_types evm_version in
  let* () = Eth_cli.add_abi ~label:call_types.label ~abi:call_types.abi () in
  let* _contract_address, create_tx =
    send_transaction_to_sequencer
      (Eth_cli.deploy
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

let test_trace_delegate_call =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call_trace"; "delegate_call"]
    ~title:"calltracer correctly display delegate call infos"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* delegated = Solidity_contracts.delegatecall_delegated evm_version in
  let* delegator = Solidity_contracts.delegatecall_delegator evm_version in
  let* () = Eth_cli.add_abi ~label:delegated.label ~abi:delegated.abi () in
  let* () = Eth_cli.add_abi ~label:delegator.label ~abi:delegator.abi () in
  let* delegated_address, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:sender.Eth_account.private_key
         ~endpoint
         ~abi:delegated.label
         ~bin:delegated.bin)
      sequencer
  in
  let* _ = produce_block sequencer in
  let* delegator_address, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:sender.Eth_account.private_key
         ~endpoint
         ~abi:delegator.label
         ~bin:delegator.bin)
      sequencer
  in
  let* _ = produce_block sequencer in
  let* tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.contract_send
         ~source_private_key:sender.private_key
         ~endpoint
         ~abi_label:delegator.label
         ~address:delegator_address
         ~method_call:(Format.sprintf "setVars(\"%s\",%d)" delegated_address 42))
      sequencer
  in
  let* _ = produce_block sequencer in
  let*@ top_call =
    Rpc.trace_transaction
      ~tracer:"callTracer"
      ~transaction_hash:tx_hash
      ~tracer_config:[("withLog", `Bool false); ("onlyTopCall", `Bool false)]
      sequencer
  in
  Check.(
    JSON.(
      top_call |-> "to" |> as_string = String.lowercase_ascii delegator_address)
      string
      ~error_msg:"Top call should have been traced as coming from %R but was %L") ;
  Check.(
    JSON.(top_call |-> "calls" |> as_list <> [])
      (list json)
      ~error_msg:"Top call should have some subcalls") ;
  let delegatecall = JSON.(top_call |-> "calls" |> as_list |> List.hd) in
  Check.(
    JSON.(
      delegatecall |-> "from" |> as_string
      = String.lowercase_ascii delegator_address)
      string
      ~error_msg:"Delegate call should be traced as coming from %R but was %L") ;
  Check.(
    JSON.(
      delegatecall |-> "to" |> as_string
      = String.lowercase_ascii delegated_address)
      string
      ~error_msg:
        "Delegate call should be traced as going to delegated contract %R but \
         was %L") ;
  unit

let test_trace_transaction_calltracer_all_types =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call_trace"; "types"]
    ~title:
      "debug_traceTransaction with calltracer can produce all the call types"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* call_types = Solidity_contracts.call_types evm_version in
  let* () = Eth_cli.add_abi ~label:call_types.label ~abi:call_types.abi () in
  let* contract_address, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
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
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call_trace"; "with_logs"]
    ~title:"debug_traceTransaction with calltracer can produce a call with logs"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* simple_logger = Solidity_contracts.simple_logger evm_version in
  let* () =
    Eth_cli.add_abi ~label:simple_logger.label ~abi:simple_logger.abi ()
  in
  let* contract_address, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
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
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call_trace"; "depth"]
    ~title:"debug_traceTransaction with calltracer to see difficult depth"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* call_tracer_depth = Solidity_contracts.call_tracer_depth evm_version in
  let* () =
    Eth_cli.add_abi ~label:call_tracer_depth.label ~abi:call_tracer_depth.abi ()
  in
  let* contract_address, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
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

let test_trace_transaction_call_revert =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call_trace"; "revert"; "propagate"]
    ~title:
      "debug_traceTransaction with calltracer to see how propagated revert is \
       handled"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* call_revert = Solidity_contracts.call_revert evm_version in
  let* () = Eth_cli.add_abi ~label:call_revert.label ~abi:call_revert.abi () in
  let* contract_address, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:sender.Eth_account.private_key
         ~endpoint
         ~abi:call_revert.label
         ~bin:call_revert.bin)
      sequencer
  in
  let* _ = produce_block sequencer in
  let* txn =
    Cast.craft_tx
      ~chain_id:1337
      ~source_private_key:sender.private_key
      ~nonce:1
      ~gas_price:1_000_000_000
      ~gas:30_000
      ~value:Wei.zero
      ~address:contract_address
      ~signature:"callerFunction()"
      ()
  in
  let*@ txn_hash = Rpc.send_raw_transaction ~raw_tx:txn sequencer in
  let* _ = produce_block sequencer in
  let*@ trace_result =
    Rpc.trace_transaction
      ~tracer:"callTracer"
      ~transaction_hash:txn_hash
      ~tracer_config:[("withLog", `Bool false); ("onlyTopCall", `Bool false)]
      sequencer
  in
  Check.(
    (JSON.(trace_result |-> "error" |> as_string) = "execution reverted")
      string
      ~error_msg:"Unexpected error got %L but %R was expected") ;
  Check.(
    (JSON.(trace_result |-> "revertReason" |> as_string)
    = "This function reverts")
      string
      ~error_msg:"Unexpected revert reason got %L but %R was expected") ;
  unit

let test_trace_transaction_call_trace_revert =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call_trace"; "revert"]
    ~title:"debug_traceTransaction with calltracer to see how revert is handled"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* call_tracer_revert = Solidity_contracts.call_tracer_revert evm_version in
  let* () =
    Eth_cli.add_abi
      ~label:call_tracer_revert.label
      ~abi:call_tracer_revert.abi
      ()
  in
  let* contract_address, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
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
        let revert_reason_call = JSON.(call |-> "revertReason" |> as_string) in
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
  check_error_and_revert_reason ~error:"ReentrancySentryOOG" call ;
  let call = List.nth call_list 3 in
  check_error_and_revert_reason ~error:"InvalidFEOpcode" call ;
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
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call_trace"; "multi_txs"]
    ~title:
      "debug_traceTransaction handles blocks containing several transactions"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let sender_0 = Eth_account.bootstrap_accounts.(0) in
  let sender_1 = Eth_account.bootstrap_accounts.(1) in
  let* call_types = Solidity_contracts.call_types evm_version in
  let* () = Eth_cli.add_abi ~label:call_types.label ~abi:call_types.abi () in
  let* address, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
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
    ~__FILE__
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
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call_trace"; "precompiles"]
    ~title:"debug_traceTransaction with calltracer can trace precompiles"
    ~da_fee:Wei.zero
    ~maximum_allowed_ticks:100_000_000_000_000L
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* precompiles = Solidity_contracts.precompiles evm_version in
  let* () = Eth_cli.add_abi ~label:precompiles.label ~abi:precompiles.abi () in
  let* contract_address, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
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
    ~__FILE__
    ~kernels:[Latest]
    ~time_between_blocks:Nothing
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "rpc"; "trace"; "call_trace"; "deposit"]
    ~title:"debug_traceTransaction with calltracer can trace deposits"
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
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
    Eth_cli.balance ~account:receiver.address ~endpoint ()
  in
  let deposit_info =
    {receiver = EthereumAddr receiver.address; chain_id = None}
  in
  let* () =
    send_deposit_to_delayed_inbox
      ~amount
      ~bridge:l1_contracts.bridge
      ~depositor
      ~deposit_info
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
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in
  let* receiver_balance_next =
    Eth_cli.balance ~account:receiver.address ~endpoint ()
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

let test_trace_transaction_calltracer_on_nested_delegatecalls =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "trace"; "call_trace"; "nested_delegatecalls"]
    ~title:
      "debug_traceTransaction with calltracer can trace nested delegatecalls \
       with appropriate sender addresses for each call"
    ~da_fee:Wei.zero
    ~maximum_allowed_ticks:100_000_000_000_000L
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let source_private_key, address_eoa =
    Eth_account.
      ( bootstrap_accounts.(0).private_key,
        String.lowercase_ascii bootstrap_accounts.(0).address )
  in
  let init_contract ~contract =
    let open Solidity_contracts in
    let* contract = contract evm_version in
    let* () = Eth_cli.add_abi ~label:contract.label ~abi:contract.abi () in
    let* address, _ =
      send_transaction_to_sequencer
        (Eth_cli.deploy
           ~source_private_key
           ~endpoint
           ~abi:contract.label
           ~bin:contract.bin)
        sequencer
    in
    return (String.lowercase_ascii address, contract.label)
  in
  let* address_A, label_A =
    init_contract ~contract:Solidity_contracts.nested_delegatecalls_A
  in
  let* address_B, _ =
    init_contract ~contract:Solidity_contracts.nested_delegatecalls_B
  in
  let* address_C, _ =
    init_contract ~contract:Solidity_contracts.nested_delegatecalls_C
  in
  let* address_D, _ =
    init_contract ~contract:Solidity_contracts.nested_delegatecalls_D
  in
  let* _ = produce_block sequencer in
  let* transaction_hash =
    send_transaction_to_sequencer
      (Eth_cli.contract_send
         ~source_private_key
         ~endpoint
         ~abi_label:label_A
         ~address:address_A
         ~method_call:
           (Format.sprintf
              "callB(\"%s\",\"%s\",\"%s\")"
              address_B
              address_C
              address_D))
      sequencer
  in
  let* _ = produce_block sequencer in
  let*@ trace_result =
    Rpc.trace_transaction
      ~tracer:"callTracer"
      ~transaction_hash
      ~tracer_config:[("withLog", `Bool true); ("onlyTopCall", `Bool false)]
      sequencer
  in
  assert (JSON.(trace_result |-> "type" |> as_string) = "CALL") ;
  assert (JSON.(trace_result |-> "from" |> as_string) = address_eoa) ;
  assert (JSON.(trace_result |-> "to" |> as_string) = address_A) ;
  let call_list = JSON.(trace_result |-> "calls" |> as_list) in
  let delegatecall_1 = List.hd call_list in
  assert (JSON.(delegatecall_1 |-> "type" |> as_string) = "DELEGATECALL") ;
  assert (JSON.(delegatecall_1 |-> "from" |> as_string) = address_A) ;
  assert (JSON.(delegatecall_1 |-> "to" |> as_string) = address_B) ;
  let delegatecall_1_call_list = JSON.(delegatecall_1 |-> "calls" |> as_list) in
  let delegatecall_2 = List.hd delegatecall_1_call_list in
  assert (JSON.(delegatecall_2 |-> "type" |> as_string) = "CALL") ;
  assert (JSON.(delegatecall_2 |-> "from" |> as_string) = address_A) ;
  assert (JSON.(delegatecall_2 |-> "to" |> as_string) = address_C) ;
  let delegatecall_2_call_list = JSON.(delegatecall_2 |-> "calls" |> as_list) in
  let delegatecall_3 = List.hd delegatecall_2_call_list in
  assert (JSON.(delegatecall_3 |-> "type" |> as_string) = "CALL") ;
  assert (JSON.(delegatecall_3 |-> "from" |> as_string) = address_C) ;
  assert (JSON.(delegatecall_3 |-> "to" |> as_string) = address_D) ;
  unit

let test_miner =
  let sequencer_pool_address =
    String.lowercase_ascii "0x8aaD6553Cf769Aa7b89174bE824ED0e53768ed70"
  in
  register_all
    ~__FILE__
    ~tags:["evm"; "miner"; "coinbase"]
    ~title:"Sequencer pool address is the block's miner"
    ~sequencer_pool_address
  @@ fun {sequencer; evm_version; _} _protocol ->
  let*@ block = Rpc.get_block_by_number ~block:"latest" sequencer in
  Check.((String.lowercase_ascii block.miner = sequencer_pool_address) string)
    ~error_msg:
      "Block miner should be the sequencer pool address, expected %R got %L" ;
  (* We deploy a contract that stores the block coinbase in its storage, and
     also has a view to get the block coinbase. *)
  let* coinbase_resolved = Solidity_contracts.coinbase evm_version in
  let* () =
    Eth_cli.add_abi ~label:coinbase_resolved.label ~abi:coinbase_resolved.abi ()
  in
  let* contract, _tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.deploy
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

  (*
     Regression test:

     Custom RPC call parameters to make sure we support it correctly. A bug
     made the node consider `"params" : []` to be invalid.
  *)
  let* r1 =
    Evm_node.jsonrpc sequencer {method_ = "eth_coinbase"; parameters = `Null}
  in
  let r1 = JSON.(r1 |-> "result" |> as_string) in
  let* r2 =
    Evm_node.jsonrpc sequencer {method_ = "eth_coinbase"; parameters = `A []}
  in
  let r2 = JSON.(r2 |-> "result" |> as_string) in
  Check.is_true
    (r1 = r2 && r2 = rpc_coinbase)
    ~error_msg:"Expected all coinbase addresses to be equal" ;

  unit

let test_fa_bridge_feature_flag =
  register_all
    ~__FILE__
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

let test_multichain_feature_flag =
  register_all
    ~__FILE__
    ~tags:["multichain"; "feature_flag"]
    ~title:"Check the multichain feature value in storage"
  @@ fun {sequencer; enable_multichain; _} _protocol ->
  let*@ flag =
    Rpc.state_value sequencer Durable_storage_path.enable_multichain
  in
  Check.(Option.is_some flag = enable_multichain)
    Check.bool
    ~error_msg:
      "Multichain feature flag in the durable storage is %L, expected %R" ;
  unit

let test_fast_withdrawal_feature_flag =
  register_all
    ~__FILE__
    ~tags:["fast_withdrawal"; "feature_flag"]
    ~title:"Fast withdrawal feature is set in storage"
    ~enable_fast_withdrawal:true
  @@ fun {sequencer; _} _protocol ->
  (* We simply check that the flag is set in the storage. *)
  let*@ flag =
    Rpc.state_value sequencer Durable_storage_path.enable_fast_withdrawal
  in
  Check.is_true
    (Option.is_some flag)
    ~error_msg:
      (sf
         "Expected to have a value at %s"
         Durable_storage_path.enable_fast_withdrawal) ;
  unit

let fast_withdrawal ?(expect_failure = false) ~sender ~endpoint ~amount_wei
    ~produce_block ~receiver ~fast_withdrawal_contract_address () =
  let* () =
    Eth_cli.add_abi
      ~label:"fast_withdraw_base58"
      ~abi:(predep_xtz_bridge_abi_path ())
      ()
  in
  (* Call the withdrawal precompiled contract. *)
  let call_fast_withdrawal =
    Eth_cli.contract_send
      ~expect_failure
      ~source_private_key:sender.Eth_account.private_key
      ~endpoint
      ~abi_label:"fast_withdraw_base58"
      ~address:Solidity_contracts.Precompile.xtz_bridge
        (* NB: the third parameter is unused for now, could be used later for
           maximum fees to pay, whitelist of service providers etc. *)
      ~method_call:
        (sf
           {|fast_withdraw_base58("%s","%s","%s")|}
           receiver
           fast_withdrawal_contract_address
           "0x0000000000000000000000000000000000000000000000000000000000000001")
      ~value:amount_wei
      ~gas:16_000_000
  in
  wait_for_application ~produce_block call_fast_withdrawal

type fast_withdrawal_event =
  | Tez of {
      target : string;
      withdrawal_id : string;
      amount : Tez.t;
      timestamp : string;
      payload : string;
      l2_caller : string;
    }
  | FA of {
      sender : string; (* Holds the L2 caller *)
      ticket_owner : string;
      receiver : string;
      proxy : string;
      amount : Tez.t;
          (* The amount of an FA token is denominated in the underlying asset's unit.
             Since we're testing wrapped Tez, the unit in this case is Tez. *)
      withdrawal_id : string;
      timestamp : string;
      payload : string;
    }

let fast_withdrawal_tez_event_signature =
  "FastWithdrawal(bytes22,uint256,uint256,uint256,bytes,address)"

let fast_withdrawal_fa_token_event_signature =
  "FastFaWithdrawal(address,address,bytes22,bytes22,uint256,uint256,uint256,bytes)"

let revm_fast_withdrawal_fa_token_event_signature =
  "FastFaWithdrawal(uint256,address,address,bytes22,bytes22,uint256,uint256,uint256,bytes)"

let revm_queued_xtz_deposit_event_signature =
  "QueuedDeposit(uint256,uint256,address,uint256,uint256)"

let revm_xtz_deposit_event_signature =
  "Deposit(uint256,address,uint256,uint256)"

let eip7702_fallback_event_signature = "EIP7702FallbackCall(address)"

let event_topic ~event_signature =
  Tezos_crypto.Hacl.Hash.Keccak_256.digest (Bytes.of_string event_signature)
  |> Hex.of_bytes |> Hex.show |> add_0x

let fast_withdrawal_tez_event_topic =
  event_topic ~event_signature:fast_withdrawal_tez_event_signature

let fast_withdrawal_fa_token_event_topic =
  event_topic ~event_signature:fast_withdrawal_fa_token_event_signature

let revm_fast_withdrawal_fa_token_event_topic =
  event_topic ~event_signature:revm_fast_withdrawal_fa_token_event_signature

let revm_queued_xtz_deposit_event_topic =
  event_topic ~event_signature:revm_queued_xtz_deposit_event_signature

let revm_xtz_deposit_event_topic =
  event_topic ~event_signature:revm_xtz_deposit_event_signature

let eip7702_fallback_event_topic =
  event_topic ~event_signature:eip7702_fallback_event_signature

let find_and_decode_fast_withdrawal_event ?(fa_tokens = false)
    ?(enable_revm = false) receipt : fast_withdrawal_event Lwt.t =
  (* Define the fast withdrawal event log topic, which will be searched for in the EVM logs.
     This topic is a hashed identifier that corresponds to the fast withdrawal transaction event. *)
  let fast_withdrawal_event_signature, fast_withdrawal_event_topic =
    if fa_tokens then
      ( fast_withdrawal_fa_token_event_signature,
        fast_withdrawal_fa_token_event_topic )
    else (fast_withdrawal_tez_event_signature, fast_withdrawal_tez_event_topic)
  in

  (* Extract the event data from the EVM logs based on the provided topic.
     This searches the transaction logs for the specified topic and returns the corresponding data. *)
  (* If REVM is enabled we want to match the topic containing ticket hash at the start of the signature.
     Ticket hash is an indexed uint256 and will not appear in the encoded data.
     Indexed fields is the standard way to create a topic in solidity, previous signature and topics pair were incorrect. *)
  let queried_topic =
    if fa_tokens && enable_revm then revm_fast_withdrawal_fa_token_event_topic
    else fast_withdrawal_event_topic
  in
  let log =
    let tx_log =
      List.find_opt
        (fun tx ->
          List.exists (String.equal queried_topic) tx.Transaction.topics)
        receipt.Transaction.logs
      |> Option.get
    in
    tx_log.Transaction.data
  in

  let* res =
    Eth_cli.decode_method
      ~abi_label:fast_withdrawal_event_signature
      ~method_:(String.sub fast_withdrawal_event_topic 2 8 ^ log)
      ()
  in
  return
  @@
  match JSON.parse ~origin:"tx_log" res |> JSON.unannotate with
  | `A
      [
        `String target;
        `String withdrawal_id;
        `String amount;
        `String timestamp;
        `String payload;
        `String l2_caller;
      ]
    when not fa_tokens ->
      Tez
        {
          target;
          withdrawal_id;
          amount = Wei.of_string amount |> Wei.to_tez;
          timestamp;
          payload;
          l2_caller;
        }
  | `A
      [
        `String sender;
        `String ticket_owner;
        `String receiver;
        `String proxy;
        `String amount;
        `String withdrawal_id;
        `String timestamp;
        `String payload;
      ]
    when fa_tokens ->
      FA
        {
          sender;
          ticket_owner;
          receiver;
          proxy;
          amount = Tez.of_int @@ int_of_string amount;
          withdrawal_id;
          timestamp;
          payload;
        }
  | _ -> failwith "Failed to decode fast withdrawal's event"

let execute_payout ~service_provider_pkh ~exchanger
    ~fast_withdrawal_contract_address ~service_provider_proxy
    ?(fa_tokens = false) ?(enable_revm = false) client receipt =
  let* fast_withdrawal_event =
    find_and_decode_fast_withdrawal_event ~fa_tokens ~enable_revm receipt
  in
  let withdraw_amount, withdrawal_id, target, timestamp, payload, l2_caller =
    match fast_withdrawal_event with
    | Tez {amount; withdrawal_id; target; timestamp; payload; l2_caller; _} ->
        (amount, withdrawal_id, target, timestamp, payload, l2_caller)
    | FA {amount; withdrawal_id; receiver; timestamp; payload; sender; _} ->
        (amount, withdrawal_id, receiver, timestamp, payload, sender)
  in
  Client.transfer
    ~fee:(Tez.of_int 1) (* Small fee for the transaction *)
    ~fee_cap:(Tez.of_int 1)
    ~gas_limit:100_000_000
    ~storage_limit:Int.max_int
    ~burn_cap:(Tez.of_int 100)
    ~amount:withdraw_amount
    ~giver:Constant.bootstrap1.public_key_hash
    ~receiver:service_provider_proxy
    ~entrypoint:(if fa_tokens then "payout_proxy_fa" else "payout_proxy_tez")
    ~arg:
      (Printf.sprintf
         "(Pair %S %S %s%s %s %s %S %s %s)"
         fast_withdrawal_contract_address
         exchanger
         (if fa_tokens then Tez.to_string withdraw_amount ^ " " else "")
         withdrawal_id
         target
         timestamp
         service_provider_pkh
         payload
         l2_caller)
    client

let test_fast_withdrawal_l2_caller =
  let commitment_period = 5 and challenge_window = 5 in
  register_all
    ~__FILE__
    ~tags:["fast_withdrawal"; "deposit"]
    ~title:"Deposit and fast withdraw tez with forwarding contract"
    ~commitment_period
    ~challenge_window
    ~enable_fast_withdrawal:true
    ~time_between_blocks:Nothing
    ~kernels:[Kernel.Mainnet; Kernel.Latest]
  @@ fun {sequencer; evm_version; _} _protocol ->
  let fast_withdrawal_contract_address =
    "KT1TczPwz5KjAuuJKvkTmttS7bBioT5gjQ4Y"
  in
  let produce_block () = Rpc.produce_block sequencer in
  let endpoint = Evm_node.endpoint sequencer in
  let* withdrawal_forwarder_contract =
    Solidity_contracts.call_fast_withdrawal evm_version
  in
  let sender = Eth_account.bootstrap_accounts.(0) in
  (* deploy the contract *)
  let* () =
    Eth_cli.add_abi
      ~label:withdrawal_forwarder_contract.label
      ~abi:withdrawal_forwarder_contract.abi
      ()
  in
  let* withdrawal_forwarder_contract_address, _tx =
    Eth_cli.deploy
      ~source_private_key:sender.Eth_account.private_key
      ~endpoint
      ~abi:withdrawal_forwarder_contract.label
      ~bin:withdrawal_forwarder_contract.bin
    |> wait_for_application ~produce_block
  in

  let withdraw_amount = Tez.of_int 50 in

  (* Define the Tezos address that will receive the fast withdrawal on L1. *)
  let withdraw_receiver = "tz1fp5ncDmqYwYC568fREYz9iwQTgGQuKZqX" in

  (* Define the amount for fast withdrawal as 50 tez (half of the deposited amount). *)
  let withdraw_amount_wei = Wei.of_tez withdraw_amount in

  let* tx_hash =
    Eth_cli.contract_send
      ~source_private_key:sender.Eth_account.private_key
      ~endpoint
      ~abi_label:withdrawal_forwarder_contract.label
      ~address:withdrawal_forwarder_contract_address
      ~method_call:
        (sf
           "forwardWithdrawal(\"%s\", \"%s\")"
           withdraw_receiver
           fast_withdrawal_contract_address)
      ~value:withdraw_amount_wei
      ~gas:16_000_000
    |> wait_for_application ~produce_block
  in

  let*@ receipt = Rpc.get_transaction_receipt ~tx_hash sequencer in
  let receipt = Option.get receipt in

  let* fast_withdrawal_event = find_and_decode_fast_withdrawal_event receipt in
  let l2_caller =
    match fast_withdrawal_event with
    | Tez {l2_caller; _} -> l2_caller
    | _ -> failwith "Tez case of the fast withdrawal event expected"
  in
  Check.((withdrawal_forwarder_contract_address = l2_caller) string)
    ~error_msg:"Expected %L as L2 caller instead of %R" ;
  unit

let test_deposit_and_fast_withdraw =
  let commitment_period = 5 and challenge_window = 5 in
  register_all
    ~__FILE__
    ~tags:["fast_withdrawal"; "deposit"]
    ~title:"Deposit and fast withdraw tez"
    ~commitment_period
    ~challenge_window
    ~enable_fast_withdrawal:true
    ~time_between_blocks:Nothing
    ~kernels:[Kernel.Mainnet; Kernel.Latest]
  @@
  fun {
        sequencer;
        sc_rollup_address;
        client;
        l1_contracts;
        proxy;
        sc_rollup_node;
        kernel;
        _;
      }
      _protocol
    ->
  let enable_revm = Kernel.supports_revm kernel in
  let {exchanger; _} = l1_contracts in
  let* fast_withdrawal_contract_address =
    Client.originate_contract
      ~alias:"fast_withdrawal_contract_address"
      ~amount:Tez.zero
      ~src:Constant.bootstrap5.public_key_hash
      ~init:(sf "Pair %S {}" exchanger)
      ~prg:(fast_withdrawal_path ())
      ~burn_cap:Tez.one
      client
  in
  let* () = Client.bake_for_and_wait ~keys:[] client in

  let* service_provider_proxy =
    Client.originate_contract
      ~alias:"service_provider"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.public_key_hash
      ~init:
        "Pair \"KT1CeFqjJRJPNVvhvznQrWfHad2jCiDZ6Lyj\" \
         \"KT1CeFqjJRJPNVvhvznQrWfHad2jCiDZ6Lyj\" 0 \
         \"tz1etHLky7fuVumvBDi92ogXQZZPESiFimWR\" 0 \
         \"tz1etHLky7fuVumvBDi92ogXQZZPESiFimWR\" 0x00 0x00"
      ~prg:(service_provider_path ())
      ~burn_cap:(Tez.of_int 890)
      client
  in
  let admin = Constant.bootstrap5 in
  let produce_block () = Rpc.produce_block sequencer in

  let withdraw_amount = Tez.of_int 50 in
  (* Define the amount to deposit in tez (100 tez), and specify the Ethereum-based receiver for the rollup. *)
  let deposit_amount = Tez.of_int 100 in
  let service_provider_pkh = "tz1TGKSrZrBpND3PELJ43nVdyadoeiM1WMzb" in
  let* initial_service_provider_balance =
    Client.get_balance_for ~account:service_provider_pkh client
  in

  let receiver =
    Eth_account.
      {
        address = "0x1074Fd1EC02cbeaa5A90450505cF3B48D834f3EB";
        private_key =
          "0xb7c548b5442f5b28236f0dcd619f65aaaafd952240908adcf9642d8e616587ee";
      }
  in

  (* Define the Tezos address that will receive the fast withdrawal on L1. *)
  let withdraw_receiver = "tz1fp5ncDmqYwYC568fREYz9iwQTgGQuKZqX" in

  (* Check the initial balance of the L1 withdraw receiver. It should be 0 before the fast withdrawal occurs. *)
  let* balance_withdraw_receiver =
    Client.get_balance_for ~account:withdraw_receiver client
  in
  Check.((balance_withdraw_receiver = Tez.of_int 0) Tez.typ)
    ~error_msg:"Expected %R as initial balance instead of %L" ;

  (* Execute the deposit of 100 tez to the rollup. The depositor is the admin account, and the receiver is the Ethereum address. *)
  let deposit_info =
    {receiver = EthereumAddr receiver.address; chain_id = None}
  in
  let* () =
    send_deposit_to_delayed_inbox
      ~bridge:l1_contracts.bridge
      ~amount:deposit_amount
      ~sc_rollup_address
      ~sc_rollup_node
      ~depositor:admin
      ~deposit_info
      client
  in

  (* Wait for the sequencer to detect and include the deposit. *)
  let* () =
    wait_for_delayed_inbox_add_tx_and_injected
      ~sequencer
      ~sc_rollup_node
      ~client
  in
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in

  (* Check that the receiver's balance in the rollup matches the deposited amount. *)
  let check_balance ~receiver ~endpoint expected_balance =
    let* balance = Eth_cli.balance ~account:receiver ~endpoint () in
    let balance = Wei.truncate_to_mutez balance in
    Check.((balance = Tez.to_mutez expected_balance) int)
      ~error_msg:
        (sf "Expected balance of %s should be %%R, but got %%L" receiver) ;
    unit
  in
  let* () =
    check_balance
      ~receiver:receiver.address
      ~endpoint:(Evm_node.endpoint sequencer)
      deposit_amount
  in

  (* Define the amount for fast withdrawal as 50 tez (half of the deposited amount). *)
  let withdraw_amount_wei = Wei.of_tez withdraw_amount in

  let* withdrawal_level = Client.level client in
  let* tx_hash =
    fast_withdrawal
      ~produce_block
      ~amount_wei:withdraw_amount_wei
      ~sender:receiver
      ~receiver:withdraw_receiver
      ~fast_withdrawal_contract_address
      ~endpoint:(Evm_node.endpoint sequencer)
      ()
  in

  let*@ receipt = Rpc.get_transaction_receipt ~tx_hash sequencer in
  let receipt = Option.get receipt in

  let* () =
    execute_payout
      ~exchanger
      ~service_provider_pkh
      ~fast_withdrawal_contract_address
      ~service_provider_proxy
      ~enable_revm
      client
      receipt
  in

  let* _ =
    find_and_execute_withdrawal
      ~outbox_lookup_depth:100
      ~withdrawal_level
      ~commitment_period
      ~challenge_window
      ~evm_node:proxy
      ~sc_rollup_node
      ~sc_rollup_address
      ~client
      ()
  in

  let* balance = Client.get_balance_for ~account:withdraw_receiver client in
  let* final_withdraw_receiver_balance =
    Client.get_balance_for ~account:service_provider_pkh client
  in

  (* Check that the destination address of the withdrawal has a balance of
     50 tez after the fast withdrawal is complete. *)
  Check.((balance = withdraw_amount) Tez.typ)
    ~error_msg:"Expected %R amount instead of %L after withdrawal" ;

  (* Verify that the service provider's balance increased by 50 tez after the
     fast withdrawal payout. *)
  Check.(
    (Tez.(initial_service_provider_balance + withdraw_amount)
    = final_withdraw_receiver_balance)
      Tez.typ)
    ~error_msg:
      "Expected %R amount instead of %L after outbox message was executed" ;

  return ()

let test_deposit_and_fa_fast_withdraw =
  let commitment_period = 5 and challenge_window = 5 in
  register_all
    ~__FILE__
    ~tags:["fast_withdrawal"; "fa_tokens"; "deposit"]
    ~title:"Deposit and fast withdraw FA tokens"
    ~commitment_period
    ~challenge_window
    ~time_between_blocks:Nothing
    ~kernels:[Kernel.Latest]
    ~additional_uses:[Constant.octez_codec]
    ~enable_fa_bridge:true
    ~enable_fast_fa_withdrawal:true
  @@
  fun {
        sequencer;
        sc_rollup_address;
        client;
        l1_contracts;
        proxy;
        sc_rollup_node;
        kernel;
        _;
      }
      _protocol
    ->
  let enable_revm = Kernel.supports_revm kernel in
  let* fast_withdrawal_contract_address =
    Client.originate_contract
      ~alias:"fast_withdrawal_contract_address"
      ~amount:Tez.zero
      ~src:Constant.bootstrap5.public_key_hash
      ~init:(sf "Pair %S {}" l1_contracts.exchanger)
      ~prg:(fast_withdrawal_path ())
      ~burn_cap:Tez.one
      client
  in
  let* () = Client.bake_for_and_wait ~keys:[] client in

  let* ticketer = ticket_creator l1_contracts.ticket_router_tester in

  let* service_provider_proxy =
    Client.originate_contract
      ~alias:"service_provider"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.public_key_hash
      ~init:
        "Pair \"KT1CeFqjJRJPNVvhvznQrWfHad2jCiDZ6Lyj\" \
         \"KT1CeFqjJRJPNVvhvznQrWfHad2jCiDZ6Lyj\" 0 \
         \"tz1etHLky7fuVumvBDi92ogXQZZPESiFimWR\" 0 \
         \"tz1etHLky7fuVumvBDi92ogXQZZPESiFimWR\" 0x00 0x00"
      ~prg:(service_provider_path ())
      ~burn_cap:(Tez.of_int 890)
      client
  in
  let depositor = Constant.bootstrap5 in

  let withdraw_amount = 50 in
  (* Define the amount to deposit in tez (100 tez), and specify the Ethereum-based receiver for the rollup. *)
  let deposit_amount = 100 in
  let service_provider_pkh = "tz1TGKSrZrBpND3PELJ43nVdyadoeiM1WMzb" in
  let* initial_service_provider_balance =
    Client.get_balance_for ~account:service_provider_pkh client
  in

  let receiver = Eth_account.bootstrap_accounts.(0) in

  (* Define the Tezos address that will receive the fast withdrawal on L1. *)
  let withdraw_receiver = "tz1fp5ncDmqYwYC568fREYz9iwQTgGQuKZqX" in

  (* Switch ticket tester contract to rollup node *)
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

  (* Check the initial balance of the L1 withdraw receiver. It should be 0 before the fast withdrawal occurs. *)
  let* receiver_balance =
    Client.ticket_balance
      ~contract:withdraw_receiver
      ~ticketer:l1_contracts.ticket_router_tester
      ~content_type:"pair nat (option bytes)"
      ~content:"Pair 0 None"
      client
  in

  Check.((int_of_string @@ String.trim receiver_balance = 0) int)
    ~error_msg:"Expected %R as initial balance instead of %L" ;
  let* () = Client.bake_for_and_wait ~keys:[] client in
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

  (* Execute the deposit of 100 tez to the rollup. The depositor is the admin account, and the receiver is the Ethereum address. *)
  let* () =
    send_fa_deposit_to_delayed_inbox
      ~l1_contracts
      ~amount:deposit_amount
      ~sc_rollup_address
      ~sc_rollup_node
      ~depositor
      ~receiver:receiver.address
      client
  in

  (* Wait for the sequencer to detect and include the deposit. *)
  let* () =
    wait_for_delayed_inbox_add_tx_and_injected
      ~sequencer
      ~sc_rollup_node
      ~client
  in
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in

  (* Check that the receiver's balance in the rollup matches the deposited amount. *)
  let* zero_ticket_hash = ticket_hash l1_contracts.ticket_router_tester 0 in

  let* ticket_balance_after_deposit =
    ticket_balance
      ~ticket_hash:zero_ticket_hash
      ~account:receiver.address
      (Either.Right sequencer)
  in
  Check.((deposit_amount = ticket_balance_after_deposit) int)
    ~error_msg:
      "After deposit we expect %L ticket balance in the sequencer, got %R" ;

  let* withdrawal_level = Client.level client in
  let* content = ticket_content 0 in
  let* tx_hash =
    call_fa_fast_withdraw
      ~sender:receiver
      ~sequencer
      ~ticket_owner:receiver.address
      ~receiver:withdraw_receiver
      ~amount:withdraw_amount
      ~ticketer:(ticketer |> Hex.of_bytes |> Hex.show)
      ~content:(content |> Hex.of_bytes |> Hex.show)
      ~fast_withdrawal_contract_address
      ()
  in

  let*@! receipt = Rpc.get_transaction_receipt ~tx_hash sequencer in

  let* () =
    execute_payout
      ~exchanger:l1_contracts.ticket_router_tester
      ~service_provider_pkh
      ~fast_withdrawal_contract_address
      ~service_provider_proxy
      ~fa_tokens:true
      ~enable_revm
      client
      receipt
  in
  let* () = Client.bake_for_and_wait ~keys:[] client in
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

  let* receiver_balance =
    Client.ticket_balance
      ~contract:withdraw_receiver
      ~ticketer:l1_contracts.ticket_router_tester
      ~content_type:"pair nat (option bytes)"
      ~content:"Pair 0 None"
      client
  in

  (* Check that the destination address of the withdrawal has a balance of
     50 tez after the fast withdrawal is complete, i.e. the payout is applied. *)
  Check.((int_of_string @@ String.trim receiver_balance = withdraw_amount) int)
    ~error_msg:"Expected %R amount instead of %L after withdrawal" ;

  let* _ =
    find_and_execute_withdrawal
      ~outbox_lookup_depth:100
      ~withdrawal_level
      ~commitment_period
      ~challenge_window
      ~evm_node:proxy
      ~sc_rollup_node
      ~sc_rollup_address
      ~client
      ()
  in

  (* Verify that the service provider's balance increased by 50 tez after the
     fast withdrawal payout. *)
  let* final_service_provider_balance =
    Client.ticket_balance
      ~contract:service_provider_pkh
      ~ticketer:l1_contracts.ticket_router_tester
      ~content_type:"pair nat (option bytes)"
      ~content:"Pair 0 None"
      client
  in
  Check.(
    (Tez.(initial_service_provider_balance + of_int withdraw_amount)
    = Tez.of_int (int_of_string (String.trim final_service_provider_balance)))
      Tez.typ)
    ~error_msg:
      "After outbox message execution we expect %L ticket balance for the \
       receiver, got %R" ;
  Lwt.return_unit

let test_trace_call =
  register_all
    ~__FILE__
    ~kernels:Kernel.etherlink_all
    ~tags:["evm"; "rpc"; "trace"; "call"]
    ~title:"Sequencer can run debug_traceCall and return a valid log"
    ~da_fee:Wei.zero
  @@ fun {sc_rollup_node; sequencer; client; evm_version; _} _protocol ->
  (* Start a RPC node as well, since we will want to check it returns the
     same result as the sequencer *)
  let* rpc_node = run_new_rpc_endpoint sequencer in
  (* Transfer funds to a random address. *)
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* simple_storage_resolved =
    Solidity_contracts.simple_storage evm_version
  in

  (* deploy contract *)
  let* () =
    Eth_cli.add_abi
      ~label:simple_storage_resolved.label
      ~abi:simple_storage_resolved.abi
      ()
  in
  let* contract_address, _tx_deployment =
    send_transaction_to_sequencer
      (Eth_cli.deploy
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
      ()
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
  let* () =
    Evm_node.patch_kernel sequencer Uses.(path Constant.WASM.mainnet_kernel)
  in
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

  (* Ensure blueprints are finalized by baking two more blocks *)
  let* _l1_lvl =
    repeat 2 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in

  (* Check the finalized blueprint is indeed the expected one *)
  let*@ sequencer_finalized_head =
    Rpc.get_block_by_number ~block:"finalized" sequencer
  in
  Check.(
    (sequencer_finalized_head.number = target_finalized)
      int32
      ~error_msg:"Expected sequencer finalized level %R, got %L") ;

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

let test_regression_block_hash_gen =
  (* This test is created because of bug in blockConstant in simulation,
     which caused the simulation to return a wrong estimate of gas limit,
     leading to failed contract deployment for block_hash_gen.
     This test checks regression for the fix *)
  let timestamp = "2020-01-01T00:00:05Z" in
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~tags:["evm"; "l2_call"; "block_hash"; "timestamp"]
    ~title:"Random generation based on block hash and timestamp"
    ~genesis_timestamp:Client.(At (Time.of_notation_exn timestamp))
  @@ fun {sequencer; evm_version; _} _protocol ->
  let* () =
    repeat 3 (fun _ ->
        let*@ _ = produce_block sequencer ~timestamp in
        unit)
  in
  let* {abi; bin; _} = Solidity_contracts.block_hash_gen evm_version in
  let* _contract, _tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.deploy
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

let test_batch_limit_size_rpc =
  register_all
    ~__FILE__
    ~eth_bootstrap_accounts:Eth_account.lots_of_address
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
    ~title:"Test batch size limit"
    ~tags:["rpc"; "batch_limit"]
    ~time_between_blocks:Nothing
    ~use_multichain:Register_without_feature
  (* TODO #7843: Adapt this test to multichain context *)
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
    ~__FILE__
    ~eth_bootstrap_accounts:Eth_account.lots_of_address
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
    ~time_between_blocks:Nothing
    ~kernels:[Latest]
    ~tags:["evm"; "rpc"; "relay"]
    ~title:"Relay restricted RPC only accept eth_sendRawTransaction"
    ~da_fee:Wei.zero
    ~use_multichain:Register_without_feature
  (* TODO #7843: Adapt this test to multichain context *)
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

let test_da_fees_after_execution =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~tags:
      [
        "evm";
        "da_fees";
        (* Those tests take about 13 minutes in the CI. *)
        Tag.slow;
      ]
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
  let* {sequencer; observer; _} =
    Setup.setup_sequencer
      ~mainnet_compat:false
      ~enable_dal:false
      ~enable_multichain:false
      protocol
  in
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

  Regression.capture (JSON.encode (remove_public_rpc_port @@ sequencer_config)) ;
  Regression.capture (JSON.encode (remove_public_rpc_port @@ observer_config)) ;

  unit

let test_produce_block_with_no_delayed_transactions =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~tags:["evm"; "delayed_transaction"]
    ~title:"Produce block with no delayed transactions"
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
  (* Send a random transaction in the delayed inbox. *)
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
  (* Finalize the transaction so the sequencer sees the event. *)
  let* l1_level = Client.level client in
  let wait_for =
    Evm_node.wait_for_processed_l1_level ~level:l1_level sequencer
  in
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* _ = wait_for in

  let*@ n = Rpc.produce_block ~with_delayed_transactions:false sequencer in
  Check.((n = 0) int) ~error_msg:"Block should be empty but got %L transactions" ;
  let*@ n = Rpc.produce_block sequencer in
  Check.((n = 1) int) ~error_msg:"Block should have one transaction but got %L" ;

  unit

let test_websocket_rpcs =
  register_all
    ~__FILE__
    ~tags:["evm"; "rpc"; "websocket"]
    ~title:"RPC methods over websocket"
    ~time_between_blocks:Nothing
    ~eth_bootstrap_accounts:
      ((Array.to_list Eth_account.bootstrap_accounts
       |> List.map (fun a -> a.Eth_account.address))
      @ Eth_account.lots_of_address)
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
    ~websockets:true
  @@ fun {sequencer; _} _protocol ->
  Log.info "Opening a websocket connection with the node" ;
  let* websocket = Evm_node.open_websocket sequencer in
  Log.info "getBalance" ;
  let*@ balance =
    Rpc.get_balance
      ~websocket
      ~address:Eth_account.bootstrap_accounts.(0).address
      sequencer
  in
  Check.((balance = default_bootstrap_account_balance) Wei.typ)
    ~error_msg:
      (sf
         "Expected balance of %s should be %%R, but got %%L"
         Eth_account.bootstrap_accounts.(0).address) ;
  Log.info "getBlockByNumber" ;
  let*@ block = Rpc.get_block_by_number ~websocket ~block:"0" sequencer in
  Check.((block.number = 0l) int32)
    ~error_msg:"Unexpected block number, should be %%R, but got %%L" ;
  Log.info "getBlockByHash" ;
  let* block' =
    let* block =
      Evm_node.(
        jsonrpc
          ~websocket
          sequencer
          {
            method_ = "eth_getBlockByHash";
            parameters = `A [`String block.hash; `Bool false];
          })
    in
    return @@ (block |> Evm_node.extract_result |> Block.of_json)
  in
  assert (block = block') ;
  Log.info "blockNumber" ;
  let* () =
    repeat 2 (fun () ->
        let*@ _ = produce_block sequencer in
        unit)
  in
  let*@ block_number = Rpc.block_number ~websocket sequencer in
  Check.((block_number = 2l) int32)
    ~error_msg:"Expected a block number of %R, but got %L" ;
  Log.info "getTransactionCount" ;
  let*@ transaction_count =
    Rpc.get_transaction_count
      ~websocket
      ~address:Eth_account.bootstrap_accounts.(0).address
      sequencer
  in
  Check.((transaction_count = 0L) int64)
    ~error_msg:"Expected a nonce of %R, but got %L" ;
  Log.info "netVersion" ;
  let*@ net_version = Rpc.net_version ~websocket sequencer in
  Check.((net_version = "1337") string)
    ~error_msg:"Expected net_version is %R, but got %L" ;
  Log.info "coinbase" ;
  let*@ coinbase = Rpc.coinbase ~websocket sequencer in
  Check.((coinbase = "0x0000000000000000000000000000000000000000") string)
    ~error_msg:"eth_coinbase returned %L, expected %R" ;
  unit

let test_websocket_subscription_rpcs_cant_be_called_via_http_requests =
  register_all
    ~__FILE__
    ~tags:["evm"; "rpc"; "websocket"; "http"]
    ~title:
      "Check that subscriptions rpcs can't be called via regular http requests"
    ~time_between_blocks:Nothing
    ~eth_bootstrap_accounts:
      ((Array.to_list Eth_account.bootstrap_accounts
       |> List.map (fun a -> a.Eth_account.address))
      @ Eth_account.lots_of_address)
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
    ~websockets:true
  @@ fun {sequencer; _} _protocol ->
  let* () =
    Lwt.catch
      (fun () ->
        let* _ = Rpc.subscribe ~kind:NewHeads sequencer in
        failwith "eth_subscribe shouldn't be callable via http requests")
      (fun _ -> unit)
  in
  Lwt.catch
    (fun () ->
      let* _ = Rpc.unsubscribe ~id:"0x0" sequencer in
      failwith "eth_unsubscribe shouldn't be callable via http requests")
    (fun _ -> unit)

let produce_block_and_wait_for_sync ~sequencer evm_node =
  let*@ head = Rpc.block_number sequencer in
  let level = Int32.(succ head |> to_int) in
  if Evm_node.can_apply_blueprint evm_node then
    let wait_for =
      Evm_node.wait_for_blueprint_applied evm_node Int32.(succ head |> to_int)
    in
    let*@ _ = produce_block sequencer in
    let* () = wait_for in
    return level
  else
    let*@ _ = produce_block sequencer in
    return level

let check_unsubscription ~websocket ~id ~sequencer evm_node =
  let* sub_status = Rpc.unsubscribe ~websocket ~id evm_node in
  Check.((sub_status = true) bool)
    ~error_msg:"Unsubscription should be successful" ;
  (* After unsubbing to the event, we shouldn't receive data anymore. *)
  let* _ = produce_block_and_wait_for_sync ~sequencer evm_node in
  let* result =
    Lwt.pick
      [
        (let* never_return = Websocket.recv websocket in
         return (Error never_return));
        (let* () = Lwt_unix.sleep 2. in
         return (Ok ()));
      ]
  in
  (match result with
  | Ok () -> ()
  | Error _ -> failwith "The websocket shouldn't have received any new data.") ;
  (* If we try to unsubscribe from this event again, it should return false as
     we were already unsubbed. *)
  let* sub_status = Rpc.unsubscribe ~websocket ~id evm_node in
  Check.((sub_status = false) bool)
    ~error_msg:"Unsubscribing from the same event twice should return false" ;
  unit

let test_websocket_newHeads_event =
  register_all
    ~__FILE__
    ~tags:["evm"; "rpc"; "websocket"; "new_heads"]
    ~title:"Check that websocket event `newHeads` is behaving correctly"
    ~time_between_blocks:Nothing
    ~eth_bootstrap_accounts:
      ((Array.to_list Eth_account.bootstrap_accounts
       |> List.map (fun a -> a.Eth_account.address))
      @ Eth_account.lots_of_address)
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
    ~websockets:true
    ~use_multichain:
      (* TODO #7843: Adapt this test to multichain context *)
      Register_without_feature
  @@ fun {sequencer; observer; _} _protocol ->
  let scenario evm_node nb_websockets =
    let* websockets =
      Lwt_list.map_p
        (fun () ->
          let* websocket = Evm_node.open_websocket evm_node in
          let* id = Rpc.subscribe ~websocket ~kind:NewHeads evm_node in
          return (websocket, id))
        (List.init nb_websockets (fun _ -> ()))
    in
    let check_block_number () =
      (* We always wait for the observer to be synced, to know that the
         blueprint was fully propagated. *)
      let* level = produce_block_and_wait_for_sync ~sequencer observer in
      Lwt_list.iter_s
        (fun (websocket, _id) ->
          let* block = Websocket.recv ~timeout:10. websocket in
          let Block.{number; _} =
            JSON.(block |-> "params" |-> "result" |> Block.of_json)
          in
          Check.((number = Int32.of_int level) int32)
            ~error_msg:"Received block level was %L, expected %R" ;
          unit)
        websockets
    in
    let* () = repeat 2 check_block_number in
    Lwt_list.iter_p
      (fun (websocket, id) ->
        check_unsubscription ~websocket ~id ~sequencer evm_node)
      websockets
  in
  let* rpc_node = run_new_rpc_endpoint sequencer in
  let* () = scenario sequencer 3 in
  let* () = scenario observer 3 in
  let* () = scenario rpc_node 3 in
  unit

(* This test is flaky because Dream may not correctly detect websocket
   connections closed by the client. *)
let test_websocket_cleanup =
  register_all
    ~__FILE__
    ~tags:["evm"; "rpc"; "websocket"; "cleanup"]
    ~title:"Check that websocket subscriptions are cleaned up on close"
    ~time_between_blocks:Nothing
    ~eth_bootstrap_accounts:
      ((Array.to_list Eth_account.bootstrap_accounts
       |> List.map (fun a -> a.Eth_account.address))
      @ Eth_account.lots_of_address)
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
    ~rpc_server:Resto (* Websockets disconnection is flaky in Dream *)
    ~websockets:true
  @@ fun {sequencer; _} _protocol ->
  let* websocket = Evm_node.open_websocket sequencer in
  let* id1 = Rpc.subscribe ~websocket ~kind:NewHeads sequencer in
  let* id2 = Rpc.subscribe ~websocket ~kind:NewHeads sequencer in
  let*@ _ = produce_block sequencer in
  let*@ _ = produce_block sequencer in
  Log.info "Closing websocket" ;
  let* () = Websocket.close websocket in
  let* () = Lwt_unix.sleep 1. in
  Log.info "New websocket connection" ;
  let* websocket = Evm_node.open_websocket sequencer in
  let* sub_status = Rpc.unsubscribe ~websocket ~id:id2 sequencer in
  Check.((sub_status = false) bool)
    ~error_msg:"Subscription should have been cleaned up from node" ;
  let* sub_status = Rpc.unsubscribe ~websocket ~id:id1 sequencer in
  Check.((sub_status = false) bool)
    ~error_msg:"All subscriptions should have been cleaned up from node" ;
  unit

let test_websocket_max_message_length () =
  let max_message_length = 512 in
  let patch_config json =
    json
    |> Evm_node.patch_config_with_experimental_feature
         ~rpc_server:Resto (* The limit is not implemented for Dream *)
         ()
    |> Evm_node.patch_config_websockets_if_enabled ~max_message_length
  in
  register_sandbox
    ~__FILE__
    ~tags:["evm"; "rpc"; "websocket"; "max"]
    ~title:"Websocket server does not accept messages larger than maximum"
    ~patch_config
    ~websockets:true
  @@ fun sequencer ->
  let* websocket = Evm_node.open_websocket sequencer in
  let shutdown =
    Lwt.pick
      [
        ( Evm_node.wait_for sequencer "websocket_shutdown.v0" @@ fun json ->
          Some JSON.(json |-> "reason" |> as_string) );
        (let* () = Lwt_unix.sleep 5. in
         Test.fail ~__LOC__ "Websocket worker did not close.");
      ]
  in
  let* () =
    Websocket.send_raw websocket (String.make (max_message_length + 1) '\000')
  in
  let* reason = shutdown in
  Log.info "Websocket shutdown reason: %S." reason ;
  Check.(reason =~ rex "too big")
    ~error_msg:"Expected reason to match %R, got %L" ;
  Lwt.catch
    (fun () ->
      let* _ = Websocket.recv ~timeout:10. websocket in
      Test.fail ~__LOC__ "Connection was not closed.")
    (function
      | Websocket.Connection_closed ->
          Log.info "Connection was closed on client end." ;
          unit
      | e -> Lwt.reraise e)

let test_websocket_rate_limit strategy =
  let max_messages = 5 in
  let interval = 3 in
  let strategy_str =
    match strategy with
    | `Wait -> "wait"
    | `Error -> "error"
    | `Close -> "close"
  in
  let patch_config json =
    json
    |> Evm_node.patch_config_with_experimental_feature
         ~rpc_server:Resto (* The limit is not implemented for Dream *)
         ()
    |> Evm_node.patch_config_websockets_if_enabled
         ~monitor_heartbeat:false (* To not count ping/pong frames *)
         ~rate_limit:
           (`O
              [
                ("max_messages", `Float (float_of_int max_messages));
                ("interval", `Float (float_of_int interval));
                ("strategy", `String strategy_str);
              ])
  in
  register_sandbox
    ~__FILE__
    ~tags:["evm"; "rpc"; "websocket"; "rate_limit"; strategy_str]
    ~title:(sf "Websocket server limits rate of messages (%s)" strategy_str)
    ~patch_config
    ~websockets:true
  @@ fun sequencer ->
  let* websocket = Evm_node.open_websocket sequencer in
  let*@ _ = produce_block sequencer in
  Log.info "Send messages below limit" ;
  let send_requests n =
    List.init n Fun.id
    |> Lwt_list.map_s (fun _ ->
           Lwt.catch
             (fun () ->
               Rpc.block_number ~websocket sequencer
               |> Lwt_result.map_error (fun e -> `Rpc_error e))
             (fun e -> Lwt.return_error (`Exn e)))
  in
  let* responses = send_requests max_messages in
  List.iteri
    (fun i -> function
      | Ok _ -> ()
      | Error (`Rpc_error ({code; message; _} : Rpc.error)) ->
          Test.fail
            ~__LOC__
            "Websocket message %i resulted in error %d: %s"
            i
            code
            message
      | Error (`Exn e) -> raise e)
    responses ;
  Log.info "Wait for rate limit interval" ;
  let* () = Lwt_unix.sleep (float_of_int interval) in
  let ws_server_shutdown =
    Evm_node.wait_for sequencer "websocket_shutdown.v0" @@ fun json ->
    Some JSON.(json |-> "reason" |> as_string)
  in
  let t1 = Unix.gettimeofday () in
  Log.info "Sending messages above limit" ;
  let* responses = send_requests (max_messages + 1) in
  let t2 = Unix.gettimeofday () in
  let handle_time = t2 -. t1 in
  let errors =
    List.filter_map (function Ok _ -> None | Error e -> Some e) responses
  in
  match (strategy, errors) with
  | `Error, [] ->
      Test.fail ~__LOC__ "Websocket server should have rate limited a message"
  | `Error, _ :: _ :: _ ->
      Test.fail
        ~__LOC__
        "Websocket server should have rate limited only one message"
  | `Error, [`Rpc_error {code; message; _}] ->
      Check.((code = -32005) int)
        ~error_msg:"Rate limit error should have code %R but was %L" ;
      Check.(message =~ rex "Rate limited by messages on websocket")
        ~error_msg:"Expected message to match %R, got %L" ;
      unit
  | `Wait, _ :: _ ->
      Test.fail
        ~__LOC__
        "Websocket server should have blocked in rate limit instead of error"
  | `Wait, [] ->
      Check.((handle_time > float_of_int interval) float)
        ~error_msg:"Rate limit blocked for %L but should block for %R" ;
      unit
  | `Close, _ ->
      let* reason =
        Lwt.pick
          [
            ws_server_shutdown;
            (let* () = Lwt_unix.sleep 5. in
             Test.fail
               ~__LOC__
               "Websocket worker should have closed on rate limit.");
          ]
      in
      Check.(reason =~ rex "Rate limited by messages on websocket")
        ~error_msg:"Expected close reason to match %R, got %L" ;
      unit
  | _, [`Exn e] -> raise e

let test_websocket_frames_rate_limit () =
  let max_frames = 100 in
  let interval = 3 in
  let patch_config json =
    json
    |> Evm_node.patch_config_with_experimental_feature
         ~rpc_server:Resto (* The limit is not implemented for Dream *)
         ()
    |> Evm_node.patch_config_websockets_if_enabled
         ~monitor_heartbeat:false (* To not count ping/pong frames *)
         ~rate_limit:
           (`O
              [
                ("max_frames", `Float (float_of_int max_frames));
                ("interval", `Float (float_of_int interval));
              ])
  in

  register_sandbox
    ~__FILE__
    ~tags:["evm"; "rpc"; "websocket"; "rate_limit"; "frames"]
    ~title:"Websocket server limits rate of frames"
    ~patch_config
    ~websockets:true
  @@ fun sequencer ->
  let* websocket = Evm_node.open_websocket sequencer in
  let*@ _ = produce_block sequencer in
  let send_frames n =
    List.init n Fun.id
    |> Lwt_list.iter_p (fun _ -> Websocket.send_raw websocket "")
  in
  let ws_server_shutdown =
    Evm_node.wait_for sequencer "websocket_shutdown.v0" @@ fun json ->
    Some JSON.(json |-> "reason" |> as_string)
  in
  Log.info "Send frames below limit" ;
  let* () = send_frames max_frames in
  let* () =
    Lwt.choose
      [
        (let* reason = ws_server_shutdown in
         Test.fail ~__LOC__ "Websocket connection closed because %s." reason);
        Lwt_unix.sleep 1.;
      ]
  in
  Log.info "Wait for rate limit interval" ;
  let* () = Lwt_unix.sleep (float_of_int interval) in
  let* () =
    Lwt.catch
      (fun () -> send_frames (max_frames + 1))
      (function Websocket.Connection_closed -> unit | e -> raise e)
  in
  let* reason =
    Lwt.pick
      [
        ws_server_shutdown;
        (let* () = Lwt_unix.sleep 5. in
         Test.fail ~__LOC__ "Websocket worker did not close.");
      ]
  in
  Log.info "Websocket shutdown reason: %S." reason ;
  Check.(reason =~ rex "Rate limited by frames on websocket")
    ~error_msg:"Expected close reason to match %R, got %L" ;
  unit

let test_websocket_heartbeat_monitoring () =
  let patch_config json =
    json
    |> Evm_node.patch_config_with_experimental_feature
         ~rpc_server:Resto (* Monitoring is not implemented for Dream *)
         ()
    |> Evm_node.patch_config_websockets_if_enabled ~monitor_heartbeat:true
  in
  register_sandbox
    ~__FILE__
    ~tags:["evm"; "rpc"; "websocket"; "monitoring"]
    ~title:"Websocket server closes connection when client unresponsive"
    ~patch_config
    ~websockets:true
  @@ fun sequencer ->
  let ws_connection_established =
    Evm_node.wait_for sequencer "websocket_starting.v0" @@ fun _json -> Some ()
  in

  let* websocket = Evm_node.open_websocket sequencer in
  let shutdown =
    Lwt.pick
      [
        ( Evm_node.wait_for sequencer "websocket_shutdown.v0" @@ fun json ->
          Some JSON.(json |-> "reason" |> as_string) );
        (let* () = Lwt_unix.sleep 5. in
         Test.fail ~__LOC__ "Websocket worker did not close.");
      ]
  in
  Log.info "Wait for websocket client to be connected" ;
  let* () = ws_connection_established in
  Log.info "Pausing websocket client" ;
  Websocket.pause websocket ;
  let* reason = shutdown in
  Log.info "Websocket shutdown reason: %S." reason ;
  Check.(reason =~ rex "Timeout")
    ~error_msg:"Expected reason to match %R, got %L" ;
  unit

let test_websocket_newPendingTransactions_event =
  register_all
    ~__FILE__
    ~tags:["evm"; "rpc"; "websocket"; "new_pending_transactions"]
    ~title:
      "Check that websocket event `newPendingTransactions` is behaving \
       correctly"
    ~time_between_blocks:Nothing
    ~eth_bootstrap_accounts:
      ((Array.to_list Eth_account.bootstrap_accounts
       |> List.map (fun a -> a.Eth_account.address))
      @ Eth_account.lots_of_address)
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
    ~websockets:true
    ~use_multichain:
      (* TODO #7843: Adapt this test to multichain context *)
      Register_without_feature
  @@ fun {sequencer; sc_rollup_node; _} _protocol ->
  let* observer =
    run_new_observer_node
      ~finalized_view:false
      ~sc_rollup_node:(Some sc_rollup_node)
      ~websockets:true
      sequencer
  in
  let* rpc_node = run_new_rpc_endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let scenario evm_node transaction_hash =
    let* websocket = Evm_node.open_websocket evm_node in
    let* id = Rpc.subscribe ~websocket ~kind:NewPendingTransactions evm_node in
    (* In observer mode, the transaction will never be mined which will cause
       the following lines of code to be blocking hence the [Background.register]. *)
    Background.register
      (Lwt.catch
         (fun () ->
           let _ =
             Eth_cli.transaction_send
               ~source_private_key:sender.private_key
               ~to_public_key:sender.address
               ~value:(Wei.of_eth_int 10)
               ~endpoint:(Evm_node.endpoint evm_node)
               ()
           in
           unit)
         (fun exn ->
           Log.debug "TX send failed because %s" (Printexc.to_string exn) ;
           unit)) ;
    let* tx = Websocket.recv ~timeout:10. websocket in
    let tx_hash = JSON.(tx |-> "params" |-> "result" |> as_string) in
    Check.((transaction_hash = tx_hash) string)
      ~error_msg:"Received tx_hash was %R, expected %L" ;
    check_unsubscription ~websocket ~id ~sequencer evm_node
  in
  Log.info "Scenario with sequencer" ;
  let* () =
    scenario
      sequencer
      "0x1b5678a27af55582f2bd6fa07223ff59ee93e16c228e40a948d09ee593560d36"
  in
  Log.info "Scenario with observer" ;
  let* () =
    scenario
      observer
      "0xf5e6dcb59cbf260cfe04d89d07a0f270c11e489a6de4df319916c7ddb19f3a34"
  in
  let* _ = produce_block_and_wait_for_sync ~sequencer observer in
  Log.info "Scenario with RPC node" ;
  let* () =
    scenario
      rpc_node
      "0x10318840345abfa7d61f34ce6b09061176f375062802646f98dc2e88f0639bdf"
  in
  unit

let test_websocket_logs_event =
  register_all
    ~__FILE__
    ~tags:["evm"; "rpc"; "websocket"; "logs"]
    ~title:"Check that websocket event `logs` is behaving correctly"
    ~time_between_blocks:Nothing
    ~eth_bootstrap_accounts:
      ((Array.to_list Eth_account.bootstrap_accounts
       |> List.map (fun a -> a.Eth_account.address))
      @ Eth_account.lots_of_address)
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
    ~websockets:true
    ~use_multichain:
      (* TODO #7843: Adapt this test to multichain context *)
      Register_without_feature
  @@ fun {sequencer; observer; evm_version; _} _protocol ->
  let scenario evm_node =
    let* websocket = Evm_node.open_websocket evm_node in
    let endpoint = Evm_node.endpoint evm_node in
    let sender = Eth_account.bootstrap_accounts.(0) in
    let* erc20 = Solidity_contracts.erc20 evm_version in
    let* () = Eth_cli.add_abi ~label:erc20.label ~abi:erc20.abi () in
    let* address, _tx =
      send_transaction_to_sequencer
        (Eth_cli.deploy
           ~source_private_key:sender.private_key
           ~endpoint
           ~abi:erc20.label
           ~bin:erc20.bin)
        sequencer
    in
    let address = String.lowercase_ascii address in
    let transfer_event_topic =
      let h =
        Tezos_crypto.Hacl.Hash.Keccak_256.digest
          (Bytes.of_string "Transfer(address,address,uint256)")
      in
      "0x" ^ Hex.show (Hex.of_bytes h)
    in
    let zero_address = "0x" ^ String.make 64 '0' in
    let value = 42 in
    let* id_all_logs =
      Rpc.subscribe
        ~websocket
        ~kind:(Logs (Some {address = Some (Single address); topics = None}))
        evm_node
    in
    let* _tx =
      send_transaction_to_sequencer
        (Eth_cli.contract_send
           ~source_private_key:sender.private_key
           ~endpoint
           ~abi_label:erc20.label
           ~address
           ~method_call:(Printf.sprintf "mint(%d)" value))
        sequencer
    in
    let* json = Websocket.recv ~timeout:10. websocket in
    let logs =
      JSON.(json |-> "params" |-> "result" |> Transaction.logs_of_json)
    in
    Check.((address = logs.address) string)
      ~error_msg:"Received logs address was %R, expected %L" ;
    Check.(
      ("0x000000000000000000000000000000000000000000000000000000000000002a"
     = logs.data)
        string)
      ~error_msg:"Received logs data was %R, expected %L" ;
    let* sub_status = Rpc.unsubscribe ~websocket ~id:id_all_logs evm_node in
    Check.((sub_status = true) bool)
      ~error_msg:"Unsubscription from logs should be successful" ;
    let* _id_specific_topics =
      Rpc.subscribe
        ~websocket
        ~kind:
          (Logs
             (Some
                {
                  address = None;
                  topics =
                    Some
                      [
                        transfer_event_topic;
                        hex_256_of_address sender;
                        zero_address;
                      ];
                }))
        evm_node
    in
    let* _tx =
      send_transaction_to_sequencer
        (Eth_cli.contract_send
           ~expect_failure:false
           ~source_private_key:sender.private_key
           ~endpoint
           ~abi_label:erc20.label
           ~address
           ~method_call:(Printf.sprintf "burn(%d)" value))
        sequencer
    in
    let* json = Websocket.recv ~timeout:10. websocket in
    let sender_burn_logs =
      JSON.(json |-> "params" |-> "result" |> Transaction.logs_of_json)
    in
    Check.((address = logs.address) string)
      ~error_msg:"Received logs address was %R, expected %L" ;
    Check.(("0x" ^ hex_256_of_int value = logs.data) string)
      ~error_msg:"Received logs data was %R, expected %L" ;
    Check.(
      (sender_burn_logs.topics
      = [transfer_event_topic; hex_256_of_address sender; zero_address])
        (list string))
      ~error_msg:"Expected topics %R, got %L" ;
    unit
  in
  let* rpc_node = run_new_rpc_endpoint sequencer in
  let* () = scenario sequencer in
  let* () = scenario observer in
  let* () = scenario rpc_node in
  unit

(* TODO #7843: Adapt this test to multichain context *)
let test_websocket_tez_newIncludedTransactions_event =
  register_test
    ~__FILE__
    ~time_between_blocks:Nothing
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
    ~eth_bootstrap_accounts:
      ((Array.to_list Eth_account.bootstrap_accounts
       |> List.map (fun a -> a.Eth_account.address))
      @ Eth_account.lots_of_address)
    ~tags:["evm"; "rpc"; "websocket"; "tez_new_included_transactions"]
    ~title:
      "Check that websocket event `newIncludedTransactions` is behaving \
       correctly"
    ~kernel:Latest
    ~websockets:true
    ~enable_dal:false
    ~enable_multichain:false
  @@ fun {sequencer; observer; _} _protocol ->
  (*
    To avoid updating all the tooling.
    This experimental feature will be deprecated in the short future.
  *)
  let patch_config =
    Evm_node.patch_config_with_experimental_feature
      ~preconfirmation_stream_enabled:true
      ()
  in
  let* () = Evm_node.terminate sequencer in
  let* () = Evm_node.terminate observer in
  let* () = Evm_node.Config_file.update sequencer patch_config in
  let* () = Evm_node.Config_file.update observer patch_config in
  let p = Evm_node.wait_for_start_history_mode observer in
  let* () = Evm_node.run sequencer in
  let* () = Evm_node.run observer in
  let* _ = p in
  let* _res = produce_block sequencer in
  let* rpc_node = run_new_rpc_endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let scenario evm_node transaction_hash =
    let* websocket = Evm_node.open_websocket evm_node in
    let* id = Rpc.subscribe ~websocket ~kind:NewIncludedTransactions evm_node in
    (* In observer mode, the transaction will never be mined which will cause
       the following lines of code to be blocking hence the [Background.register]. *)
    Background.register
      (Lwt.catch
         (fun () ->
           let _ =
             Eth_cli.transaction_send
               ~source_private_key:sender.private_key
               ~to_public_key:sender.address
               ~value:(Wei.of_eth_int 10)
               ~endpoint:(Evm_node.endpoint evm_node)
               ()
           in
           unit)
         (fun exn ->
           Log.debug "TX send failed because %s" (Printexc.to_string exn) ;
           unit)) ;
    let* tx = Websocket.recv ~timeout:10. websocket in
    let tx =
      JSON.(tx |-> "params" |-> "result" |> transaction_object_of_json)
    in
    Check.((transaction_hash = tx.hash) string)
      ~error_msg:"Received tx_hash was %R, expected %L" ;
    check_unsubscription ~websocket ~id ~sequencer evm_node
  in
  Log.info "Scenario with sequencer" ;
  let* () =
    scenario
      sequencer
      "0x1b5678a27af55582f2bd6fa07223ff59ee93e16c228e40a948d09ee593560d36"
  in
  Log.info "Scenario with observer" ;
  let* () =
    scenario
      observer
      "0xf5e6dcb59cbf260cfe04d89d07a0f270c11e489a6de4df319916c7ddb19f3a34"
  in
  let* _ = produce_block_and_wait_for_sync ~sequencer observer in
  Log.info "Scenario with RPC node" ;
  let* () =
    scenario
      rpc_node
      "0x10318840345abfa7d61f34ce6b09061176f375062802646f98dc2e88f0639bdf"
  in
  unit

(* TODO #7843: Adapt this test to multichain context *)
let test_websocket_tez_newPreconfirmedReceipts_event =
  register_test
    ~__FILE__
    ~time_between_blocks:Nothing
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
    ~eth_bootstrap_accounts:
      ((Array.to_list Eth_account.bootstrap_accounts
       |> List.map (fun a -> a.Eth_account.address))
      @ Eth_account.lots_of_address)
    ~tags:["evm"; "rpc"; "websocket"; "tez_new_preconfirmed_receipts"]
    ~title:
      "Check that websocket event `newPreconfirmedReceipts` is behaving \
       correctly"
    ~kernel:Latest
    ~websockets:true
    ~enable_dal:false
    ~enable_multichain:false
  @@ fun {sequencer; observer; _} _protocol ->
  (*
    To avoid updating all the tooling.
    This experimental feature will be deprecated in the short future.
  *)
  let patch_config =
    Evm_node.patch_config_with_experimental_feature
      ~preconfirmation_stream_enabled:true
      ()
  in
  let* () = Evm_node.terminate sequencer in
  let* () = Evm_node.terminate observer in
  let* () = Evm_node.Config_file.update sequencer patch_config in
  let* () = Evm_node.Config_file.update observer patch_config in
  let p = Evm_node.wait_for_start_history_mode observer in
  let* () = Evm_node.run sequencer in
  let* () = Evm_node.run observer in
  let* _ = p in
  let* _res = produce_block sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let scenario evm_node transaction_hash =
    let* websocket = Evm_node.open_websocket evm_node in
    let* id = Rpc.subscribe ~websocket ~kind:NewPreconfirmedReceipts evm_node in
    (* In observer mode, the transaction will never be mined which will cause
       the following lines of code to be blocking hence the [Background.register]. *)
    Background.register
      (Lwt.catch
         (fun () ->
           let* _ =
             Eth_cli.transaction_send
               ~source_private_key:sender.private_key
               ~to_public_key:sender.address
               ~value:(Wei.of_eth_int 10)
               ~endpoint:(Evm_node.endpoint evm_node)
               ()
           in
           unit)
         (fun exn ->
           Log.debug "TX send failed because %s" (Printexc.to_string exn) ;
           unit)) ;
    let* receipt = Websocket.recv ~timeout:10. websocket in
    let receipt =
      JSON.(
        receipt |-> "params" |-> "result"
        |> Transaction.transaction_receipt_of_json)
    in
    Check.((transaction_hash = receipt.transactionHash) string)
      ~error_msg:"Received tx_hash was %R, expected %L" ;
    check_unsubscription ~websocket ~id ~sequencer evm_node
  in
  Log.info "Scenario with observer" ;
  let* () =
    scenario
      observer
      "0x1b5678a27af55582f2bd6fa07223ff59ee93e16c228e40a948d09ee593560d36"
  in
  let* _ = produce_block_and_wait_for_sync ~sequencer observer in
  unit

let test_node_correctly_uses_batcher_heap =
  let max_blueprints_lag = 10 in
  let max_blueprints_catchup = 10 in
  let catchup_cooldown = 4 in
  register_test
    ~__FILE__
    ~kernel:Kernel.Latest
    ~enable_dal:false
    ~enable_multichain:false
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

let test_trace_empty_block =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "trace"; "block"; "empty"]
    ~title:"debug_traceBlockByNumber succeeds on empty block"
    ~kernels:Kernel.etherlink_all
  @@ fun {client; sc_rollup_node; sequencer; _} _protocol ->
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
  let*@ trace_result = Rpc.trace_block ~block:Rpc.Latest sequencer in
  Check.(
    (trace_result = [])
      (list json)
      ~error_msg:"Wrong trace, expected %R but got %L") ;
  unit

let test_trace_block_struct_logger =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "trace"; "block"; "empty"; "struct_logger"]
    ~title:"debug_traceBlockByNumber not implemented for struct logger"
    ~kernels:Kernel.etherlink_all
  @@ fun {client; sc_rollup_node; sequencer; _} _protocol ->
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
  let* trace_result =
    Rpc.trace_block ~tracer:"structLogger" ~block:Rpc.Latest sequencer
  in
  match trace_result with
  | Ok _ -> Test.fail "should have failed, structLogger not implemented"
  | Error {data; code; _} ->
      Check.((code = -32002) int ~error_msg:"Expected %R but got %L") ;
      Check.(
        (data = Some "structLogger")
          (option string)
          ~error_msg:"Error should be the name of the tracer") ;
      unit

let test_trace_block =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "trace"; "block"]
    ~title:"debug_traceBlockByNumber succeeds on non empty block"
    ~kernels:Kernel.etherlink_all
  @@ fun {client; sc_rollup_node; sequencer; evm_version; _} _protocol ->
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
  let endpoint = Evm_node.endpoint sequencer in
  let sender_0 = Eth_account.bootstrap_accounts.(0) in
  let sender_1 = Eth_account.bootstrap_accounts.(1) in
  let* call_types = Solidity_contracts.call_types evm_version in
  let* () = Eth_cli.add_abi ~label:call_types.label ~abi:call_types.abi () in
  let* address, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
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
      ~value:Wei.one
      ~address:sender_0.address
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
  let*@ trace_result = Rpc.trace_block ~block:Rpc.Latest sequencer in
  Check.(
    (List.length trace_result = 2)
      int
      ~error_msg:"Wrong nb of traces, expected %R but got %L") ;
  let t0 = List.nth trace_result 0 in
  let t1 = List.nth trace_result 1 in
  (* first tx is a contract call with internal calls *)
  Check.(
    JSON.(t0 |-> "txHash" |> as_string = transaction_hash_0)
      string
      ~error_msg:"Wrong hash, expected %R but got %L") ;
  Check.(
    JSON.(t0 |-> "result" |-> "calls" |> as_list <> [])
      (list json)
      ~error_msg:"Should have a list of subcalls") ;
  (* second tx is a simple transfer *)
  Check.(
    JSON.(t1 |-> "txHash" |> as_string = transaction_hash_1)
      string
      ~error_msg:"Wrong hash, expected %R but got %L") ;
  Check.(
    JSON.(t1 |-> "result" |-> "calls" |> as_list = [])
      (list json)
      ~error_msg:"Should not have a list of subcalls") ;
  unit

let test_trace_block_txs_same_caller =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "trace"; "block"]
    ~title:
      "debug_traceBlockByNumber succeeds on block with transactions using same \
       caller"
    ~kernels:Kernel.etherlink_all
  @@ fun {client; sc_rollup_node; sequencer; evm_version; _} _protocol ->
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
  let endpoint = Evm_node.endpoint sequencer in
  let sender_0 = Eth_account.bootstrap_accounts.(0) in
  let* call_types = Solidity_contracts.call_types evm_version in
  let* () = Eth_cli.add_abi ~label:call_types.label ~abi:call_types.abi () in
  let* address, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
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
      ~source_private_key:sender_0.private_key
      ~nonce:2
      ~gas_price:1_000_000_000
      ~gas:300_000
      ~value:Wei.one
      ~address:sender_0.address
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
  let*@ trace_result = Rpc.trace_block ~block:Rpc.Latest sequencer in
  Check.(
    (List.length trace_result = 2)
      int
      ~error_msg:"Wrong nb of traces, expected %R but got %L") ;
  let t0 = List.nth trace_result 0 in
  let t1 = List.nth trace_result 1 in
  (* first tx is a contract call with internal calls *)
  Check.(
    JSON.(t0 |-> "txHash" |> as_string = transaction_hash_0)
      string
      ~error_msg:"Wrong hash, expected %R but got %L") ;
  Check.(
    JSON.(t0 |-> "result" |-> "calls" |> as_list <> [])
      (list json)
      ~error_msg:"Should have a list of subcalls") ;
  (* second tx is a simple transfer *)
  Check.(
    JSON.(t1 |-> "txHash" |> as_string = transaction_hash_1)
      string
      ~error_msg:"Wrong hash, expected %R but got %L") ;
  Check.(
    JSON.(t1 |-> "result" |-> "calls" |> as_list = [])
      (list json)
      ~error_msg:"Should not have a list of subcalls") ;
  unit

let test_init_config_network network =
  Regression.register
    ~__FILE__
    ~title:(sf "EVM Node: init config --network %s" network)
    ~tags:["evm"; "init"; "config"]
    ~uses:[Constant.octez_evm_node]
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun () ->
  let data_dir = Temp.dir "fake-evm-node" in
  let config_file = data_dir // "config.json" in
  let* () =
    Process.check
    @@ Evm_node.spawn_init_config_minimal
         ~config_file
         ~extra_arguments:["--network"; network]
         ()
  in
  let config = read_file config_file in
  Regression.capture config ;
  unit

let test_filling_max_slots_cant_lead_to_out_of_memory =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "slots"; "out_of_memory"]
    ~title:
      "Calling a contract that fills the maximum amount of slots leads to \
       OutOfGas not an out of memory error"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* slot_filler = Solidity_contracts.slot_filler evm_version in
  let* () = Eth_cli.add_abi ~label:slot_filler.label ~abi:slot_filler.abi () in
  let* contract_address, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:sender.Eth_account.private_key
         ~endpoint
         ~abi:slot_filler.label
         ~bin:slot_filler.bin)
      sequencer
  in
  let* _ = produce_block sequencer in
  let* failure =
    Eth_cli.contract_send
      ~source_private_key:sender.private_key
      ~endpoint
      ~abi_label:slot_filler.label
      ~expect_failure:true
      ~address:contract_address
      ~method_call:"fillSlots()"
      ()
  in
  let contains s sub =
    let re = Str.regexp_string sub in
    try
      ignore (Str.search_forward re s 0) ;
      true
    with Not_found -> false
  in
  let error_out_of_gas = "OutOfGas(Basic)" in
  if contains failure error_out_of_gas then unit
  else Test.fail "Test should fail with error: %s" error_out_of_gas

let test_rpc_getLogs_with_earliest_fail =
  register_all
    ~__FILE__
    ~tags:["evm"; "rpc"; "get_logs"; "earliest"]
    ~title:"RPC method getLogs with earliest block"
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
  @@ fun {sequencer; evm_version; _} _protocol ->
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* erc20_resolved = Solidity_contracts.erc20 evm_version in
  let* () =
    Eth_cli.add_abi ~label:erc20_resolved.label ~abi:erc20_resolved.abi ()
  in
  let* contract, _tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi:erc20_resolved.abi
         ~bin:erc20_resolved.bin)
      sequencer
  in
  let call_mint n =
    Eth_cli.contract_send
      ~source_private_key:sender.private_key
      ~endpoint
      ~abi_label:erc20_resolved.label
      ~address:contract
      ~method_call:(Printf.sprintf "mint(%d)" n)
  in
  let nb_generated_logs = 5 in
  let* _ =
    repeat nb_generated_logs (fun _ ->
        let* _ =
          wait_for_application
            ~produce_block:(fun _ -> produce_block sequencer)
            (call_mint 42)
        in
        unit)
  in
  let*@ get_logs_using_number = Rpc.get_logs ~from_block:(Number 0) sequencer in
  let*@ get_logs_using_earliest = Rpc.get_logs ~from_block:Earliest sequencer in
  Check.(
    (List.length get_logs_using_number = nb_generated_logs)
      int
      ~error_msg:"Expected %R logs, got %L") ;
  Check.(
    (List.length get_logs_using_earliest = List.length get_logs_using_number)
      int
      ~error_msg:"Expected %R logs using Earliest as from_block, got %L") ;
  (* Check that when from > to we get an empty list *)
  let*@ empty_logs =
    Rpc.get_logs ~from_block:Latest ~to_block:Earliest sequencer
  in
  Check.((List.length empty_logs = 0) int) ~error_msg:"Expected %R logs, got %L" ;
  unit

let test_estimate_gas_with_block_param =
  register_all
    ~__FILE__
    ~tags:["evm"; "eth_estimategas"; "simulate"; "estimate_gas"; "earliest"]
    ~title:"eth_estimateGas with block parameter"
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* gas_consumer = Solidity_contracts.even_block_gas_consumer evm_version in
  let* () =
    Eth_cli.add_abi ~label:gas_consumer.label ~abi:gas_consumer.abi ()
  in
  let* address_contract, tx =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:sender.private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi:gas_consumer.label
         ~bin:gas_consumer.bin)
      sequencer
  in
  let* receipt =
    Eth_cli.get_receipt ~endpoint:(Evm_node.endpoint sequencer) ~tx ()
  in
  match receipt with
  | Some {blockNumber; _} when blockNumber = 1l ->
      let blockNumber = Int32.to_int blockNumber in
      let* data =
        Eth_cli.encode_method
          ~abi_label:gas_consumer.label
          ~method_:"consume()"
          ()
      in
      let call_params =
        [
          ("from", `String sender.address);
          ("to", `String address_contract);
          ("input", `String data);
        ]
      in
      let* _ = produce_block sequencer in
      (* Estimate_gas does its estimations on a block about to be minted so if we run it on block `N` the solidity code execute itself with block.number = `N`+ 1 *)
      let*@ evenGasCost =
        Rpc.estimate_gas call_params sequencer ~block:(Number blockNumber)
      in
      let*@ oddGasCost =
        Rpc.estimate_gas call_params sequencer ~block:(Number (blockNumber + 1))
      in
      Check.(
        (evenGasCost > oddGasCost)
          int64
          ~error_msg:
            "When calling for the contract EvenBlockGasConsumer.consume(), The \
             cost of gas should be lower in odd block numbers than in even \
             block numbers but got even = %L and odd = %R") ;
      unit
  | _ -> Test.fail "Test contract deployment failed"

let test_transaction_object expected_type_ name make_transaction =
  register_all
    ~__FILE__
    ~tags:[name; "transaction_object"]
    ~time_between_blocks:Nothing
    ~kernels:[Latest]
    ~use_dal:Register_without_feature
    ~title:
      (sf "RPC returns the correct transaction object for %s transactions" name)
  @@ fun {sequencer; _} _protocol ->
  let* hash =
    send_transaction_to_sequencer
      (fun () ->
        let* raw_tx = make_transaction () in

        let*@ hash = Rpc.send_raw_transaction ~raw_tx sequencer in
        return hash)
      sequencer
  in
  let* json =
    Evm_node.jsonrpc
      sequencer
      (Rpc.Request.eth_getTransactionByHash ~transaction_hash:hash)
  in
  let type_ = JSON.(json |-> "result" |-> "type" |> as_string) in
  Check.(
    (type_ = expected_type_)
      string
      ~error_msg:
        ("Type was expected to be %R (" ^ name ^ "), but the RPC returned %L")) ;
  unit

let test_eip2930_transaction_object =
  test_transaction_object
    "0x1"
    "eip2930"
    (Cast.craft_tx
       ~legacy:true
       ~access_list:
         [
           ( Eth_account.bootstrap_accounts.(0).address,
             ["0x" ^ String.make 64 '0'] );
         ]
       ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
       ~chain_id:1337
       ~nonce:0
       ~gas_price:1_000_000_000
         (* BASE_CALL_GAS_COST + 1xACCESS_LIST_ADDRESS_COST + 1xACCESS_LIST_STORAGE_KEY_COST
            = 21_000 + 2400 + 1900 = 25_300 *)
       ~gas:25_300
       ~value:Wei.zero
       ~address:Eth_account.bootstrap_accounts.(1).address)

let test_eip1559_transaction_object =
  test_transaction_object
    "0x2"
    "eip1559"
    (Cast.craft_tx
       ~legacy:false
       ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
       ~chain_id:1337
       ~nonce:0
       ~gas_price:1_000_000_000
       ~gas:23_300
       ~value:Wei.zero
       ~address:Eth_account.bootstrap_accounts.(1).address)

let test_apply_from_full_history_mode =
  register_all
    ~__FILE__
    ~genesis_timestamp
    ~time_between_blocks:Nothing
    ~tags:["evm"; "observer"]
    ~title:"Can apply blueprints from full mode"
  @@ fun {sequencer; observer; _} _protocol ->
  (* Stop the observer *)
  let* () = Evm_node.terminate observer in

  (* Restart sequencer in full mode *)
  let* () = Evm_node.terminate sequencer in
  let*! () = Evm_node.switch_history_mode sequencer (Full 3) in
  let wait_full =
    Evm_node.wait_for_start_history_mode ~history_mode:"full:3" sequencer
  in
  let* () = Evm_node.run sequencer and* _ = wait_full in

  (* Trigger gc twice with 9 blocks and confirm it *)
  let wait_gc = Evm_node.wait_for_gc_finished sequencer in
  let* _ =
    fold 9 () (fun i _ ->
        let* _ = Rpc.produce_block ~timestamp:(get_timestamp i) sequencer in
        Evm_node.wait_for_blueprint_injected sequencer i)
  and* _ = wait_gc in

  Log.info "first block in sequencer is accessible" ;
  let*@ _ = Rpc.get_block_by_number ~block:"1" sequencer in

  (* Start the observer and check it applied block 1  *)
  let* () = Evm_node.run observer in
  let* _ = Evm_node.wait_for_blueprint_applied observer 1 in

  Log.info "first block in observer after the catchup is accessible" ;
  let*@ _ = Rpc.get_block_by_number ~block:"1" observer in
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
    ~use_multichain:Register_without_feature
  (* TODO #7843: Adapt this test to multichain context *)
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
    ~use_multichain:Register_without_feature
  (* TODO #7843: Adapt this test to multichain context *)
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
    ~use_multichain:Register_without_feature
  (* TODO #7843: Adapt this test to multichain context *)
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
  @@
  fun {sequencer; sc_rollup_node; l2_chains; enable_multichain; _} _protocol ->
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
  let* () =
    match enable_multichain with
    | true ->
        let patch_config =
          Evm_node.patch_config_with_experimental_feature ~l2_chains ()
        in
        Evm_node.Config_file.update observer patch_config
    | false -> unit
  in
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
    ~use_multichain:Register_without_feature
  (* TODO #7843: Adapt this test to multichain context *)
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
    ~use_multichain:Register_without_feature
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

let get_one_receipt_from_latest_or_fail evm_node =
  let*@ block =
    Rpc.get_block_by_number ~full_tx_objects:false ~block:"latest" evm_node
  in
  match block.transactions with
  | Hash [tx_hash] -> (
      let*@ rpc_res = Rpc.get_transaction_receipt ~tx_hash evm_node in
      match rpc_res with
      | Some receipt -> return receipt
      | None -> Test.fail "Could not fetch the transaction receipt")
  | Hash _ | Empty ->
      Test.fail
        "Received a block with a transaction count different than the one \
         expected"
  | Full _ ->
      Test.fail
        "The EVM node returned the full transaction objects while it was \
         expected to return only the hashes"

let capture_logs ~header logs =
  Regression.capture (sf "# %s" header) ;
  List.iteri
    (fun i tx_log ->
      Regression.capture (sf "## Log %d" i) ;
      Regression.capture (sf "Address: %s" tx_log.Transaction.address) ;
      Regression.capture (sf "Topics:") ;
      List.iter (fun t -> Regression.capture (sf "- %s" t)) tx_log.topics ;
      Regression.capture (sf "Data: %s" tx_log.data))
    logs

(* {Note timestamp}

   Some events emitted by Etherlink contains timestamp. In order for the
   regression test to always generate the same trace, we control the timestamp
   of each L2 blocks using the {!timestamp_generator} helper. *)

let timestamp_generator () =
  let block_count = ref 0 in
  fun () ->
    let res =
      sf "2020-01-01T00:%02d:%02dZ" (!block_count / 60) (!block_count mod 60)
    in
    incr block_count ;
    res

let test_deposit_event =
  Protocol.register_regression_test
    ~__FILE__
    ~tags:["evm"; "deposit"; "event"]
    ~title:"Regression test for the deposit event"
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
      ])
  @@ fun protocol ->
  (* See {Note timestamp} *)
  let next_timestamp = timestamp_generator () in
  let genesis_timestamp = next_timestamp () in
  let* {sequencer; l1_contracts; sc_rollup_node; sc_rollup_address; client; _} =
    Setup.setup_sequencer
      ~genesis_timestamp:Client.(At (Time.of_notation_exn genesis_timestamp))
      ~time_between_blocks:Nothing
      ~mainnet_compat:false
      ~enable_dal:false
      ~enable_multichain:false
      protocol
  in
  (* Send a deposit to the delayed inbox *)
  let deposit_info =
    {
      receiver = EthereumAddr Eth_account.bootstrap_accounts.(0).address;
      chain_id = None;
    }
  in
  let* () =
    send_deposit_to_delayed_inbox
      ~amount:Tez.one
      ~bridge:l1_contracts.bridge
      ~depositor:Constant.bootstrap5
      ~deposit_info
      ~sc_rollup_node
      ~sc_rollup_address
      client
  in
  (* Bake two blocks to let the sequencer see the deposit *)
  let* () =
    repeat 2 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  (* Produce an Etherlink block *)
  let*@ nb_txns = produce_block ~timestamp:(next_timestamp ()) sequencer in
  Check.(
    (nb_txns = 1)
      int
      ~error_msg:
        "Expecting the block to contain %R transaction (the deposit), got %L") ;

  (* Fetch the deposit events *)
  let* receipt = get_one_receipt_from_latest_or_fail sequencer in

  (* Capture the log *)
  capture_logs ~header:"XTZ Deposit" receipt.logs ;

  unit

let test_withdrawal_events =
  Protocol.register_regression_test
    ~__FILE__
    ~tags:["evm"; "withdrawal"; "event"]
    ~title:"Regression test for the withdrawal events"
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
      ])
  @@ fun protocol ->
  (* See {Note timestamp} *)
  let next_timestamp = timestamp_generator () in
  let genesis_timestamp = next_timestamp () in
  let* {sequencer; _} =
    Setup.setup_sequencer
      ~genesis_timestamp:Client.(At (Time.of_notation_exn genesis_timestamp))
      ~time_between_blocks:Nothing
      ~mainnet_compat:false
      ~enable_dal:false
      ~enable_multichain:false
      ~enable_fast_withdrawal:true
      ~instant_confirmations:true
      protocol
  in

  (* Make a regular withdrawal *)
  let* () =
    Eth_cli.add_abi ~label:"withdraw" ~abi:(predep_xtz_bridge_abi_path ()) ()
  in
  let* _ =
    send_transaction_to_sequencer
      ~timestamp:(next_timestamp ())
      (Eth_cli.contract_send
         ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi_label:"withdraw"
         ~address:Solidity_contracts.Precompile.xtz_bridge
         ~method_call:
           (sf {|withdraw_base58("%s")|} Constant.bootstrap5.public_key_hash)
         ~value:Wei.one_eth
         ~gas:16_000_000)
      sequencer
  in

  (* Fetch the withdrawal event *)
  let* receipt = get_one_receipt_from_latest_or_fail sequencer in

  (* Capture the log *)
  capture_logs ~header:"XTZ Withdrawal" receipt.logs ;

  (* Do the same thing for fast withdrawal *)
  let* () =
    Eth_cli.add_abi
      ~label:"fast_withdraw_base58"
      ~abi:(predep_xtz_bridge_abi_path ())
      ()
  in
  let* _ =
    send_transaction_to_sequencer
      ~timestamp:(next_timestamp ())
      (Eth_cli.contract_send
         ~source_private_key:Eth_account.bootstrap_accounts.(1).private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi_label:"fast_withdraw_base58"
         ~address:Solidity_contracts.Precompile.xtz_bridge
         ~method_call:
           (sf
              {|fast_withdraw_base58("%s","%s","%s")|}
              Constant.bootstrap5.public_key_hash
              "KT1TczPwz5KjAuuJKvkTmttS7bBioT5gjQ4Y"
              "0x0000000000000000000000000000000000000000000000000000000000000001")
         ~value:Wei.one_eth
         ~gas:16_000_000)
      sequencer
  in

  (* Fetch the fast withdrawal event *)
  let* receipt = get_one_receipt_from_latest_or_fail sequencer in

  (* Capture the log *)
  capture_logs ~header:"XTZ Fast Withdrawal" receipt.logs ;

  unit

let test_fa_deposit_and_withdrawals_events =
  Protocol.register_regression_test
    ~__FILE__
    ~tags:["evm"; "fa_bridge"; "event"]
    ~title:"Regression test for the FA deposit and withdrawal events"
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
        Constant.octez_codec;
      ])
  @@ fun protocol ->
  (* See {Note timestamp} *)
  let next_timestamp = timestamp_generator () in
  let* {sequencer; l1_contracts; sc_rollup_node; sc_rollup_address; client; _} =
    let genesis_timestamp = next_timestamp () in
    Setup.setup_sequencer
      ~genesis_timestamp:Client.(At (Time.of_notation_exn genesis_timestamp))
      ~time_between_blocks:Nothing
      ~mainnet_compat:false
      ~enable_dal:false
      ~enable_multichain:false
      ~enable_fa_bridge:true
      ~enable_fast_fa_withdrawal:true
      ~instant_confirmations:true
      protocol
  in
  (* Send a FA deposit to the delayed inbox *)
  let* () =
    send_fa_deposit_to_delayed_inbox
      ~amount:42
      ~l1_contracts
      ~depositor:Constant.bootstrap5
      ~receiver:Eth_account.bootstrap_accounts.(0).address
      ~sc_rollup_node
      ~sc_rollup_address
      client
  in
  (* Bake two blocks to let the sequencer see the FA deposit *)
  let* () =
    (* Keep the following number >= 4, otherwise the test becomes flaky. *)
    repeat 4 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  (* Produce an Etherlink block *)
  let*@ nb_txns = produce_block ~timestamp:(next_timestamp ()) sequencer in
  Check.(
    (nb_txns = 1)
      int
      ~error_msg:
        "Expecting the block to contain %R transaction (the deposit), got %L") ;

  (* Fetch the deposit events *)
  let* receipt = get_one_receipt_from_latest_or_fail sequencer in

  (* Capture the log *)
  capture_logs ~header:"FA Deposit" receipt.logs ;

  (* Now, we can do a withdrawal *)
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
  let* _ =
    call_fa_withdraw
      ~timestamp:(next_timestamp ())
      ~sender:Eth_account.bootstrap_accounts.(0)
      ~ticket_owner:Eth_account.bootstrap_accounts.(0).address
      ~endpoint:(Evm_node.endpoint sequencer)
      ~evm_node:sequencer
      ~routing_info
      ~amount:40
      ~ticketer:(ticketer |> Hex.of_bytes |> Hex.show)
      ~content:(content |> Hex.of_bytes |> Hex.show)
      ()
  in

  (* Fetch the deposit events *)
  let* receipt = get_one_receipt_from_latest_or_fail sequencer in

  (* Capture the log *)
  capture_logs ~header:"FA Withdrawal" receipt.logs ;

  let* fast_withdrawal_contract_address =
    Client.originate_contract
      ~alias:"fast_withdrawal_contract_address"
      ~amount:Tez.zero
      ~src:Constant.bootstrap5.public_key_hash
      ~init:(sf "Pair %S {}" l1_contracts.exchanger)
      ~prg:(fast_withdrawal_path ())
      ~burn_cap:Tez.one
      client
  in
  let* () = Client.bake_for_and_wait ~keys:[] client in

  (* Withdrawing to the zero implicit account *)
  let* _ =
    call_fa_fast_withdraw
      ~timestamp:(next_timestamp ())
      ~sender:Eth_account.bootstrap_accounts.(0)
      ~sequencer
      ~ticket_owner:Eth_account.bootstrap_accounts.(0).address
      ~amount:2
      ~ticketer:(ticketer |> Hex.of_bytes |> Hex.show)
      ~content:(content |> Hex.of_bytes |> Hex.show)
      ~receiver:Constant.bootstrap5.public_key_hash
      ~fast_withdrawal_contract_address
      ()
  in

  (* Fetch the deposit events *)
  let* receipt = get_one_receipt_from_latest_or_fail sequencer in

  (* Capture the log *)
  capture_logs ~header:"FA Fast Withdrawal" receipt.logs ;
  unit

let test_block_producer_validation () =
  register_sandbox_with_observer
  (* Disable preconfirmations because transaction are rejected before the block production when IC are enabled *)
    ~patch_config:
      (Evm_node.patch_config_with_experimental_feature
         ~preconfirmation_stream_enabled:false
         ())
    ~__FILE__
    ~tags:["observer"; "tx_queue"; "validation"]
    ~title:"Test part of the validation is done when producing blocks."
  (* TODO #7843: Adapt this test to multichain context *)
  @@ fun {sandbox; observer} ->
  let send_and_wait_sequencer_receive ~raw_tx =
    let wait_sequencer_see_tx =
      Evm_node.wait_for_tx_queue_add_transaction sandbox
    in
    let* hash =
      let*@ hash = Rpc.send_raw_transaction ~raw_tx observer in
      return hash
    and* _ = wait_sequencer_see_tx in
    return hash
  in

  (* helper to craft a tx with given nonce. *)
  let gas_price = 1_000_000_000 in
  let gas = 23_000 in
  let gas_cost = Wei.of_eth_int (1 * 23) in
  let*@ balance_b0 =
    Rpc.get_balance ~address:Eth_account.bootstrap_accounts.(0).address observer
  in
  let*@ balance_b1 =
    Rpc.get_balance ~address:Eth_account.bootstrap_accounts.(1).address observer
  in

  let* raw_tx_empty_account_bO =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price
      ~gas
      ~value:Wei.(balance_b0 - gas_cost)
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  in
  let* raw_tx_empty_account_b1 =
    let*@ _ =
      Rpc.get_balance
        ~address:Eth_account.bootstrap_accounts.(1).address
        observer
    in
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(1).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price
      ~gas
      ~value:Wei.(balance_b0 + balance_b1 - (gas_cost * Z.of_int 2))
      ~address:Eth_account.bootstrap_accounts.(2).address
      ()
  in
  let* raw_tx_invalid_value =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:1
      ~gas_price
      ~gas
      ~value:(Wei.of_eth_int 100)
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  in
  let* raw_tx_invalid_nonce =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(1).private_key
      ~chain_id:1337
      ~nonce:10
      ~gas_price
      ~gas
      ~value:Wei.one
      ~address:Eth_account.bootstrap_accounts.(2).address
      ()
  in
  let* _hash =
    (* tx included *)
    send_and_wait_sequencer_receive ~raw_tx:raw_tx_empty_account_bO
  in
  let* invalid_balance_hash1 =
    send_and_wait_sequencer_receive ~raw_tx:raw_tx_invalid_value
  in
  let* _hash =
    (* The transaction is included because the balance of b1 is
       covered by the preceding transaction,
       `raw_tx_empty_account_b0`. *)
    send_and_wait_sequencer_receive ~raw_tx:raw_tx_empty_account_b1
  in
  let* invalid_nonce_hash2 =
    send_and_wait_sequencer_receive ~raw_tx:raw_tx_invalid_nonce
  in
  let* txs =
    let*@ txs = produce_block sandbox in
    return txs
  and* reason1 =
    Evm_node.wait_for_block_producer_rejected_transaction
      ~hash:invalid_balance_hash1
      sandbox
  and* reason2 =
    Evm_node.wait_for_block_producer_rejected_transaction
      ~hash:invalid_nonce_hash2
      sandbox
  in
  Check.(reason1 =~ rex "Not enough funds")
    ~error_msg:"transaction rejected for invalid reason, found %L, expected %R" ;
  Check.(reason2 =~ rex "Transaction nonce is not the expected nonce")
    ~error_msg:"transaction rejected for invalid reason, found %L, expected %R" ;
  Check.((txs = 2) int ~error_msg:"block has %L, but expected %R") ;
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

(* Test that everyone agrees on what the storage version of the rollup
   is. *)
let test_durable_storage_consistency =
  register_all
    ~__FILE__
    ~tags:["durable_storage"; "consistency"]
    ~time_between_blocks:Nothing
    ~title:"Everyone agrees on L1 level"
  @@ fun {sequencer; observer; sc_rollup_node; _} _protocol ->
  let key = Durable_storage_path.storage_version in
  let* val_from_rollup_node_opt =
    Sc_rollup_node.RPC.call sc_rollup_node ~rpc_hooks:Tezos_regression.rpc_hooks
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind:"wasm_2_0_0"
         ~operation:Sc_rollup_rpc.Value
         ~key
         ()
  in
  let val_from_rollup_node =
    match val_from_rollup_node_opt with
    | None ->
        Test.fail
          ~__LOC__
          "No value found in durable storage from rollup node at path %S"
          key
    | Some v -> v
  in
  let () =
    Log.info
      "Value at path %S according to rollup node: %S"
      key
      val_from_rollup_node
  in

  (* Rpc.state_value calls a private RPC so we can only test the EVM
     nodes for which a private RPC server exists and
     [Evm_node.endpoint ~private_:true] succeeds not the
     RPC EVM nodes. *)
  let evm_nodes_with_private_servers = [sequencer; observer] in

  let* () =
    Lwt_list.iter_s
      (fun evm_node ->
        let*@ val_from_evm_node_opt = Rpc.state_value evm_node key in
        match val_from_evm_node_opt with
        | Some v when v = val_from_rollup_node -> unit
        | Some val_from_evm_node ->
            let () =
              Log.info
                "Value at path %S according to EVM node %s: %S (value \
                 according to rollup node: %S)"
                key
                (Evm_node.name evm_node)
                val_from_evm_node
                val_from_rollup_node
            in
            unit
        | None ->
            Test.fail
              ~__LOC__
              "No value found in durable storage from EVM node %s at path %S"
              (Evm_node.name evm_node)
              key)
      evm_nodes_with_private_servers
  in
  unit

let get_deposit_nonce_from_latest_block evm_node =
  let*@ block =
    Rpc.get_block_by_number ~full_tx_objects:true ~block:"latest" evm_node
  in

  let*@ json =
    match block.transactions with
    | Block.Full (tx :: _) ->
        Rpc.trace_transaction
          ~transaction_hash:tx.Transaction.hash
          ~tracer:"callTracer"
          evm_node
    | _ -> Test.fail "Inconsistent result"
  in

  let data =
    JSON.(json |-> "logs" |> as_list |> List.hd |-> "data" |> as_string)
  in
  let cleaned = String.sub data 2 (String.length data - 2) in
  let nonce_string = String.sub cleaned 0 32 in
  let nonce = Z.of_string_base 16 nonce_string in
  return nonce

let expect_failure msg k =
  Lwt.catch
    (fun () ->
      let* _ = k () in
      Test.fail msg)
    (fun _ -> unit)

let produce_proxy_owned_fa_deposit_and_claim ~client ~sequencer
    ~sc_rollup_address ~evm_version ~l1_contracts ?(regression = false) () =
  let* ticketer_bytes = ticket_creator l1_contracts.fa_deposit in
  let* content_bytes = ticket_content 0 in
  let ticketer = ticketer_bytes |> Hex.of_bytes |> Hex.show in
  let content = content_bytes |> Hex.of_bytes |> Hex.show in

  let* proxy = Solidity_contracts.etherlink_fa_proxy_mock evm_version in

  let* proxy, _tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~args:(sf {|["0x%s", "0x%s"]|} ticketer content)
         ~abi:proxy.abi
         ~bin:proxy.bin)
      sequencer
  in

  Log.info "Proxy: %s" proxy ;

  let proxy_without_0x = String.sub proxy 2 (String.length proxy - 2) in

  let* () =
    Client.transfer
      ~giver:Constant.bootstrap4.public_key_hash
      ~receiver:l1_contracts.fa_deposit
      ~arg:
        (sf
           {|Pair 50 (Pair 0x%s%s "%s")|}
           proxy_without_0x
           proxy_without_0x
           sc_rollup_address)
      ~amount:Tez.zero
      ~burn_cap:Tez.one
      client
  in
  let* () = repeat 5 (fun () -> Client.bake_for_and_wait client) in

  let*@ _ = produce_block sequencer in

  let* receipt = get_one_receipt_from_latest_or_fail sequencer in

  if regression then capture_logs ~header:"Queue deposit" receipt.logs ;

  let* nonce = get_deposit_nonce_from_latest_block sequencer in

  let* () =
    Eth_cli.add_abi ~label:"claim" ~abi:(predep_fa_bridge_abi_path ()) ()
  in

  let claim =
    Eth_cli.contract_send
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~endpoint:(Evm_node.endpoint sequencer)
      ~abi_label:"claim"
      ~address:Solidity_contracts.Precompile.fa_bridge
      ~method_call:(sf {|claim(%d)|} (Z.to_int nonce))
      ~value:Wei.zero
  in
  let produce_block () = Rpc.produce_block sequencer in
  let* _res = wait_for_application ~produce_block claim in
  return (nonce, proxy, ticketer, content)

let test_fa_deposit_can_be_claimed_and_withdrawn =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "fa_deposit"; "claim"; "withdraw"]
    ~time_between_blocks:Nothing
    ~use_dal:Register_without_feature
    ~enable_fa_bridge:true
    ~use_multichain:Register_without_feature
    ~maximum_allowed_ticks:2_000_000_000L
    ~title:"FA deposit can be claimed and withdrawn"
    ~additional_uses:[Constant.octez_codec]
  @@
  fun {client; sequencer; sc_rollup_address; evm_version; l1_contracts; _}
      _protocol
    ->
  let* nonce, proxy, ticketer, content =
    produce_proxy_owned_fa_deposit_and_claim
      ~client
      ~sequencer
      ~sc_rollup_address
      ~evm_version
      ~l1_contracts
      ()
  in

  let*@ block =
    Rpc.get_block_by_number ~full_tx_objects:true ~block:"latest" sequencer
  in

  let*@ _receipt =
    match block.transactions with
    | Block.Full (tx :: _) ->
        Rpc.get_transaction_receipt ~tx_hash:tx.Transaction.hash sequencer
    | _ -> Test.fail "Inconsistent result"
  in

  let* dummy_l1_receiver = ticket_creator Constant.bootstrap4.public_key_hash in
  let routing_info =
    String.concat "" [dummy_l1_receiver |> Hex.of_bytes |> Hex.show; ticketer]
  in

  let account = Eth_account.bootstrap_accounts.(0) in
  let* _tx =
    call_fa_withdraw
      ~sender:account
      ~endpoint:(Evm_node.endpoint sequencer)
      ~evm_node:sequencer
      ~ticket_owner:proxy
      ~routing_info
      ~amount:25
      ~ticketer
      ~content
      ()
  in

  let* () =
    expect_failure "Claiming the same deposit twice should fail" @@ fun () ->
    Eth_cli.contract_send
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~endpoint:(Evm_node.endpoint sequencer)
      ~abi_label:"claim"
      ~address:Solidity_contracts.Precompile.fa_bridge
      ~method_call:(sf {|claim(%d)|} (Z.to_int nonce))
      ~value:Wei.zero
      ()
  in

  let* () =
    expect_failure "Invalid claim should fail" @@ fun () ->
    Eth_cli.contract_send
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~endpoint:(Evm_node.endpoint sequencer)
      ~abi_label:"claim"
      ~address:Solidity_contracts.Precompile.fa_bridge
      ~method_call:(sf {|claim(42)|})
      ~value:Wei.zero
      ()
  in

  unit

let test_fast_fa_deposit_can_be_claimed_and_withdrawn =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "fa_deposit"; "claim"; "withdraw"]
    ~time_between_blocks:Nothing
    ~use_dal:Register_without_feature
    ~enable_fa_bridge:true
    ~use_multichain:Register_without_feature
    ~maximum_allowed_ticks:2_000_000_000L
    ~title:"Fast FA deposit can be claimed and withdrawn"
    ~additional_uses:[Constant.octez_codec]
  @@
  fun {client; sequencer; sc_rollup_address; evm_version; l1_contracts; _}
      _protocol
    ->
  let* _nonce, proxy, ticketer, content =
    produce_proxy_owned_fa_deposit_and_claim
      ~client
      ~sequencer
      ~sc_rollup_address
      ~evm_version
      ~l1_contracts
      ()
  in

  let*@ block =
    Rpc.get_block_by_number ~full_tx_objects:true ~block:"latest" sequencer
  in

  let*@ _receipt =
    match block.transactions with
    | Block.Full (tx :: _) ->
        Rpc.get_transaction_receipt ~tx_hash:tx.Transaction.hash sequencer
    | _ -> Test.fail "Inconsistent result"
  in

  let dummy_l1_receiver = Constant.bootstrap4.public_key_hash in
  let dummy_fast_withdrawal = Constant.bootstrap3.public_key_hash in

  let* _tx =
    call_fa_fast_withdraw
      ~sequencer
      ~sender:Eth_account.bootstrap_accounts.(0)
      ~ticket_owner:proxy
      ~amount:25
      ~ticketer
      ~content
      ~fast_withdrawal_contract_address:dummy_fast_withdrawal
      ~receiver:dummy_l1_receiver
      ()
  in

  unit

let test_claim_deposit_event =
  Protocol.register_regression_test
    ~__FILE__
    ~tags:["evm"; "fa_bridge"; "claim"; "deposit"; "event"]
    ~title:"Regression test for the claimed FA deposit event"
    ~uses:(fun _protocol ->
      [
        Constant.octez_codec;
        Constant.octez_smart_rollup_node;
        Constant.octez_evm_node;
        Constant.smart_rollup_installer;
        Constant.WASM.evm_kernel;
      ])
  @@ fun protocol ->
  (* See {Note timestamp} *)
  let next_timestamp = timestamp_generator () in
  let* {sequencer; sc_rollup_address; client; evm_version; l1_contracts; _} =
    let genesis_timestamp = next_timestamp () in
    Setup.setup_sequencer
      ~genesis_timestamp:Client.(At (Time.of_notation_exn genesis_timestamp))
      ~time_between_blocks:Nothing
      ~mainnet_compat:false
      ~enable_dal:false
      ~enable_multichain:false
      ~enable_fa_bridge:true
      ~enable_fast_fa_withdrawal:true
      protocol
  in
  let* _ =
    produce_proxy_owned_fa_deposit_and_claim
      ~client
      ~sequencer
      ~sc_rollup_address
      ~evm_version
      ~l1_contracts
      ~regression:true
      ()
  in
  let* receipt = get_one_receipt_from_latest_or_fail sequencer in
  capture_logs ~header:"Claimed deposit" receipt.logs ;
  unit

let call_evm_based_sequencer_key_change ~sender ~sequencer ~sequencer_owner
    ~new_key =
  let new_pk = Tezos_crypto.Signature.Public_key.of_b58check_exn new_key in
  let bytes =
    match
      Data_encoding.Binary.to_bytes
        Tezos_crypto.Signature.Public_key.encoding
        new_pk
    with
    | Ok bytes -> bytes
    | Error err ->
        failwith
          (Format.asprintf
             "Failed to encode public key: %a"
             Data_encoding.Binary.pp_write_error
             err)
  in
  let hex_pk = Hex.of_bytes bytes |> Hex.show in
  let hex_signature =
    match
      Data_encoding.Binary.to_bytes
        Tezos_crypto.Signature.encoding
        (Account.sign_bytes ~signer:sequencer_owner bytes)
    with
    | Ok bytes -> Hex.of_bytes bytes |> Hex.show
    | Error err ->
        failwith
          (Format.asprintf
             "Failed to encode signature: %a"
             Data_encoding.Binary.pp_write_error
             err)
  in
  let* gas_price = Rpc.get_gas_price sequencer in
  let gas_price = Int32.to_int gas_price in
  let* raw_sequencer_upgrade =
    Cast.craft_tx
      ~signature:"change_sequencer_key(bytes,bytes)"
      ~source_private_key:sender
      ~chain_id:1337
      ~nonce:0
      ~gas_price
      ~gas:100_000
      ~value:(Wei.of_eth_int 1)
      ~address:Solidity_contracts.Precompile.sequencer_key_change
      ~arguments:[hex_pk; hex_signature]
      ()
  in
  let*@ tx = Rpc.send_raw_transaction ~raw_tx:raw_sequencer_upgrade sequencer in
  return tx

let test_sequencer_key_change =
  let sequencer_owner = Constant.bootstrap1 in
  let new_sequencer_owner = Constant.bootstrap2 in
  let genesis_timestamp_str = "2020-01-01T00:00:00Z" in
  let genesis_timestamp =
    Client.(At (Time.of_notation_exn genesis_timestamp_str))
  in
  let activation_timestamp = "2020-01-02T01:00:00Z" in
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "sequencer_upgrade"]
    ~title:"Sequencer can update his key from EVM"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
    ~sequencer:sequencer_owner
    ~additional_sequencer_keys:[new_sequencer_owner]
    ~genesis_timestamp
    ~instant_confirmations:true
  @@ fun {sequencer; sc_rollup_node; client; _} _protocol ->
  let whale = Eth_account.bootstrap_accounts.(0) in
  let new_key = new_sequencer_owner.public_key in
  let*@ () =
    (* We must propose a timestamp else the sequencer set it to now *)
    Rpc.propose_next_block_timestamp ~timestamp:genesis_timestamp_str sequencer
  in
  let* tx =
    call_evm_based_sequencer_key_change
      ~sender:whale.private_key
      ~sequencer
      ~sequencer_owner
      ~new_key
  in
  let*@ _ = produce_block sequencer in
  (* timestamp was set with `propose_next_block_timestamp` *)
  let*@! Transaction.{logs; status; _} =
    Rpc.get_transaction_receipt ~tx_hash:tx sequencer
  in
  Check.((status = true) bool ~error_msg:"Transaction failed") ;
  Check.((List.length logs = 1) int ~error_msg:"Expected 1 log") ;
  let*@ _ = produce_block ~timestamp:activation_timestamp sequencer in
  let* () = bake_until_sync ~sequencer ~sc_rollup_node ~client () in
  let*@ _ = produce_block ~timestamp:activation_timestamp sequencer in
  let*@! value = Rpc.state_value sequencer Durable_storage_path.sequencer in
  let expected_key = Hex.of_string new_key |> Hex.show in
  Check.(
    (value = expected_key)
      string
      ~error_msg:"Expected sequencer key to be %R, got %L") ;
  let* () = Evm_node.wait_for_blueprint_injected sequencer 3 in
  let* () = bake_until_sync ~sequencer ~sc_rollup_node ~client () in
  let* current_sequencer_in_rollup_hex =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind:"wasm_2_0_0"
         ~operation:Sc_rollup_rpc.Value
         ~key:Durable_storage_path.sequencer
         ()
  in
  Check.(
    (current_sequencer_in_rollup_hex = Some expected_key)
      (option string)
      ~error_msg:
        "Expected sequencer key in rollup to be %R, got %L (rollup might be \
         not in sync)") ;
  unit

let test_sequencer_key_change_fails_if_governance_upgrade_exists =
  let sequencer_owner = Constant.bootstrap1 in
  let new_sequencer_owner = Constant.bootstrap2 in
  let genesis_timestamp =
    Client.(At (Time.of_notation_exn "2020-01-01T00:00:00Z"))
  in
  let activation_timestamp = "2100-01-02T00:00:00Z" in

  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "sequencer_upgrade"]
    ~title:"EVM change key cannot work if governance one is pending"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
    ~genesis_timestamp
    ~sequencer:sequencer_owner
    ~additional_sequencer_keys:[new_sequencer_owner]
  @@
  fun {sequencer; sc_rollup_address; sc_rollup_node; l1_contracts; client; _}
      _protocol
    ->
  let* () =
    sequencer_upgrade
      ~sc_rollup_address
      ~sequencer_admin:Constant.bootstrap2.alias
      ~sequencer_governance_contract:l1_contracts.sequencer_governance
      ~pool_address:Eth_account.bootstrap_accounts.(0).address
      ~client
      ~upgrade_to:sequencer_owner.alias
      ~activation_timestamp
  in
  (* Wait for the sequencer to receive the upgrade *)
  let waiting_sequencer_upgrade =
    Evm_node.wait_for_pending_sequencer_upgrade sequencer
  in
  let* () =
    repeat 2 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  let* _res = waiting_sequencer_upgrade in
  let whale = Eth_account.bootstrap_accounts.(0) in
  let new_key = new_sequencer_owner.public_key in
  let* tx =
    call_evm_based_sequencer_key_change
      ~sender:whale.private_key
      ~sequencer
      ~sequencer_owner
      ~new_key
  in
  let*@ _ = produce_block sequencer in
  let*@! Transaction.{status; _} =
    Rpc.get_transaction_receipt ~tx_hash:tx sequencer
  in
  Check.((status = false) bool ~error_msg:"Transaction should have failed") ;
  unit

let test_eip2930_storage_access =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "eip2930"]
    ~title:"Check EIP-2930's semantic correctness"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  let whale = Eth_account.bootstrap_accounts.(0) in
  let* eip2930_storage_access =
    Solidity_contracts.eip2930_storage_access evm_version
  in
  let* eip2930_contract, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:whale.private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi:eip2930_storage_access.abi
         ~bin:eip2930_storage_access.bin)
      sequencer
  in
  let* gas_price = Rpc.get_gas_price sequencer in
  let gas_price = Int32.to_int gas_price in
  let base_tx ~nonce ~access_list ~arg =
    Cast.craft_tx
      ~signature:"setValue(uint256)"
      ~source_private_key:whale.private_key
      ~chain_id:1337
      ~nonce
      ~gas:100_000
      ~gas_price
      ~value:Wei.zero
      ~access_list
      ~address:eip2930_contract
      ~arguments:[arg]
      ~legacy:false
      ()
  in
  let* raw_tx_with_storage_slot_access_list =
    base_tx
      ~nonce:1
      ~access_list:
        [
          ( eip2930_contract,
            (* slot 0 =  [uint256 public value] in [eip2930_storage_access.sol] *)
            [
              "0x0000000000000000000000000000000000000000000000000000000000000000";
            ] );
        ]
      ~arg:"42"
  in
  let* raw_tx_without_storage_slot_access_list =
    base_tx ~nonce:2 ~access_list:[(eip2930_contract, [])] ~arg:"43"
  in
  let*@ tx_with_storage_slot_access_list_hash =
    Rpc.send_raw_transaction
      ~raw_tx:raw_tx_with_storage_slot_access_list
      sequencer
  in
  let* _ = produce_block sequencer in
  let*@ tx_without_storage_slot_access_list_hash =
    Rpc.send_raw_transaction
      ~raw_tx:raw_tx_without_storage_slot_access_list
      sequencer
  in
  let* _ = produce_block sequencer in
  let*@! Transaction.{gasUsed = gas_with_storage_slot_access_list; _} =
    Rpc.get_transaction_receipt
      ~tx_hash:tx_with_storage_slot_access_list_hash
      sequencer
  in
  let*@! Transaction.{gasUsed = gas_without_storage_slot_access_list; _} =
    Rpc.get_transaction_receipt
      ~tx_hash:tx_without_storage_slot_access_list_hash
      sequencer
  in
  Check.(
    Int64.(
      sub gas_without_storage_slot_access_list gas_with_storage_slot_access_list
      = of_int 200)
      int64)
    ~error_msg:
      "The gas consumption with the preheated slot should have saved exactly \
       %R but got %L" ;
  unit

let test_eip7702 =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "eip7702"]
    ~title:"Check EIP-7702's semantic correctness"
      (* See: https://eips.ethereum.org/EIPS/eip-7702. *)
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  let whale = Eth_account.bootstrap_accounts.(0) in
  let sponsored =
    Eth_account.
      {
        address = "0x202dFc8a729ac2cdE90D3B0e7A0424b6Ed6f6c34";
        private_key =
          "0x7d597ae2d861eda61e148e757478c9a07d950be9f355958195f1fc75a0cdd8b2";
      }
  in
  (* To show the benefits of having EIP-7702 we'll use an EOA with 0 balance
     on purpose. *)
  let*@ balance = Rpc.get_balance ~address:sponsored.address sequencer in
  Check.((balance = Wei.zero) Wei.typ)
    ~error_msg:
      "Expected balance of the sponsored address of zero wei, got %L wei" ;
  let endpoint = Evm_node.endpoint sequencer in
  let* eip7702 = Solidity_contracts.eip7702 evm_version in
  let* () = Eth_cli.add_abi ~label:eip7702.label ~abi:eip7702.abi () in
  (* We start by deploying a simple contract that emits a log. This will be
     the delegation contract that will be executed from the EOA's address. *)
  let* eip7702_contract, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:whale.private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi:eip7702.abi
         ~bin:eip7702.bin)
      sequencer
  in
  let* gas_price = Rpc.get_gas_price sequencer in
  let gas_price = Int32.to_int gas_price in
  let base_tx ~nonce ~authorization =
    Cast.craft_tx
      ~source_private_key:whale.private_key
      ~chain_id:1337
      ~nonce
      ~gas:100_000
      ~gas_price
      ~value:Wei.zero
      ~authorization
      ~address:sponsored.address
      ~arguments:[]
      ~legacy:false
      ()
  in
  (* The EOA can sign with no money the authorization list. Here the delegation
     is done to `eip7702_contract`.*)
  let* signed_auth =
    Cast.wallet_sign_auth
      ~authorization:eip7702_contract
      ~private_key:sponsored.private_key
      ~endpoint
      ()
  in
  (* We craft the EIP-7702 transaction thanks to cast. The sponsor that has the
     necessary balance will use the signed authorization and post in on chain. *)
  let* raw_set_eoa = base_tx ~nonce:1 ~authorization:signed_auth in
  let*@ set_eoa_hash = Rpc.send_raw_transaction ~raw_tx:raw_set_eoa sequencer in
  let* _ = produce_block sequencer in
  let*@! Transaction.{type_; _} =
    Rpc.get_transaction_receipt ~tx_hash:set_eoa_hash sequencer
  in
  (* Type 4 = EIP-7702 *)
  Check.((type_ = Int32.of_int 4) int32)
    ~error_msg:"Expected tx.type of %R, got %L" ;
  (* We can retrieve the authorization list from the transaction object: *)
  let*@! Transaction.{authorizationList; _} =
    Rpc.get_transaction_by_hash ~transaction_hash:set_eoa_hash sequencer
  in
  (match authorizationList with
  | Some [{address; _}] ->
      Check.(
        (String.lowercase_ascii address
        = String.lowercase_ascii eip7702_contract)
          string)
        ~error_msg:"Expected msg.sender of %R, got %L"
  | Some _ -> failwith "Authorization list should only contain one element."
  | None -> failwith "Authorization list should not be empty.") ;
  (* The EOA now has code that can be called. We can even reuse the abi label
     from the delegation contract. *)
  let* call_eoa_hash =
    send_transaction_to_sequencer
      (Eth_cli.contract_send
         ~source_private_key:whale.private_key
         ~endpoint
         ~abi_label:eip7702.label
         ~address:sponsored.address (* EOA's account *)
         ~method_call:"emitEvent()")
      sequencer
  in
  let*@! Transaction.{logs; _} =
    Rpc.get_transaction_receipt ~tx_hash:call_eoa_hash sequencer
  in
  (* Exactly one event emitted, see the code of `eip7702_contract`. *)
  let Transaction.{data; _} = List.hd logs in
  let data_without_padding = "0x" ^ String.sub data 26 40 in
  Check.(
    (String.lowercase_ascii data_without_padding
    = String.lowercase_ascii whale.address)
      string)
    ~error_msg:"Expected msg.sender of %R, got %L" ;
  let*@ code = Rpc.get_code ~address:sponsored.address sequencer in
  (* `0xef0100` is the current prefix of EIP-7702 transactions, see the
      exact specification for more details. *)
  let expected_code =
    "0xef0100" ^ String.(lowercase_ascii @@ sub eip7702_contract 2 40)
  in
  Check.((code = expected_code) string) ~error_msg:"Expected code of %R, got %L" ;
  (* Another property of EIP-7702 is allowing an EOA to re-delegate its code to
     another address. *)
  let* redelegation_contract =
    Solidity_contracts.eip2930_storage_access evm_version
  in
  let* redelegation_contract_address, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:whale.private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi:redelegation_contract.abi
         ~bin:redelegation_contract.bin)
      sequencer
  in
  let* signed_auth =
    Cast.wallet_sign_auth
      ~authorization:redelegation_contract_address
      ~private_key:sponsored.private_key
      ~endpoint
      ()
  in
  let* raw_set_eoa = base_tx ~nonce:4 ~authorization:signed_auth in
  let*@ set_eoa_hash = Rpc.send_raw_transaction ~raw_tx:raw_set_eoa sequencer in
  let*@ _ = produce_block sequencer in
  let*@! Transaction.{type_; _} =
    Rpc.get_transaction_receipt ~tx_hash:set_eoa_hash sequencer
  in
  (* Type 4 = EIP-7702 *)
  Check.((type_ = Int32.of_int 4) int32)
    ~error_msg:"Expected tx.type of %R, got %L" ;
  (* We can retrieve the authorization list from the transaction object: *)
  let*@! Transaction.{authorizationList; _} =
    Rpc.get_transaction_by_hash ~transaction_hash:set_eoa_hash sequencer
  in
  (match authorizationList with
  | Some [{address; _}] ->
      Check.(
        (String.lowercase_ascii address
        = String.lowercase_ascii redelegation_contract_address)
          string)
        ~error_msg:"Expected msg.sender of %R, got %L"
  | Some _ -> failwith "Authorization list should only contain one element."
  | None -> failwith "Authorization list should not be empty.") ;
  let expected_storage_slot = 2 in
  let* gas_price = Rpc.get_gas_price sequencer in
  let gas_price = Int32.to_int gas_price in
  let* raw_call_eoa_hash =
    Cast.craft_tx
      ~signature:"setValue(uint256)"
      ~source_private_key:whale.private_key
      ~chain_id:1337
      ~nonce:5
      ~gas:100_000
      ~gas_price
      ~value:Wei.zero
      ~access_list:[]
      ~address:sponsored.address
      ~arguments:["2"]
      ~legacy:false
      ()
  in
  let*@ call_eoa_hash =
    Rpc.send_raw_transaction ~raw_tx:raw_call_eoa_hash sequencer
  in
  let*@ _ = produce_block sequencer in
  let*@! Transaction.{blockNumber; _} =
    Rpc.get_transaction_receipt ~tx_hash:call_eoa_hash sequencer
  in
  let*@ storage_slot =
    Rpc.get_storage_at
      ~address:sponsored.address
      ~pos:"0x0"
      ~block:
        (Block_number
           {number = Int32.to_int blockNumber; require_canonical = false})
      sequencer
  in
  Check.((storage_slot = Printf.sprintf "0x%064x" expected_storage_slot) string)
    ~error_msg:"Expected storage slot of %R, got %L" ;
  let* _ =
    send_transaction_to_sequencer
      (Eth_cli.transaction_send
         ~source_private_key:whale.private_key
         ~to_public_key:sponsored.address
         ~value:Wei.one_eth
         ~endpoint)
      sequencer
  in
  (* EIP-7702 authorized account can also send transactions to the sequencer. *)
  let* _ =
    send_transaction_to_sequencer
      (Eth_cli.transaction_send
         ~source_private_key:sponsored.private_key
         ~to_public_key:whale.address
         ~value:(Wei.of_string "1125")
         ~endpoint)
      sequencer
  in
  unit

let test_deposits_on_eip7702_accounts =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~use_multichain:Register_without_feature
    ~tags:["evm"; "eip7702"; "deposit"]
    ~title:"Check that deposit do not break EIP-7702 accounts semantic."
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@
  fun {
        sequencer;
        evm_version;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        client;
        _;
      }
      _protocol
    ->
  let whale = Eth_account.bootstrap_accounts.(0) in
  let sponsored =
    Eth_account.
      {
        address = "0x202dFc8a729ac2cdE90D3B0e7A0424b6Ed6f6c34";
        private_key =
          "0x7d597ae2d861eda61e148e757478c9a07d950be9f355958195f1fc75a0cdd8b2";
      }
  in
  (* The future EIP-7702 account will have 0 balance before the deposit. *)
  let*@ balance = Rpc.get_balance ~address:sponsored.address sequencer in
  Check.((balance = Wei.zero) Wei.typ)
    ~error_msg:
      "Expected balance of the sponsored address of zero wei, got %L wei" ;
  let endpoint = Evm_node.endpoint sequencer in
  let* eip7702 = Solidity_contracts.eip7702_fallback evm_version in
  let* () = Eth_cli.add_abi ~label:eip7702.label ~abi:eip7702.abi () in
  (* We start by deploying a simple contract that emits a log. This will be
     the delegation contract that will be executed from the EOA's address
     when the deposit happens. *)
  let* eip7702_contract, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:whale.private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi:eip7702.abi
         ~bin:eip7702.bin)
      sequencer
  in
  let* gas_price = Rpc.get_gas_price sequencer in
  let gas_price = Int32.to_int gas_price in
  let base_tx ~nonce ~authorization =
    Cast.craft_tx
      ~source_private_key:whale.private_key
      ~chain_id:1337
      ~nonce
      ~gas:100_000
      ~gas_price
      ~value:Wei.zero
      ~authorization
      ~address:sponsored.address
      ~arguments:[]
      ~legacy:false
      ()
  in
  let* signed_auth =
    Cast.wallet_sign_auth
      ~authorization:eip7702_contract
      ~private_key:sponsored.private_key
      ~endpoint
      ()
  in
  (* We craft the EIP-7702 transaction thanks to cast. The sponsor that has the
     necessary balance will use the signed authorization and post in on chain. *)
  let* raw_set_eoa = base_tx ~nonce:1 ~authorization:signed_auth in
  let*@ set_eoa_hash = Rpc.send_raw_transaction ~raw_tx:raw_set_eoa sequencer in
  let* _ = produce_block sequencer in
  let*@! Transaction.{type_; _} =
    Rpc.get_transaction_receipt ~tx_hash:set_eoa_hash sequencer
  in
  (* Type 4 = EIP-7702 *)
  Check.((type_ = Int32.of_int 4) int32)
    ~error_msg:"Expected tx.type of %R, got %L" ;
  (* We can retrieve the authorization list from the transaction object: *)
  let*@! Transaction.{authorizationList; _} =
    Rpc.get_transaction_by_hash ~transaction_hash:set_eoa_hash sequencer
  in
  (match authorizationList with
  | Some [{address; _}] ->
      Check.(
        (String.lowercase_ascii address
        = String.lowercase_ascii eip7702_contract)
          string)
        ~error_msg:"Expected msg.sender of %R, got %L"
  | Some _ -> failwith "Authorization list should only contain one element."
  | None -> failwith "Authorization list should not be empty.") ;
  (* At this point, the EOA has code that can be called on deposit. *)
  let scenario_for ~address ~deposit_id =
    let depositor = Constant.bootstrap5 in
    let amount = Tez.of_int 1125 in
    let deposit_info = {receiver = EthereumAddr address; chain_id = None} in
    let* () =
      send_deposit_to_delayed_inbox
        ~amount
        ~bridge:l1_contracts.bridge
        ~depositor
        ~deposit_info
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
    let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in
    let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in
    (* Since it's an address that contains code that we deposit in, the balance
       should still be zero, it's a two step process, we need to claim it. *)
    let*@ balance = Rpc.get_balance ~address sequencer in
    Check.((balance = Wei.zero) Wei.typ)
      ~error_msg:
        "Expected balance of the sponsored address of zero wei, got %L wei" ;
    (* Check that the deposit was queued for the address with code. *)
    let* deposit_receipt = get_one_receipt_from_latest_or_fail sequencer in
    let deposit_log : Transaction.tx_log = List.hd deposit_receipt.logs in
    assert (Solidity_contracts.Precompile.xtz_bridge = deposit_log.address) ;
    assert (revm_queued_xtz_deposit_event_topic = List.hd deposit_log.topics) ;
    (* Claim the queued deposit. *)
    let* () =
      Eth_cli.add_abi ~label:"claim_xtz" ~abi:(predep_xtz_bridge_abi_path ()) ()
    in
    let claim =
      Eth_cli.contract_send
        ~source_private_key:whale.private_key
        ~endpoint:(Evm_node.endpoint sequencer)
        ~abi_label:"claim_xtz"
        ~address:Solidity_contracts.Precompile.xtz_bridge
        ~method_call:(sf {|claim_xtz(%d)|} deposit_id)
    in
    let produce_block () = Rpc.produce_block sequencer in
    let* _res = wait_for_application ~produce_block claim in
    (* Check that the address with code was called when the claim happened. *)
    let* claim_receipt = get_one_receipt_from_latest_or_fail sequencer in
    let claim_logs : Transaction.tx_log list = claim_receipt.logs in
    let contract_call_log = List.nth claim_logs 0 in
    assert (String.lowercase_ascii address = contract_call_log.address) ;
    assert (eip7702_fallback_event_topic = List.hd contract_call_log.topics) ;
    let deposit_log = List.nth claim_logs 1 in
    assert (Solidity_contracts.Precompile.xtz_bridge = deposit_log.address) ;
    assert (revm_xtz_deposit_event_topic = List.hd deposit_log.topics) ;

    (* Check that the balance was properly deposited. *)
    let*@ balance = Rpc.get_balance ~address sequencer in
    Check.((balance = Wei.of_tez amount) Wei.typ)
      ~error_msg:"Expected balance of the address with code of %R, got %L wei" ;
    unit
  in
  (* The scenario has to work for both the EOA (EIP-7702) account and the
     regular smart contract. *)
  let* () = scenario_for ~address:sponsored.address ~deposit_id:0 in
  scenario_for ~address:eip7702_contract ~deposit_id:1

let test_eip7702_auto_sign =
  register_all
    ~__FILE__
    ~kernels:[Latest]
    ~tags:["evm"; "eip7702"; "auto_sign"]
    ~title:
      "Check EIP-7702's semantic correctness regarding auth and tx sender and \
       signer"
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
  @@ fun {sequencer; evm_version; _} _protocol ->
  let whale = Eth_account.bootstrap_accounts.(0) in
  let endpoint = Evm_node.endpoint sequencer in
  let* eip7702 = Solidity_contracts.eip7702 evm_version in
  let* () = Eth_cli.add_abi ~label:eip7702.label ~abi:eip7702.abi () in
  let* eip7702_contract, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:whale.private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi:eip7702.abi
         ~bin:eip7702.bin)
      sequencer
  in
  let* gas_price = Rpc.get_gas_price sequencer in
  let gas_price = Int32.to_int gas_price in
  let base_tx ~nonce ~authorization =
    Cast.craft_tx
      ~source_private_key:whale.private_key
      ~chain_id:1337
      ~nonce
      ~gas:100_000
      ~gas_price
      ~value:Wei.zero
      ~authorization
      ~address:whale.address
      ~arguments:[]
      ~legacy:false
      ()
  in
  let signed_auth ~nonce =
    Cast.wallet_sign_auth
      ~nonce
      ~authorization:eip7702_contract
      ~private_key:whale.private_key
      ~endpoint
      ()
  in
  let* authorization = signed_auth ~nonce:2 in
  let* raw_set_eoa = base_tx ~nonce:1 ~authorization in
  let*@ set_eoa_hash = Rpc.send_raw_transaction ~raw_tx:raw_set_eoa sequencer in
  let* _ = produce_block sequencer in
  let*@! Transaction.{type_; _} =
    Rpc.get_transaction_receipt ~tx_hash:set_eoa_hash sequencer
  in
  (* Type 4 = EIP-7702 *)
  Check.((type_ = Int32.of_int 4) int32)
    ~error_msg:"Expected tx.type of %R, got %L" ;
  (* We can retrieve the authorization list from the transaction object: *)
  let*@! Transaction.{authorizationList; _} =
    Rpc.get_transaction_by_hash ~transaction_hash:set_eoa_hash sequencer
  in
  (match authorizationList with
  | Some [{address; _}] ->
      Check.(
        (String.lowercase_ascii address
        = String.lowercase_ascii eip7702_contract)
          string)
        ~error_msg:"Expected msg.sender of %R, got %L"
  | Some _ -> failwith "Authorization list should only contain one element."
  | None -> failwith "Authorization list should not be empty.") ;
  let* call_eoa_hash =
    send_transaction_to_sequencer
      (Eth_cli.contract_send
         ~source_private_key:whale.private_key
         ~endpoint
         ~abi_label:eip7702.label
         ~address:whale.address
         ~method_call:"emitEvent()")
      sequencer
  in
  let*@! Transaction.{logs; _} =
    Rpc.get_transaction_receipt ~tx_hash:call_eoa_hash sequencer
  in
  let Transaction.{data; _} = List.hd logs in
  let data_without_padding = "0x" ^ String.sub data 26 40 in
  Check.(
    (String.lowercase_ascii data_without_padding
    = String.lowercase_ascii whale.address)
      string)
    ~error_msg:"Expected msg.sender of %R, got %L" ;
  unit

let test_eip2537 =
  register_all
    ~__FILE__
    ~tags:["evm"; "bls"; "precompile"; "eip2537"]
    ~title:"EIP-2537 is activated on Etherlink"
    ~kernels:[Latest]
  @@ fun {sequencer; _} _protocol ->
  let bls_g1add = "0x000000000000000000000000000000000000000b" in
  (* First entry of BLS12_G1ADD at https://eips.ethereum.org/assets/eip-2537/test-vectors *)
  let input =
    "0x0000000000000000000000000000000017f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb0000000000000000000000000000000008b3f481e3aaa0f1a09e30ed741d8ae4fcf5e095d5d00af600db18cb2c04b3edd03cc744a2888ae40caa232946c5e7e100000000000000000000000000000000112b98340eee2777cc3c14163dea3ec97977ac3dc5c70da32e6e87578f44912e902ccef9efe28d4a78b8999dfbca942600000000000000000000000000000000186b28d92356c4dfec4b5201ad099dbdede3781f8998ddf929b4cd7756192185ca7b8f4ef7088f813270ac3d48868a21"
  in
  let expected =
    "0x000000000000000000000000000000000a40300ce2dec9888b60690e9a41d3004fda4886854573974fab73b046d3147ba5b7a5bde85279ffede1b45b3918d82d0000000000000000000000000000000006d3d887e9f53b9ec4eb6cedf5607226754b07c01ace7834f57f3e7315faefb739e59018e22c492006190fba4a870025"
  in
  let* value =
    Cast.raw_call
      ~endpoint:(Evm_node.endpoint sequencer)
      ~address:bls_g1add
      ~arg:input
  in
  Check.((value = expected) string)
    ~error_msg:(Format.sprintf "BLS12_G1ADD mismatch for test bls_g1add_g1+p1") ;
  unit

let pad_to_n_bytes_le bytes length =
  let current_length = Bytes.length bytes in
  if current_length >= length then bytes
  else
    let padding_length = length - current_length in
    let padding = Bytes.make padding_length '\x00' in
    Bytes.cat bytes padding

let test_validate_encoding_compatibility_accounts =
  register_all
    ~__FILE__
    ~title:"Validate encoding compatibility of accounts"
    ~tags:["encoding"; "accounts"; "foo"]
  @@ fun {sequencer; observer; _} _protocol ->
  let nonce = Z.of_int 1234 in
  let source = Eth_account.bootstrap_accounts.(0) in
  let bits = Z.to_bits nonce |> Bytes.of_string in
  let encoded_nonce = pad_to_n_bytes_le bits 4 in
  let* () = Evm_node.terminate sequencer in
  let* () = Evm_node.terminate observer in
  let* () =
    Evm_node.patch_state
      sequencer
      ~key:(Durable_storage_path.nonce source.address)
      ~value:(Hex.of_bytes encoded_nonce |> Hex.show)
  in
  let* () =
    Evm_node.patch_state
      observer
      ~key:(Durable_storage_path.nonce source.address)
      ~value:(Hex.of_bytes encoded_nonce |> Hex.show)
  in
  let* () = Evm_node.run sequencer in
  let* () = Evm_node.run observer in
  let desination = Eth_account.bootstrap_accounts.(1) in
  let*@ balance = Rpc.get_balance ~address:desination.address sequencer in
  (* First execution of the transaction should lazy migrate the storage values
  to the new `info` format and the others transactions should execute correctly*)
  let* _ =
    repeat 5 (fun () ->
        let* _ =
          send_transaction_to_sequencer
            (Eth_cli.transaction_send
               ~source_private_key:source.private_key
               ~to_public_key:desination.address
               ~value:Wei.zero
               ~endpoint:(Evm_node.endpoint sequencer))
            sequencer
        in
        unit)
  in
  let*@ balance_after = Rpc.get_balance ~address:desination.address sequencer in
  Check.((balance_after = balance) Wei.typ)
    ~error_msg:"Expected balance of %R wei, got %L wei after %R transactions" ;
  unit

let test_eip3607_disabled_for_simulation =
  register_all
    ~__FILE__
    ~tags:["evm"; "eip3607"; "simulation"; "eth_call"]
    ~title:"EIP-3607 is disabled in simulation"
    ~kernels:[Latest]
  @@ fun {sequencer; evm_version; _} _protocol ->
  let whale = Eth_account.bootstrap_accounts.(0) in
  let* contract = Solidity_contracts.eip7702 evm_version in
  let* contract_address, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:whale.private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi:contract.abi
         ~bin:contract.bin)
      sequencer
  in
  let contract_address = Ezjsonm.encode_string contract_address in
  let* call_result =
    Evm_node.(
      call_evm_rpc
        sequencer
        {
          method_ = "eth_call";
          parameters =
            `A
              [
                `O [("from", contract_address); ("to", contract_address)];
                `String "latest";
              ];
        })
  in
  let call_result =
    Evm_node.extract_error_message call_result |> JSON.as_string
  in
  (*
    If EIP-3607 was not disabled we would get:
      {
        "jsonrpc": "2.0",
        "error": {
            "code": -32003,
            "message": "Execution error: REVM error Transaction(RejectCallerWithCode)"
        },
        "id": "0"
      }
  *)
  Check.((call_result = "execution reverted") string)
    ~error_msg:"Expected error msg %R but got %L" ;
  unit

let test_fa_deposit_watchtower =
  register_test_for_kernels
    ~__FILE__
    ~tags:["evm"; "fa_deposit"; "watchtower"]
    ~title:"FA deposit is claimed by the watchtower"
    ~enable_fa_bridge:true
    ~enable_multichain:false
    ~enable_dal:false
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
    ~additional_uses:[Constant.octez_codec; Constant.watchtower]
    ~websockets:true
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        evm_version;
        _;
      }
      _protocol
    ->
  (* The following test is hack-ish to mimic the behavior of FA deposits + a proxy involved.
     Deploying a proper proxy is pretty tedious, so we will deploy a dummy one that just
     does nothing and succeeds so we can test the behavior of the watchtower.
     BACKLOG (for better testing):
     - Deploy an actual ERC20 proxy like:
       https://explorer.etherlink.com/address/0x9121B153bbCF8C23F20eE43b494F08760B91aD64?tab=contract
     - Deploy the according L1 contract and use its data as the constructor of the proxy contract.
     - At the end of the test also check the receiver's balance on the proxy contract. *)
  let whale = Eth_account.bootstrap_accounts.(0) in
  let* dummy_proxy = Solidity_contracts.dummy_proxy evm_version in
  let* () = Eth_cli.add_abi ~label:dummy_proxy.label ~abi:dummy_proxy.abi () in
  (* Dummy proxy just to enable the two step process queue/claim for the watchtower. *)
  let* proxy, _tx_hash =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:whale.private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi:dummy_proxy.abi
         ~bin:dummy_proxy.bin)
      sequencer
  in
  let amount = 1125 in
  let receiver = "0x1074Fd1EC02cbeaa5A90450505cF3B48D834f3EB" in
  let* _watchtower =
    let config =
      Watchtower.
        {
          evm_node_endpoint = Evm_node.endpoint sequencer;
          gas_limit = None;
          max_fee_per_gas = None;
          rpc = None;
          secret_key = Some whale.private_key;
          whitelist = Some [{proxy; ticket_hashes = None}];
        }
    in
    Watchtower.init ~config ~first_block:1 ()
  in
  let* () =
    send_fa_deposit_to_delayed_inbox
      ~proxy
      ~amount
      ~l1_contracts
      ~depositor:Constant.bootstrap5
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
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in
  let* () =
    (* We let some time to the watchtower to inject the claim. This avoids flakyness. *)
    repeat 4 (fun () ->
        let* _ = produce_block sequencer in
        unit)
  in
  (* We fetch the only new ticket hash that was produced via the durable storage. *)
  let*@ ticket_hashes =
    Rpc.state_subkeys
      sequencer
      "/evm/world_state/eth_accounts/0000000000000000000000000000000000000000/ticket_table"
  in
  let ticket_hash =
    match ticket_hashes with
    | Some [t] -> t
    | _ -> failwith "ticket hash not found"
  in
  (* Since the proxy call succeeded the owner is the proxy, we make sure of that.
     NB: If the proxy was not a dummy one we could check the ERC20 balance of the
     receiver's address as an additional check. *)
  let* ticket_balance_via_sequencer =
    ticket_balance ~ticket_hash ~account:proxy (Either.Right sequencer)
  in
  Check.((amount = ticket_balance_via_sequencer) int)
    ~error_msg:
      "After deposit we expect %L ticket balance in the sequencer, got %R" ;
  unit

let test_xtz_deposit_watchtower =
  (* TODO: When 6.2 is released, replace `register_test` by
     `register_test_for_kernels`.*)
  register_test
    ~__FILE__
    ~kernel:Kernel.Latest
    ~tags:["evm"; "xtz_deposit"; "watchtower"]
    ~title:"XTZ deposit is claimed by the watchtower"
    ~enable_fa_bridge:true
    ~enable_multichain:false
    ~enable_dal:false
    ~da_fee:Wei.zero
    ~time_between_blocks:Nothing
    ~additional_uses:[Constant.octez_codec; Constant.watchtower]
    ~websockets:true
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        evm_version;
        _;
      }
      _protocol
    ->
  let whale = Eth_account.bootstrap_accounts.(0) in
  let sponsored =
    Eth_account.
      {
        address = "0x202dFc8a729ac2cdE90D3B0e7A0424b6Ed6f6c34";
        private_key =
          "0x7d597ae2d861eda61e148e757478c9a07d950be9f355958195f1fc75a0cdd8b2";
      }
  in
  (* The future EIP-7702 account will have 0 balance before the deposit. *)
  let*@ balance = Rpc.get_balance ~address:sponsored.address sequencer in
  Check.((balance = Wei.zero) Wei.typ)
    ~error_msg:
      "Expected balance of the sponsored address of zero wei, got %L wei" ;
  let endpoint = Evm_node.endpoint sequencer in
  let* eip7702 = Solidity_contracts.eip7702_fallback evm_version in
  let* () = Eth_cli.add_abi ~label:eip7702.label ~abi:eip7702.abi () in
  (* We start by deploying a simple contract that emits a log. This will be
     the delegation contract that will be executed from the EOA's address
     when the deposit happens. *)
  let* eip7702_contract, _ =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:whale.private_key
         ~endpoint:(Evm_node.endpoint sequencer)
         ~abi:eip7702.abi
         ~bin:eip7702.bin)
      sequencer
  in
  let* gas_price = Rpc.get_gas_price sequencer in
  let gas_price = Int32.to_int gas_price in
  let base_tx ~nonce ~authorization =
    Cast.craft_tx
      ~source_private_key:whale.private_key
      ~chain_id:1337
      ~nonce
      ~gas:100_000
      ~gas_price
      ~value:Wei.zero
      ~authorization
      ~address:sponsored.address
      ~arguments:[]
      ~legacy:false
      ()
  in
  let* signed_auth =
    Cast.wallet_sign_auth
      ~authorization:eip7702_contract
      ~private_key:sponsored.private_key
      ~endpoint
      ()
  in
  (* We craft the EIP-7702 transaction thanks to cast. The sponsor that has the
     necessary balance will use the signed authorization and post in on chain. *)
  let* raw_set_eoa = base_tx ~nonce:1 ~authorization:signed_auth in
  let*@ set_eoa_hash = Rpc.send_raw_transaction ~raw_tx:raw_set_eoa sequencer in
  let* _ = produce_block sequencer in
  let*@! Transaction.{type_; _} =
    Rpc.get_transaction_receipt ~tx_hash:set_eoa_hash sequencer
  in
  (* Type 4 = EIP-7702 *)
  Check.((type_ = Int32.of_int 4) int32)
    ~error_msg:"Expected tx.type of %R, got %L" ;
  (* We can retrieve the authorization list from the transaction object: *)
  let*@! Transaction.{authorizationList; _} =
    Rpc.get_transaction_by_hash ~transaction_hash:set_eoa_hash sequencer
  in
  (match authorizationList with
  | Some [{address; _}] ->
      Check.(
        (String.lowercase_ascii address
        = String.lowercase_ascii eip7702_contract)
          string)
        ~error_msg:"Expected msg.sender of %R, got %L"
  | Some _ -> failwith "Authorization list should only contain one element."
  | None -> failwith "Authorization list should not be empty.") ;
  (* At this point, the EOA has code that can be called on deposit. *)
  let* _watchtower =
    let config =
      Watchtower.
        {
          evm_node_endpoint = Evm_node.endpoint sequencer;
          gas_limit = None;
          max_fee_per_gas = None;
          rpc = None;
          secret_key = Some whale.private_key;
          whitelist = None;
        }
    in
    Watchtower.init ~config ~first_block:1 ()
  in
  let scenario_for ~address =
    let depositor = Constant.bootstrap5 in
    let amount = Tez.of_int 1125 in
    let deposit_info = {receiver = EthereumAddr address; chain_id = None} in
    let* () =
      send_deposit_to_delayed_inbox
        ~amount
        ~bridge:l1_contracts.bridge
        ~depositor
        ~deposit_info
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
    let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in
    let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in
    (* Since it's an address that contains code that we deposit in, the balance
       should still be zero, it's a two step process, we need to claim it. *)
    let*@ balance = Rpc.get_balance ~address sequencer in
    Check.((balance = Wei.zero) Wei.typ)
      ~error_msg:
        "Expected balance of the sponsored address of zero wei, got %L wei" ;
    (* Check that the deposit was queued for the address with code. *)
    let* deposit_receipt = get_one_receipt_from_latest_or_fail sequencer in
    let deposit_log : Transaction.tx_log = List.hd deposit_receipt.logs in
    assert (Solidity_contracts.Precompile.xtz_bridge = deposit_log.address) ;
    assert (revm_queued_xtz_deposit_event_topic = List.hd deposit_log.topics) ;
    let* () =
      (* We let some time to the watchtower to inject the claim. This avoids flakyness. *)
      repeat 4 (fun () ->
          let* _ = produce_block sequencer in
          unit)
    in
    (* Check that the balance was properly deposited. *)
    let*@ balance = Rpc.get_balance ~address sequencer in
    Check.((balance = Wei.of_tez amount) Wei.typ)
      ~error_msg:"Expected balance of the address with code of %R, got %L wei" ;
    unit
  in
  (* The scenario has to work for both the EOA (EIP-7702) account and the
     regular smart contract. *)
  let* () = scenario_for ~address:sponsored.address in
  scenario_for ~address:eip7702_contract

let test_evm_events_cleanup () =
  Test_helpers.register_sandbox
    ~__FILE__
    ~title:
      "The EVM node does not accumulate unnecessary EVM events in its durable \
       storage"
    ~tags:["events"]
  @@ fun sandbox ->
  let*@ _ = Rpc.produce_block sandbox in
  let*@ rpc_result = Rpc.state_subkeys sandbox "/evm/events" in
  match rpc_result with
  | None | Some [] -> unit
  | Some l ->
      Test.fail
        "Expected an empty result, got %a instead"
        Format.(
          pp_print_list
            ~pp_sep:(fun fmt () -> pp_print_string fmt ", ")
            pp_print_string)
        l

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

let protocols = Protocol.all

let () =
  test_remove_sequencer protocols ;
  test_persistent_state () ;
  test_snapshots ~desync:false protocols ;
  test_snapshots ~desync:true protocols ;
  test_patch_state [Protocol.Alpha] ;
  test_publish_blueprints protocols ;
  test_publish_blueprints_signatory protocols ;
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
  test_fa_reentrant_deposit_reverts protocols ;
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
  test_delayed_inbox_flushing_event protocols ;
  test_flushed_blueprint_reorg protocols ;
  test_multiple_flushed_blueprints protocols ;
  test_observer_reorg_on_blueprint_stream protocols ;
  test_observer_reorg_on_blueprint_catchup protocols ;
  test_flushed_blueprint_reorg_late protocols ;
  test_flushed_blueprint_reorg_done_late protocols ;
  test_upgrade_injected_before_flush_level protocols ;
  test_upgrade_injected_after_flush_level protocols ;
  test_upgrade_activated_after_flush_level protocols ;
  test_flushed_blueprint_reorg_upgrade protocols ;
  test_delayed_inbox_flushing protocols ;
  test_no_automatic_block_production protocols ;
  test_non_increasing_timestamp protocols ;
  test_timestamp_from_the_future protocols ;
  test_sequencer_upgrade protocols ;
  test_duplicate_sequencer_upgrade [Protocol.Alpha] ;
  test_sequencer_sunset protocols ;
  test_sequencer_diverge protocols ;
  test_sequencer_can_catch_up_on_event protocols ;
  test_sequencer_dont_read_level_twice protocols ;
  test_stage_one_reboot protocols ;
  test_blueprint_is_limited_in_size protocols ;
  test_blueprint_limit_with_delayed_inbox protocols ;
  test_reset protocols ;
  test_preimages_endpoint protocols ;
  test_preimages_endpoint_retry protocols ;
  test_store_smart_rollup_address protocols ;
  test_replay_rpc protocols ;
  test_trace_transaction protocols ;
  test_trace_transaction_on_invalid_transaction protocols ;
  test_trace_transaction_call protocols ;
  test_miner protocols ;
  test_fa_bridge_feature_flag protocols ;
  test_multichain_feature_flag protocols ;
  test_make_l2_kernel_installer_config "EVM" protocols ;
  test_make_l2_kernel_installer_config "Michelson" protocols ;
  test_fast_withdrawal_feature_flag protocols ;
  test_deposit_and_fast_withdraw protocols ;
  test_deposit_and_fa_fast_withdraw protocols ;
  test_fast_withdrawal_l2_caller protocols ;
  test_trace_call protocols ;
  test_trace_empty_block protocols ;
  test_trace_block protocols ;
  test_trace_block_txs_same_caller protocols ;
  test_trace_block_struct_logger protocols ;
  test_patch_kernel protocols ;
  test_observer_finalized_view protocols ;
  test_finalized_persistent protocols ;
  test_finalized_view protocols ;
  test_finalized_view_forward_txn protocols ;
  test_finalized_block_param protocols ;
  test_regression_block_hash_gen protocols ;
  test_sequencer_sandbox () ;
  test_rpc_mode_while_block_are_produced protocols ;
  test_trace_transaction_call_trace protocols ;
  test_relay_restricted_rpcs protocols ;
  test_batch_limit_size_rpc protocols ;
  test_trace_delegate_call protocols ;
  test_trace_transaction_calltracer_all_types protocols ;
  test_trace_transaction_call_tracer_with_logs protocols ;
  test_trace_transaction_call_revert protocols ;
  test_trace_transaction_call_trace_certain_depth protocols ;
  test_trace_transaction_call_trace_revert protocols ;
  test_trace_transaction_calltracer_multiple_txs protocols ;
  test_trace_transaction_calltracer_on_simple_transfer protocols ;
  test_trace_transaction_calltracer_precompiles protocols ;
  test_trace_transaction_calltracer_deposit protocols ;
  test_trace_transaction_calltracer_on_nested_delegatecalls [Alpha] ;
  test_debug_print_store_schemas () ;
  test_man () ;
  test_describe_config () ;
  test_outbox_size_limit_resilience ~slow:true protocols ;
  test_outbox_size_limit_resilience ~slow:false protocols ;
  test_batch_eth_send_raw_transaction_sync_rpc () ;
  test_tx_pool_pending_nonce () ;
  test_da_fees_after_execution protocols ;
  test_trace_transaction_calltracer_failed_create protocols ;
  test_configuration_service [Protocol.Alpha] ;
  test_clean_bps protocols ;
  test_produce_block_with_no_delayed_transactions protocols ;
  test_observer_reset [Protocol.Alpha] ;
  test_websocket_rpcs [Protocol.Alpha] ;
  test_websocket_subscription_rpcs_cant_be_called_via_http_requests
    [Protocol.Alpha] ;
  test_websocket_newHeads_event [Protocol.Alpha] ;
  test_websocket_cleanup [Protocol.Alpha] ;
  test_websocket_max_message_length () ;
  test_websocket_rate_limit `Close ;
  test_websocket_rate_limit `Error ;
  test_websocket_rate_limit `Wait ;
  test_websocket_frames_rate_limit () ;
  test_websocket_heartbeat_monitoring () ;
  test_websocket_newPendingTransactions_event [Protocol.Alpha] ;
  test_websocket_logs_event [Protocol.Alpha] ;
  test_websocket_tez_newIncludedTransactions_event [Protocol.Alpha] ;
  test_websocket_tez_newPreconfirmedReceipts_event [Protocol.Alpha] ;
  test_node_correctly_uses_batcher_heap [Protocol.Alpha] ;
  test_init_config_network "mainnet" ;
  test_init_config_network "testnet" ;
  test_estimate_gas_with_block_param protocols ;
  test_filling_max_slots_cant_lead_to_out_of_memory protocols ;
  test_rpc_getLogs_with_earliest_fail protocols ;
  test_eip2930_transaction_object [Alpha] ;
  test_eip1559_transaction_object [Alpha] ;
  test_apply_from_full_history_mode protocols ;
  test_tx_queue [Alpha] ;
  test_tx_queue_clear [Alpha] ;
  test_spawn_rpc protocols ;
  test_observer_init_from_snapshot protocols ;
  test_tx_queue_nonce [Alpha] ;
  test_tx_queue_limit [Alpha] ;
  test_observer_periodic_snapshot [Alpha] ;
  test_deposit_event [Alpha] ;
  test_withdrawal_events [Alpha] ;
  test_fa_deposit_and_withdrawals_events [Alpha] ;
  test_block_producer_validation () ;
  test_observer_divergence_fallback_on_instant_confirmations () ;
  test_durable_storage_consistency [Alpha] ;
  test_fa_deposit_can_be_claimed_and_withdrawn [Alpha] ;
  test_fast_fa_deposit_can_be_claimed_and_withdrawn [Alpha] ;
  test_claim_deposit_event [Alpha] ;
  test_sequencer_key_change [Alpha] ;
  test_sequencer_key_change_fails_if_governance_upgrade_exists [Alpha] ;
  test_eip2930_storage_access [Alpha] ;
  test_eip7702 [Alpha] ;
  test_deposits_on_eip7702_accounts [Alpha] ;
  test_eip7702_auto_sign [Alpha] ;
  test_validate_encoding_compatibility_accounts [Alpha] ;
  test_eip2537 [Alpha] ;
  test_eip3607_disabled_for_simulation [Alpha] ;
  test_fa_deposit_watchtower [Alpha] ;
  test_xtz_deposit_watchtower [Alpha] ;
  test_evm_events_cleanup () ;
  test_locked_tx_queue_timestamp ()
