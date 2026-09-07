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
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file node_cli.ml
   Subject:      The evm-node command line and configuration surface.
*)

open Sc_rollup_helpers
open Rpc.Syntax
open Test_helpers
open Setup

let test_make_l2_kernel_installer_config chain_family =
  Test.register
    ~__FILE__
    ~title:
      (Printf.sprintf
         "Test that the command make_l2_kernel_installer setup bootstrap \
          account at the right path with %s family"
         chain_family)
    ~tags:["evm"; "multichain"; "installer"]
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
      (Evm_node.make_kernel_setup (* No need for a real sequencer governance *)
         ~sequencer_governance:"KT1"
         ~l2_chain_ids:[chain_id_1; chain_id_2]
         ())
      ~output:rollup_config
      ()
  in

  let preimages_dir = Temp.dir "wasm_2_0_0" in
  let* {output = kernel; _} =
    prepare_installer_kernel_with_multiple_setup_file
      ~output:(Temp.file "kernel.hex")
      ~preimages_dir
      ~configs:[rollup_config; l2_config_1; l2_config_2]
      (Kernel.path Latest)
  in

  (* Setup a sequencer with a private rpc port to verify the durable storage. *)
  let sequencer_config : Evm_node.sequencer_config =
    {
      time_between_blocks = Some Nothing;
      genesis_timestamp = None;
      max_number_of_chunks = None;
      wallet_dir = None;
    }
  in
  let sequencer_mode =
    Evm_node.Sandbox
      {
        sequencer_config;
        sequencer_keys = [];
        funded_addresses = [];
        network = None;
        michelson_hard_gas_limit_per_block = None;
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

module Protocol = Test_helpers.Protocol_no_direct_register

let test_kernel_activation_dry_run_success =
  (* Add a delay between first block and activation timestamp. *)
  let genesis_timestamp_notation = "2020-01-01T00:00:00Z" in
  let genesis_timestamp =
    Client.(At (Time.of_notation_exn genesis_timestamp_notation))
  in
  let activation_timestamp = "2020-01-01T00:00:10Z" in
  register_all
    ~__FILE__
    ~sequencer:Constant.bootstrap1
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "preimages_endpoint"]
    ~title:"Kernel activation dry-run success is advertised"
    ~kernels:[Mainnet]
    ~additional_uses:[Constant.WASM.evm_kernel]
    ~genesis_timestamp
  @@
  fun {
        sc_rollup_node;
        l1_contracts;
        sc_rollup_address;
        client;
        sequencer;
        observer = _;
        _;
      }
      _protocol
    ->
  let provider_port = Port.fresh () in
  Sc_rollup_helpers.serve_files
    ~name:"preimages_server"
    ~port:provider_port
    ~root:(Sc_rollup_node.data_dir sc_rollup_node // "wasm_2_0_0")
  @@ fun () ->
  let preimages_endpoint =
    sf "http://%s:%d" Constant.default_host provider_port
  in
  let* () = Evm_node.terminate sequencer in
  let* root_hash =
    upgrade
      ~sc_rollup_node
      ~sc_rollup_address
      ~admin:Constant.bootstrap2.public_key_hash
      ~admin_contract:l1_contracts.admin
      ~client
      ~upgrade_to:Constant.WASM.evm_kernel
      ~activation_timestamp
  in
  let* () =
    Evm_node.run
      ~extra_arguments:["--preimages-endpoint"; preimages_endpoint]
      sequencer
  in
  let success =
    Evm_node.wait_for_kernel_activation_dry_run sequencer ~root_hash
  in
  let failure =
    Evm_node.wait_for_kernel_activation_dry_run_failed sequencer ~root_hash
  in
  let* _ =
    repeat 2 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~client ~sc_rollup_node in
        unit)
  in
  let* () =
    Lwt.pick
      [
        success;
        (let* () = failure in
         Test.fail "Should not have reported a failure");
      ]
  in
  (* Demonstrate the sequencer can still progress (i.e., the dry-run state is
     correctly thrown away) *)
  let* _ = Evm_node.wait_for_blueprint_applied sequencer 1
  and* _ = produce_block ~timestamp:genesis_timestamp_notation sequencer in
  unit

let test_kernel_activation_dry_run_failure =
  (* Add a delay between first block and activation timestamp. *)
  let genesis_timestamp_notation = "2020-01-01T00:00:00Z" in
  let genesis_timestamp =
    Client.(At (Time.of_notation_exn genesis_timestamp_notation))
  in
  let activation_timestamp = "2020-01-01T00:00:10Z" in
  register_all
    ~__FILE__
    ~sequencer:Constant.bootstrap1
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "preimages_endpoint"]
    ~title:"Kernel activation dry-run failure is advertised"
    ~kernels:[Latest]
    ~additional_uses:[Constant.WASM.sink]
    ~genesis_timestamp
  @@
  fun {
        sc_rollup_node;
        l1_contracts;
        sc_rollup_address;
        client;
        sequencer;
        observer = _;
        _;
      }
      _protocol
    ->
  let provider_port = Port.fresh () in
  Sc_rollup_helpers.serve_files
    ~name:"preimages_server"
    ~port:provider_port
    ~root:(Sc_rollup_node.data_dir sc_rollup_node // "wasm_2_0_0")
  @@ fun () ->
  let preimages_endpoint =
    sf "http://%s:%d" Constant.default_host provider_port
  in
  let* () = Evm_node.terminate sequencer in
  let* root_hash =
    upgrade
      ~sc_rollup_node
      ~sc_rollup_address
      ~admin:Constant.bootstrap2.public_key_hash
      ~admin_contract:l1_contracts.admin
      ~client
      ~upgrade_to:Constant.WASM.sink
      ~activation_timestamp
  in
  let* () =
    Evm_node.run
      ~extra_arguments:["--preimages-endpoint"; preimages_endpoint]
      sequencer
  in
  let success =
    Evm_node.wait_for_kernel_activation_dry_run sequencer ~root_hash
  in
  let failure =
    Evm_node.wait_for_kernel_activation_dry_run_failed sequencer ~root_hash
  in
  let* _ =
    repeat 2 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~client ~sc_rollup_node in
        unit)
  in
  let* () =
    Lwt.pick
      [
        failure;
        (let* () = success in
         Test.fail "Should not have reported a success");
      ]
  in
  (* Demonstrate the sequencer can still progress (i.e., the dry-run state is
     correctly thrown away) *)
  let* _ = Evm_node.wait_for_blueprint_applied sequencer 1
  and* _ = produce_block ~timestamp:genesis_timestamp_notation sequencer in
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
    ~kernels:[Mainnet]
    ~additional_uses:[Constant.WASM.evm_kernel]
    ~genesis_timestamp
  @@
  fun {
        sc_rollup_node;
        l1_contracts;
        sc_rollup_address;
        client;
        sequencer;
        observer = _;
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
  let new_sequencer =
    Evm_node.create ~node_setup:(Evm_node.make_setup ()) ~mode:sequencer_mode ()
  in
  let* () = Process.check @@ Evm_node.spawn_init_config new_sequencer in
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
      ~upgrade_to:Constant.WASM.evm_kernel
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

(* Prepares a fresh sequencer, with an empty preimages directory, whose store
   holds an upgrade that is announced but not activated yet. Returns the
   sequencer, the root hash of the announced kernel, and the port and endpoint
   its preimages are expected to be served on — no server listens there yet. *)
let setup_pending_upgrade ~sc_rollup_node ~l1_contracts ~sc_rollup_address
    ~client ~sequencer ~activation_timestamp () =
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
  let* () = Evm_node.terminate sequencer in
  let finalizeL1 () =
    repeat 2 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  (* Prepares the sequencer without [preimages-dir], to force the use of the
     preimages endpoint. Not passing ~preimages_dir to create means it will be
     None. *)
  let sequencer_mode =
    match Evm_node.mode sequencer with
    | Evm_node.Sequencer _ as mode -> mode
    | _ -> Test.fail "the tests below need a sequencer"
  in
  let new_sequencer =
    Evm_node.create ~node_setup:(Evm_node.make_setup ()) ~mode:sequencer_mode ()
  in
  let* () = Process.check @@ Evm_node.spawn_init_config new_sequencer in
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
      ~upgrade_to:Constant.WASM.evm_kernel
      ~activation_timestamp
  in
  let* () = finalizeL1 () in
  let provider_port = Port.fresh () in
  let preimages_endpoint =
    sf "http://%s:%d" Constant.default_host provider_port
  in
  return (new_sequencer, root_hash, provider_port, preimages_endpoint)

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
    ~kernels:[Mainnet]
    ~additional_uses:[Constant.WASM.evm_kernel]
    ~genesis_timestamp
  @@
  fun {sc_rollup_node; l1_contracts; sc_rollup_address; client; sequencer; _}
      _protocol
    ->
  let* new_sequencer, root_hash, provider_port, preimages_endpoint =
    setup_pending_upgrade
      ~sc_rollup_node
      ~l1_contracts
      ~sc_rollup_address
      ~client
      ~sequencer
      ~activation_timestamp
      ()
  in
  let* () =
    Evm_node.run
      ~extra_arguments:["--preimages-endpoint"; preimages_endpoint]
      new_sequencer
  in
  let* () =
    Evm_node.wait_for_predownload_kernel_failed
      ~timeout:120.
      new_sequencer
      ~root_hash
  in
  (* Create a file server that serves the preimages. *)
  let served = ref false in
  Sc_rollup_helpers.serve_files
    ~name:"preimages_server"
    ~port:provider_port
    ~root:(Sc_rollup_node.data_dir sc_rollup_node // "wasm_2_0_0")
    ~on_request:(fun _ -> served := true)
  @@ fun () ->
  let* () =
    Evm_node.wait_for_predownload_kernel ~timeout:90. new_sequencer ~root_hash
  in
  Check.is_true
    !served
    ~error_msg:"The sequencer should have used the file server" ;
  unit

let test_preimages_endpoint_restart =
  (* Add a delay between first block and activation timestamp. *)
  let genesis_timestamp =
    Client.(At (Time.of_notation_exn "2020-01-01T00:00:00Z"))
  in
  let activation_timestamp = "2020-01-01T00:00:10Z" in
  register_all
    ~__FILE__
    ~sequencer:Constant.bootstrap1
    ~time_between_blocks:Nothing
    ~tags:["evm"; "sequencer"; "preimages_endpoint"; "restart"]
    ~title:"Sequencer gets the preimages of a pending upgrade after a restart"
    ~kernels:[Mainnet]
    ~additional_uses:[Constant.WASM.evm_kernel]
    ~genesis_timestamp
  @@
  fun {sc_rollup_node; l1_contracts; sc_rollup_address; client; sequencer; _}
      _protocol
    ->
  let* new_sequencer, root_hash, provider_port, preimages_endpoint =
    setup_pending_upgrade
      ~sc_rollup_node
      ~l1_contracts
      ~sc_rollup_address
      ~client
      ~sequencer
      ~activation_timestamp
      ()
  in
  let run () =
    Evm_node.run
      ~extra_arguments:["--preimages-endpoint"; preimages_endpoint]
      new_sequencer
  in
  (* The node learns about the upgrade, but no server serves its preimages
     yet. *)
  let* () = run () in
  let* () =
    Evm_node.wait_for_predownload_kernel_failed
      ~timeout:120.
      new_sequencer
      ~root_hash
  in
  (* The requests of the node are processed one at a time, so a block produced
     now is produced after the announcement has been committed to the store.
     The restarted node therefore has a pending upgrade, and cannot apply the
     announcement a second time: the download it starts can only come from the
     restart. The block is produced before the activation timestamp, so the
     upgrade is not applied. *)
  let*@ _ = produce_block ~timestamp:"2020-01-01T00:00:09Z" new_sequencer in
  let* () = Evm_node.terminate new_sequencer in
  (* The preimages become available while the node is down. *)
  let served = ref false in
  Sc_rollup_helpers.serve_files
    ~name:"preimages_server"
    ~port:provider_port
    ~root:(Sc_rollup_node.data_dir sc_rollup_node // "wasm_2_0_0")
    ~on_request:(fun _ -> served := true)
  @@ fun () ->
  let* () = run () in
  let* () =
    Evm_node.wait_for_predownload_kernel ~timeout:90. new_sequencer ~root_hash
  in
  Check.is_true
    !served
    ~error_msg:"The sequencer should have used the file server" ;
  unit

let test_evm_node_flag =
  register_all
    ~__FILE__
    ~tags:["evm_node"; "flag"]
    ~title:"EVM node flag is set in durable storage"
    ~use_dal:Register_without_feature
  @@ fun {sequencer; kernel; _} _protocol ->
  let key = Durable_storage_path.evm_node_flag kernel in
  let*@ flag = Rpc.state_value sequencer key in
  Check.is_true
    (Option.is_some flag)
    ~error_msg:(sf "Expected evm_node flag to be set at %s" key) ;
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
    Setup.setup_sequencer ~enable_dal:false protocol
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

let protocols = [Protocol.Alpha]

let () =
  test_preimages_endpoint protocols ;
  test_preimages_endpoint_retry protocols ;
  test_preimages_endpoint_restart protocols ;
  test_evm_node_flag [Alpha] ;
  test_make_l2_kernel_installer_config "EVM" ;
  test_make_l2_kernel_installer_config "Michelson" ;
  test_sequencer_sandbox () ;
  test_debug_print_store_schemas () ;
  test_man () ;
  test_describe_config () ;
  test_configuration_service [Protocol.Alpha] ;
  test_init_config_network "mainnet" ;
  test_init_config_network "testnet" ;
  test_kernel_activation_dry_run_success [Alpha] ;
  test_kernel_activation_dry_run_failure [Alpha]
