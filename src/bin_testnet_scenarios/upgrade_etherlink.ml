(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(*
     Invocation :
        - dune exec src/bin_testnet_scenarios/main.exe -- -f upgrade_etherlink.ml -a network=mainnet --verbose --keep-temp -a upgrade-kernel=<NEW_KERNEL>
        - dune exec src/bin_testnet_scenarios/main.exe -- -f upgrade_etherlink.ml -a network=mainnet --verbose --keep-temp -a upgrade-kernel=<NEW_KERNEL> -a node-snapshot=<OCTEZ_NODE_ROLLING_SNAPSHOT> -a rollup-node-snapshot=<ROLLUP_NODE_FULL_SNAPSHOT>

     Note :
         - <..._SNAPSHOT> can either be a URL (the snapshot will be downloaded)
           or the path to a file (a symlink will be created).
         - network is either mainnet or testnet
*)

open Rpc.Syntax

let protocol_of_string s =
  match String.lowercase_ascii s with
  | "alpha" -> Protocol.Alpha
  | s -> failwith (sf "%s is an invalid protocol" s)

let bake ~env rollup_node node yes_wallet () =
  let* () =
    Client.bake_for_and_wait
      ~context_path:(Node.data_dir node)
      ~env
      ~keys:[]
      ~minimal_timestamp:true
      yes_wallet
  in
  let* _ = Sc_rollup_node.wait_sync rollup_node ~timeout:Float.max_float in
  unit

type network = Mainnet | Testnet

let network_of_string = function
  | "mainnet" -> Mainnet
  | "testnet" -> Testnet
  | _ -> Test.fail "Support only mainnet and testnet"

let network_to_string = function Mainnet -> "mainnet" | Testnet -> "ghostnet"

let rollup_of_network = function
  | Mainnet -> "sr1Ghq66tYK9y3r8CC1Tf8i8m5nxh8nTvZEf"
  | Testnet -> "sr18wx6ezkeRjt1SZSeZ2UQzQN3Uc3YLMLqg"

let endpoint_of_network = function
  | Mainnet -> "https://rpc.tzkt.io/mainnet"
  | Testnet -> "https://rpc.tzkt.io/ghostnet"

let rollup_node_snapshot_url_of_network = function
  | Mainnet ->
      "https://snapshots.eu.tzinit.org/etherlink-mainnet/eth-mainnet.full"
  | Testnet ->
      "https://snapshots.eu.tzinit.org/etherlink-ghostnet/eth-ghostnet.archive"

let node_snapshot_url_of_network = function
  | Mainnet -> "https://snapshots.eu.tzinit.org/mainnet/rolling"
  | Testnet -> "https://snapshots.eu.tzinit.org/ghostnet/rolling"

let yes_wallet ~network node =
  let yes_wallet = Temp.dir "yes_wallet" in
  let* () =
    Process.check
    @@ Process.spawn
         "dune"
         [
           "exec";
           "devtools/yes_wallet/yes_wallet.exe";
           "--";
           "create";
           "from";
           "context";
           Node.data_dir node;
           "in";
           yes_wallet;
           "--network";
           network_to_string network;
         ]
  in
  return (Client.create ~base_dir:yes_wallet ~endpoint:(Node node) ())

let run_and_bootstrap_rollup_node ~rollup_node_snapshot ~rollup network
    yes_wallet node =
  (* Create a rollup node. *)
  let rollup_node =
    Sc_rollup_node.create
      ~base_dir:(Client.base_dir yes_wallet)
      ~kind:"wasm_2_0_0"
      ~default_operator:"baker_58"
      ~gc_frequency:1_000_000
      Batcher
      node
  in
  (* Import the snapshot. *)
  let* rollup_node_snapshot =
    Scenario_helpers.fetch rollup_node_snapshot "rollup_node.snapshot"
  in
  let*? process =
    Sc_rollup_node.import_snapshot
      rollup_node
      ~snapshot_file:rollup_node_snapshot
      ~no_check:true
  in
  let* () = Process.check process in
  (* Bootstrap until the node level with an archive endpoint. *)
  let* node_level = Client.level yes_wallet in
  let endpoint = endpoint_of_network network in
  let* () = Sc_rollup_node.run ~endpoint rollup_node rollup [] in
  let* _ =
    Sc_rollup_node.wait_for_level
      ~timeout:Float.max_float
      rollup_node
      node_level
  in
  (* Restart the rollup node targetting the local node. *)
  let* () = Sc_rollup_node.terminate rollup_node in
  let* () = Sc_rollup_node.run rollup_node rollup [] in
  let* _ = Sc_rollup_node.wait_sync ~timeout:Float.max_float rollup_node in
  return rollup_node

(* Environment where yes crypto is enabled and fake time is enabled. *)
let env =
  String_map.add Tezos_crypto.Helpers.yes_crypto_environment_variable "y"
  @@ String_map.add
       "LD_PRELOAD"
       "/usr/lib/x86_64-linux-gnu/faketime/libfaketime.so.1"
  @@ String_map.add "FAKETIME" "@2026-01-01 11:15:13"
  @@ String_map.add "FAKETIME_DONT_RESET" "1"
  @@ String_map.empty

let patch_rollup_node ~rollup rollup_node dictator =
  let* () = Sc_rollup_node.terminate rollup_node in
  let* () =
    Sc_rollup_node.patch_durable_storage
      rollup_node
      ~key:"/evm/max_blueprint_lookahead_in_seconds"
      ~value:"8001660900000000"
  in
  let* () =
    Sc_rollup_node.patch_durable_storage
      rollup_node
      ~key:"/evm/sequencer"
      ~value:
        "6564706b754d376243755844514c50583559704b3859384264577a727667614d7146714a735736586852485135627446585956657644"
  in
  let* () =
    Sc_rollup_node.patch_durable_storage
      rollup_node
      ~key:"/evm/admin"
      ~value:(Hex.of_string dictator |> Hex.show)
  in
  Sc_rollup_node.run ~env rollup_node rollup [Sc_rollup_node.Log_kernel_debug]

let prepare_and_run_sequencer rollup_node =
  (* Create an evm node and patch the sequencer key. *)
  let sequencer =
    Evm_node.create
      ~node_setup:
        (Evm_node.make_setup
           ~preimages_dir:(Sc_rollup_node.data_dir rollup_node ^ "/wasm_2_0_0")
           ~private_rpc_port:8546
           ())
      ~mode:
        (Evm_node.Sequencer
           {
             rollup_node_endpoint = Sc_rollup_node.endpoint rollup_node;
             sequencer_config =
               {
                 time_between_blocks = Some Nothing;
                 genesis_timestamp = None;
                 max_number_of_chunks = None;
                 wallet_dir = None;
               };
             sequencer_keys =
               [
                 "unencrypted:edsk3tNH5Ye6QaaRQev3eZNcXgcN6sjCJRXChYFz42L6nKfRVwuL1n";
               ];
             max_blueprints_lag = None;
             max_blueprints_ahead = None;
             max_blueprints_catchup = None;
             catchup_cooldown = None;
             dal_slots = None;
             sequencer_sunset_sec = None;
           })
      ()
  in
  let process = Evm_node.spawn_init_config sequencer in
  let* () = Process.check process in
  let* () = Evm_node.init_from_rollup_node_data_dir sequencer rollup_node in
  let* () =
    Evm_node.patch_state
      sequencer
      ~key:"/evm/admin"
      ~value:
        "4b543152746f7271374166486b625561457764644243384d674d514c436d74336b656471"
  in
  let* () = Evm_node.run sequencer in
  return sequencer

let etherlink_upgrade () =
  (* Configuration variables. *)
  let network = Cli.get_string "network" |> network_of_string in
  let node_snapshot =
    Cli.get_string_opt "node-snapshot"
    |> Option.value ~default:(node_snapshot_url_of_network network)
  in
  let rollup_node_snapshot =
    Cli.get_string_opt "rollup-node-snapshot"
    |> Option.value ~default:(rollup_node_snapshot_url_of_network network)
  in
  let upgrade_kernel = Cli.get_string "upgrade-kernel" in
  let rollup = rollup_of_network network in

  (* Run a node connected to the network. *)
  let* _client, node =
    Scenario_helpers.setup_octez_node
      ~testnet:
        {
          network = network_to_string network;
          snapshot = Some node_snapshot;
          protocol = Protocol.Alpha;
          (* The protocol is not used to setup an octez node. *)
          data_dir = None;
          client_dir = None;
          operator = None;
        }
      ()
  in

  (* Produce a yes wallet. *)
  let* yes_wallet = yes_wallet ~network node in
  let* rollup_node =
    run_and_bootstrap_rollup_node
      ~rollup_node_snapshot
      ~rollup
      network
      yes_wallet
      node
  in
  (* Helper function that bakes and waits for the rollup node. *)
  let bake = bake ~env rollup_node node yes_wallet in
  (* Shutdown both octez node and rollup node and start the private network. *)
  let* () = Node.terminate node in
  let* () = Sc_rollup_node.terminate rollup_node in
  let* () =
    Node.run
      ~env
      node
      [
        Allow_yes_crypto;
        Bootstrap_threshold 0;
        Connections 0;
        Private_mode;
        No_bootstrap_peers;
      ]
  in
  let* () =
    Sc_rollup_node.run rollup_node rollup [Sc_rollup_node.Log_kernel_debug]
  in
  (* Bake a few blocks to make sure everything works. *)
  let* () = repeat 4 bake in
  (* Replace constants to fork Etherlink. *)
  let* admin = Client.show_address ~alias:"baker_60" yes_wallet in
  let* dictator =
    Client.originate_contract
      ~env
      ~alias:"dictator"
      ~amount:Tez.zero
      ~src:admin.alias
      ~prg:"etherlink/tezos_contracts/admin.tz"
      ~burn_cap:(Tez.of_int 5)
      ~init:(sf "%S" admin.public_key_hash)
      yes_wallet
  and* () = bake () in
  let* () = patch_rollup_node ~rollup rollup_node dictator in
  let* () = repeat 2 bake in
  (* Run a sequencer. *)
  let* sequencer = prepare_and_run_sequencer rollup_node in
  (* Prepare the upgrade. *)
  let* {root_hash; _} =
    Sc_rollup_helpers.prepare_installer_kernel_with_arbitrary_file
      ~preimages_dir:(Sc_rollup_node.data_dir rollup_node // "wasm_2_0_0")
      upgrade_kernel
  in
  let* upgrade_payload =
    Evm_node.upgrade_payload ~root_hash ~activation_timestamp:"0"
  in
  let* () =
    Client.transfer
      ~env
      ~amount:Tez.zero
      ~giver:admin.alias
      ~receiver:dictator
      ~arg:(sf {|Pair "%s" 0x%s|} rollup upgrade_payload)
      ~burn_cap:Tez.one
      yes_wallet
  in
  let* () = repeat 3 bake in
  let*@ _ = Rpc.produce_block sequencer in
  let* () = repeat 5 bake in

  unit

let register () =
  Test.register
    ~__FILE__
    ~title:"Fork Etherlink and upgrade it"
    ~tags:["etherlink"]
    etherlink_upgrade
