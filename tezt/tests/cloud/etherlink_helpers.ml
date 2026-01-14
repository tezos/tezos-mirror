(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Agent_kind
open Scenarios_helpers
open Tezos
open Yes_crypto

type etherlink_configuration = {
  etherlink_sequencer : bool;
  etherlink_producers : int;
  (* Empty list means DAL FF is set to false. *)
  etherlink_dal_slots : int list;
  chain_id : int option;
  tezlink : bool;
}

type etherlink_operator_setup = {
  node : Node.t;
  client : Client.t;
  sc_rollup_node : Sc_rollup_node.t;
  evm_node : Tezt_etherlink.Evm_node.t;
  is_sequencer : bool;
  sc_rollup_address : string;
  account : Account.key;
  batching_operators : Account.key list;
}

type etherlink = {
  configuration : etherlink_configuration;
  operator : etherlink_operator_setup;
  accounts : Tezt_etherlink.Eth_account.t Array.t;
}

let total_operator_balance ~client ~operators =
  List.fold_left
    (fun acc operator ->
      let* acc in
      let* balance =
        Client.get_full_balance_for
          ~account:operator.Account.public_key_hash
          client
      in
      return Tez.(acc + balance))
    (return Tez.zero)
    operators

let init_etherlink_operators ~client = function
  | None -> Lwt.return ([], [])
  | Some _ ->
      let* etherlink_rollup_operator_key =
        let () = toplog "Generating keys for Etherlink operator and batching" in
        Client.stresstest_gen_keys ~alias_prefix:"etherlink_operator" 1 client
      in
      let* etherlink_batching_operator_keys =
        Client.stresstest_gen_keys ~alias_prefix:"etherlink_batching" 20 client
      in
      return (etherlink_rollup_operator_key, etherlink_batching_operator_keys)

let init_etherlink_dal_node ~external_rpc ~network ~snapshot
    ~ppx_profiling_verbosity ~ppx_profiling_backends ~memtrace ~simulate_network
    ~node_p2p_endpoint ~dal_node_p2p_endpoint ~dal_slots ~next_agent ~otel
    ~cloud =
  match dal_slots with
  | [] ->
      toplog "Etherlink will run without DAL support" ;
      none
  | [_] ->
      (* On a single DAL slot index, we launch a single DAL node for
         this index on a dedicated VM and give it directly as endpoint
         to the rollup node. *)
      toplog "Etherlink sequencer will run its own DAL node" ;
      let name = name_of Etherlink_dal_operator in
      let* agent = next_agent ~name in
      let env, with_yes_crypto = may_set_yes_crypto_env simulate_network in
      let* node =
        Node_helpers.init
          ?env
          ~name
          ~arguments:
            [
              Peer node_p2p_endpoint;
              refutation_game_minimal_rolling_history_mode;
            ]
          ~rpc_external:external_rpc
          ~with_yes_crypto
          ~snapshot
          ~ppx_profiling_verbosity
          ~ppx_profiling_backends
          network
          cloud
          agent
      in
      let* dal_node = Dal_node.Agent.create ~name ~node cloud agent in
      let* () =
        Dal_node.init_config
          ~expected_pow:(Network.expected_pow network)
          ~operator_profiles:dal_slots
          ~peers:(Option.to_list dal_node_p2p_endpoint)
          dal_node
      in
      let* () =
        Dal_node.Agent.run
          ?otel
          ~ppx_profiling_verbosity
          ~ppx_profiling_backends
          dal_node
      in
      some dal_node
  | _ :: _ :: _ ->
      (* On several slot indices, we launch one observer DAL node per
         slot index + an operator DAL node on no index + a reverse proxy
         which is passed as endpoint to the rollup node.  The operator
         DAL node runs on the same VM than the reverse proxy, the
         observer DAL nodes run on dedicated VMs. *)
      toplog
        "Etherlink will run with DAL support on indices %a"
        (Format.pp_print_list
           ~pp_sep:(fun out () -> Format.fprintf out ",")
           Format.pp_print_int)
        dal_slots ;
      toplog "Etherlink sequencer will use a reverse proxy" ;
      let name = name_of Etherlink_dal_operator in
      let* agent = next_agent ~name in
      let env, with_yes_crypto = may_set_yes_crypto_env simulate_network in
      let* node =
        Node_helpers.init
          ?env
          ~name
          ~arguments:
            [
              Peer node_p2p_endpoint;
              refutation_game_minimal_rolling_history_mode;
            ]
          ~rpc_external:external_rpc
          ~with_yes_crypto
          ~snapshot
          ~ppx_profiling_verbosity
          ~ppx_profiling_backends
          network
          cloud
          agent
      in
      let* default_dal_node = Dal_node.Agent.create ~name ~node cloud agent in
      let* () =
        Dal_node.init_config
          ~expected_pow:(Network.expected_pow network)
          ~peers:(Option.to_list dal_node_p2p_endpoint)
          default_dal_node
      in
      let* () =
        Dal_node.Agent.run
          ?otel
          ~memtrace
          ~ppx_profiling_verbosity
          ~ppx_profiling_backends
          default_dal_node
      in
      let default_endpoint = Dal_node.rpc_endpoint default_dal_node in
      let* reverse_proxy_dal_node =
        Dal_reverse_proxy.init_dal_reverse_proxy_observers
          ~external_rpc
          ~network
          ~snapshot
          ~ppx_profiling_verbosity
          ~ppx_profiling_backends
          ~memtrace
          ~simulate_network
          ~name_of:(fun slot_index ->
            name_of (Etherlink_dal_observer {slot_index}))
          ~default_endpoint:(Some default_endpoint)
          ~node_p2p_endpoint
          ~dal_node_p2p_endpoint
          ~dal_slots
          ~index:0
          ~next_agent
          ~otel
          ~cloud
      in
      some reverse_proxy_dal_node

let init_etherlink_operator_setup cloud ~data_dir ~external_rpc ~network
    ~snapshot ~ppx_profiling_verbosity ~ppx_profiling_backends ~memtrace
    ~simulate_network etherlink_configuration name ~node_p2p_endpoint
    ~dal_node_p2p_endpoint ~dal_slots ~tezlink account batching_operators agent
    next_agent =
  let chain_id = Option.value ~default:1 etherlink_configuration.chain_id in
  let is_sequencer = etherlink_configuration.etherlink_sequencer in
  let data_dir = data_dir |> Option.map (fun data_dir -> data_dir // name) in
  let env, with_yes_crypto = may_set_yes_crypto_env simulate_network in
  let* node =
    Node_helpers.init
      ?env
      ?data_dir
      ~name
      ~arguments:
        [Peer node_p2p_endpoint; refutation_game_minimal_rolling_history_mode]
      ~rpc_external:external_rpc
      ~with_yes_crypto
      ~snapshot
      ~ppx_profiling_verbosity
      ~ppx_profiling_backends
      network
      cloud
      agent
  in
  let endpoint = Client.Node node in
  let* client = Client.Agent.create ~endpoint agent in
  let () = toplog "Init Etherlink: importing the sequencer secret key" in
  let* () =
    Lwt_list.iter_s
      (fun account ->
        Client.import_secret_key
          client
          ~endpoint
          account.Account.secret_key
          ~alias:account.Account.alias)
      (account :: batching_operators)
  in
  let l = Node.get_last_seen_level node in
  let () = toplog "Init Etherlink: revealing the sequencer account" in
  let* () =
    Lwt_list.iter_s
      (fun account ->
        let*! () = Client.reveal client ~endpoint ~src:account.Account.alias in
        unit)
      (account :: batching_operators)
  in
  let () = toplog "Init Etherlink operator: waiting for level %d" (l + 2) in
  let* _ = Node.wait_for_level node (l + 2) in
  let () = toplog "Init Etherlink: waiting for level %d: done" (l + 2) in
  (* A configuration is generated locally by the orchestrator. The resulting
     kernel will be pushed to Etherlink. *)
  let tezlink_config = Temp.file "l2-tezlink-config.yaml" in
  let tez_bootstrap_accounts = Account.Bootstrap.keys |> Array.to_list in
  let* () =
    if tezlink then
      let*! () =
        Evm_node.make_l2_kernel_installer_config
          ~chain_id
          ~chain_family:"Michelson"
          ~eth_bootstrap_accounts:[]
          ~tez_bootstrap_accounts
          ~output:tezlink_config
          ()
      in
      let* () = Process.spawn "cat" [tezlink_config] |> Process.check in
      unit
    else unit
  in
  let rollup_config = Temp.file "rollup-config.yaml" in
  let eth_bootstrap_accounts =
    Tezt_etherlink.Eth_account.bootstrap_accounts |> Array.to_list
    |> List.map (fun account -> account.Tezt_etherlink.Eth_account.address)
  in
  let*! () =
    let sequencer = if is_sequencer then Some account.public_key else None in
    let () = toplog "Init Etherlink: configuring the kernel" in
    Tezt_etherlink.Evm_node.make_kernel_installer_config
      ?sequencer
      ~eth_bootstrap_accounts
      ~output:rollup_config
      ~enable_dal:(Option.is_some dal_slots)
      ~chain_id
      ?dal_slots
      ~enable_multichain:tezlink
      ?l2_chain_ids:(if tezlink then Some [chain_id] else None)
      ()
  in
  let* () = Process.spawn "cat" [rollup_config] |> Process.check in
  let otel = Cloud.open_telemetry_endpoint cloud in
  let* dal_node =
    init_etherlink_dal_node
      ~external_rpc
      ~network
      ~snapshot
      ~ppx_profiling_verbosity
      ~ppx_profiling_backends
      ~memtrace
      ~simulate_network
      ~node_p2p_endpoint
      ~dal_node_p2p_endpoint
      ~next_agent
      ~dal_slots:etherlink_configuration.etherlink_dal_slots
      ~otel
      ~cloud
  in
  let operators =
    List.map
      (fun account -> (Sc_rollup_node.Batching, account.Account.alias))
      batching_operators
  in
  let* sc_rollup_node =
    Sc_rollup_node.Agent.create
      ~name:(name_of_daemon (Etherlink_sc_rollup_node name))
      ~base_dir:(Client.base_dir client)
      ~kind:"wasm_2_0_0"
      ~default_operator:account.alias
      ~operators
      ?dal_node
      cloud
      agent
      Operator
      node
  in
  let preimages_dir =
    Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) "wasm_2_0_0"
  in
  let* output =
    if tezlink then
      let* remote_rollup_config = Agent.copy agent ~source:rollup_config in
      let* remote_tezlink_config = Agent.copy agent ~source:tezlink_config in
      let* {output; _} =
        Sc_rollup_helpers.Agent
        .prepare_installer_kernel_with_multiple_setup_file
          ~configs:[remote_rollup_config; remote_tezlink_config]
          ~preimages_dir
          (Uses.path Constant.WASM.evm_kernel)
          agent
      in
      return output
    else
      let* remote_rollup_config = Agent.copy agent ~source:rollup_config in
      let* {output; _} =
        Sc_rollup_helpers.Agent.prepare_installer_kernel
          ~config:(`Path remote_rollup_config)
          ~preimages_dir
          Constant.WASM.evm_kernel
          agent
      in
      return output
  in
  let pvm_kind = "wasm_2_0_0" in
  let l = Node.get_last_seen_level node in
  let () = toplog "Init Etherlink: originating the rollup" in
  let* sc_rollup_address =
    Sc_rollup_helpers.Agent.originate_sc_rollup
      ~kind:pvm_kind
      ~boot_sector:output
      ~parameters_ty:Tezt_etherlink.Rollup.evm_type
      ~src:account.alias
      client
  in
  let () = toplog "Init Etherlink: waiting again, for level %d" (l + 2) in
  let* _ = Node.wait_for_level node (l + 2) in
  let () = toplog "Init Etherlink: waiting again, for level %d: done" (l + 2) in
  let () = toplog "Init Etherlink: launching the rollup node" in
  let* () =
    Sc_rollup_node.run sc_rollup_node sc_rollup_address [Log_kernel_debug]
  in
  let () = toplog "Init Etherlink: launching the rollup node: done" in
  let private_rpc_port = Agent.next_available_port agent |> Option.some in
  let time_between_blocks = Some (Evm_node.Time_between_blocks 10.) in
  let sequencer_mode =
    Evm_node.Sequencer
      {
        initial_kernel = output;
        preimage_dir = Some preimages_dir;
        private_rpc_port;
        time_between_blocks;
        sequencer_keys = [account.alias];
        genesis_timestamp = None;
        max_blueprints_lag = Some 300;
        max_blueprints_ahead = Some 2000;
        max_blueprints_catchup = None;
        catchup_cooldown = None;
        max_number_of_chunks = None;
        wallet_dir = Some (Client.base_dir client);
        tx_queue_max_lifespan = None;
        tx_queue_max_size = None;
        tx_queue_tx_per_addr_limit = None;
        dal_slots;
        sequencer_sunset_sec = None;
      }
  in
  let endpoint = Sc_rollup_node.endpoint sc_rollup_node in
  let mode = if is_sequencer then sequencer_mode else Evm_node.Proxy in
  let () = toplog "Init Etherlink: launching the EVM node" in
  let* evm_node =
    Tezos.Evm_node.Agent.init
      ~patch_config:(fun json ->
        JSON.update
          "public_rpc"
          (fun json ->
            JSON.update
              "cors_headers"
              (fun _ ->
                JSON.annotate ~origin:"patch-config:cors_headers"
                @@ `A [`String "*"])
              json
            |> JSON.update "cors_origins" (fun _ ->
                   JSON.annotate ~origin:"patch-config:cors_origins"
                   @@ `A [`String "*"]))
          json
        |> Evm_node.patch_config_with_experimental_feature
             ~drop_duplicate_when_injection:true
             ~blueprints_publisher_order_enabled:true
             ~rpc_server:Resto
             ~spawn_rpc:(Port.fresh ())
             ?l2_chains:
               (if tezlink then
                  Some
                    [
                      {
                        (Evm_node.default_l2_setup ~l2_chain_id:chain_id) with
                        l2_chain_family = "Michelson";
                        tez_bootstrap_accounts = Some tez_bootstrap_accounts;
                      };
                    ]
                else None)
             ())
      ~name:(name_of_daemon (Etherlink_evm_node name))
      ~mode
      endpoint
      cloud
      agent
  in
  let () = toplog "Init Etherlink: launching the EVM node: done" in
  let operator =
    {
      node;
      client;
      sc_rollup_node;
      evm_node;
      is_sequencer;
      account;
      batching_operators;
      sc_rollup_address;
    }
  in
  let* () =
    add_prometheus_source
      ?dal_node
      ~node
      ~sc_rollup_node
      ~evm_node
      cloud
      agent
      (Format.asprintf "etherlink-%s" name)
  in
  return operator

let init_etherlink_producer_setup operator name ~node_p2p_endpoint ~rpc_external
    cloud agent =
  let* node =
    Node.Agent.init
      ~rpc_external
      ~name:(name_of_daemon (Etherlink_producer_node name))
      ~arguments:[Peer node_p2p_endpoint; Synchronisation_threshold 0]
      cloud
      agent
  in
  let endpoint = Client.Node node in
  let* client = Client.Agent.create ~endpoint agent in
  let l = Node.get_last_seen_level node in
  let* _ = Node.wait_for_level node (l + 2) in
  (* A configuration is generated locally by the orchestrator. The resulting
     kernel will be pushed to Etherlink. *)
  let output_config = Temp.file "config.yaml" in
  let eth_bootstrap_accounts =
    Tezt_etherlink.Eth_account.bootstrap_accounts |> Array.to_list
    |> List.map (fun account -> account.Tezt_etherlink.Eth_account.address)
  in
  let*! () =
    let sequencer =
      if operator.is_sequencer then Some operator.account.public_key else None
    in
    Tezt_etherlink.Evm_node.make_kernel_installer_config
      ?sequencer
      ~eth_bootstrap_accounts
      ~output:output_config
      ()
  in
  let* sc_rollup_node =
    Sc_rollup_node.Agent.create
      ~name:(name_of_daemon (Etherlink_sc_rollup_node name))
      ~base_dir:(Client.base_dir client)
      ~kind:"wasm_2_0_0"
      cloud
      agent
      Observer
      node
  in
  let preimages_dir =
    Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) "wasm_2_0_0"
  in
  let* remote_output_config = Agent.copy agent ~source:output_config in
  let* {output; _} =
    Sc_rollup_helpers.Agent.prepare_installer_kernel
      ~config:(`Path remote_output_config)
      ~preimages_dir
      Constant.WASM.evm_kernel
      agent
  in
  let* () =
    Sc_rollup_node.run
      sc_rollup_node
      operator.sc_rollup_address
      [Log_kernel_debug]
  in
  let mode =
    Evm_node.Observer
      {
        private_rpc_port = None;
        initial_kernel = Some output;
        preimages_dir = Some preimages_dir;
        rollup_node_endpoint = Some (Sc_rollup_node.endpoint sc_rollup_node);
        tx_queue_max_lifespan = None;
        tx_queue_max_size = None;
        tx_queue_tx_per_addr_limit = None;
      }
  in
  let () = toplog "Init Etherlink: init producer %s" name in
  let endpoint = Evm_node.endpoint operator.evm_node in
  (* TODO: try using this local EVM node for Floodgate confirmations. *)
  let* evm_node =
    Evm_node.Agent.init
      ~name:(name_of_daemon (Etherlink_evm_node name))
      ~mode
      endpoint
      cloud
      agent
  in
  let* () =
    (* This is to avoid producing operations too soon. *)
    Evm_node.wait_for_blueprint_applied evm_node 1
  in
  (* Launch floodgate *)
  let* () =
    Floodgate.Agent.run
      ~rpc_endpoint:endpoint
      ~max_active_eoa:210
      ~max_transaction_batch_length:70
      ~tick_interval:1.0
      ~controller:Tezt_etherlink.Eth_account.bootstrap_accounts.(0)
      ~base_fee_factor:1000.0
      cloud
      agent
  in
  Lwt.return_unit

let init_etherlink cloud ~data_dir ~external_rpc ~network ~snapshot
    ~ppx_profiling_verbosity ~ppx_profiling_backends ~memtrace ~simulate_network
    etherlink_configuration ~node_p2p_endpoint ~dal_node_p2p_endpoint
    etherlink_rollup_operator_key batching_operators ~dal_slots ~tezlink
    next_agent =
  let () = toplog "Initializing an Etherlink operator" in
  let name = name_of Etherlink_operator in
  let* operator_agent = next_agent ~name in
  let* operator =
    init_etherlink_operator_setup
      cloud
      ~data_dir
      ~external_rpc
      ~network
      ~snapshot
      ~ppx_profiling_verbosity
      ~ppx_profiling_backends
      ~memtrace
      ~simulate_network
      etherlink_configuration
      "operator"
      ~node_p2p_endpoint
      ~dal_node_p2p_endpoint
      ~dal_slots
      ~tezlink
      etherlink_rollup_operator_key
      batching_operators
      operator_agent
      next_agent
  in
  let accounts = Tezt_etherlink.Eth_account.bootstrap_accounts in
  let* producers_agents =
    List.init etherlink_configuration.etherlink_producers (fun i ->
        let name = name_of (Etherlink_producer i) in
        next_agent ~name)
    |> Lwt.all
  in
  let* () =
    producers_agents
    |> List.mapi (fun i agent ->
           assert (i < Array.length accounts) ;
           init_etherlink_producer_setup
             operator
             (Format.asprintf "producer-%d" i)
             ~node_p2p_endpoint
             ~rpc_external:external_rpc
             cloud
             agent)
    |> Lwt.join
  in
  return {configuration = etherlink_configuration; operator; accounts}

let init_etherlink ~data_dir ~simulate_network ~external_rpc ~network ~snapshot
    ~ppx_profiling_verbosity ~ppx_profiling_backends ~memtrace
    ~node_p2p_endpoint ~dal_node_p2p_endpoint ~next_agent ~cloud
    etherlink_rollup_operator_key etherlink_batching_operator_keys = function
  | Some etherlink_configuration ->
      let () = toplog "Init: initializing Etherlink" in
      let () = toplog "Init: Getting Etherlink operator key" in
      let etherlink_rollup_operator_key =
        Option.get etherlink_rollup_operator_key
      in
      let () = toplog "Init: Getting allowed DAL slot indices" in
      let dal_slots =
        match etherlink_configuration.etherlink_dal_slots with
        | [] -> None
        | slots -> Some slots
      in
      let () =
        toplog "Init: Etherlink+DAL feature flag: %b" (Option.is_some dal_slots)
      in
      let* etherlink =
        let () = toplog "Init: calling init_etherlink" in
        init_etherlink
          cloud
          ~data_dir
          ~external_rpc
          ~network
          ~snapshot
          ~ppx_profiling_verbosity
          ~ppx_profiling_backends
          ~memtrace
          ~simulate_network
          etherlink_configuration
          ~node_p2p_endpoint
          ~dal_node_p2p_endpoint
          etherlink_rollup_operator_key
          etherlink_batching_operator_keys
          next_agent
          ~dal_slots
          ~tezlink:etherlink_configuration.tezlink
      in
      some etherlink
  | None ->
      let () =
        toplog
          "Init: skipping Etherlink initialization because --etherlink was not \
           given on the CLI"
      in
      none
