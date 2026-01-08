(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

module Cryptobox = Dal_common.Cryptobox
module Helpers = Dal_common.Helpers
module Cli = Scenarios_cli
open Agent_kind
open Scenarios_helpers
open Tezos
open Yes_crypto

type configuration = {
  with_dal : bool;
  stake : int list Lwt.t;
  bakers : string list; (* unencrypted secret keys *)
  stake_machine_type : string list;
  dal_node_producers : int list; (* slot indices *)
  observer_slot_indices : int list;
  observers_multi_slot_indices : int list list;
  archivers_slot_indices : int list list;
  observer_pkhs : string list;
  protocol : Protocol.t;
  producer_machine_type : string option;
  (* The first argument is the deconnection frequency, the second is the
     reconnection delay *)
  echo_rollups : int;
  disconnect : (int * int) option;
  network : Network.t;
  simulate_network : Network_simulation.t;
  snapshot : Snapshot_helpers.t;
  bootstrap : bool;
  teztale : bool;
  memtrace : bool;
  data_dir : string option;
  producer_key : string option;
  fundraiser : string option;
  producers_delay : int;
  blocks_history : int;
  bootstrap_node_identity_file : string option;
  bootstrap_dal_node_identity_file : string option;
  external_rpc : bool;
  disable_shard_validation : bool;
  disable_amplification : bool;
  ignore_pkhs : string list;
  ppx_profiling_verbosity : string option;
  ppx_profiling_backends : string list;
  network_health_monitoring : bool;
  daily_logs_destination : string option;
  slot_size : int option;
  number_of_slots : int option;
  attestation_lag : int option;
  traps_fraction : Q.t option;
}

type bootstrap = {
  node : Node.t option;
  dal_node : Dal_node.t option;
  node_p2p_endpoint : string;
  node_rpc_endpoint : Endpoint.t;
  dal_node_p2p_endpoint : string option;
  client : Client.t;
}

type t = {
  configuration : configuration;
  cloud : Cloud.t;
  bootstrap : bootstrap;
  some_node_rpc_endpoint : Endpoint.t;
  (* endpoint to be used for get various information about L1; for testnets, it
     is a public endpoint only if no L1 node is run by the scenario, in contrast
     to [bootstrap.node_rpc_endpoint] which is a public endpoint when the
     '--bootstrap' argument is not provided *)
  bakers : Baker_helpers.baker list;
  producers : Dal_node_helpers.producer list;
      (* NOTE: they have the observer profile*)
  observers : Dal_node_helpers.observer list;
  archivers : Dal_node_helpers.archiver list;
  etherlink : Etherlink_helpers.etherlink option;
  echo_rollups : Echo_rollup.operator list;
  time_between_blocks : int;
  parameters : Dal_common.Parameters.t;
  infos : (int, Metrics.per_level_info) Hashtbl.t;
  metrics : (int, Metrics.t) Hashtbl.t;
  disconnection_state : Disconnect.t option;
  first_level : int;
  teztale : Teztale.t option;
  mutable versions : (string, string) Hashtbl.t;
      (* mapping from baker addresses to their octez versions (if known) *)
  otel : string option;
}

let get_infos_per_level t ~level ~metadata =
  let open Metrics in
  let client = t.bootstrap.client in
  let endpoint = t.some_node_rpc_endpoint in
  let etherlink_operators =
    match t.etherlink with
    | None -> []
    | Some setup -> setup.operator.account :: setup.operator.batching_operators
  in
  let cycle = JSON.(metadata |-> "level_info" |-> "cycle" |> as_int) in
  let* operations =
    let block = string_of_int level in
    RPC_core.call endpoint @@ RPC.get_chain_block_operations ~block ()
  in
  let attested_commitments =
    JSON.(metadata |-> "dal_attestation" |> as_string |> Z.of_string)
  in
  let manager_operation_batches = JSON.(operations |=> 3 |> as_list) in
  let is_published_commitment operation =
    JSON.(operation |-> "kind" |> as_string = "dal_publish_commitment")
  in
  let get_commitment operation =
    JSON.(operation |-> "slot_header" |-> "commitment" |> as_string)
  in
  let get_publisher operation = JSON.(operation |-> "source" |> as_string) in
  let commitment_info operation =
    let commitment = get_commitment operation in
    let publisher_pkh = get_publisher operation in
    {commitment; publisher_pkh}
  in
  let get_slot_index operation =
    JSON.(operation |-> "slot_header" |-> "slot_index" |> as_int)
  in
  let published_commitments =
    manager_operation_batches |> List.to_seq
    |> Seq.concat_map (fun batch ->
           JSON.(batch |-> "contents" |> as_list |> List.to_seq))
    |> Seq.filter is_published_commitment
    |> Seq.map (fun operation ->
           (get_slot_index operation, commitment_info operation))
    |> Hashtbl.of_seq
  in
  let consensus_operations = JSON.(operations |=> 0 |> as_list) in
  let get_dal_attestations operation =
    let contents = JSON.(operation |-> "contents" |=> 0) in
    let kind = JSON.(contents |-> "kind" |> as_string) in
    match kind with
    | "attestation_with_dal" ->
        let pkh = JSON.(contents |-> "metadata" |-> "delegate" |> as_string) in
        let slot = JSON.(contents |-> "slot" |> as_int) in
        let dal =
          if slot >= 512 then Out_of_committee
          else
            With_DAL
              JSON.(contents |-> "dal_attestation" |> as_string |> Z.of_string)
        in
        [(PKH pkh, dal)]
    | "attestations_aggregate" ->
        let metadata_committee =
          JSON.(contents |-> "metadata" |-> "committee" |> as_list)
        in
        let committee_info = JSON.(contents |-> "committee" |> as_list) in
        List.map2
          (fun member_info committee_meta ->
            let slot = JSON.(member_info |-> "slot" |> as_int) in
            let dal =
              if slot >= 512 then Out_of_committee
              else
                let json = JSON.(member_info |-> "dal_attestation") in
                if JSON.is_null json then Without_DAL
                else With_DAL (json |> JSON.as_string |> Z.of_string)
            in
            let pkh = JSON.(committee_meta |-> "delegate" |> as_string) in
            (PKH pkh, dal))
          committee_info
          metadata_committee
    | "attestation" ->
        let pkh = JSON.(contents |-> "metadata" |-> "delegate" |> as_string) in
        let slot = JSON.(contents |-> "slot" |> as_int) in
        let dal = if slot >= 512 then Out_of_committee else Without_DAL in
        [(PKH pkh, dal)]
    | _ -> []
  in
  let* attestation_rights =
    RPC_core.call endpoint
    @@ RPC.get_chain_block_helper_attestation_rights ~level ()
  in
  (* We fill the [attestations] table with [Expected_to_DAL_attest] when a baker is in the DAL committee. *)
  let attestations =
    JSON.(attestation_rights |-> "delegates" |> as_list |> List.to_seq)
    |> Seq.filter_map (fun delegate ->
           let slot = JSON.(delegate |-> "first_slot" |> as_int) in
           if slot < 512 then
             let pkh = PKH JSON.(delegate |-> "delegate" |> as_string) in
             Some (pkh, Expected_to_DAL_attest)
           else None)
    |> Hashtbl.of_seq
  in
  (* And then update the [attestations] table with the attestations actually received. *)
  let () =
    consensus_operations
    |> List.iter (fun operation ->
           let dal_attestations = get_dal_attestations operation in
           List.iter
             (fun (pkh, dal_status) ->
               Hashtbl.replace attestations pkh dal_status)
             dal_attestations)
  in
  let* etherlink_operator_balance_sum =
    Etherlink_helpers.total_operator_balance
      ~client
      ~operators:etherlink_operators
  in
  (* None of these actions are performed if `--dal-slack-webhook` is
     not provided. *)
  let* () =
    if t.configuration.network_health_monitoring then
      let open Monitoring_app.Alert in
      let* () =
        check_for_dal_accusations
          ~cloud:t.cloud
          ~network:t.configuration.network
          ~cycle
          ~level
          ~operations
          ~endpoint:t.some_node_rpc_endpoint
      in
      let* () =
        check_for_recently_missed_a_lot
          ~cloud:t.cloud
          ~endpoint:t.some_node_rpc_endpoint
          ~network:t.configuration.network
          ~level
          ~metadata
      in
      unit
    else Lwt.return_unit
  in
  let* echo_rollup_fetched_data =
    Echo_rollup.fetch_echo_rollup_data
      ~echo_rollup:(List.nth_opt t.echo_rollups 0)
      ~dal_node_producers:t.configuration.dal_node_producers
      ~level
  in
  Lwt.return
    {
      level;
      published_commitments;
      attestations;
      attested_commitments;
      etherlink_operator_balance_sum;
      echo_rollup_fetched_data;
    }

let init_teztale (configuration : configuration) cloud agent =
  if configuration.teztale then init_teztale cloud agent |> Lwt.map Option.some
  else Lwt.return_none

let init_public_network cloud (configuration : configuration)
    etherlink_configuration teztale agent network =
  toplog "Init public network" ;
  let* bootstrap =
    match agent with
    | None ->
        let () = toplog "No agent given" in
        let node = None in
        let dal_node = None in
        let node_p2p_endpoint = Network.default_bootstrap network in
        let dal_node_p2p_endpoint =
          Some (Network.default_dal_bootstrap network)
        in
        let node_rpc_endpoint = Network.public_rpc_endpoint network in
        let () = toplog "Creating the client" in
        let client =
          Client.create ~endpoint:(Foreign_endpoint node_rpc_endpoint) ()
        in
        let bootstrap =
          {
            node;
            dal_node;
            node_p2p_endpoint;
            node_rpc_endpoint;
            dal_node_p2p_endpoint;
            client;
          }
        in
        Lwt.return bootstrap
    | Some agent ->
        let () = toplog "Some agent given (%s)" (Agent.name agent) in
        let () = toplog "Initializing the bootstrap node agent" in
        let with_yes_crypto =
          should_enable_yes_crypto configuration.simulate_network
        in
        let* node =
          Node_helpers.init
            ?identity_file:configuration.bootstrap_node_identity_file
            ~rpc_external:configuration.external_rpc
            ~name:"bootstrap-node"
            configuration.network
            ~with_yes_crypto
            ~snapshot:configuration.snapshot
            ~ppx_profiling_verbosity:configuration.ppx_profiling_verbosity
            ~ppx_profiling_backends:configuration.ppx_profiling_backends
            cloud
            agent
        in
        let* dal_node =
          if not configuration.with_dal then Lwt.return_none
          else
            let disable_shard_validation =
              configuration.disable_shard_validation
            in
            let* dal_node =
              Dal_node.Agent.create
                ~name:"bootstrap-dal-node"
                cloud
                agent
                ~node
                ~disable_shard_validation
            in
            let* () =
              Dal_node.init_config
                ~expected_pow:26.
                ~bootstrap_profile:true
                dal_node
            in
            let* () =
              Dal_node_helpers.may_copy_dal_node_identity_file
                agent
                dal_node
                configuration.bootstrap_dal_node_identity_file
            in
            let* () = Node.wait_for_ready node in
            let otel = Cloud.open_telemetry_endpoint cloud in
            let* () =
              Dal_node.Agent.run
                ~prometheus:Tezt_cloud_cli.prometheus
                ?otel
                ~memtrace:configuration.memtrace
                ~event_level:`Notice
                ~disable_shard_validation
                ~ppx_profiling_verbosity:configuration.ppx_profiling_verbosity
                ~ppx_profiling_backends:configuration.ppx_profiling_backends
                dal_node
            in
            Lwt.return_some dal_node
        in
        let client = Client.create ~endpoint:(Node node) () in
        let node_rpc_endpoint =
          Endpoint.make
            ~scheme:"http"
            ~host:
              (match Agent.point agent with
              | None -> "127.0.0.1"
              | Some point -> fst point)
            ~port:(Node.rpc_port node)
            ()
        in
        let bootstrap =
          {
            node = Some node;
            dal_node;
            node_p2p_endpoint = Node.point_str node;
            node_rpc_endpoint;
            dal_node_p2p_endpoint = Option.map Dal_node.point_str dal_node;
            client;
          }
        in
        let* () =
          match teztale with
          | None -> Lwt.return_unit
          | Some teztale ->
              Teztale.add_archiver
                teztale
                cloud
                agent
                ~node_name:(Node.name node)
                ~node_port:(Node.rpc_port node)
        in
        Lwt.return bootstrap
  in
  let () = toplog "Initializing the bakers" in
  let* stake = configuration.stake in
  let* baker_accounts =
    Lwt_list.mapi_s
      (fun i _stake ->
        let* delegates =
          (* We assume that a baker holds only one key. *)
          Client.stresstest_gen_keys
            ~alias_prefix:(Format.sprintf "baker-%d" i)
            1
            bootstrap.client
        in
        List.map
          (fun delegate -> Baker_helpers.{delegate; consensus_key = None})
          delegates
        |> return)
      stake
  in
  let* producer_accounts =
    Dal_node_helpers.init_producer_accounts
      ~client:bootstrap.client
      ~producer_key:configuration.producer_key
      ~dal_node_producers:configuration.dal_node_producers
  in
  let* etherlink_rollup_operator_key, etherlink_batching_operator_keys =
    Etherlink_helpers.init_etherlink_operators
      ~client:bootstrap.client
      etherlink_configuration
  in
  let* echo_rollup_keys =
    Client.stresstest_gen_keys
      ~alias_prefix:"echo_operator"
      configuration.echo_rollups
      bootstrap.client
  in
  let accounts_to_fund =
    (if configuration.producer_key = None then
       List.map (fun producer -> (producer, 10 * 1_000_000)) producer_accounts
     else
       (* When a producer key has been given, it is assumed to have money and should not be funded. *)
       [])
    @ List.map
        (fun operator -> (operator, 11_000 * 1_000_000))
        etherlink_rollup_operator_key
    @ List.map
        (fun batcher -> (batcher, 10 * 1_000_000))
        etherlink_batching_operator_keys
    @ List.map (fun operator -> (operator, 11_000 * 1_000_000)) echo_rollup_keys
  in
  let* () =
    Dal_node_helpers.fund_producers_accounts
      ~client:bootstrap.client
      ~fundraiser:configuration.fundraiser
      accounts_to_fund
  in
  let etherlink_rollup_operator_key =
    match etherlink_rollup_operator_key with key :: _ -> Some key | [] -> None
  in
  Lwt.return
    ( bootstrap,
      baker_accounts,
      producer_accounts,
      etherlink_rollup_operator_key,
      etherlink_batching_operator_keys,
      echo_rollup_keys )

let round_robin_split m lst =
  assert (m > 0) ;
  let buckets = Array.make m [] in
  List.iteri
    (fun idx x ->
      let bucket = idx mod m in
      buckets.(bucket) <- x :: buckets.(bucket))
    lst ;
  Array.to_list buckets |> List.map List.rev

let init_sandbox_and_activate_protocol cloud (configuration : configuration)
    ?(etherlink_configuration :
       Etherlink_helpers.etherlink_configuration option) agent =
  let dal_bootstrap_node_net_port = Agent.next_available_port agent in
  let dal_config : Cryptobox.Config.t =
    {
      activated = true;
      bootstrap_peers =
        [Format.asprintf "127.0.0.1:%d" dal_bootstrap_node_net_port];
    }
  in
  let name = "bootstrap-node" in
  let data_dir =
    configuration.data_dir |> Option.map (fun data_dir -> data_dir // name)
  in
  let env, with_yes_crypto =
    may_set_yes_crypto_env configuration.simulate_network
  in
  let* bootstrap_node =
    Node_helpers.init
      ?env
      ?data_dir
      ?identity_file:configuration.bootstrap_node_identity_file
      ~rpc_external:configuration.external_rpc
      ~dal_config
      ~name
      configuration.network
      ~with_yes_crypto
      ~snapshot:configuration.snapshot
      ~ppx_profiling_verbosity:configuration.ppx_profiling_verbosity
      ~ppx_profiling_backends:configuration.ppx_profiling_backends
      cloud
      agent
  in
  let* () = Node.wait_for_ready bootstrap_node in
  let* () = init_explorus cloud bootstrap_node in
  let* dal_bootstrap_node =
    if configuration.with_dal then
      let* dal_node =
        Dal_node.Agent.create
          ~net_port:dal_bootstrap_node_net_port
          ~name:"bootstrap-dal-node"
          cloud
          agent
          ~node:bootstrap_node
          ~disable_shard_validation:configuration.disable_shard_validation
          ~disable_amplification:configuration.disable_amplification
      in
      Lwt.return_some dal_node
    else Lwt.return_none
  in
  let* stake = configuration.stake in
  let* client =
    if configuration.simulate_network = Disabled then
      Client.init ~endpoint:(Node bootstrap_node) ()
    else
      let* client =
        Client.Agent.create
          ~name:(Tezt_cloud.Agent.name agent ^ "-client")
          ~endpoint:(Client.Node bootstrap_node)
          agent
      in
      let* yes_wallet = Node_helpers.yes_wallet agent in
      let* snapshot_network =
        match configuration.snapshot with
        | Docker_embedded path | Local_file path ->
            let* network =
              Snapshot_helpers.get_snapshot_info_network bootstrap_node path
            in
            (* Yes-wallet requires the config url for protocol-specific test
               networks.*)
            let network =
              match network with
              | ("ghostnet" | "mainnet" | "sandbox") as s -> s
              | s ->
                  (* We assume that unknown networks are protocol specific ones. *)
                  "https://teztnets.com/" ^ s
            in
            Lwt.return network
        | Url _url ->
            (* FIXME: We can overcome this by either downloading the snapshot, or by
               retrieving the name of the network from the URL. I am not sure how much
               of a priority this is. *)
            Lwt.fail_with "URL snapshot not available for sandbox case"
        | No_snapshot -> Lwt.return "ghostnet"
      in
      let* _filename =
        Yes_wallet.create_from_context
          ~node:bootstrap_node
          ~client
          ~network:snapshot_network
          yes_wallet
      in
      return client
  in
  (* [delegate_accounts] stands for the list of delegates keys (registered
     baking account) that will be used by the producers daemons. These producers
     need accounts with funds in order to inject DAL publish commitment
     operations. *)
  let* delegate_accounts =
    match configuration.simulate_network with
    | Scatter (selected_bakers_count, baker_daemons_count) ->
        let* all_delegates_aliases =
          Client.list_known_addresses client |> Lwt.map (List.map fst)
        in
        let selected_delegates =
          Tezos_stdlib.TzList.take_n selected_bakers_count all_delegates_aliases
        in
        let* delegates =
          Lwt_list.map_s
            (fun alias -> Client.show_address ~alias client)
            selected_delegates
        in
        round_robin_split baker_daemons_count delegates |> Lwt.return
    | Map
        ( selected_bakers_count,
          single_key_baker_daemons_count,
          multiple_keys_baker_daemons_count ) ->
        let* all_delegates_aliases =
          Client.list_known_addresses client |> Lwt.map (List.map fst)
        in
        let selected_delegates_aliases =
          Tezos_stdlib.TzList.take_n selected_bakers_count all_delegates_aliases
        in
        let single_key_bakers_aliases, remaining_baker_aliases =
          Tezos_stdlib.TzList.split_n
            single_key_baker_daemons_count
            selected_delegates_aliases
        in
        let* single_key_bakers =
          Lwt_list.map_s
            (fun alias ->
              let* a = Client.show_address ~alias client in
              Lwt.return [a])
            single_key_bakers_aliases
        in
        let* remaining_bakers =
          let* r =
            Lwt_list.map_s
              (fun alias -> Client.show_address ~alias client)
              remaining_baker_aliases
          in
          round_robin_split multiple_keys_baker_daemons_count r |> Lwt.return
        in
        Lwt.return (single_key_bakers @ remaining_bakers)
    | Disabled ->
        Lwt_list.mapi_s
          (fun i _stake ->
            (* We assume that a baker holds only one key. *)
            Client.stresstest_gen_keys
              ~alias_prefix:(Format.sprintf "baker-%d" i)
              1
              client)
          stake
  in
  (* [baker_accounts] stands for the list of [baker_account] that are actually
     used for baking. Meaning that if a baker uses a consensus key, the
     [baker_account.consensus_key] will hold the associated consensus key. *)
  let* baker_accounts =
    match configuration.simulate_network with
    | Disabled ->
        (* Generated baker accounts are not using any consensus key. *)
        List.map
          (fun l ->
            List.map
              (fun delegate -> Baker_helpers.{delegate; consensus_key = None})
              l)
          delegate_accounts
        |> return
    | Map _ | Scatter _ ->
        (* Substitute consensus pkh with delegate pkh *)
        let* yw = Node_helpers.yes_wallet agent in
        let* ckm = Yes_wallet.load_consensus_key_mapping yw ~client in
        List.map
          (fun l ->
            List.map
              (fun (delegate : Account.key) ->
                try
                  let ck =
                    List.find
                      (fun {Yes_wallet.public_key_hash; _} ->
                        public_key_hash = delegate.public_key_hash)
                      ckm
                  in
                  let consensus_key =
                    Some
                      {
                        Account.alias = delegate.alias;
                        public_key_hash = ck.consensus_public_key_hash;
                        public_key = ck.consensus_public_key;
                        secret_key =
                          (* That's ok, because we're using yes-crypto. *)
                          Account.Unencrypted
                            Tezos_crypto.Signature.(to_b58check zero);
                      }
                  in
                  Baker_helpers.{delegate; consensus_key}
                with Not_found -> {delegate; consensus_key = None})
              l)
          delegate_accounts
        |> return
  in
  List.iteri
    (fun i l ->
      toplog
        "Baker agent %d will run for the following delegates: %a"
        i
        (Format.pp_print_list
           ~pp_sep:(fun out () -> Format.fprintf out ",")
           (fun fmt Baker_helpers.{delegate; _} ->
             Format.fprintf fmt "%s" delegate.alias))
        l)
    baker_accounts ;
  let* producer_accounts =
    match configuration.simulate_network with
    | Scatter _ | Map _ ->
        Tezos_stdlib.TzList.take_n
          (List.length configuration.dal_node_producers)
          (List.flatten delegate_accounts)
        |> return
    | Disabled ->
        Client.stresstest_gen_keys
          ~alias_prefix:"dal_producer"
          (List.length configuration.dal_node_producers)
          client
  in
  let* etherlink_rollup_operator_key, etherlink_batching_operator_keys =
    Etherlink_helpers.init_etherlink_operators ~client etherlink_configuration
  in
  let* echo_rollup_keys =
    Client.stresstest_gen_keys
      ~alias_prefix:"echo_operator_key"
      configuration.echo_rollups
      client
  in
  let* () =
    if configuration.simulate_network = Disabled then
      let* parameter_file =
        let base =
          Either.right (configuration.protocol, Some Protocol.Constants_mainnet)
        in
        let bootstrap_accounts =
          List.mapi
            (fun i Baker_helpers.{delegate; _} ->
              ( delegate,
                Some
                  {
                    Protocol.balance =
                      Some (List.nth stake i * 1_000_000_000_000);
                    consensus_key = None;
                    delegate = None;
                  } ))
            (List.flatten baker_accounts)
        in
        let additional_bootstrap_accounts =
          List.map
            (fun key ->
              ( key,
                Some
                  {
                    Protocol.balance = Some 1_000_000_000_000;
                    consensus_key = None;
                    delegate = None;
                  },
                false ))
            (producer_accounts @ etherlink_rollup_operator_key
           @ etherlink_batching_operator_keys @ echo_rollup_keys)
        in
        let overrides =
          (match configuration.slot_size with
          | Some slot_size ->
              [(["dal_parametric"; "slot_size"], `Int slot_size)]
          | None -> [])
          @ (match configuration.number_of_slots with
            | Some number_of_slots ->
                [(["dal_parametric"; "number_of_slots"], `Int number_of_slots)]
            | None -> [])
          @ (match configuration.attestation_lag with
            | Some attestation_lag ->
                [(["dal_parametric"; "attestation_lag"], `Int attestation_lag)]
            | None -> [])
          @
          match configuration.traps_fraction with
          | Some {num; den} ->
              [
                ( ["dal_parametric"; "traps_fraction"; "numerator"],
                  `String (Z.to_string num) );
                ( ["dal_parametric"; "traps_fraction"; "denominator"],
                  `String (Z.to_string den) );
              ]
          | None -> []
        in
        Protocol.write_parameter_file
          ~overwrite_bootstrap_accounts:(Some bootstrap_accounts)
          ~additional_bootstrap_accounts
          ~base
          overrides
      in
      let* () =
        Client.activate_protocol_and_wait
          ~timestamp:Client.Now
          ~parameter_file
          ~protocol:configuration.protocol
          client
      in
      Lwt.return_unit
    else Lwt.return_unit
  in
  let etherlink_rollup_operator_key =
    match etherlink_rollup_operator_key with [key] -> Some key | _ -> None
  in
  let* () =
    match dal_bootstrap_node with
    | None -> Lwt.return_unit
    | Some dal_bootstrap_node ->
        let* () =
          Dal_node.init_config
            ~expected_pow:0.
            ~bootstrap_profile:true
            dal_bootstrap_node
        in
        let otel = Cloud.open_telemetry_endpoint cloud in
        let* () =
          Dal_node_helpers.may_copy_dal_node_identity_file
            agent
            dal_bootstrap_node
            configuration.bootstrap_dal_node_identity_file
        in
        Dal_node.Agent.run
          ~prometheus:Tezt_cloud_cli.prometheus
          ?otel
          ~memtrace:configuration.memtrace
          ~event_level:`Notice
          ~disable_shard_validation:configuration.disable_shard_validation
          ~ppx_profiling_verbosity:configuration.ppx_profiling_verbosity
          ~ppx_profiling_backends:configuration.ppx_profiling_backends
          dal_bootstrap_node
  in
  let node_rpc_endpoint =
    Endpoint.make
      ~scheme:"http"
      ~host:
        (match Agent.point agent with
        | None -> "127.0.0.1"
        | Some point -> fst point)
      ~port:(Node.rpc_port bootstrap_node)
      ()
  in
  let (bootstrap : bootstrap) =
    {
      node = Some bootstrap_node;
      dal_node = dal_bootstrap_node;
      node_p2p_endpoint = Node.point_str bootstrap_node;
      node_rpc_endpoint;
      dal_node_p2p_endpoint = Option.map Dal_node.point_str dal_bootstrap_node;
      client;
    }
  in
  Lwt.return
    ( bootstrap,
      baker_accounts,
      producer_accounts,
      etherlink_rollup_operator_key,
      etherlink_batching_operator_keys,
      echo_rollup_keys )

let obtain_some_node_rpc_endpoint agent network (bootstrap : bootstrap)
    (bakers : Baker_helpers.baker list)
    (producers : Dal_node_helpers.producer list)
    (observers : Dal_node_helpers.observer list)
    (archivers : Dal_node_helpers.archiver list) etherlink =
  match (agent, network) with
  | None, #Network.public -> (
      match (bakers, producers, observers, archivers, etherlink) with
      | baker :: _, _, _, _, _ -> Node.as_rpc_endpoint baker.node
      | [], producer :: _, _, _, _ -> Node.as_rpc_endpoint producer.node
      | [], [], observer :: _, _, _ -> Node.as_rpc_endpoint observer.node
      | [], [], [], archiver :: _, _ -> Node.as_rpc_endpoint archiver.node
      | [], [], [], [], Some etherlink ->
          Node.as_rpc_endpoint etherlink.Etherlink_helpers.operator.node
      | [], [], [], [], None -> bootstrap.node_rpc_endpoint)
  | _ -> bootstrap.node_rpc_endpoint

let init ~(configuration : configuration) etherlink_configuration cloud
    next_agent =
  let () = toplog "Init" in
  let () = toplog "Agents preparation" in
  let* bootstrap_agent =
    if configuration.bootstrap then
      let name = name_of Bootstrap in
      let* agent = next_agent ~name in
      Lwt.return_some agent
    else Lwt.return_none
  in
  let* producers_agents =
    Lwt_list.map_s
      (fun slot_index ->
        let name = name_of (Producer slot_index) in
        let* agent = next_agent ~name in
        return (agent, slot_index))
      configuration.dal_node_producers
  in
  let* observers_slot_index_agents =
    Lwt_list.map_s
      (fun slot_index ->
        let name = name_of (Observer (`Indexes [slot_index])) in
        let* agent = next_agent ~name in
        return (`Slot_indexes [slot_index], agent))
      configuration.observer_slot_indices
  in
  let* observers_multi_slot_index_agents =
    Lwt_list.map_s
      (fun slot_indexes ->
        let name = name_of (Observer (`Indexes slot_indexes)) in
        let* agent = next_agent ~name in
        return (`Slot_indexes slot_indexes, agent))
      configuration.observers_multi_slot_indices
  in
  let* archivers_slot_index_agents =
    Lwt_list.map_s
      (fun slot_indexes ->
        let name = name_of (Archiver (`Indexes slot_indexes)) in
        let* agent = next_agent ~name in
        return (`Slot_indexes slot_indexes, agent))
      configuration.archivers_slot_indices
  in
  let* observers_bakers_agents =
    Lwt_list.map_s
      (fun pkh ->
        let name = name_of (Observer (`Pkh pkh)) in
        let* agent = next_agent ~name in
        return (`Pkh pkh, agent))
      configuration.observer_pkhs
  in
  let* teztale =
    match bootstrap_agent with
    | None -> Lwt.return_none
    | Some agent -> init_teztale configuration cloud agent
  in
  let* ( bootstrap,
         baker_accounts,
         producer_accounts,
         etherlink_rollup_operator_key,
         etherlink_batching_operator_keys,
         echo_rollup_keys ) =
    match configuration.network with
    | `Sandbox ->
        let bootstrap_agent = Option.get bootstrap_agent in
        init_sandbox_and_activate_protocol
          cloud
          configuration
          ?etherlink_configuration
          bootstrap_agent
    | #Network.public ->
        let network = Network.to_public configuration.network in
        init_public_network
          cloud
          configuration
          etherlink_configuration
          teztale
          bootstrap_agent
          network
  in
  let* bakers =
    Baker_helpers.init_bakers
      ~bakers:configuration.bakers
      ~stake:configuration.stake
      ~data_dir:configuration.data_dir
      ~simulate_network:configuration.simulate_network
      ~external_rpc:configuration.external_rpc
      ~network:configuration.network
      ~snapshot:configuration.snapshot
      ~ppx_profiling_verbosity:configuration.ppx_profiling_verbosity
      ~ppx_profiling_backends:configuration.ppx_profiling_backends
      ~memtrace:configuration.memtrace
      ~with_dal:configuration.with_dal
      ~disable_shard_validation:configuration.disable_shard_validation
      ~node_p2p_endpoint:bootstrap.node_p2p_endpoint
      ~dal_node_p2p_endpoint:bootstrap.dal_node_p2p_endpoint
      cloud
      teztale
      ~baker_accounts
      next_agent
  in
  let () = toplog "Init: initializing producers and observers" in
  let* producers =
    Lwt_list.mapi_p
      (fun i ((agent, slot_index), account) ->
        Dal_node_helpers.init_producer
          cloud
          ~data_dir:configuration.data_dir
          ~simulate_network:configuration.simulate_network
          ~external_rpc:configuration.external_rpc
          ~network:configuration.network
          ~snapshot:configuration.snapshot
          ~memtrace:configuration.memtrace
          ~ppx_profiling_verbosity:configuration.ppx_profiling_verbosity
          ~ppx_profiling_backends:configuration.ppx_profiling_backends
          ~ignore_pkhs:configuration.ignore_pkhs
          ~disable_shard_validation:configuration.disable_shard_validation
          ~disable_amplification:configuration.disable_amplification
          ~node_p2p_endpoint:bootstrap.node_p2p_endpoint
          ~dal_node_p2p_endpoint:bootstrap.dal_node_p2p_endpoint
          teztale
          account
          i
          slot_index
          agent)
      (List.combine producers_agents producer_accounts)
  and* observers =
    Lwt_list.mapi_p
      (fun i (topic, agent) ->
        Dal_node_helpers.init_observer
          cloud
          ~data_dir:configuration.data_dir
          ~simulate_network:configuration.simulate_network
          ~external_rpc:configuration.external_rpc
          ~network:configuration.network
          ~snapshot:configuration.snapshot
          ~memtrace:configuration.memtrace
          ~ppx_profiling_verbosity:configuration.ppx_profiling_verbosity
          ~ppx_profiling_backends:configuration.ppx_profiling_backends
          ~disable_shard_validation:configuration.disable_shard_validation
          ~disable_amplification:configuration.disable_amplification
          ~node_p2p_endpoint:bootstrap.node_p2p_endpoint
          ~dal_node_p2p_endpoint:bootstrap.dal_node_p2p_endpoint
          teztale
          ~topic
          i
          agent)
      (observers_slot_index_agents @ observers_multi_slot_index_agents
     @ observers_bakers_agents)
  and* archivers =
    Lwt_list.mapi_p
      (fun i (topic, agent) ->
        Dal_node_helpers.init_archiver
          cloud
          ~data_dir:configuration.data_dir
          ~simulate_network:configuration.simulate_network
          ~external_rpc:configuration.external_rpc
          ~network:configuration.network
          ~snapshot:configuration.snapshot
          ~memtrace:configuration.memtrace
          ~ppx_profiling_verbosity:configuration.ppx_profiling_verbosity
          ~ppx_profiling_backends:configuration.ppx_profiling_backends
          ~disable_shard_validation:configuration.disable_shard_validation
          ~disable_amplification:configuration.disable_amplification
          ~node_p2p_endpoint:bootstrap.node_p2p_endpoint
          ~dal_node_p2p_endpoint:bootstrap.dal_node_p2p_endpoint
          teztale
          ~topic
          i
          agent)
      archivers_slot_index_agents
  in
  let () = toplog "Init: all producers and observers have been initialized" in
  let* echo_rollups =
    Lwt_list.mapi_s
      (fun index echo_rollup_key ->
        Echo_rollup.init_echo_rollup
          cloud
          ~data_dir:configuration.data_dir
          ~simulate_network:configuration.simulate_network
          ~external_rpc:configuration.external_rpc
          ~network:configuration.network
          ~snapshot:configuration.snapshot
          ~ppx_profiling_verbosity:configuration.ppx_profiling_verbosity
          ~ppx_profiling_backends:configuration.ppx_profiling_backends
          ~memtrace:configuration.memtrace
          ~node_p2p_endpoint:bootstrap.node_p2p_endpoint
          ~dal_node_p2p_endpoint:bootstrap.dal_node_p2p_endpoint
          ~next_agent
          producers
          index
          echo_rollup_key)
      echo_rollup_keys
  in
  let* etherlink =
    Etherlink_helpers.init_etherlink
      ~data_dir:configuration.data_dir
      ~simulate_network:configuration.simulate_network
      ~external_rpc:configuration.external_rpc
      ~network:configuration.network
      ~snapshot:configuration.snapshot
      ~ppx_profiling_verbosity:configuration.ppx_profiling_verbosity
      ~ppx_profiling_backends:configuration.ppx_profiling_backends
      ~memtrace:configuration.memtrace
      ~node_p2p_endpoint:bootstrap.node_p2p_endpoint
      ~dal_node_p2p_endpoint:bootstrap.dal_node_p2p_endpoint
      ~next_agent
      ~cloud
      etherlink_rollup_operator_key
      etherlink_batching_operator_keys
      etherlink_configuration
  in
  let some_node_rpc_endpoint =
    obtain_some_node_rpc_endpoint
      bootstrap_agent
      configuration.network
      bootstrap
      bakers
      producers
      observers
      archivers
      etherlink
  in
  let* constants =
    RPC_core.call
      some_node_rpc_endpoint
      (RPC.get_chain_block_context_constants_parametric ())
  in
  let time_between_blocks =
    JSON.(constants |-> "minimal_block_delay" |> as_int)
  in
  let* parameters =
    Dal_common.Parameters.from_endpoint some_node_rpc_endpoint
  in
  let infos = Hashtbl.create 101 in
  let metrics = Hashtbl.create 101 in
  let* first_level =
    match configuration.network with
    | `Sandbox -> (
        match configuration.simulate_network with
        | Disabled -> Lwt.return 1
        | Scatter _ | Map _ -> Network.get_level some_node_rpc_endpoint)
    | _ -> Network.get_level some_node_rpc_endpoint
  in
  Hashtbl.replace metrics first_level Metrics.default ;
  let disconnection_state =
    Option.map Disconnect.init configuration.disconnect
  in
  let* init_aliases =
    let accounts =
      let open Baker_helpers in
      List.concat_map
        (fun ({accounts; _} : baker) ->
          List.map (fun {delegate; _} -> delegate) accounts)
        bakers
    in
    Network.aliases ~accounts configuration.network
  in
  let* versions = Network.versions configuration.network in
  Metrics.merge_aliases init_aliases ;
  let versions = Option.value ~default:(Hashtbl.create 0) versions in
  let otel = Cloud.open_telemetry_endpoint cloud in
  (* Adds monitoring for all agents for octez-dal-node and octez-node
     TODO: monitor only specific agents for specific binaries *)
  Lwt.return
    {
      cloud;
      configuration;
      bootstrap;
      some_node_rpc_endpoint;
      bakers;
      producers;
      observers;
      archivers;
      echo_rollups;
      etherlink;
      time_between_blocks;
      parameters;
      infos;
      metrics;
      disconnection_state;
      first_level;
      teztale;
      versions;
      otel;
    }

let wait_for_level t level =
  match t.bootstrap.node with
  | None ->
      let rec loop () =
        let* head_level = Network.get_level t.some_node_rpc_endpoint in
        if head_level >= level then Lwt.return_unit
        else
          let* () = Lwt_unix.sleep 4. in
          loop ()
      in
      loop ()
  | Some node ->
      let* _ = Node.wait_for_level node level in
      Lwt.return_unit

let clean_up t level =
  Hashtbl.remove t.infos level ;
  Hashtbl.remove t.metrics level

let update_bakers_infos t =
  let open Baker_helpers in
  let* new_aliases =
    let accounts =
      List.(
        concat_map
          (fun ({accounts; _} : baker) ->
            List.map (fun {delegate; _} -> delegate) accounts)
          t.bakers)
    in
    Network.aliases ~accounts t.configuration.network
  in
  let* versions = Network.versions t.configuration.network in
  Metrics.merge_aliases new_aliases ;
  let versions = Option.value ~default:t.versions versions in
  t.versions <- versions ;
  Lwt.return_unit

let on_new_level t level ~metadata =
  toplog "Start processing level %d" level ;
  clean_up t (level - t.configuration.blocks_history) ;
  let* () =
    if level mod 1_000 = 0 then update_bakers_infos t else Lwt.return_unit
  in
  let* infos_per_level = get_infos_per_level t ~level ~metadata in
  toplog "Level %d's info processed" level ;
  Hashtbl.replace t.infos level infos_per_level ;
  let metrics =
    Metrics.get
      ~first_level:t.first_level
      ~attestation_lag:t.parameters.attestation_lag
      ~dal_node_producers:t.configuration.dal_node_producers
      ~number_of_slots:t.parameters.number_of_slots
      ~infos:t.infos
      infos_per_level
      (Hashtbl.find t.metrics (level - 1))
  in
  Metrics.pp ~bakers:t.bakers metrics ;
  Metrics.push ~versions:t.versions ~cloud:t.cloud metrics ;
  Hashtbl.replace t.metrics level metrics ;
  let* t =
    match t.disconnection_state with
    | Some disconnection_state when t.configuration.with_dal ->
        let nb_bakers = List.length t.bakers in
        let* disconnection_state =
          Disconnect.disconnect disconnection_state level (fun b ->
              let baker_to_disconnect = List.nth t.bakers (b mod nb_bakers) in
              (* Invariant: Option.get don't fail because t.configuration.dal is true *)
              let dal_node = baker_to_disconnect.dal_node |> Option.get in
              Dal_node.Agent.terminate dal_node)
        in
        let* disconnection_state =
          Disconnect.reconnect disconnection_state level (fun b ->
              let baker_to_reconnect = List.nth t.bakers (b mod nb_bakers) in
              (* Invariant: Option.get don't fail because t.configuration.dal is true *)
              let dal_node = baker_to_reconnect.dal_node |> Option.get in
              Dal_node.Agent.run
                ~prometheus:Tezt_cloud_cli.prometheus
                ?otel:t.otel
                ~memtrace:t.configuration.memtrace
                ~ppx_profiling_verbosity:t.configuration.ppx_profiling_verbosity
                ~ppx_profiling_backends:t.configuration.ppx_profiling_backends
                dal_node)
        in
        Lwt.return {t with disconnection_state = Some disconnection_state}
    | _ -> Lwt.return t
  in
  toplog "Level %d processed" level ;
  Lwt.return t

let on_new_cycle t ~level =
  let endpoint = t.some_node_rpc_endpoint in
  let last_block_of_prev_cycle = string_of_int (level - 1) in
  let* metadata =
    RPC_core.call endpoint
    @@ RPC.get_chain_block_metadata_raw ~block:last_block_of_prev_cycle ()
  in
  (* This action is performed only if `--dal-slack-webhook` is provided. *)
  if t.configuration.network_health_monitoring then
    Monitoring_app.Alert.check_for_lost_dal_rewards
      ~cloud:t.cloud
      ~network:t.configuration.network
      ~metadata
  else Lwt.return_unit

let on_new_block t ~level =
  let* () = wait_for_level t level in
  let endpoint = t.some_node_rpc_endpoint in
  let block = string_of_int level in
  let* metadata =
    RPC_core.call endpoint @@ RPC.get_chain_block_metadata_raw ~block ()
  in
  let cycle_position =
    JSON.(metadata |-> "level_info" |-> "cycle_position" |> as_int)
  in
  let is_new_cycle = cycle_position = 0 in
  let* () = if is_new_cycle then on_new_cycle t ~level else unit in
  on_new_level t level ~metadata

let rec loop t level =
  let p = on_new_block t ~level in
  let _p2 =
    if Dal_node_helpers.producers_not_ready ~producers:t.producers then (
      toplog "Producers not ready for level %d" level ;
      Lwt.return_unit)
    else
      Seq.ints 0
      |> Seq.take (List.length t.configuration.dal_node_producers)
      |> Seq.map (fun i ->
             Dal_node_helpers.produce_slot
               t.cloud
               ~client:t.bootstrap.client
               ~producers:t.producers
               ~network:t.configuration.network
               ~producer_key:t.configuration.producer_key
               ~some_node_rpc_endpoint:t.some_node_rpc_endpoint
               ~producers_delay:t.configuration.producers_delay
               ~slot_size:t.parameters.cryptobox.slot_size
               level
               i)
      |> List.of_seq |> Lwt.join
  in
  let* t = p in
  loop t (level + 1)

let yes_wallet_exe = Uses.path Constant.yes_wallet

let register (module Cli : Scenarios_cli.Dal) =
  let simulate_network = Cli.simulate_network in
  let stake =
    Stake_repartition.Dal.parse_arg
      ~stake_arg:Cli.stake
      ~simulation_arg:simulate_network
  in
  let configuration, etherlink_configuration =
    let stake_machine_type = Cli.stake_machine_type in
    let dal_node_producers =
      let last_index = ref (-1) in
      List.init Cli.producers (fun i ->
          match List.nth_opt Cli.dal_producers_slot_indices i with
          | None ->
              incr last_index ;
              !last_index
          | Some index ->
              last_index := index ;
              index)
    in
    let observer_slot_indices = Cli.observer_slot_indices in
    let observers_multi_slot_indices = Cli.observers_multi_slot_indices in
    let archivers_slot_indices = Cli.archivers_slot_indices in
    let observer_pkhs = Cli.observer_pkhs in
    let protocol = Cli.protocol in
    let producer_machine_type = Cli.producer_machine_type in
    let etherlink = Cli.etherlink in
    let etherlink_sequencer = Cli.etherlink_sequencer in
    let etherlink_producers = Cli.etherlink_producers in
    let echo_rollups = Cli.echo_rollups in
    let disconnect = Cli.disconnect in
    let network = Cli.network in
    let snapshot = Cli.snapshot in
    let bootstrap = Cli.bootstrap in
    let etherlink_dal_slots = Cli.etherlink_dal_slots in
    let teztale = Cli.teztale in
    let memtrace = Cli.memtrace in
    let data_dir = Cli.data_dir in
    let producer_key = Cli.producer_key in
    let producers_delay = Cli.producers_delay in
    let ignore_pkhs = Cli.ignore_pkhs in
    let tezlink = Cli.tezlink in
    let fundraiser =
      Option.fold
        ~none:(Sys.getenv_opt "TEZT_CLOUD_FUNDRAISER")
        ~some:Option.some
        Cli.fundraiser
    in
    let etherlink_chain_id = Cli.etherlink_chain_id in
    let etherlink =
      if etherlink then
        Some
          Etherlink_helpers.
            {
              etherlink_sequencer;
              etherlink_producers;
              etherlink_dal_slots;
              chain_id = etherlink_chain_id;
              tezlink;
            }
      else None
    in
    let blocks_history = Cli.blocks_history in
    let bootstrap_node_identity_file = Cli.bootstrap_node_identity_file in
    let bootstrap_dal_node_identity_file =
      Cli.bootstrap_dal_node_identity_file
    in
    let with_dal = Cli.with_dal in
    let bakers = Cli.bakers in
    let external_rpc = Cli.node_external_rpc_server in
    let disable_shard_validation = Cli.disable_shard_validation in
    let disable_amplification = Cli.disable_amplification in
    let ppx_profiling_verbosity = Cli.ppx_profiling_verbosity in
    let ppx_profiling_backends = Cli.ppx_profiling_backends in
    let network_health_monitoring = Cli.enable_network_health_monitoring in
    let daily_logs_destination =
      Option.map (fun dir -> dir // "daily_logs") Tezt_cloud_cli.artifacts_dir
    in
    let slot_size = Cli.slot_size in
    let number_of_slots = Cli.number_of_slots in
    let attestation_lag = Cli.attestation_lag in
    let traps_fraction = Cli.traps_fraction in
    let t =
      {
        with_dal;
        bakers;
        stake;
        stake_machine_type;
        dal_node_producers;
        observer_slot_indices;
        observers_multi_slot_indices;
        archivers_slot_indices;
        observer_pkhs;
        protocol;
        producer_machine_type;
        echo_rollups;
        disconnect;
        network;
        simulate_network;
        snapshot;
        bootstrap;
        teztale;
        memtrace;
        data_dir;
        fundraiser;
        producer_key;
        producers_delay;
        blocks_history;
        bootstrap_node_identity_file;
        bootstrap_dal_node_identity_file;
        external_rpc;
        disable_shard_validation;
        disable_amplification;
        ignore_pkhs;
        ppx_profiling_verbosity;
        ppx_profiling_backends;
        network_health_monitoring;
        daily_logs_destination;
        slot_size;
        number_of_slots;
        attestation_lag;
        traps_fraction;
      }
    in
    (t, etherlink)
  in
  toplog "Parsing CLI done" ;
  let baker_daemon_count =
    match simulate_network with
    | Network_simulation.Disabled -> 0
    | Scatter (_selected_baker_count, baker_daemon_count) -> baker_daemon_count
    | Map
        ( _selected_baker_count,
          single_baker_daemon_count,
          multiple_baker_daemon_count ) ->
        single_baker_daemon_count + multiple_baker_daemon_count
  in
  let vms =
    let* stake = configuration.stake in
    return
    @@ List.concat
         [
           (if configuration.bootstrap then [Bootstrap] else []);
           List.mapi (fun i _ -> Baker i) stake;
           List.init baker_daemon_count (fun i -> Baker i);
           List.map (fun i -> Producer i) configuration.dal_node_producers;
           List.map
             (fun index -> Observer (`Indexes [index]))
             configuration.observer_slot_indices;
           List.map
             (fun indexes -> Observer (`Indexes indexes))
             configuration.observers_multi_slot_indices;
           List.map
             (fun indexes -> Archiver (`Indexes indexes))
             configuration.archivers_slot_indices;
           List.map (fun pkh -> Observer (`Pkh pkh)) configuration.observer_pkhs;
           (if etherlink_configuration <> None then [Etherlink_operator] else []);
           (match etherlink_configuration with
           | None | Some {etherlink_dal_slots = []; _} -> []
           | Some {etherlink_dal_slots = [_]; _} -> [Etherlink_dal_operator]
           | Some {etherlink_dal_slots; _} ->
               Reverse_proxy :: Etherlink_dal_operator
               :: List.map
                    (fun slot_index -> Etherlink_dal_observer {slot_index})
                    etherlink_dal_slots);
           (match etherlink_configuration with
           | None -> []
           | Some {etherlink_producers; _} ->
               List.init etherlink_producers (fun i -> Etherlink_producer i));
           (if configuration.echo_rollups > 0 then
              Reverse_proxy
              :: List.concat
                   (List.init configuration.echo_rollups (fun operator ->
                        Echo_rollup_operator operator
                        :: List.map
                             (fun slot_index ->
                               Echo_rollup_dal_observer {operator; slot_index})
                             configuration.dal_node_producers))
            else []);
         ]
  in
  let docker_image =
    Option.map
      (fun tag -> Agent.Configuration.Octez_release {tag})
      Cli.octez_release
  in
  let default_vm_configuration ~name =
    Agent.Configuration.make ?docker_image ~name ()
  in
  let vms () =
    let* vms in
    return
    @@ List.map
         (fun agent_kind ->
           let name = name_of agent_kind in
           match agent_kind with
           | Bootstrap -> default_vm_configuration ~name
           | Baker i -> (
               try
                 let machine_type =
                   List.nth configuration.stake_machine_type i
                 in
                 Agent.Configuration.make ?docker_image ~machine_type ~name ()
               with _ -> default_vm_configuration ~name)
           | Producer _ ->
               let machine_type = configuration.producer_machine_type in
               Agent.Configuration.make ?docker_image ?machine_type ~name ()
           | Observer _ | Archiver _ | Etherlink_dal_operator
           | Etherlink_dal_observer _ | Echo_rollup_dal_observer _ ->
               Agent.Configuration.make ?docker_image ~name ()
           | Echo_rollup_operator _ -> default_vm_configuration ~name
           | Etherlink_operator -> default_vm_configuration ~name
           | Etherlink_producer _ -> default_vm_configuration ~name
           | Reverse_proxy -> default_vm_configuration ~name
           | Stresstest _ -> default_vm_configuration ~name)
         vms
  in
  let endpoint, resolver_endpoint = Lwt.wait () in
  Cloud.register
  (* docker images are pushed before executing the test in case binaries are modified locally. This way we always use the latest ones. *)
    ~vms
    ~proxy_files:
      ([
         yes_wallet_exe;
         Format.asprintf
           "src/%s/parameters/mainnet-parameters.json"
           (Protocol.directory configuration.protocol);
         "octez-node";
       ]
      @ (if Cli.refresh_binaries then
           [
             "octez-dal-node";
             "octez-client";
             Tezt_wrapper.Uses.path Constant.octez_agnostic_baker;
           ]
           @ (if Cli.etherlink then
                ["evm_kernel.wasm"; "octez-evm-node"; "octez-smart-rollup-node"]
              else [])
           @
           if Cli.teztale then
             ["octez-teztale-archiver"; "octez-teztale-server"]
           else []
         else [])
      @ (if Cli.echo_rollups > 0 then ["dal_echo_kernel_bandwidth.wasm"] else [])
      @ Option.fold
          ~none:[]
          ~some:(fun x -> [x])
          configuration.bootstrap_node_identity_file
      @ Option.fold
          ~none:[]
          ~some:(fun x -> [x])
          configuration.bootstrap_dal_node_identity_file)
    ~proxy_args:
      (match configuration.fundraiser with
      | None -> []
      | Some fundraiser_key -> ["--fundraiser"; fundraiser_key])
    ~__FILE__
    ~title:"DAL node benchmark"
    ~tags:[]
    (fun cloud ->
      toplog "Prepare artifacts directory and export the full configuration" ;
      Artifact_helpers.prepare_artifacts
        ~scenario_config:
          ( "DAL",
            Data_encoding.Json.construct
              Scenarios_configuration.DAL.encoding
              Cli.config )
        () ;
      toplog "Creating the agents" ;
      if
        (not configuration.with_dal)
        && List.length configuration.dal_node_producers > 0
        && configuration.network = `Sandbox
      then
        Log.warn
          "You are running in sandboxed mode with some DAL producers but with \
           DAL deactivated for bakers. DAL network can't work properly. This \
           is probably a configuration issue." ;
      let agents = Cloud.agents cloud in
      let next_agent ~name =
        let agent =
          match List.find_opt (fun agent -> Agent.name agent = name) agents with
          | None ->
              if Cli.proxy_localhost then List.hd agents
              else Test.fail ~__LOC__ "Agent not found: %s" name
          | Some agent -> agent
        in
        Lwt.return agent
      in
      if configuration.network_health_monitoring then
        Monitoring_app.Tasks.register_chronos_task
          cloud
          ~network:configuration.network
          endpoint ;
      let* t = init ~configuration etherlink_configuration cloud next_agent in
      Lwt.wakeup resolver_endpoint t.some_node_rpc_endpoint ;
      toplog "Starting main loop" ;
      loop t (t.first_level + 1))
