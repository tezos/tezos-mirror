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

type baker_account = {
  delegate : Account.key;
  consensus_key : Account.key option;
}

type baker = {
  node : Node.t;
  dal_node : Dal_node.t option;
  baker : Agnostic_baker.t;
  accounts : baker_account list;
  stake : int;
}

let init_baker ?stake ~configuration_stake ~data_dir ~simulate_network
    ~external_rpc ~network ~snapshot ~ppx_profiling_verbosity
    ~ppx_profiling_backends ~memtrace ~with_dal ~disable_shard_validation
    ~node_p2p_endpoint ~dal_node_p2p_endpoint cloud teztale ~baker_accounts i
    agent =
  (* Use the consensus keys when available. *)
  let baking_keys =
    List.map
      (fun {delegate; consensus_key} ->
        match consensus_key with
        | Some ck ->
            {
              delegate with
              public_key_hash = ck.public_key_hash;
              public_key = ck.public_key;
            }
        | None -> delegate)
      baker_accounts
  in
  let* stake =
    (* As simulate_network and stake are mutually exclusive, the stake is used
       only when the simulation is Disabled. *)
    match simulate_network with
    | Network_simulation.Disabled -> (
        match stake with
        | None -> return (List.nth configuration_stake i)
        | Some stake -> return stake)
    | Scatter _ | Map _ -> Lwt.return 0
  in
  let name = name_of_daemon (Baker_l1_node i) in
  let data_dir = data_dir |> Option.map (fun data_dir -> data_dir // name) in
  let env, with_yes_crypto = may_set_yes_crypto_env simulate_network in
  let* node =
    Node_helpers.init
      ?env
      ?data_dir
      ~arguments:Node.[Peer node_p2p_endpoint]
      ~name
      ~rpc_external:external_rpc
      network
      ~with_yes_crypto
      ~snapshot
      ~ppx_profiling_verbosity
      ~ppx_profiling_backends
      cloud
      agent
  in
  let* dal_node =
    if not with_dal then Lwt.return_none
    else
      let* dal_node =
        Dal_node.Agent.create
          ~name:(name_of_daemon (Baker_dal_node i))
          ~node
          ~disable_shard_validation
          cloud
          agent
      in
      let attester_profiles =
        List.map
          (fun {delegate; _} -> delegate.Account.public_key_hash)
          baker_accounts
      in
      let* () =
        Dal_node.init_config
          ~expected_pow:(Network.expected_pow network)
          ~attester_profiles
          ~peers:[dal_node_p2p_endpoint |> Option.get]
          (* Invariant: Option.get don't fail because t.dal is true *)
          dal_node
      in
      let otel = Cloud.open_telemetry_endpoint cloud in
      let* () =
        Dal_node.Agent.run
          ~prometheus:Tezt_cloud_cli.prometheus
          ?otel
          ~memtrace
          ~event_level:`Notice
          ~disable_shard_validation
          ~ppx_profiling_verbosity
          ~ppx_profiling_backends
          dal_node
      in
      Lwt.return_some dal_node
  in
  let* () =
    match teztale with
    | None -> Lwt.return_unit
    | Some teztale ->
        let* () =
          Teztale.add_archiver
            teztale
            cloud
            agent
            ~node_name:(Node.name node)
            ~node_port:(Node.rpc_port node)
        in
        Lwt_list.iter_s
          (fun {delegate; _} ->
            Teztale.update_alias
              teztale
              ~address:delegate.public_key_hash
              ~alias:delegate.alias)
          baker_accounts
  in
  let* client = Client.Agent.create ~endpoint:(Node node) agent in
  let* () =
    match simulate_network with
    | Scatter _ | Map _ ->
        let* yes_wallet = Node_helpers.yes_wallet agent in
        let* () =
          Lwt_list.iter_s
            (fun account ->
              Client.import_public_key
                client
                ~public_key:account.Account.public_key
                ~alias:account.alias)
            baking_keys
        in
        let* () = Yes_wallet.convert_wallet_inplace ~client yes_wallet in
        Lwt.return_unit
    | Disabled ->
        Lwt_list.iter_s
          (fun account ->
            Client.import_secret_key
              client
              account.Account.secret_key
              ~alias:account.alias)
          baking_keys
  in
  let delegates = List.map (fun account -> account.Account.alias) baking_keys in
  let* baker =
    let dal_node_rpc_endpoint = Option.map Dal_node.as_rpc_endpoint dal_node in
    Agnostic_baker.Agent.init
      ?env
      ~name:(Format.asprintf "baker-%d" i)
      ~delegates
      ~client
      ?dal_node_rpc_endpoint
      ~ppx_profiling_verbosity
      ~ppx_profiling_backends
      node
      cloud
      agent
  in
  let* () =
    add_prometheus_source
      ~node
      ?dal_node
      cloud
      agent
      (Format.asprintf "baker-%d" i)
  in
  Lwt.return {node; dal_node; baker; accounts = baker_accounts; stake}

let init_bakers ~bakers ~stake ~data_dir ~simulate_network ~external_rpc
    ~network ~snapshot ~ppx_profiling_verbosity ~ppx_profiling_backends
    ~memtrace ~with_dal ~disable_shard_validation ~node_p2p_endpoint
    ~dal_node_p2p_endpoint cloud teztale ~baker_accounts next_agent =
  let* stake in
  let* attesters_agents =
    (* As simulate_network and stake are mutually exclusive, the stake is used
       only when the simulation is Disabled. *)
    match simulate_network with
    | Network_simulation.Scatter (_, baker_count) ->
        Lwt_list.mapi_s
          (fun i _ ->
            let name = name_of (Baker i) in
            next_agent ~name)
          (List.init baker_count Fun.id)
    | Map (_, single_baker_count, multiple_baker_count) ->
        Lwt_list.mapi_s
          (fun i _ ->
            let name = name_of (Baker i) in
            next_agent ~name)
          (List.init (single_baker_count + multiple_baker_count) Fun.id)
    | Disabled ->
        Lwt_list.mapi_s
          (fun i _stake ->
            let name = name_of (Baker i) in
            next_agent ~name)
          stake
  in
  let* bakers_agents =
    Lwt_list.mapi_s
      (fun i _stake ->
        let name = name_of (Baker i) in
        next_agent ~name)
      (match simulate_network with
      | Scatter (_selected_baker_count, baker_daemon_count) ->
          List.init baker_daemon_count string_of_int
      | Map
          ( _selected_baker_count,
            single_baker_daemon_count,
            multiple_baker_daemon_count ) ->
          List.init
            (single_baker_daemon_count + multiple_baker_daemon_count)
            string_of_int
      | Disabled -> bakers)
  in
  match simulate_network with
  | Scatter _ | Map _ ->
      Lwt_list.mapi_p
        (fun i (agent, accounts) ->
          init_baker
            ~configuration_stake:stake
            ~data_dir
            ~simulate_network
            ~external_rpc
            ~network
            ~snapshot
            ~ppx_profiling_verbosity
            ~ppx_profiling_backends
            ~memtrace
            ~with_dal
            ~disable_shard_validation
            ~node_p2p_endpoint
            ~dal_node_p2p_endpoint
            cloud
            teztale
            ~baker_accounts:accounts
            i
            agent)
        (List.combine attesters_agents baker_accounts)
  | Disabled ->
      let* fresh_bakers =
        Lwt_list.mapi_p
          (fun i (agent, accounts) ->
            init_baker
              ~configuration_stake:stake
              ~data_dir
              ~simulate_network
              ~external_rpc
              ~network
              ~snapshot
              ~ppx_profiling_verbosity
              ~ppx_profiling_backends
              ~memtrace
              ~with_dal
              ~disable_shard_validation
              ~node_p2p_endpoint
              ~dal_node_p2p_endpoint
              cloud
              teztale
              ~baker_accounts:accounts
              i
              agent)
          (List.combine attesters_agents baker_accounts)
      in
      let* bakers_with_secret_keys =
        Lwt_list.mapi_p
          (fun i (agent, sk) ->
            let sk = Account.Unencrypted sk in
            let client = Client.create () in
            let alias = Format.asprintf "baker-%02d" i in
            let* () = Client.import_secret_key client sk ~alias in
            let* accounts =
              let* addresses = Client.list_known_addresses client in
              Lwt_list.map_s
                (fun (alias, _) ->
                  let* delegate = Client.show_address ~alias client in
                  return {delegate; consensus_key = None})
                addresses
            in
            (* A bit random, to fix later. *)
            init_baker
              ~stake:1
              ~configuration_stake:stake
              ~data_dir
              ~simulate_network
              ~external_rpc
              ~network
              ~snapshot
              ~ppx_profiling_verbosity
              ~ppx_profiling_backends
              ~memtrace
              ~with_dal
              ~disable_shard_validation
              ~node_p2p_endpoint
              ~dal_node_p2p_endpoint
              cloud
              teztale
              ~baker_accounts:accounts
              i
              agent)
          (List.combine bakers_agents bakers)
      in
      Lwt.return (fresh_bakers @ bakers_with_secret_keys)
