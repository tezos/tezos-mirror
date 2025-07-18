(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Scenarios_helpers
open Tezos
open Yes_crypto

type observer = {
  node : Node.t;
  dal_node : Dal_node.t;
  topic : [`Slot_index of int | `Pkh of string];
}

type producer = {
  node : Node.t;
  dal_node : Dal_node.t;
  client : Client.t;
  account : Account.key;
  is_ready : unit Lwt.t;
  slot_index : int;
}

type dal_status =
  | With_DAL of Z.t
  | Without_DAL
  | Out_of_committee
  | Expected_to_DAL_attest

let may_copy_dal_node_identity_file agent node = function
  | None -> Lwt.return_unit
  | Some source ->
      toplog "Copying the DAL node identity file for %s" (Agent.name agent) ;
      let* _ =
        Agent.copy agent ~source ~destination:(Dal_node.identity_file node)
      in
      Lwt.return_unit

(* Producer functions *)

let fund_producers_accounts ~client ~fundraiser accounts_to_fund =
  if List.length accounts_to_fund > 0 then
    let () = toplog "Funding the producer accounts" in
    let fundraiser_key =
      match fundraiser with
      | None ->
          Test.fail
            "No fundraiser key was specified. Please use either `--fundraiser` \
             or the variable environment `TEZT_CLOUD_FUNDRAISER` to specified \
             an unencrypted secret key of an account having funds to run the \
             scenario"
      | Some key -> key
    in
    let* () =
      Client.import_secret_key
        client
        (Unencrypted fundraiser_key)
        ~alias:"fundraiser"
    in
    let () = toplog "Revealing the fundraiser public key" in
    let* () =
      let*? process = Client.reveal ~src:"fundraiser" client in
      let* _ = Process.wait process in
      Lwt.return_unit
    in
    let* fundraiser = Client.show_address ~alias:"fundraiser" client in
    let () = toplog "Fetching fundraiser's counter" in
    let* counter = Operation.get_next_counter ~source:fundraiser client in
    let () = toplog "Fetching fundraiser's balance" in
    let* _balance = Client.get_balance_for ~account:"fundraiser" client in
    let () = toplog "Injecting the batch" in
    let* _op_hash =
      accounts_to_fund
      |> List.map (fun (dest, amount) ->
             Operation.Manager.transfer ~amount ~dest ())
      |> Operation.Manager.make_batch ~source:fundraiser ~counter
      |> Fun.flip (Operation.Manager.inject ~dont_wait:true) client
    in
    (* Wait a bit. *)
    let () = toplog "Waiting 10 seconds" in
    let* () = Lwt_unix.sleep 10. in
    let () = toplog "Waiting 10 seconds: done" in
    Lwt.return_unit
  else
    let () =
      toplog "Skipping batch injection because there is no account to fund"
    in
    Lwt.return_unit

let init_producer_accounts ~client ~producer_key ~dal_node_producers =
  let () = toplog "Initializing the producers" in
  match producer_key with
  | None ->
      Client.stresstest_gen_keys
        ~alias_prefix:"dal_producer"
        (List.length dal_node_producers)
        client
  | Some producer_key -> (
      match dal_node_producers with
      | [_] ->
          let* () =
            Client.import_secret_key
              client
              (Unencrypted producer_key)
              ~alias:"producer_key"
          in
          let* account = Client.show_address ~alias:"producer_key" client in
          return [account]
      | _ ->
          Test.fail
            "A producer key can only be used if there is exactly one slot on \
             which data are produced.")

let init_producer cloud ~data_dir ~simulate_network ~external_rpc ~network
    ~snapshot ~memtrace ~ppx_profiling ~ppx_profiling_backends ~ignore_pkhs
    ~disable_shard_validation ~node_p2p_endpoint ~dal_node_p2p_endpoint teztale
    account i slot_index agent =
  let name = Format.asprintf "producer-node-%i" i in
  let () = toplog "Initializing the DAL producer %s" name in
  let data_dir = data_dir |> Option.map (fun data_dir -> data_dir // name) in
  let () = toplog "Init producer %s: init L1 node" name in
  let env, with_yes_crypto = may_set_yes_crypto_env simulate_network in
  let* node =
    Node_helpers.init
      ?env
      ?data_dir
      ~name
      ~arguments:Node.[Peer node_p2p_endpoint]
      ~rpc_external:external_rpc
      network
      ~with_yes_crypto
      ~snapshot
      ~ppx_profiling
      cloud
      agent
  in
  let endpoint = Client.Node node in
  let () = toplog "Init %s producer: create client" name in
  let* client = Client.Agent.create ~endpoint agent in
  let () = toplog "Init %s producer: import key" name in
  let* () =
    Client.import_secret_key
      client
      ~endpoint
      account.Account.secret_key
      ~alias:account.Account.alias
  in
  let () = toplog "Init producer %s: reveal account" name in
  let*? process = Client.reveal client ~endpoint ~src:account.Account.alias in
  let* _ = Process.wait process in
  let () = toplog "Init producer %s: create agent" name in
  let* dal_node =
    let ignore_pkhs = if ignore_pkhs = [] then None else Some ignore_pkhs in
    Dal_node.Agent.create
      ~name:(Format.asprintf "producer-dal-node-%i" i)
      ~node
      ~disable_shard_validation
      ?ignore_pkhs
      cloud
      agent
  in
  let () = toplog "Init producer %s: init DAL node config" name in
  let* () =
    Dal_node.init_config
      ~expected_pow:(Network.expected_pow network)
      ~observer_profiles:[slot_index]
      ~peers:(Option.to_list dal_node_p2p_endpoint)
      (* If `--no-dal` is given with `--producers 2`, then DAL nodes
         are run for the producers. While this is weird in sandboxed
         mode, it can make sense on ghostnet for example. *)
      dal_node
  in
  let () = toplog "Init producer %s: add DAL node metrics" name in
  let* () =
    add_prometheus_source
      ~node
      ~dal_node
      cloud
      agent
      (Format.asprintf "producer-%d" i)
  in
  let* () =
    match teztale with
    | None -> Lwt.return_unit
    | Some teztale ->
        Teztale.update_alias
          teztale
          ~address:account.Account.public_key_hash
          ~alias:account.Account.alias
  in
  (* We do not wait on the promise because loading the SRS takes some time.
     Instead we will publish commitments only once this promise is fulfilled. *)
  let () = toplog "Init producer %s: wait for DAL node to be ready" name in
  let otel = Cloud.open_telemetry_endpoint cloud in
  let is_ready =
    let ignore_pkhs = if ignore_pkhs = [] then None else Some ignore_pkhs in
    Dal_node.Agent.run
      ~prometheus:Tezt_cloud_cli.prometheus
      ?otel
      ~memtrace
      ~event_level:`Notice
      ~disable_shard_validation
      ?ignore_pkhs
      ~ppx_profiling
      ~ppx_profiling_backends
      dal_node
  in
  let () = toplog "Init producer %s: DAL node is ready" name in
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
  Lwt.return {client; node; dal_node; account; is_ready; slot_index}

(* Observer functions *)

let init_observer cloud ~data_dir ~simulate_network ~external_rpc ~network
    ~snapshot ~memtrace ~ppx_profiling ~ppx_profiling_backends
    ~disable_shard_validation ~node_p2p_endpoint ~dal_node_p2p_endpoint teztale
    ~topic i agent =
  let name = Format.asprintf "observer-node-%i" i in
  let data_dir = data_dir |> Option.map (fun data_dir -> data_dir // name) in
  let env, with_yes_crypto = may_set_yes_crypto_env simulate_network in
  let* node =
    Node_helpers.init
      ?env
      ?data_dir
      ~name
      ~arguments:[Peer node_p2p_endpoint]
      ~rpc_external:external_rpc
      network
      ~with_yes_crypto
      ~snapshot
      ~ppx_profiling
      cloud
      agent
  in
  let* dal_node =
    Dal_node.Agent.create
      ~name:(Format.asprintf "observer-dal-node-%i" i)
      ~node
      ~disable_shard_validation
      cloud
      agent
  in
  let* () =
    match topic with
    | `Slot_index slot_index ->
        Dal_node.init_config
          ~expected_pow:(Network.expected_pow network)
          ~observer_profiles:[slot_index]
          ~peers:(Option.to_list dal_node_p2p_endpoint)
          dal_node
    | `Pkh pkh ->
        Dal_node.init_config
          ~expected_pow:(Network.expected_pow network)
          ~attester_profiles:[pkh]
          ~peers:(Option.to_list dal_node_p2p_endpoint)
          dal_node
  in
  let* () =
    add_prometheus_source
      ~node
      ~dal_node
      cloud
      agent
      (Format.asprintf "observer-%d" i)
  in
  let otel = Cloud.open_telemetry_endpoint cloud in
  let* () =
    Dal_node.Agent.run
      ~prometheus:Tezt_cloud_cli.prometheus
      ?otel
      ~memtrace
      ~event_level:`Notice
      ~disable_shard_validation
      ~ppx_profiling
      ~ppx_profiling_backends
      dal_node
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
  Lwt.return {node; dal_node; topic}
