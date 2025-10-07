(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Agent_kind
module Helpers = Dal_common.Helpers
open Scenarios_helpers
open Tezos
open Yes_crypto

type observer = {
  node : Node.t;
  dal_node : Dal_node.t;
  topic : [`Slot_indexes of int list | `Pkh of string];
}

type producer = {
  node : Node.t;
  dal_node : Dal_node.t;
  client : Client.t;
  account : Account.key;
  is_ready : unit Lwt.t;
  slot_index : int;
}

type archiver = {
  node : Node.t;
  dal_node : Dal_node.t;
  topic : [`Slot_indexes of int list];
}

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
    ~snapshot ~memtrace ~ppx_profiling_verbosity ~ppx_profiling_backends
    ~ignore_pkhs ~disable_shard_validation ~disable_amplification
    ~node_p2p_endpoint ~dal_node_p2p_endpoint teztale account i slot_index agent
    =
  let name = name_of_daemon (Producer_l1_node i) in
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
      ~ppx_profiling_verbosity
      ~ppx_profiling_backends
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
      ~name:(name_of_daemon (Producer_dal_node i))
      ~node
      ~disable_shard_validation
      ~disable_amplification
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
      ~ppx_profiling_verbosity
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

let ensure_enough_funds cloud ~client ~fee ~producers ~network ~producer_key
    ~some_node_rpc_endpoint i =
  let alert_if_balance_is_close_to_zero pkh balance =
    (* [is_just_under ~balance ~limit] is true if the [balance] is under the [limit],
       but by at most one publication cost, since we do not want to spam the Slack channel when uder the threshold.*)
    let is_just_under ~balance ~limit =
      balance < Tez.of_mutez_int limit
      && balance >= Tez.of_mutez_int (limit - fee)
    in
    if
      List.exists
        (fun limit -> is_just_under ~balance ~limit)
        [100_000; 50_000; 10_000]
    then
      Monitoring_app.Alert.report_funds_are_getting_short
        ~cloud
        ~network
        ~fee
        ~pkh
        ~balance:(Tez.to_mutez balance)
    else Lwt.return_unit
  in
  let producer = List.nth producers i in
  match (network, producer_key) with
  | `Sandbox, _ -> (* Producer has enough money *) Lwt.return_unit
  | _, Some _ ->
      let id = producer.account.public_key_hash in
      (* Producer key is assumed to have enough money. We simply check that it is the case,
         but do not refund it. *)
      let* balance =
        RPC_core.call some_node_rpc_endpoint
        @@ RPC.get_chain_block_context_contract_balance ~id ()
      in
      let* () = alert_if_balance_is_close_to_zero id balance in
      if balance < Tez.of_mutez_int fee then
        Lwt.fail_with
          "Producer key has not enough money anymore to publish slots"
      else Lwt.return_unit
  | _ ->
      let* balance =
        RPC_core.call some_node_rpc_endpoint
        @@ RPC.get_chain_block_context_contract_balance
             ~id:producer.account.public_key_hash
             ()
      in
      (* This is to prevent having to refund two producers at the same time and ensure it can produce at least one slot. *)
      let random = Random.int 5_000_000 + 10_000 in
      if balance < Tez.of_mutez_int random then
        let* fundraiser = Client.show_address ~alias:"fundraiser" client in
        let fundraiser_pkh = fundraiser.public_key_hash in
        let* fundraiser_balance =
          RPC_core.call some_node_rpc_endpoint
          @@ RPC.get_chain_block_context_contract_balance ~id:fundraiser_pkh ()
        in
        let* () =
          alert_if_balance_is_close_to_zero fundraiser_pkh fundraiser_balance
        in
        let* _op_hash =
          Operation.Manager.transfer
            ~amount:10_000_000
            ~dest:producer.account
            ()
          |> Operation.Manager.make ~fee ~source:fundraiser
          |> Seq.return |> List.of_seq
          |> Fun.flip (Operation.Manager.inject ~dont_wait:true) client
        in
        Lwt.return_unit
      else Lwt.return_unit

let produce_slot cloud ~client ~producers ~network ~producer_key
    ~some_node_rpc_endpoint ~producers_delay ~slot_size level i =
  if level mod producers_delay = 0 then (
    let all_start = Unix.gettimeofday () in
    let producer = List.nth producers i in
    toplog
      "Producing a slot for index %d for level %d"
      producer.slot_index
      level ;
    let fee = 800 in
    let* () =
      ensure_enough_funds
        cloud
        ~client
        ~fee
        ~producers
        ~network
        ~producer_key
        ~some_node_rpc_endpoint
        i
    in
    toplog "Ensured enough funds are available" ;
    let index = producer.slot_index in
    let content =
      Format.asprintf "%d:%d" level index
      |> Helpers.make_slot ~padding:false ~slot_size
    in
    let* _ = Node.wait_for_level producer.node level in
    let make_commitment_start = Unix.gettimeofday () in
    let* _commitment =
      (* A dry-run of the "publish dal commitment" command for each tz kinds outputs:
         - tz1: fees of 513µtz and 1333 gas consumed
         - tz2: fees of 514µtz and 1318 gas consumed
         - tz3: fees of 543µtz and 1607 gas consumed
         - tz4: fees of 700µtz and 2837 gas consumed
         We added a (quite small) margin to it. *)
      Helpers.publish_and_store_slot
        ~fee
        ~gas_limit:3000
        ~dont_wait:true
        producer.client
        producer.dal_node
        producer.account
        ~force:true
        ~index
        content
    in
    let make_commitment_end = Unix.gettimeofday () in
    Log.info
      "publish_commitment operation for index %d injected at level %d"
      producer.slot_index
      level ;
    let all_end = Unix.gettimeofday () in
    let all_duration = all_end -. all_start in
    let commitment_duration = make_commitment_end -. make_commitment_start in
    Log.info
      "Produce slot (for index %d) duration:@.- publish_and_store_slot: %f \
       s@.- overall (including wait for level): %f s@."
      producer.slot_index
      commitment_duration
      all_duration ;
    Lwt.return_unit)
  else Lwt.return_unit

let producers_not_ready ~producers =
  (* If not all the producer nodes are ready, we do not publish the commitment
       for the current level. Another attempt will be done at the next level. *)
  let producer_ready producer =
    match Lwt.state producer.is_ready with
    | Sleep -> true
    | Fail exn -> Lwt.reraise exn
    | Return () -> false
  in
  List.for_all producer_ready producers

(* Observer functions *)

let init_observer cloud ~data_dir ~simulate_network ~external_rpc ~network
    ~snapshot ~memtrace ~ppx_profiling_verbosity ~ppx_profiling_backends
    ~disable_shard_validation ~disable_amplification ~node_p2p_endpoint
    ~dal_node_p2p_endpoint teztale ~topic i agent : observer Lwt.t =
  let name = name_of_daemon (Observer_l1_node i) in
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
      ~ppx_profiling_verbosity
      ~ppx_profiling_backends
      cloud
      agent
  in
  let* dal_node =
    Dal_node.Agent.create
      ~name:(name_of_daemon (Observer_dal_node i))
      ~node
      ~disable_shard_validation
      ~disable_amplification
      cloud
      agent
  in
  let* () =
    match topic with
    | `Slot_indexes slot_indexes ->
        Dal_node.init_config
          ~expected_pow:(Network.expected_pow network)
          ~observer_profiles:slot_indexes
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
      ~ppx_profiling_verbosity
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
  Lwt.return ({node; dal_node; topic} : observer)

let init_archiver cloud ~data_dir ~simulate_network ~external_rpc ~network
    ~snapshot ~memtrace ~ppx_profiling_verbosity ~ppx_profiling_backends
    ~disable_shard_validation ~disable_amplification ~node_p2p_endpoint
    ~dal_node_p2p_endpoint teztale ~topic i agent : archiver Lwt.t =
  let name = name_of_daemon (Archiver_l1_node i) in
  let () = toplog "Initializing the DAL archiver %s" name in
  let data_dir = data_dir |> Option.map (fun data_dir -> data_dir // name) in
  let () = toplog "Init archiver %s: init L1 node" name in
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
      ~ppx_profiling_verbosity
      ~ppx_profiling_backends
      cloud
      agent
  in
  let* dal_node =
    Dal_node.Agent.create
      ~name:(name_of_daemon (Archiver_dal_node i))
      ~node
      ~disable_shard_validation
      ~disable_amplification
      cloud
      agent
  in
  let () = toplog "Init archiver %s: init DAL node config" name in
  let* () =
    match topic with
    | `Slot_indexes slot_indexes ->
        Dal_node.init_config
          ~expected_pow:(Network.expected_pow network)
          ~operator_profiles:slot_indexes
          ~peers:(Option.to_list dal_node_p2p_endpoint)
          dal_node
  in
  let () = toplog "Init archiver %s: add DAL node metrics" name in
  let* () =
    add_prometheus_source
      ~node
      ~dal_node
      cloud
      agent
      (Format.asprintf "archiver-%d" i)
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
  Lwt.return ({node; dal_node; topic} : archiver)
