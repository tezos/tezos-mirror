(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Cryptobox = Dal_common.Cryptobox
module Helpers = Dal_common.Helpers

module Cli = struct
  let section =
    Clap.section
      ~description:
        "All the options related to running DAL scenarions onto the cloud"
      "Cloud DAL"

  let stake =
    let stake_typ =
      let parse string =
        try
          string |> String.split_on_char ',' |> List.map int_of_string
          |> Option.some
        with _ -> None
      in
      let show l = l |> List.map string_of_int |> String.concat "," in
      Clap.typ ~name:"stake" ~dummy:[100] ~parse ~show
    in
    Clap.default
      ~section
      ~long:"stake"
      ~placeholder:"<integer>, <integer>, <integer>, ..."
      ~description:
        "Specify the stake repartition. Each number specify the number of \
         shares old by one baker. The total stake is given by the sum of all \
         shares."
      stake_typ
      [100]

  let producers =
    Clap.default_int
      ~section
      ~long:"producers"
      ~description:"Specify the number of DAL producers for this test"
      1

  let protocol =
    let protocol_typ =
      let parse string =
        try
          Data_encoding.Json.from_string string
          |> Result.get_ok
          |> Data_encoding.Json.destruct Protocol.encoding
          |> Option.some
        with _ -> None
      in
      let show = Protocol.name in
      Clap.typ ~name:"protocol" ~dummy:Protocol.Alpha ~parse ~show
    in
    Clap.default
      ~section
      ~long:"protocol"
      ~placeholder:"<protocol_name> (such as alpha, oxford,...)"
      ~description:"Specify the economic protocol used for this test"
      protocol_typ
      Protocol.Alpha

  let producer_spreading_factor =
    Clap.default_int
      ~long:"producer-spreading-factor"
      ~description:
        "Per producer, slot are produced every n blocks where n is the \
         spreading factor"
      1
end

type configuration = {
  stake : int list;
  dal_node_producer : int;
  protocol : Protocol.t;
  producer_spreading_factor : int;
}

type bootstrap = {node : Node.t; dal_node : Dal_node.t; client : Client.t}

type baker = {
  node : Node.t;
  dal_node : Dal_node.t;
  baker : Baker.t;
  account : Account.key;
  stake : int;
}

type producer = {
  node : Node.t;
  dal_node : Dal_node.t;
  client : Client.t;
  account : Account.key;
  is_ready : unit Lwt.t;
}

type public_key_hash = string

type commitment = string

type per_level_info = {
  level : int;
  published_commitments : (int, commitment) Hashtbl.t;
  attestations : (public_key_hash, Z.t) Hashtbl.t;
  attested_commitments : Z.t;
}

type metrics = {
  level_first_commitment_published : int option;
  level_first_commitment_attested : int option;
  total_published_commitments : int;
  expected_published_commitments : int;
  total_attested_commitments : int;
  ratio_published_commitments : int;
  ratio_attested_commitments : int;
  ratio_attested_commitments_per_baker : (public_key_hash, int) Hashtbl.t;
}

let default_metrics =
  {
    level_first_commitment_published = None;
    level_first_commitment_attested = None;
    total_published_commitments = 0;
    expected_published_commitments = 0;
    total_attested_commitments = 0;
    ratio_published_commitments = 0;
    ratio_attested_commitments = 0;
    ratio_attested_commitments_per_baker = Hashtbl.create 0;
  }

type t = {
  configuration : configuration;
  cloud : Cloud.t;
  bootstrap : bootstrap;
  bakers : baker list;
  producers : producer list;
  parameters : Dal_common.Parameters.t;
  infos : (int, per_level_info) Hashtbl.t;
  metrics : (int, metrics) Hashtbl.t;
}

let pp_metrics t
    {
      level_first_commitment_published;
      level_first_commitment_attested;
      total_published_commitments;
      expected_published_commitments;
      total_attested_commitments;
      ratio_published_commitments;
      ratio_attested_commitments;
      ratio_attested_commitments_per_baker;
    } =
  (match level_first_commitment_published with
  | None -> ()
  | Some level_first_commitment_published ->
      Log.info
        "First commitment published level: %d"
        level_first_commitment_published) ;
  (match level_first_commitment_attested with
  | None -> ()
  | Some level_first_commitment_attested ->
      Log.info
        "First commitment attested level: %d"
        level_first_commitment_attested) ;
  Log.info "Total published commitments: %d" total_published_commitments ;
  Log.info "Expected published commitments: %d" expected_published_commitments ;
  Log.info "Total attested commitments: %d" total_attested_commitments ;
  Log.info "Ratio published commitments: %d" ratio_published_commitments ;
  Log.info "Ratio attested commitments: %d" ratio_attested_commitments ;
  t.bakers |> List.to_seq
  |> Seq.iter (fun {account; stake; _} ->
         match
           Hashtbl.find_opt
             ratio_attested_commitments_per_baker
             account.Account.public_key_hash
         with
         | None -> Log.info "No ratio for %s" account.Account.public_key_hash
         | Some ratio ->
             Log.info
               "Ratio for %s (with stake %d): %d"
               account.Account.public_key_hash
               stake
               ratio)

let push_metrics t
    {
      level_first_commitment_published = _;
      level_first_commitment_attested = _;
      total_published_commitments;
      expected_published_commitments;
      total_attested_commitments;
      ratio_published_commitments;
      ratio_attested_commitments;
      ratio_attested_commitments_per_baker;
    } =
  (* There are three metrics grouped by labels. *)
  t.bakers |> List.to_seq
  |> Seq.iter (fun {account; stake; _} ->
         let name =
           Format.asprintf
             "%s (stake: %d)"
             account.Account.public_key_hash
             stake
         in
         let value =
           match
             Hashtbl.find_opt
               ratio_attested_commitments_per_baker
               account.Account.public_key_hash
           with
           | None -> 0
           | Some d -> d
         in
         Cloud.push_metric
           t.cloud
           ~labels:[("attester", name)]
           ~name:"tezt_attested_ratio_per_baker"
           value) ;
  Cloud.push_metric
    t.cloud
    ~name:"tezt_commitments_ratio"
    ~labels:[("kind", "published")]
    ratio_published_commitments ;
  Cloud.push_metric
    t.cloud
    ~name:"tezt_commitments_ratio"
    ~labels:[("kind", "attested")]
    ratio_attested_commitments ;
  Cloud.push_metric
    t.cloud
    ~name:"tezt_commitments"
    ~labels:[("kind", "expected")]
    expected_published_commitments ;
  Cloud.push_metric
    t.cloud
    ~name:"tezt_commitments"
    ~labels:[("kind", "published")]
    total_published_commitments ;
  Cloud.push_metric
    t.cloud
    ~name:"tezt_commitments"
    ~labels:[("kind", "attested")]
    total_attested_commitments

let published_level_of_attested_level t level =
  level - t.parameters.attestation_lag

let update_level_first_commitment_published _t per_level_info metrics =
  match metrics.level_first_commitment_published with
  | None ->
      if Hashtbl.length per_level_info.published_commitments > 0 then
        Some per_level_info.level
      else None
  | Some l -> Some l

let update_level_first_commitment_attested _t per_level_info metrics =
  match metrics.level_first_commitment_attested with
  | None ->
      if Z.popcount per_level_info.attested_commitments > 0 then
        Some per_level_info.level
      else None
  | Some l -> Some l

let update_total_published_commitments _t per_level_info metrics =
  metrics.total_published_commitments
  + Hashtbl.length per_level_info.published_commitments

let update_expected_published_commitments t per_level_info metrics =
  match metrics.level_first_commitment_published with
  | None -> 0
  | Some _ ->
      (* -1 since we are looking at level n operation submitted at the previous
         level. *)
      let k =
        (per_level_info.level - 1) mod t.configuration.producer_spreading_factor
      in
      let producers =
        t.configuration.dal_node_producer - (k * t.parameters.number_of_slots)
        |> min t.parameters.number_of_slots
        |> max 0
      in
      metrics.expected_published_commitments + producers

let update_total_attested_commitments _t per_level_info metrics =
  metrics.total_attested_commitments
  + Z.popcount per_level_info.attested_commitments

let update_ratio_published_commitments _t _per_level_info metrics =
  if metrics.expected_published_commitments = 0 then 0
  else
    metrics.total_published_commitments * 100
    / metrics.expected_published_commitments

let update_ratio_attested_commitments t per_level_info metrics =
  match metrics.level_first_commitment_attested with
  | None -> 0
  | Some level_first_commitment_attested -> (
      let published_level =
        published_level_of_attested_level t per_level_info.level
      in
      match Hashtbl.find_opt t.infos published_level with
      | None ->
          Log.warn
            "Unexpected error: The level %d is missing in the infos table"
            published_level ;
          0
      | Some old_per_level_info ->
          let n = Hashtbl.length old_per_level_info.published_commitments in
          let weight = per_level_info.level - level_first_commitment_attested in
          if n = 0 then metrics.ratio_attested_commitments
          else
            let ratio =
              ((metrics.ratio_attested_commitments * weight)
              + (Z.popcount per_level_info.attested_commitments * 100 / n))
              / (weight + 1)
            in
            ratio)

let update_ratio_attested_commitments_per_baker t per_level_info metrics =
  match metrics.level_first_commitment_attested with
  | None -> Hashtbl.create 0
  | Some level_first_commitment_attested -> (
      let published_level =
        published_level_of_attested_level t per_level_info.level
      in
      match Hashtbl.find_opt t.infos published_level with
      | None ->
          Log.warn
            "Unexpected error: The level %d is missing in the infos table"
            published_level ;
          Hashtbl.create 0
      | Some old_per_level_info ->
          let n = Hashtbl.length old_per_level_info.published_commitments in
          let weight = per_level_info.level - level_first_commitment_attested in
          t.bakers |> List.to_seq
          |> Seq.map (fun ({account; _} : baker) ->
                 let bitset =
                   match
                     Hashtbl.find_opt
                       per_level_info.attestations
                       account.Account.public_key_hash
                   with
                   | None -> Z.zero
                   | Some z -> z
                 in
                 let old_ratio =
                   match
                     Hashtbl.find_opt
                       metrics.ratio_attested_commitments_per_baker
                       account.Account.public_key_hash
                   with
                   | None -> 0
                   | Some ratio -> ratio
                 in
                 if n = 0 then (account.Account.public_key_hash, old_ratio)
                 else
                   ( account.Account.public_key_hash,
                     ((old_ratio * weight) + (Z.popcount bitset * 100 / n))
                     / (weight + 1) ))
          |> Hashtbl.of_seq)

let get_metrics t infos_per_level metrics =
  let level_first_commitment_published =
    update_level_first_commitment_published t infos_per_level metrics
  in
  let level_first_commitment_attested =
    update_level_first_commitment_attested t infos_per_level metrics
  in
  (* Metrics below depends on the new value for the metrics above. *)
  let metrics =
    {
      metrics with
      level_first_commitment_attested;
      level_first_commitment_published;
    }
  in
  let total_published_commitments =
    update_total_published_commitments t infos_per_level metrics
  in
  let expected_published_commitments =
    update_expected_published_commitments t infos_per_level metrics
  in
  let total_attested_commitments =
    update_total_attested_commitments t infos_per_level metrics
  in
  (* Metrics below depends on the new value for the metrics above. *)
  let metrics =
    {
      metrics with
      level_first_commitment_attested;
      level_first_commitment_published;
      total_published_commitments;
      expected_published_commitments;
      total_attested_commitments;
    }
  in
  let ratio_published_commitments =
    update_ratio_published_commitments t infos_per_level metrics
  in
  let ratio_attested_commitments =
    update_ratio_attested_commitments t infos_per_level metrics
  in
  let ratio_attested_commitments_per_baker =
    update_ratio_attested_commitments_per_baker t infos_per_level metrics
  in
  {
    level_first_commitment_published;
    level_first_commitment_attested;
    total_published_commitments;
    expected_published_commitments;
    total_attested_commitments;
    ratio_published_commitments;
    ratio_attested_commitments;
    ratio_attested_commitments_per_baker;
  }

let get_infos_per_level client ~level =
  let block = string_of_int level in
  let* header =
    Client.RPC.call client @@ RPC.get_chain_block_header ~block ()
  in
  let* metadata =
    Client.RPC.call client @@ RPC.get_chain_block_metadata_raw ~block ()
  in
  let* operations =
    Client.RPC.call client @@ RPC.get_chain_block_operations ~block ()
  in
  let level = JSON.(header |-> "level" |> as_int) in
  let attested_commitments =
    JSON.(metadata |-> "dal_attestation" |> as_string |> Z.of_string)
  in
  let manager_operations = JSON.(operations |=> 3 |> as_list) in
  let is_published_commitment operation =
    JSON.(
      operation |-> "contents" |=> 0 |-> "kind" |> as_string
      = "dal_publish_commitment")
  in
  let get_commitment operation =
    JSON.(
      operation |-> "contents" |=> 0 |-> "slot_header" |-> "commitment"
      |> as_string)
  in
  let get_slot_index operation =
    JSON.(
      operation |-> "contents" |=> 0 |-> "slot_header" |-> "slot_index"
      |> as_int)
  in
  let published_commitments =
    manager_operations |> List.to_seq
    |> Seq.filter is_published_commitment
    |> Seq.map (fun operation ->
           (get_slot_index operation, get_commitment operation))
    |> Hashtbl.of_seq
  in
  let consensus_operations = JSON.(operations |=> 0 |> as_list) in
  let is_dal_attestation operation =
    JSON.(
      operation |-> "contents" |=> 0 |-> "kind" |> as_string
      = "attestation_with_dal")
  in
  let get_public_key_hash operation =
    JSON.(
      operation |-> "contents" |=> 0 |-> "metadata" |-> "delegate" |> as_string)
  in
  let get_dal_attestation operation =
    JSON.(
      operation |-> "contents" |=> 0 |-> "dal_attestation" |> as_string
      |> Z.of_string)
  in
  let attestations =
    consensus_operations |> List.to_seq
    |> Seq.filter is_dal_attestation
    |> Seq.map (fun operation ->
           let public_key_hash = get_public_key_hash operation in
           let dal_attestation = get_dal_attestation operation in
           (public_key_hash, dal_attestation))
    |> Hashtbl.of_seq
  in
  Lwt.return {level; published_commitments; attestations; attested_commitments}

let add_source cloud agent ~job_name node dal_node =
  let agent_name = Agent.name agent in
  let node_metric_target =
    Cloud.
      {
        agent;
        port = Node.metrics_port node;
        app_name = Format.asprintf "%s:%s" agent_name (Node.name node);
      }
  in
  let dal_node_metric_target =
    Cloud.
      {
        agent;
        port = Dal_node.metrics_port dal_node;
        app_name = Format.asprintf "%s:%s" agent_name (Dal_node.name dal_node);
      }
  in
  Cloud.add_prometheus_source
    cloud
    ~job_name
    [node_metric_target; dal_node_metric_target]

let init_bootstrap cloud (configuration : configuration) agent =
  let* bootstrap_node = Node.Agent.create ~name:"bootstrap-node" agent in
  let* dal_bootstrap_node =
    Dal_node.Agent.create ~name:"bootstrap-dal-node" agent ~node:bootstrap_node
  in
  let* () = Node.config_init bootstrap_node [] in
  let config : Cryptobox.Config.t =
    {
      activated = true;
      use_mock_srs_for_testing = false;
      bootstrap_peers = [Dal_node.point_str dal_bootstrap_node];
    }
  in
  let* () =
    Node.Config_file.update
      bootstrap_node
      (Node.Config_file.set_sandbox_network_with_dal_config config)
  in
  let* () =
    Node.run
      bootstrap_node
      [No_bootstrap_peers; Synchronisation_threshold 0; Cors_origin "*"]
  in
  let* () = Node.wait_for_ready bootstrap_node in
  let* client = Client.init ~endpoint:(Node bootstrap_node) () in
  let* baker_accounts =
    Client.stresstest_gen_keys (List.length configuration.stake) client
  in
  let* producer_accounts =
    Client.stresstest_gen_keys configuration.dal_node_producer client
  in
  let* parameter_file =
    let base =
      Either.right (configuration.protocol, Some Protocol.Constants_mainnet)
    in
    let bootstrap_accounts =
      List.mapi
        (fun i key ->
          (key, Some (List.nth configuration.stake i * 1_000_000_000_000)))
        baker_accounts
    in
    let additional_bootstrap_accounts =
      List.map
        (fun key -> (key, Some 1_000_000_000_000, false))
        producer_accounts
    in
    Protocol.write_parameter_file
      ~bootstrap_accounts
      ~additional_bootstrap_accounts
      ~base
      []
  in
  let* () =
    Client.activate_protocol_and_wait
      ~timestamp:Client.Now
      ~parameter_file
      ~protocol:configuration.protocol
      client
  in
  let* () =
    Dal_node.init_config
      ~expected_pow:0.
      ~bootstrap_profile:true
      dal_bootstrap_node
  in
  let* () = Dal_node.run ~event_level:`Notice dal_bootstrap_node in
  let* () =
    add_source
      cloud
      agent
      ~job_name:"bootstrap"
      bootstrap_node
      dal_bootstrap_node
  in
  let (bootstrap : bootstrap) =
    {node = bootstrap_node; dal_node = dal_bootstrap_node; client}
  in
  Lwt.return (bootstrap, baker_accounts, producer_accounts)

let init_baker cloud (configuration : configuration) ~bootstrap_node
    ~dal_bootstrap_node account i agent =
  let stake = List.nth configuration.stake i in
  let* node =
    Node.Agent.init
      ~name:(Format.asprintf "baker-node-%d" i)
      ~arguments:
        [Peer (Node.point_str bootstrap_node); Synchronisation_threshold 0]
      agent
  in
  let* dal_node =
    let* dal_node =
      Dal_node.Agent.create
        ~name:(Format.asprintf "baker-dal-node-%d" i)
        ~node
        agent
    in
    let* () =
      Dal_node.init_config
        ~expected_pow:0.
        ~attester_profiles:[account.Account.public_key_hash]
        ~peers:[Dal_node.point_str dal_bootstrap_node] (* no need for peer *)
        dal_node
    in
    let* () = Dal_node.run ~event_level:`Notice dal_node in
    Lwt.return dal_node
  in
  let* client = Client.Agent.create agent in
  let* () =
    Client.import_secret_key
      client
      account.Account.secret_key
      ~alias:account.alias
  in
  let* baker =
    Baker.Agent.init
      ~name:(Format.asprintf "baker-%d" i)
      ~delegate:account.Account.alias
      ~protocol:configuration.protocol
      ~client
      dal_node
      node
      agent
  in
  let* () =
    add_source
      cloud
      agent
      ~job_name:(Format.asprintf "baker-%d" i)
      node
      dal_node
  in
  Lwt.return {node; dal_node; baker; account; stake}

let init_producer cloud ~bootstrap_node ~dal_bootstrap_node ~number_of_slots
    account i agent =
  let* node =
    Node.Agent.init
      ~name:(Format.asprintf "producer-node-%i" i)
      ~arguments:
        [Peer (Node.point_str bootstrap_node); Synchronisation_threshold 0]
      agent
  in
  let* client = Client.Agent.create ~node agent in
  let* () =
    Client.import_secret_key
      client
      ~endpoint:(Node node)
      account.Account.secret_key
      ~alias:account.Account.alias
  in
  let*! () =
    Client.reveal client ~endpoint:(Node node) ~src:account.Account.alias
  in
  let* dal_node =
    Dal_node.Agent.create
      ~name:(Format.asprintf "producer-dal-node-%i" i)
      ~node
      agent
  in
  let* () =
    Dal_node.init_config
      ~expected_pow:0.
      ~producer_profiles:[i mod number_of_slots]
      ~peers:[Dal_node.point_str dal_bootstrap_node]
      dal_node
  in
  let* () =
    add_source
      cloud
      agent
      ~job_name:(Format.asprintf "producer-%d" i)
      node
      dal_node
  in
  (* We do not wait on the promise because loading the SRS takes some time.
     Instead we will publish commitments only once this promise is fulfilled. *)
  let is_ready = Dal_node.run ~event_level:`Notice dal_node in
  Lwt.return {client; node; dal_node; account; is_ready}

let init ~configuration cloud next_agent =
  let* bootstrap_agent = next_agent ~name:"bootstrap" in
  let* producers_agents =
    List.init configuration.dal_node_producer (fun i ->
        let name = Format.asprintf "producer-%d" i in
        next_agent ~name)
    |> Lwt.all
  in
  let* attesters_agents =
    List.init (List.length configuration.stake) (fun i ->
        let name = Format.asprintf "attester-%d" i in
        next_agent ~name)
    |> Lwt.all
  in
  let* bootstrap, baker_accounts, producer_accounts =
    init_bootstrap cloud configuration bootstrap_agent
  in
  let* bakers =
    Lwt_list.mapi_p
      (fun i (agent, account) ->
        init_baker
          cloud
          configuration
          ~bootstrap_node:bootstrap.node
          ~dal_bootstrap_node:bootstrap.dal_node
          account
          i
          agent)
      (List.combine attesters_agents baker_accounts)
  in
  let client = Client.create ~endpoint:(Node bootstrap.node) () in
  let* parameters = Dal_common.Parameters.from_client client in
  let* producers =
    Lwt_list.mapi_p
      (fun i (agent, account) ->
        init_producer
          cloud
          ~bootstrap_node:bootstrap.node
          ~dal_bootstrap_node:bootstrap.dal_node
          ~number_of_slots:parameters.number_of_slots
          account
          i
          agent)
      (List.combine producers_agents producer_accounts)
  in
  let infos = Hashtbl.create 101 in
  let metrics = Hashtbl.create 101 in
  Hashtbl.replace metrics 1 default_metrics ;
  Lwt.return
    {
      cloud;
      configuration;
      bootstrap;
      bakers;
      producers;
      parameters;
      infos;
      metrics;
    }

let on_new_level t level =
  let node = t.bootstrap.node in
  let client = t.bootstrap.client in
  let* () =
    let* _ = Node.wait_for_level node level in
    Lwt.return_unit
  in
  Log.info "Start process level %d" level ;
  let* infos_per_level = get_infos_per_level client ~level in
  Hashtbl.replace t.infos level infos_per_level ;
  let metrics =
    get_metrics t infos_per_level (Hashtbl.find t.metrics (level - 1))
  in
  pp_metrics t metrics ;
  push_metrics t metrics ;
  Hashtbl.replace t.metrics level metrics ;
  Lwt.return_unit

let produce_slot t level i =
  let producer = List.nth t.producers i in
  let index = i mod t.parameters.number_of_slots in
  let should_publish =
    level mod t.configuration.producer_spreading_factor
    = i / t.parameters.number_of_slots
  in
  if not should_publish then Lwt.return_unit
  else
    let content =
      Format.asprintf "%d:%d" level index
      |> Helpers.make_slot
           ~padding:false
           ~slot_size:t.parameters.cryptobox.slot_size
    in
    let* _ = Node.wait_for_level producer.node level in
    let* _ =
      Dal.publish_and_store_slot
        ~dont_wait:true
        ~with_proof:true
        producer.client
        producer.dal_node
        producer.account
        ~force:true
        ~index
        content
    in
    Lwt.return_unit

let producers_ready t =
  (* If not all the producer nodes are ready, we do not publish the commitment
       for the current level. Another attempt will be done at the next level. *)
  let producer_ready producer =
    match Lwt.state producer.is_ready with
    | Sleep -> true
    | Fail exn -> Lwt.reraise exn
    | Return () -> false
  in
  List.for_all producer_ready t.producers

let rec loop t level =
  let p = on_new_level t level in
  let _p2 =
    if producers_ready t then Lwt.return_unit
    else
      Seq.ints 0
      |> Seq.take t.configuration.dal_node_producer
      |> Seq.map (fun i -> produce_slot t level i)
      |> List.of_seq |> Lwt.join
  in
  let* () = p in
  loop t (level + 1)

let configuration =
  let stake = Cli.stake in
  let dal_node_producer = Cli.producers in
  let protocol = Cli.protocol in
  let producer_spreading_factor = Cli.producer_spreading_factor in
  {stake; dal_node_producer; protocol; producer_spreading_factor}

let benchmark () =
  let vms =
    1 + List.length configuration.stake + configuration.dal_node_producer
  in
  Cloud.register
    ~vms
    ~__FILE__
    ~title:"DAL node benchmark"
    ~tags:[Tag.cloud; "dal"; "benchmark"]
    (fun cloud ->
      match Cloud.agents cloud with
      | [] -> Test.fail "The test should run with a positive number of agents."
      | agents ->
          (* We give to the [init] function a sequence of agents (and cycle if
             they were all consumed). We set their name only if the number of
             agents is the computed one. Otherwise, the user has mentioned
             explicitely a reduced number of agents and it is not clear how to give
             them proper names. *)
          let set_name agent name =
            if List.length agents = vms then
              Cloud.set_agent_name cloud agent name
            else Lwt.return_unit
          in
          let next_agent =
            let f = List.to_seq agents |> Seq.cycle |> Seq.to_dispenser in
            fun ~name ->
              let agent = f () |> Option.get in
              let* () = set_name agent name in
              Lwt.return agent
          in
          let* t = init ~configuration cloud next_agent in
          let first_protocol_level = 2 in
          loop t first_protocol_level)

let register () = benchmark ()
