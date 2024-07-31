(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Cryptobox = Dal_common.Cryptobox
module Helpers = Dal_common.Helpers
open Tezos

let toplog (fmt : ('a, Format.formatter, unit, unit) format4) : 'a =
  Log.info ~prefix:"TOP" ~color:Log.Color.FG.green fmt

module Disconnect = struct
  module IMap = Map.Make (Int)

  (* The [disconnected_bakers] map contains bakers indexes whose DAL node
     have been disconnected, associated with the level at which they have
     been disconnected.
     Each [frequency] number of levels, a baker, chosen in a round-robin
     fashion, is disconnected.
     A disconnected baker reconnects after [reconnection_delay] levels.
     The next baker to disconnect is stored in [next_to_disconnect];
     it is 0 when no baker has been disconnected yet *)
  type t = {
    disconnected_bakers : int IMap.t;
    frequency : int;
    reconnection_delay : int;
    next_to_disconnect : int;
  }

  let init (frequency, reconnection_delay) =
    {
      disconnected_bakers = IMap.empty;
      frequency;
      reconnection_delay;
      next_to_disconnect = 0;
    }

  (* When a relevant level is reached, [disconnect t level f] puts the baker of
     index [t.next_to_disconnect] in [t.disconnected_bakers] and applies [f] to
     this baker. If it is already disconnected, the function does nothing and
     returns [t] unchanged *)
  let disconnect t level f =
    if level mod t.frequency <> 0 then Lwt.return t
    else
      match IMap.find_opt t.next_to_disconnect t.disconnected_bakers with
      | Some _ ->
          toplog
            "disconnect: all bakers have been disconnected, waiting for next \
             baker to reconnect." ;
          Lwt.return t
      | None ->
          let* () = f t.next_to_disconnect in
          Lwt.return
            {
              t with
              disconnected_bakers =
                IMap.add t.next_to_disconnect level t.disconnected_bakers;
              next_to_disconnect = t.next_to_disconnect + 1;
            }

  (* Applies [f] on the bakers DAL nodes that have been disconnected for long
     enough *)
  let reconnect t level f =
    let bakers_to_reconnect, bakers_to_keep_disconnected =
      IMap.partition
        (fun _ disco_level -> level >= disco_level + t.reconnection_delay)
        t.disconnected_bakers
    in
    let* () =
      IMap.to_seq bakers_to_reconnect
      |> List.of_seq
      |> Lwt_list.iter_p (fun (b, _) -> f b)
    in
    Lwt.return {t with disconnected_bakers = bakers_to_keep_disconnected}
end

module Network = struct
  type t = Ghostnet | Sandbox

  let to_string = function Ghostnet -> "ghostnet" | Sandbox -> "sandbox"

  let get_level network endpoint =
    match network with
    | Sandbox -> Lwt.return 1
    | Ghostnet ->
        let* json = RPC_core.call endpoint (RPC.get_chain_block_header ()) in
        JSON.(json |-> "level" |> as_int) |> Lwt.return
end

module Node = struct
  let runner_of_agent = Agent.runner

  include Node

  let init ?(arguments = []) ?data_dir ?dal_config ~name network agent =
    toplog "Inititializing a node" ;
    match network with
    | Network.Ghostnet -> (
        match data_dir with
        | Some data_dir ->
            let* node = Node.Agent.create ~arguments ~data_dir ~name agent in
            let* () = Node.run node [Network "ghostnet"] in
            let* () = Node.wait_for_ready node in
            Lwt.return node
        | None ->
            toplog
              "No data dir given, we will attempt to bootstrap the node from a \
               rolling snapshot." ;
            toplog "Creating the agent." ;
            let* node =
              Node.Agent.create
                ~arguments:
                  [Network "ghostnet"; Expected_pow 26; Cors_origin "*"]
                ?data_dir
                ~name
                agent
            in
            toplog "Downloading a rolling snapshot" ;
            let* () =
              Process.spawn
                ~runner:(runner_of_agent agent)
                "wget"
                [
                  "-O";
                  "snapshot_file";
                  "https://snapshots.eu.tzinit.org/ghostnet/rolling";
                ]
              |> Process.check
            in
            toplog "Initializing node configuration" ;
            let* () = Node.config_init node [] in
            toplog "Importing the snapshot" ;
            let* () =
              Node.snapshot_import ~no_check:true node "snapshot_file"
            in
            toplog "Launching the node." ;
            let* () = Node.run node arguments in
            toplog "Waiting for the node to be ready." ;
            let* () = wait_for_ready node in
            toplog "Node is ready." ;
            Lwt.return node)
    | Sandbox -> (
        match data_dir with
        | None ->
            let* node = Node.Agent.create ~name agent in
            let* () = Node.config_init node [] in
            let* () =
              match dal_config with
              | None -> Lwt.return_unit
              | Some config ->
                  Node.Config_file.update
                    node
                    (Node.Config_file.set_sandbox_network_with_dal_config
                       config)
            in
            let* () =
              Node.run
                node
                ([
                   No_bootstrap_peers;
                   Synchronisation_threshold 0;
                   Cors_origin "*";
                 ]
                @ arguments)
            in
            let* () = wait_for_ready node in
            Lwt.return node
        | Some data_dir ->
            let arguments =
              [No_bootstrap_peers; Synchronisation_threshold 0; Cors_origin "*"]
              @ arguments
            in
            let* node = Node.Agent.create ~arguments ~data_dir ~name agent in
            let* () = Node.run node [] in
            let* () = Node.wait_for_ready node in
            Lwt.return node)
end

module Cli = struct
  let section =
    Clap.section
      ~description:
        "All the options related to running DAL scenarions onto the cloud"
      "Cloud DAL"

  let fundraiser =
    Clap.default_string
      ~section
      ~long:"fundraiser"
      ~description:
        "Fundraiser secret key that have enough money of test network"
      "edsk3AWajGUgzzGi3UrQiNWeRZR1YMRYVxfe642AFSKBTFXaoJp5hu"

  let network_typ =
    Clap.typ
      ~name:"network"
      ~dummy:Network.Ghostnet
      ~parse:(function
        | "ghostnet" -> Some Ghostnet | "sandbox" -> Some Sandbox | _ -> None)
      ~show:Network.to_string

  let network =
    Clap.default
      ~section
      ~long:"network"
      ~placeholder:"<network> (sandbox,ghostnet,...)"
      ~description:"Allow to specify a network to use for the scenario"
      network_typ
      Sandbox

  let bootstrap =
    Clap.flag
      ~section
      ~set_long:"bootstrap"
      (match network with Sandbox -> true | Ghostnet -> false)

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
      (match network with Sandbox -> [100] | Ghostnet -> [])

  let stake_machine_type =
    let stake_machine_type_typ =
      let parse string =
        try string |> String.split_on_char ',' |> Option.some with _ -> None
      in
      let show l = l |> String.concat "," in
      Clap.typ ~name:"stake_machine_type" ~dummy:["foo"] ~parse ~show
    in
    Clap.optional
      ~section
      ~long:"stake-machine-type"
      ~placeholder:"<machine_type>,<machine_type>,<machine_type>, ..."
      ~description:
        "Specify the machine type used by the stake. The nth machine type will \
         be assigned to the nth stake specified with [--stake]. If less \
         machine types are specified, the default one will be used."
      stake_machine_type_typ
      ()

  let producers =
    Clap.default_int
      ~section
      ~long:"producers"
      ~description:"Specify the number of DAL producers for this test"
      0

  let producer_machine_type =
    Clap.optional_string
      ~section
      ~long:"producer-machine-type"
      ~description:"Machine type used for the DAL producers"
      ()

  let observer_slot_indices =
    let slot_indices_typ =
      let parse string =
        try
          string |> String.split_on_char ',' |> List.map int_of_string
          |> Option.some
        with _ ->
          raise
            (Invalid_argument
               (Printf.sprintf
                  "Cli.observer_slot_indices: could not parse %s"
                  string))
      in
      let show l = l |> List.map string_of_int |> String.concat "," in
      Clap.typ ~name:"observer-slot-indices" ~dummy:[] ~parse ~show
    in
    Clap.default
      ~section
      ~long:"observer-slot-indices"
      ~placeholder:"<slot_index>,<slot_index>,<slot_index>, ..."
      ~description:
        "For each slot index specified, an observer will be created to observe \
         this slot index."
      slot_indices_typ
      []

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
      (match network with Sandbox -> Alpha | Ghostnet -> ParisC)

  let data_dir =
    Clap.optional_string ~section ~long:"data-dir" ~placeholder:"<data_dir>" ()

  let etherlink = Clap.flag ~section ~set_long:"etherlink" false

  let etherlink_sequencer =
    (* We want the sequencer to be active by default if etherlink is activated. *)
    Clap.flag ~section ~unset_long:"no-etherlink-sequencer" etherlink

  let etherlink_producers =
    Clap.default_int ~section ~long:"etherlink-producers" 0

  let disconnect =
    let disconnect_typ =
      let parse string =
        try
          match String.split_on_char ',' string with
          | [disconnection; reconnection] ->
              Some (int_of_string disconnection, int_of_string reconnection)
          | _ -> None
        with _ -> None
      in
      let show (d, r) = Format.sprintf "%d,%d" d r in
      Clap.typ ~name:"disconnect" ~dummy:(10, 10) ~parse ~show
    in
    Clap.optional
      ~section
      ~long:"disconnect"
      ~placeholder:"<disconnect_frequency>,<levels_disconnected>"
      ~description:
        "If this argument is provided, bakers will disconnect in turn each \
         <disconnect_frequency> levels, and each will reconnect after a delay \
         of <levels_disconnected> levels."
      disconnect_typ
      ()

  let etherlink_dal_slots =
    Clap.list_int ~section ~long:"etherlink-dal-slots" ()
end

type configuration = {
  stake : int list;
  stake_machine_type : string list option;
  dal_node_producer : int;
  observer_slot_indices : int list;
  protocol : Protocol.t;
  producer_machine_type : string option;
  etherlink : bool;
  etherlink_sequencer : bool;
  etherlink_producers : int;
  (* The first argument is the deconnection frequency, the second is the
     reconnection delay *)
  disconnect : (int * int) option;
  network : Network.t;
  bootstrap : bool;
  (* Empty list means DAL FF is set to false. *)
  etherlink_dal_slots : int list;
}

type bootstrap = {
  node : Node.t option;
  dal_node : Dal_node.t option;
  node_p2p_endpoint : string;
  node_rpc_endpoint : Endpoint.t;
  dal_node_p2p_endpoint : string;
  client : Client.t;
}

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

type observer = {node : Node.t; dal_node : Dal_node.t; slot_index : int}

type etherlink_operator_setup = {
  node : Node.t;
  client : Client.t;
  sc_rollup_node : Sc_rollup_node.t;
  evm_node : Tezt_etherlink.Evm_node.t;
  is_sequencer : bool;
  sc_rollup_address : string;
  account : Account.key;
}

type etherlink_producer_setup = {
  agent : Agent.t;
  node : Node.t;
  client : Client.t;
  sc_rollup_node : Sc_rollup_node.t;
  evm_node : Tezt_etherlink.Evm_node.t;
  account : Tezt_etherlink.Eth_account.t;
}

type etherlink = {
  operator : etherlink_operator_setup;
  producers : etherlink_producer_setup list;
  accounts : Tezt_etherlink.Eth_account.t Array.t;
}

type public_key_hash = string

type commitment = string

type per_level_info = {
  level : int;
  published_commitments : (int, commitment) Hashtbl.t;
  attestations : (public_key_hash, Z.t option) Hashtbl.t;
  attested_commitments : Z.t;
}

type metrics = {
  level_first_commitment_published : int option;
  level_first_commitment_attested : int option;
  total_published_commitments : int;
  expected_published_commitments : int;
  total_attested_commitments : int;
  ratio_published_commitments : float;
  ratio_attested_commitments : float;
  ratio_published_commitments_last_level : float;
  ratio_attested_commitments_per_baker : (public_key_hash, float) Hashtbl.t;
}

let default_metrics =
  {
    level_first_commitment_published = None;
    level_first_commitment_attested = None;
    total_published_commitments = 0;
    expected_published_commitments = 0;
    total_attested_commitments = 0;
    ratio_published_commitments = 0.;
    ratio_attested_commitments = 0.;
    ratio_published_commitments_last_level = 0.;
    ratio_attested_commitments_per_baker = Hashtbl.create 0;
  }

type t = {
  configuration : configuration;
  cloud : Cloud.t;
  bootstrap : bootstrap;
  bakers : baker list;
  producers : producer list;
  observers : observer list;
  etherlink : etherlink option;
  parameters : Dal_common.Parameters.t;
  infos : (int, per_level_info) Hashtbl.t;
  metrics : (int, metrics) Hashtbl.t;
  disconnection_state : Disconnect.t option;
  first_level : int;
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
      ratio_published_commitments_last_level;
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
  Log.info "Ratio published commitments: %f" ratio_published_commitments ;
  Log.info "Ratio attested commitments: %f" ratio_attested_commitments ;
  Log.info
    "Ratio published commitments last level: %f"
    ratio_published_commitments_last_level ;
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
               "Ratio for %s (with stake %d): %f"
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
      ratio_published_commitments_last_level;
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
           | None -> 0.
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
    ~name:"tezt_commitments_ratio"
    ~labels:[("kind", "published_last_level")]
    ratio_published_commitments_last_level ;
  Cloud.push_metric
    t.cloud
    ~name:"tezt_commitments"
    ~labels:[("kind", "expected")]
    (float_of_int expected_published_commitments) ;
  Cloud.push_metric
    t.cloud
    ~name:"tezt_commitments"
    ~labels:[("kind", "published")]
    (float_of_int total_published_commitments) ;
  Cloud.push_metric
    t.cloud
    ~name:"tezt_commitments"
    ~labels:[("kind", "attested")]
    (float_of_int total_attested_commitments)

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

let update_expected_published_commitments t metrics =
  match metrics.level_first_commitment_published with
  | None -> 0
  | Some _ ->
      (* -1 since we are looking at level n operation submitted at the previous
         level. *)
      let producers =
        min t.configuration.dal_node_producer t.parameters.number_of_slots
      in
      metrics.expected_published_commitments + producers

let update_total_attested_commitments _t per_level_info metrics =
  metrics.total_attested_commitments
  + Z.popcount per_level_info.attested_commitments

let update_ratio_published_commitments _t _per_level_info metrics =
  if metrics.expected_published_commitments = 0 then 0.
  else
    float_of_int metrics.total_published_commitments
    *. 100.
    /. float_of_int metrics.expected_published_commitments

let update_ratio_published_commitments_last_level t per_level_info metrics =
  match metrics.level_first_commitment_published with
  | None -> 0.
  | Some _ ->
      let producers =
        min t.configuration.dal_node_producer t.parameters.number_of_slots
      in
      if producers = 0 then 100.
      else
        float_of_int (Hashtbl.length per_level_info.published_commitments)
        *. 100. /. float_of_int producers

let update_ratio_attested_commitments t per_level_info metrics =
  match metrics.level_first_commitment_attested with
  | None -> 0.
  | Some level_first_commitment_attested -> (
      let published_level =
        published_level_of_attested_level t per_level_info.level
      in
      match Hashtbl.find_opt t.infos published_level with
      | None ->
          Log.warn
            "Unexpected error: The level %d is missing in the infos table"
            published_level ;
          0.
      | Some old_per_level_info ->
          let n = Hashtbl.length old_per_level_info.published_commitments in
          let weight =
            per_level_info.level - level_first_commitment_attested
            |> float_of_int
          in
          if n = 0 then metrics.ratio_attested_commitments
          else
            let bitset =
              Z.popcount per_level_info.attested_commitments * 100 / n
              |> float_of_int
            in
            let ratio =
              ((metrics.ratio_attested_commitments *. weight) +. bitset)
              /. (weight +. 1.)
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
          let weight =
            per_level_info.level - level_first_commitment_attested
            |> float_of_int
          in
          t.bakers |> List.to_seq
          |> Seq.map (fun ({account; _} : baker) ->
                 let bitset =
                   float_of_int
                   @@
                   match
                     Hashtbl.find_opt
                       per_level_info.attestations
                       account.Account.public_key_hash
                   with
                   | None -> (* No attestation in block *) 0
                   | Some (Some z) when n = 0 ->
                       if z = Z.zero then (* No slot were published. *) 100
                       else
                         Test.fail
                           "Wow wow wait! It seems an invariant is broken. \
                            Either on the test side, or on the DAL node side"
                   | Some (Some z) ->
                       (* Attestation with DAL payload *)
                       if n = 0 then 100 else Z.popcount z * 100 / n
                   | Some None ->
                       (* Attestation without DAL payload: no DAL rights. *) 100
                 in
                 let old_ratio =
                   match
                     Hashtbl.find_opt
                       metrics.ratio_attested_commitments_per_baker
                       account.Account.public_key_hash
                   with
                   | None -> 0.
                   | Some ratio -> ratio
                 in
                 if n = 0 then (account.Account.public_key_hash, old_ratio)
                 else
                   ( account.Account.public_key_hash,
                     ((old_ratio *. weight) +. bitset) /. (weight +. 1.) ))
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
    update_expected_published_commitments t metrics
  in
  let ratio_published_commitments_last_level =
    update_ratio_published_commitments_last_level t infos_per_level metrics
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
      ratio_published_commitments_last_level;
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
    ratio_published_commitments_last_level;
    ratio_attested_commitments_per_baker;
  }

let get_infos_per_level client ~level =
  let block = string_of_int level in
  let* header = Client.RPC.call client @@ RPC.get_chain_block_header ~block ()
  and* metadata =
    Client.RPC.call client @@ RPC.get_chain_block_metadata_raw ~block ()
  and* operations =
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
      |> Z.of_string |> Option.some)
  in
  let attestations =
    consensus_operations |> List.to_seq
    |> Seq.map (fun operation ->
           let public_key_hash = get_public_key_hash operation in
           let dal_attestation =
             if is_dal_attestation operation then get_dal_attestation operation
             else None
           in
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

let add_etherlink_source cloud agent ~job_name node sc_rollup_node evm_node =
  let agent_name = Agent.name agent in
  let node_metric_target =
    Cloud.
      {
        agent;
        port = Node.metrics_port node;
        app_name = Format.asprintf "%s:%s" agent_name (Node.name node);
      }
  in
  let sc_rollup_metric_target =
    let metrics = Sc_rollup_node.metrics sc_rollup_node in
    Cloud.
      {
        agent;
        port = snd metrics;
        app_name =
          Format.asprintf
            "%s:%s"
            agent_name
            (Sc_rollup_node.name sc_rollup_node);
      }
  in
  let evm_node_metric_target =
    Cloud.
      {
        agent;
        port = Tezos.Evm_node.rpc_port evm_node;
        app_name =
          Format.asprintf
            "%s:%s"
            agent_name
            (Tezt_etherlink.Evm_node.name evm_node);
      }
  in
  Cloud.add_prometheus_source
    cloud
    ~job_name
    [node_metric_target; sc_rollup_metric_target; evm_node_metric_target]

let init_ghostnet cloud (configuration : configuration) agent =
  toplog "Init testnet" ;
  let* bootstrap =
    match agent with
    | None ->
        let () = toplog "No agent given" in
        let node = None in
        let dal_node = None in
        (* Taken from ghostnet configuration. *)
        let node_p2p_endpoint = "ghostnet.tzinit.org" in
        let dal_node_p2p_endpoint = "dalboot.ghostnet.tzinit.org" in
        let node_rpc_endpoint =
          Endpoint.
            {scheme = "https"; host = "rpc.ghostnet.teztnets.com"; port = 443}
        in
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
        let () = toplog "Some agent given" in
        let () = toplog "Initializing the bootstrap node agent" in
        let* node =
          Node.init ~name:"bootstrap-node" configuration.network agent
        in
        let* dal_node =
          Dal_node.Agent.create ~name:"bootstrap-dal-node" agent ~node
        in
        let* () =
          Dal_node.init_config
            ~expected_pow:26.
            ~bootstrap_profile:true
            dal_node
        in
        let* () = Node.wait_for_ready node in
        let* () = add_source cloud agent ~job_name:"bootstrap" node dal_node in
        let* () = Dal_node.run ~event_level:`Notice dal_node in
        let client = Client.create ~endpoint:(Node node) () in
        let node_rpc_endpoint =
          Endpoint.
            {
              scheme = "http";
              host = Agent.point agent |> fst;
              port = Node.rpc_port node;
            }
        in
        let bootstrap =
          {
            node = Some node;
            dal_node = Some dal_node;
            node_p2p_endpoint = Node.point_str node;
            node_rpc_endpoint;
            dal_node_p2p_endpoint = Dal_node.point_str dal_node;
            client;
          }
        in
        Lwt.return bootstrap
  in
  let () = toplog "Initializing the bakers" in
  let* baker_accounts =
    Client.stresstest_gen_keys
      ~alias_prefix:"baker"
      (List.length configuration.stake)
      bootstrap.client
  in
  let () = toplog "Initializing the producers" in
  let* producer_accounts =
    Client.stresstest_gen_keys
      ~alias_prefix:"dal_producer"
      configuration.dal_node_producer
      bootstrap.client
  in
  let () = toplog "Funding the producer accounts" in
  let* () =
    Client.import_secret_key
      bootstrap.client
      (Unencrypted Cli.fundraiser)
      ~alias:"fundraiser"
  in
  let () = toplog "Revealing the fundraiser public key" in
  let* () =
    let*? process = Client.reveal ~src:"fundraiser" bootstrap.client in
    let* _ = Process.wait process in
    Lwt.return_unit
  in
  let* fundraiser = Client.show_address ~alias:"fundraiser" bootstrap.client in
  let () = toplog "Fetching fundraiser's counter" in
  let* counter =
    Operation.get_next_counter ~source:fundraiser bootstrap.client
  in
  let () = toplog "Fetching fundraiser's balance" in
  let* _balance =
    Client.get_balance_for ~account:"fundraiser" bootstrap.client
  in
  let* etherlink_rollup_operator_key =
    if configuration.etherlink then
      let () = toplog "Generating a key pair for Etherlink operator" in
      Client.stresstest_gen_keys
        ~alias_prefix:"etherlink_operator"
        1
        bootstrap.client
    else Lwt.return []
  in
  let etherlink_rollup_operator_key =
    match etherlink_rollup_operator_key with key :: _ -> Some key | [] -> None
  in
  let* () =
    if List.length producer_accounts > 0 then
      let () = toplog "Injecting the batch" in
      let* _op_hash =
        producer_accounts
        |> List.map (fun producer ->
               Operation.Manager.transfer
                 ~amount:(10 * 1_000_000)
                 ~dest:producer
                 ())
        |> Operation.Manager.make_batch ~source:fundraiser ~counter
        |> Fun.flip (Operation.Manager.inject ~dont_wait:true) bootstrap.client
      in
      (* Wait a bit. *)
      let () = toplog "Waiting 10 seconds" in
      let* () = Lwt_unix.sleep 10. in
      let () = toplog "Waiting 10 seconds: done" in
      unit
    else
      let () =
        toplog "Skipping batch injection because there are no producers"
      in
      unit
  in
  Lwt.return
    (bootstrap, baker_accounts, producer_accounts, etherlink_rollup_operator_key)

let init_bootstrap_and_activate_protocol cloud (configuration : configuration)
    agent =
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
    Cli.data_dir |> Option.map (fun data_dir -> data_dir // name)
  in
  let* bootstrap_node =
    Node.init ?data_dir ~dal_config ~name configuration.network agent
  in
  let* dal_bootstrap_node =
    Dal_node.Agent.create
      ~net_port:dal_bootstrap_node_net_port
      ~name:"bootstrap-dal-node"
      agent
      ~node:bootstrap_node
  in
  let* () = Node.wait_for_ready bootstrap_node in
  let* client = Client.init ~endpoint:(Node bootstrap_node) () in
  let* baker_accounts =
    Client.stresstest_gen_keys
      ~alias_prefix:"baker"
      (List.length configuration.stake)
      client
  in
  let* producer_accounts =
    Client.stresstest_gen_keys
      ~alias_prefix:"dal_producer"
      configuration.dal_node_producer
      client
  in
  let* etherlink_rollup_operator_key =
    if configuration.etherlink then
      Client.stresstest_gen_keys ~alias_prefix:"etherlink_operator" 1 client
    else Lwt.return []
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
        (producer_accounts @ etherlink_rollup_operator_key)
    in
    Protocol.write_parameter_file
      ~bootstrap_accounts
      ~additional_bootstrap_accounts
      ~base
      []
  in
  let etherlink_rollup_operator_key =
    match etherlink_rollup_operator_key with [key] -> Some key | _ -> None
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
  let node_rpc_endpoint =
    Endpoint.
      {
        scheme = "http";
        host = Agent.point agent |> fst;
        port = Node.rpc_port bootstrap_node;
      }
  in
  let (bootstrap : bootstrap) =
    {
      node = Some bootstrap_node;
      dal_node = Some dal_bootstrap_node;
      node_p2p_endpoint = Node.point_str bootstrap_node;
      node_rpc_endpoint;
      dal_node_p2p_endpoint = Dal_node.point_str dal_bootstrap_node;
      client;
    }
  in
  Lwt.return
    (bootstrap, baker_accounts, producer_accounts, etherlink_rollup_operator_key)

let init_baker cloud (configuration : configuration) ~bootstrap account i agent
    =
  let stake = List.nth configuration.stake i in
  let name = Format.asprintf "baker-node-%d" i in
  let data_dir =
    Cli.data_dir |> Option.map (fun data_dir -> data_dir // name)
  in
  let* node =
    Node.init
      ?data_dir
      ~arguments:Node.[Peer bootstrap.node_p2p_endpoint]
      ~name:(Format.asprintf "baker-node-%d" i)
      configuration.network
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
        ~peers:[bootstrap.dal_node_p2p_endpoint] (* no need for peer *)
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

let init_producer cloud configuration ~bootstrap ~number_of_slots account i
    agent =
  let () = toplog "Init producer" in
  let name = Format.asprintf "producer-node-%i" i in
  let data_dir =
    Cli.data_dir |> Option.map (fun data_dir -> data_dir // name)
  in
  let () = toplog "Init producer: init node" in
  let* node =
    Node.init
      ?data_dir
      ~name
      ~arguments:Node.[Peer bootstrap.node_p2p_endpoint]
      configuration.network
      agent
  in
  let () = toplog "Init producer: create client" in
  let* client = Client.Agent.create ~node agent in
  let () = toplog "Init producer: import key" in
  let* () =
    Client.import_secret_key
      client
      ~endpoint:(Node node)
      account.Account.secret_key
      ~alias:account.Account.alias
  in
  let () = toplog "Init producer: wait for the node to be bootstrapped" in
  let* () = Node.wait_for_synchronisation ~statuses:["synced"] node in
  let () = toplog "Init producer: the node is bootstrapped" in
  let () = toplog "Init producer: reveal account" in
  let*! () =
    Client.reveal client ~endpoint:(Node node) ~src:account.Account.alias
  in
  let () = toplog "Init producer: create agent" in
  let* dal_node =
    Dal_node.Agent.create
      ~name:(Format.asprintf "producer-dal-node-%i" i)
      ~node
      agent
  in
  let () = toplog "Init producer: init DAL node config" in
  let* () =
    Dal_node.init_config
      ~expected_pow:0.
      ~observer_profiles:[i mod number_of_slots]
      ~peers:[bootstrap.dal_node_p2p_endpoint]
      dal_node
  in
  let () = toplog "Init producer: add DAL node metrics" in
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
  let () = toplog "Init producer: wait for DAL node to be ready" in
  let is_ready = Dal_node.run ~event_level:`Notice dal_node in
  let () = toplog "Init producer: DAL node is ready" in
  Lwt.return {client; node; dal_node; account; is_ready}

let init_observer cloud configuration ~bootstrap ~slot_index i agent =
  let name = Format.asprintf "observer-node-%i" i in
  let data_dir =
    Cli.data_dir |> Option.map (fun data_dir -> data_dir // name)
  in
  let* node =
    Node.init
      ?data_dir
      ~name
      ~arguments:[Peer bootstrap.node_p2p_endpoint]
      configuration.network
      agent
  in
  let* dal_node =
    Dal_node.Agent.create
      ~name:(Format.asprintf "observer-dal-node-%i" i)
      ~node
      agent
  in
  let* () =
    Dal_node.init_config
      ~expected_pow:0.
      ~observer_profiles:[slot_index]
      ~peers:[bootstrap.dal_node_p2p_endpoint]
      dal_node
  in
  let* () =
    add_source
      cloud
      agent
      ~job_name:(Format.asprintf "observer-%d" i)
      node
      dal_node
  in
  let* () = Dal_node.run ~event_level:`Notice dal_node in
  Lwt.return {node; dal_node; slot_index}

let init_etherlink_operator_setup cloud configuration is_sequencer name
    ~bootstrap ~dal_slots account agent =
  let () = toplog "Init Etherlink operator setup" in
  let name = Format.asprintf "etherlink-%s-node" name in
  let data_dir =
    Cli.data_dir |> Option.map (fun data_dir -> data_dir // name)
  in
  let* node =
    Node.init
      ?data_dir
      ~name
      ~arguments:[Peer bootstrap.node_p2p_endpoint]
      configuration.network
      agent
  in
  let () = toplog "Init Etherlink operator setup: create agent" in
  let* client = Client.Agent.create ~node agent in
  let () = toplog "Init Etherlink operator setup: import key" in
  let* () =
    Client.import_secret_key
      client
      ~endpoint:(Node node)
      account.Account.secret_key
      ~alias:account.Account.alias
  in
  let () = toplog "Init Etherlink operator setup: get last seen level" in
  let l = Node.get_last_seen_level node in
  let () = toplog "Init Etherlink operator setup: reveal account" in
  let*! () =
    Client.reveal client ~endpoint:(Node node) ~src:account.Account.alias
  in
  let () = toplog "Init Etherlink operator setup: wait for level %d" (l + 2) in
  let* _ = Node.wait_for_level node (l + 2) in
  let () =
    toplog "Init Etherlink operator setup: wait for level %d: done" (l + 2)
  in
  (* A configuration is generated locally by the orchestrator. The resulting
     kernel will be pushed to Etherlink. *)
  let output_config = Temp.file "config.yaml" in
  let bootstrap_accounts =
    Tezt_etherlink.Eth_account.bootstrap_accounts |> Array.to_list
    |> List.map (fun account -> account.Tezt_etherlink.Eth_account.address)
  in
  let*! () =
    let sequencer = if is_sequencer then Some account.public_key else None in
    let () =
      toplog "Init Etherlink operator setup: make kernel installer config"
    in
    Tezt_etherlink.Evm_node.make_kernel_installer_config
      ?sequencer
      ~bootstrap_accounts
      ~output:output_config
      ()
  in
  let () = toplog "Init Etherlink operator setup: create rollup node" in
  let* sc_rollup_node =
    Sc_rollup_node.Agent.create
      ~name:(Format.asprintf "etherlink-%s-rollup-node" name)
      ~base_dir:(Client.base_dir client)
      ~default_operator:account.Account.alias
      agent
      Operator
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
  let pvm_kind = "wasm_2_0_0" in
  let l = Node.get_last_seen_level node in
  let () = toplog "Init Etherlink operator setup: originate rollup" in
  let* sc_rollup_address =
    Sc_rollup_helpers.Agent.originate_sc_rollup
      ~kind:pvm_kind
      ~boot_sector:output
      ~parameters_ty:Tezt_etherlink.Helpers.evm_type
      ~src:account.alias
      client
  in
  let () =
    toplog "Init Etherlink operator setup: wait again, for level %d" (l + 2)
  in
  let* _ = Node.wait_for_level node (l + 2) in
  let () =
    toplog
      "Init Etherlink operator setup: wait again, for level %d: done"
      (l + 2)
  in
  let () = toplog "Init Etherlink operator setup: running the rollup node" in
  let* () =
    Sc_rollup_node.run sc_rollup_node sc_rollup_address [Log_kernel_debug]
  in
  let private_rpc_port = Agent.next_available_port agent |> Option.some in
  let time_between_blocks = Some (Evm_node.Time_between_blocks 10.) in
  let sequencer_mode =
    Evm_node.Sequencer
      {
        initial_kernel = output;
        preimage_dir = Some preimages_dir;
        private_rpc_port;
        time_between_blocks;
        sequencer = account.alias;
        genesis_timestamp = None;
        max_blueprints_lag = None;
        max_blueprints_ahead = None;
        max_blueprints_catchup = None;
        catchup_cooldown = None;
        max_number_of_chunks = None;
        wallet_dir = Some (Client.base_dir client);
        tx_pool_timeout_limit = None;
        tx_pool_addr_limit = None;
        tx_pool_tx_per_addr_limit = None;
        dal_slots;
      }
  in
  let endpoint = Sc_rollup_node.endpoint sc_rollup_node in
  let mode =
    if is_sequencer then sequencer_mode
    else Evm_node.Proxy {finalized_view = false}
  in
  let () = toplog "Init Etherlink operator setup: init the operator agent" in
  let* evm_node =
    Tezos.Evm_node.Agent.init
      ~name:(Format.asprintf "etherlink-%s-evm-node" name)
      ~mode
      endpoint
      agent
  in
  let operator =
    {
      node;
      client;
      sc_rollup_node;
      evm_node;
      is_sequencer;
      account;
      sc_rollup_address;
    }
  in
  let () = toplog "Init Etherlink operator setup: add metrics source" in
  let* () =
    add_etherlink_source
      cloud
      agent
      ~job_name:(Format.asprintf "etherlink-%s" name)
      node
      sc_rollup_node
      evm_node
  in
  return operator

let init_etherlink_producer_setup cloud operator name account ~bootstrap agent =
  let* node =
    Node.Agent.init
      ~name:(Format.asprintf "etherlink-%s-node" name)
      ~arguments:[Peer bootstrap.node_p2p_endpoint; Synchronisation_threshold 0]
      agent
  in
  let* client = Client.Agent.create ~node agent in
  let l = Node.get_last_seen_level node in
  let* _ = Node.wait_for_level node (l + 2) in
  (* A configuration is generated locally by the orchestrator. The resulting
     kernel will be pushed to Etherlink. *)
  let output_config = Temp.file "config.yaml" in
  let bootstrap_accounts =
    Tezt_etherlink.Eth_account.bootstrap_accounts |> Array.to_list
    |> List.map (fun account -> account.Tezt_etherlink.Eth_account.address)
  in
  let*! () =
    let sequencer =
      if operator.is_sequencer then Some operator.account.public_key else None
    in
    Tezt_etherlink.Evm_node.make_kernel_installer_config
      ?sequencer
      ~bootstrap_accounts
      ~output:output_config
      ()
  in
  let* sc_rollup_node =
    Sc_rollup_node.Agent.create
      ~name:(Format.asprintf "etherlink-%s-rollup-node" name)
      ~base_dir:(Client.base_dir client)
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
        initial_kernel = output;
        preimages_dir;
        rollup_node_endpoint = Sc_rollup_node.endpoint sc_rollup_node;
      }
  in
  let endpoint = Evm_node.endpoint operator.evm_node in
  let* evm_node =
    Evm_node.Agent.init
      ~name:(Format.asprintf "etherlink-%s-evm-node" name)
      ~mode
      endpoint
      agent
  in
  let operator = {agent; node; client; sc_rollup_node; evm_node; account} in
  let* () =
    add_etherlink_source
      cloud
      agent
      ~job_name:(Format.asprintf "etherlink-%s" name)
      node
      sc_rollup_node
      evm_node
  in
  let* () =
    (* This is to avoid producing operations too soon. *)
    Evm_node.wait_for_blueprint_applied evm_node 1
  in
  return operator

let init_etherlink cloud configuration ~bootstrap etherlink_rollup_operator_key
    ~dal_slots next_agent =
  let () = toplog "init_etherlink" in
  let* operator_agent = next_agent ~name:"etherlink-operator-agent" in
  let () = toplog "init_etherlink: init operator setup" in
  let* operator =
    init_etherlink_operator_setup
      cloud
      configuration
      ~dal_slots
      Cli.etherlink_sequencer
      "operator"
      ~bootstrap
      etherlink_rollup_operator_key
      operator_agent
  in
  let accounts = Tezt_etherlink.Eth_account.bootstrap_accounts in
  let* producers_agents =
    List.init Cli.etherlink_producers (fun i ->
        let name = Format.asprintf "etherlink-producer-%d" i in
        next_agent ~name)
    |> Lwt.all
  in
  let () = toplog "init_etherlink: init Etherlink producers" in
  let* producers =
    producers_agents
    |> List.mapi (fun i agent ->
           assert (i < Array.length accounts) ;
           init_etherlink_producer_setup
             cloud
             operator
             (Format.asprintf "producer-%d" i)
             accounts.(i)
             ~bootstrap
             agent)
    |> Lwt.all
  in
  return {operator; accounts; producers}

let init ~(configuration : configuration) cloud next_agent =
  let () = toplog "Init" in
  let* bootstrap_agent =
    if Cli.bootstrap then
      let* agent = next_agent ~name:"bootstrap" in
      Lwt.return_some agent
    else Lwt.return_none
  in
  let* attesters_agents =
    List.init (List.length configuration.stake) (fun i ->
        let name = Format.asprintf "attester-%d" i in
        next_agent ~name)
    |> Lwt.all
  in
  let* producers_agents =
    List.init configuration.dal_node_producer (fun i ->
        let name = Format.asprintf "producer-%d" i in
        next_agent ~name)
    |> Lwt.all
  in
  let* observers_agents =
    List.map
      (fun i ->
        let name = Format.asprintf "observer-%d" i in
        next_agent ~name)
      configuration.observer_slot_indices
    |> Lwt.all
  in
  let* ( bootstrap,
         baker_accounts,
         producer_accounts,
         etherlink_rollup_operator_key ) =
    match configuration.network with
    | Network.Sandbox ->
        let bootstrap_agent = Option.get bootstrap_agent in
        init_bootstrap_and_activate_protocol cloud configuration bootstrap_agent
    | Ghostnet ->
        let () = toplog "Init: initializting, testnet case" in
        init_ghostnet cloud configuration bootstrap_agent
  in
  let* bakers =
    Lwt_list.mapi_p
      (fun i (agent, account) ->
        init_baker cloud configuration ~bootstrap account i agent)
      (List.combine attesters_agents baker_accounts)
  in
  let client =
    Client.create ~endpoint:(Foreign_endpoint bootstrap.node_rpc_endpoint) ()
  in
  let* parameters = Dal_common.Parameters.from_client client in
  let () = toplog "Init: initializting producers and observers" in
  let* producers =
    Lwt_list.mapi_p
      (fun i (agent, account) ->
        init_producer
          cloud
          configuration
          ~bootstrap
          ~number_of_slots:parameters.number_of_slots
          account
          i
          agent)
      (List.combine producers_agents producer_accounts)
  and* observers =
    Lwt_list.mapi_p
      (fun i (agent, slot_index) ->
        init_observer cloud configuration ~bootstrap ~slot_index i agent)
      (List.combine observers_agents configuration.observer_slot_indices)
  in
  let () = toplog "Init: all producers and observers have been initialized" in
  let* etherlink =
    if Cli.etherlink then
      let () = toplog "Init: initializing Etherlink" in
      let () = toplog "Init: Getting Etherlink operator key" in
      let etherlink_rollup_operator_key =
        Option.get etherlink_rollup_operator_key
      in
      let () = toplog "Init: Getting allowed DAL slot indices" in
      let dal_slots =
        match configuration.etherlink_dal_slots with
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
          configuration
          ~bootstrap
          etherlink_rollup_operator_key
          next_agent
          ~dal_slots
      in
      some etherlink
    else
      let () =
        toplog
          "Init: skipping Etherlink initialization because --etherlink was not \
           given on the CLI"
      in
      none
  in
  let infos = Hashtbl.create 101 in
  let metrics = Hashtbl.create 101 in
  let* first_level =
    Network.get_level configuration.network bootstrap.node_rpc_endpoint
  in
  Hashtbl.replace metrics first_level default_metrics ;
  let disconnection_state =
    Option.map Disconnect.init configuration.disconnect
  in
  Lwt.return
    {
      cloud;
      configuration;
      bootstrap;
      bakers;
      producers;
      observers;
      etherlink;
      parameters;
      infos;
      metrics;
      disconnection_state;
      first_level;
    }

let wait_for_level t level =
  match t.bootstrap.node with
  | None ->
      let rec loop () =
        let* head_level =
          Network.get_level
            t.configuration.network
            t.bootstrap.node_rpc_endpoint
        in
        if head_level >= level then Lwt.return_unit
        else
          let* () = Lwt_unix.sleep 4. in
          loop ()
      in
      loop ()
  | Some node ->
      let* _ = Node.wait_for_level node level in
      Lwt.return_unit

let on_new_level t level =
  let client = t.bootstrap.client in
  let* () = wait_for_level t level in
  toplog "Start process level %d" level ;
  let* infos_per_level = get_infos_per_level client ~level in
  Hashtbl.replace t.infos level infos_per_level ;
  let metrics =
    get_metrics t infos_per_level (Hashtbl.find t.metrics (level - 1))
  in
  pp_metrics t metrics ;
  push_metrics t metrics ;
  Hashtbl.replace t.metrics level metrics ;
  match t.disconnection_state with
  | None -> Lwt.return t
  | Some disconnection_state ->
      let nb_bakers = List.length t.bakers in
      let* disconnection_state =
        Disconnect.disconnect disconnection_state level (fun b ->
            let baker_to_disconnect =
              (List.nth t.bakers (b mod nb_bakers)).dal_node
            in
            Dal_node.terminate baker_to_disconnect)
      in
      let* disconnection_state =
        Disconnect.reconnect disconnection_state level (fun b ->
            let baker_to_reconnect =
              (List.nth t.bakers (b mod nb_bakers)).dal_node
            in
            Dal_node.run baker_to_reconnect)
      in
      Lwt.return {t with disconnection_state = Some disconnection_state}

let produce_slot t level i =
  let producer = List.nth t.producers i in
  let index = i mod t.parameters.number_of_slots in
  let content =
    Format.asprintf "%d:%d" level index
    |> Helpers.make_slot
         ~padding:false
         ~slot_size:t.parameters.cryptobox.slot_size
  in
  let* _ = Node.wait_for_level producer.node level in
  let* _ =
    Dal_common.Helpers.publish_and_store_slot
      ~dont_wait:true
      producer.client
      producer.dal_node
      producer.account
      ~force:true
      ~index
      content
  in
  Lwt.return_unit

let producers_not_ready t =
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
    if producers_not_ready t then Lwt.return_unit
    else
      Seq.ints 0
      |> Seq.take t.configuration.dal_node_producer
      |> Seq.map (fun i -> produce_slot t level i)
      |> List.of_seq |> Lwt.join
  in
  let* t = p in
  loop t (level + 1)

let etherlink_loop (etherlink : etherlink) =
  let open Tezt_etherlink in
  let* () = Lwt_unix.sleep 30. in
  let account_loop i =
    let producer : etherlink_producer_setup =
      List.nth etherlink.producers (i mod List.length etherlink.producers)
    in
    let runner = Node.runner producer.node |> Option.get in
    let firehose =
      (Agent.configuration producer.agent).binaries_path // "firehose"
    in
    let* () =
      Process.spawn
        ~runner
        firehose
        [
          "configure";
          "--endpoint";
          Evm_node.endpoint etherlink.operator.evm_node;
          "--controller";
          etherlink.accounts.(i).private_key |> String.to_seq |> Seq.drop 2
          |> String.of_seq;
        ]
      |> Process.check
    in
    Process.spawn ~runner firehose ["flood"; "--kind"; "xtz"; "--workers"; "20"]
    |> Process.check
  in
  if List.length etherlink.producers > 0 then
    Array.mapi (fun i _ -> account_loop i) etherlink.accounts
    |> Array.to_list |> Lwt.join
  else Lwt.return_unit

let configuration =
  let stake = Cli.stake in
  let stake_machine_type = Cli.stake_machine_type in
  let dal_node_producer = Cli.producers in
  let observer_slot_indices = Cli.observer_slot_indices in
  let protocol = Cli.protocol in
  let producer_machine_type = Cli.producer_machine_type in
  let etherlink = Cli.etherlink in
  let etherlink_sequencer = Cli.etherlink_sequencer in
  let etherlink_producers = Cli.etherlink_producers in
  let disconnect = Cli.disconnect in
  let network = Cli.network in
  let bootstrap = Cli.bootstrap in
  let etherlink_dal_slots = Cli.etherlink_dal_slots in
  {
    stake;
    stake_machine_type;
    dal_node_producer;
    observer_slot_indices;
    protocol;
    producer_machine_type;
    etherlink;
    etherlink_sequencer;
    etherlink_producers;
    disconnect;
    network;
    bootstrap;
    etherlink_dal_slots;
  }

let benchmark () =
  toplog "Parsing CLI done" ;
  let vms =
    [
      (if configuration.bootstrap then [`Bootstrap] else []);
      List.map (fun i -> `Baker i) configuration.stake;
      List.init configuration.dal_node_producer (fun _ -> `Producer);
      List.map (fun _ -> `Observer) configuration.observer_slot_indices;
      (if configuration.etherlink then [`Etherlink_operator] else []);
      List.init configuration.etherlink_producers (fun i ->
          `Etherlink_producer i);
    ]
    |> List.concat
  in
  let docker_image =
    match configuration.network with
    | Ghostnet -> Some Env.Octez_latest_release
    | Sandbox -> None
  in
  let default_vm_configuration = Configuration.make ?docker_image () in
  let vms =
    vms
    |> List.map (function
           | `Bootstrap ->
               (* Configuration.make ~docker_image:Octez_latest_release () *)
               default_vm_configuration
           | `Baker i -> (
               match configuration.stake_machine_type with
               | None -> default_vm_configuration
               | Some list -> (
                   try
                     let machine_type = List.nth list i in
                     Configuration.make ~machine_type ()
                   with _ -> default_vm_configuration))
           | `Producer | `Observer -> (
               match configuration.producer_machine_type with
               | None -> Configuration.make ()
               | Some machine_type -> Configuration.make ~machine_type ())
           | `Etherlink_operator -> default_vm_configuration
           | `Etherlink_producer _ -> default_vm_configuration)
  in
  Cloud.register
  (* docker images are pushed before executing the test in case binaries are modified locally. This way we always use the latest ones. *)
    ~vms
    ~proxy_files:
      [
        Format.asprintf
          "src/%s/parameters/mainnet-parameters.json"
          (Protocol.directory configuration.protocol);
        "evm_kernel.wasm";
      ]
    ~__FILE__
    ~title:"DAL node benchmark"
    ~tags:[Tag.cloud; "dal"; "benchmark"]
    (fun cloud ->
      toplog "Creating the agents" ;
      let agents = Cloud.agents cloud in
      (* We give to the [init] function a sequence of agents (and cycle if
         they were all consumed). We set their name only if the number of
         agents is the computed one. Otherwise, the user has mentioned
         explicitely a reduced number of agents and it is not clear how to give
         them proper names. *)
      let set_name agent name =
        if List.length agents = List.length vms then
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
      toplog "Init" ;
      let* t = init ~configuration cloud next_agent in
      toplog "Starting main loop" ;
      let main_loop = loop t (t.first_level + 1) in
      let etherlink_loop =
        match t.etherlink with
        | None ->
            let () = toplog "No Etherlink loop to start" in
            unit
        | Some etherlink ->
            let () = toplog "Starting Etherlink loop" in
            etherlink_loop etherlink
      in
      Lwt.join [main_loop; etherlink_loop])

let register () = benchmark ()
