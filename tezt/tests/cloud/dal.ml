(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Cryptobox = Dal_common.Cryptobox
module Helpers = Dal_common.Helpers
module Cli = Scenarios_cli
open Scenarios_helpers
open Tezos

module Disconnect = struct
  module IMap = Map.Make (Int)

  (** The [disconnected_bakers] map contains bakers indexes whose DAL node
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
    if frequency <= 0 then
      Test.fail
        "Unexpected error: The disconnection frequency must be strictly \
         positive, rather than %d"
        frequency ;
    {
      disconnected_bakers = IMap.empty;
      frequency;
      reconnection_delay;
      next_to_disconnect = 0;
    }

  (** When a relevant level is reached, [disconnect t level f] puts the baker of
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

  (** Applies [f] on the bakers DAL nodes that have been disconnected for [t.reconnection_delay]
      levels from [level]. *)
  let reconnect t level f =
    let bakers_to_reconnect, bakers_to_keep_disconnected =
      IMap.partition
        (fun _ disconnected_level ->
          level >= disconnected_level + t.reconnection_delay)
        t.disconnected_bakers
    in
    let* () =
      IMap.to_seq bakers_to_reconnect
      |> List.of_seq
      |> Lwt_list.iter_p (fun (b, _) -> f b)
    in
    Lwt.return {t with disconnected_bakers = bakers_to_keep_disconnected}
end

module Node = struct
  let runner_of_agent = Agent.runner

  let may_copy_node_identity_file agent node = function
    | None -> Lwt.return_unit
    | Some source ->
        toplog "Copying the node identity file" ;
        let* _ =
          Agent.copy agent ~source ~destination:(Node.identity_file node)
        in
        Lwt.return_unit

  include Node

  let init ?(arguments = []) ?data_dir ?identity_file ?dal_config ~name network
      agent =
    toplog "Inititializing an L1 node for %s" name ;
    match network with
    | (`Mainnet | `Ghostnet | `Nextnet _ | `Weeklynet _) as network -> (
        match data_dir with
        | Some data_dir ->
            let rpc_external = Cli.node_external_rpc_server in
            let* node =
              Node.Agent.create ~rpc_external ~arguments ~data_dir ~name agent
            in
            let* () = may_copy_node_identity_file agent node identity_file in
            let* () =
              Node.run node [Network (Network.to_octez_network_options network)]
            in
            let* () = Node.wait_for_ready node in
            Lwt.return node
        | None ->
            toplog
              "No data dir given, we will attempt to bootstrap the node from a \
               rolling snapshot." ;
            toplog "Creating the agent." ;
            let rpc_external = Cli.node_external_rpc_server in
            let* node =
              Node.Agent.create
                ~rpc_external
                ~arguments:
                  [
                    Network (Network.to_octez_network_options network);
                    Expected_pow 26;
                    Cors_origin "*";
                  ]
                ?data_dir
                ~name
                agent
            in
            let* () = may_copy_node_identity_file agent node identity_file in
            toplog "Initializing node configuration" ;
            let* () = Node.config_init node [] in
            toplog "Trying to download a rolling snapshot" ;
            let* exit_status =
              Process.spawn
                ?runner:(runner_of_agent agent)
                "wget"
                [
                  "-O";
                  "snapshot_file";
                  sf "%s/rolling" (Network.snapshot_service network);
                ]
              |> Process.wait
            in
            let* () =
              match exit_status with
              | WEXITED 0 ->
                  toplog "Importing the snapshot" ;
                  let* () =
                    try
                      let* () =
                        Node.snapshot_import ~no_check:true node "snapshot_file"
                      in
                      let () = toplog "Snapshot import succeeded." in
                      Lwt.return_unit
                    with _ ->
                      (* Failing to import the snapshot could happen on a
                                 very young Weeklynet, before the first snapshot is
                                 available. In this case bootstrapping from the
                                 genesis block is OK. *)
                      let () =
                        toplog
                          "Snapshot import failed, the node will be \
                           bootstrapped from genesis."
                      in
                      Lwt.return_unit
                  in
                  Lwt.return_unit
              | WEXITED code ->
                  toplog
                    "Could not download the snapshot: wget exit code: %d\n\
                     Starting without snapshot. This could last long before \
                     the node is bootstrapped"
                    code ;
                  Lwt.return_unit
              | status -> (
                  match Process.validate_status status with
                  | Ok () -> Lwt.return_unit
                  | Error (`Invalid_status reason) ->
                      failwith @@ Format.sprintf "wget: %s" reason)
            in
            toplog "Launching the node." ;
            let* () =
              Node.run
                node
                (Force_history_mode_switch :: Synchronisation_threshold 1
               :: arguments)
            in
            toplog "Waiting for the node to be ready." ;
            let* () = wait_for_ready node in
            toplog "Node is ready." ;
            let* () = Node.wait_for_synchronisation ~statuses:["synced"] node in
            toplog "Node is bootstrapped" ;
            Lwt.return node)
    | `Sandbox -> (
        match data_dir with
        | None ->
            let rpc_external = Cli.node_external_rpc_server in
            let* node = Node.Agent.create ~rpc_external ~name agent in
            let* () = Node.config_init node [Cors_origin "*"] in
            let* () =
              match dal_config with
              | None -> Lwt.return_unit
              | Some config ->
                  Node.Config_file.update
                    node
                    (Node.Config_file.set_sandbox_network_with_dal_config
                       config)
            in
            let* () = may_copy_node_identity_file agent node identity_file in
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
            let rpc_external = Cli.node_external_rpc_server in
            let arguments =
              [No_bootstrap_peers; Synchronisation_threshold 0; Cors_origin "*"]
              @ arguments
            in
            let* node =
              Node.Agent.create ~rpc_external ~arguments ~data_dir ~name agent
            in
            let* () = may_copy_node_identity_file agent node identity_file in
            let* () = Node.run node [] in
            let* () = Node.wait_for_ready node in
            Lwt.return node)
end

module Dal_reverse_proxy = struct
  (** This module allows to configure NginX as a reverse proxy in
      front of a collection of DAL nodes. This allows to balance the
      load on the DAL nodes based on which DAL slot index is used
      while complying with the interface of the rollup node, which
      expects a single DAL node endpoint.

      The NginX reverse-proxy configuration is added as an NginX site
      (in directory /etc/nginx/sites-available/ which is symlinked in
      /etc/nginx/sites-enabled/). This module makes no assumption
      about the existence of other NginX sites and should be invisible
      to them except that:

      - it disables the "default" NginX site (the file
        /etc/nginx/sites-enabled/default is removed if it exists),
      - it overrides the "reverse_proxy" NgninX site if there is some.
  *)

  (* An NginX configuration file is made of directives inside
     potentially nested context blocks. *)
  type config = Directive of string | Context_block of string * config list

  let rec pp_config out = function
    | Directive s -> Format.fprintf out "%s;" s
    | Context_block (head, l) ->
        Format.fprintf
          out
          "@[<v 2>%s {@;%a@]@;}"
          head
          (Format.pp_print_list
             ~pp_sep:(fun out () -> Format.fprintf out "@;")
             pp_config)
          l

  let generate_nginx_config ~port out ~default_endpoint
      (producers : (int * string) Seq.t) =
    Format.fprintf
      out
      "@[<v 0>%a@]@."
      pp_config
      (Context_block
         ( "server",
           [
             Directive (sf "listen 0.0.0.0:%d" port);
             (* When fetching data about a slot the RPC are of the
                form (GET /levels/<level>/slots/<slot_index>/...); we
                direct the request to a DAL node subscribed to the
                topics of the queried slot index. *)
             Context_block
               ( "location /levels/",
                 producers
                 |> Seq.map (fun (slot_index, endpoint) ->
                        Context_block
                          ( sf "location ~ ^/levels/[0-9]+?/slots/%d/" slot_index,
                            [Directive (sf "proxy_pass %s" endpoint)] ))
                 |> List.of_seq );
             (* When posting a DAL slot, the format of the RPC is POST
                /slots/?slot_index=<slot_index>. In this case too we
                redirect the request to a DAL node subscribed to the
                topics of the queried slot index. *)
             Context_block
               ( "location /slots",
                 producers
                 |> Seq.map (fun (slot_index, endpoint) ->
                        Context_block
                          ( sf
                              {|if ($query_string ~ "slot_index=%d")|}
                              slot_index,
                            [Directive (sf "proxy_pass %s" endpoint)] ))
                 |> List.of_seq );
             (* Other queries can be answered by any DAL node. *)
             Context_block
               ("location /", [Directive (sf "proxy_pass %s" default_endpoint)]);
           ] ))

  let init_reverse_proxy ~next_agent ~default_endpoint
      (proxified_dal_nodes : (int * string) Seq.t) =
    (* A NGINX reverse proxy which balances load between producer DAL
       nodes based on the requested slot index. *)
    let* agent = next_agent ~name:"dal-reverse-proxy" in
    let runner = Agent.runner agent in
    let port = Agent.next_available_port agent in
    let () = toplog "Launching reverse proxy" in
    let () = toplog "Generating nginx reverse proxy config" in
    let config_filename = "nginx_reverse_proxy_config" in
    let out_chan = Stdlib.open_out config_filename in
    let config_ppf = Format.formatter_of_out_channel out_chan in
    Format.fprintf
      config_ppf
      "%a"
      (generate_nginx_config ~default_endpoint ~port)
      proxified_dal_nodes ;
    close_out out_chan ;

    (* Upload the configuration file. *)
    let* (_ : string) =
      Agent.copy
        ~destination:"/etc/nginx/sites-available/reverse_proxy"
        ~source:config_filename
        agent
    in

    (* Disable the default configuration *)
    let* () =
      Process.spawn ?runner "rm" ["-f"; "/etc/nginx/sites-enabled/default"]
      |> Process.check
    in

    (* Enable the reverse proxy configuration *)
    let* () =
      Process.spawn
        ?runner
        "ln"
        [
          "-s";
          "/etc/nginx/sites-available/reverse_proxy";
          "/etc/nginx/sites-enabled/reverse_proxy";
        ]
      |> Process.check
    in

    (* Check the NginX configuration *)
    let* () = Process.spawn ?runner "nginx" ["-t"] |> Process.check in

    (* Start the NginX service *)
    let* () =
      Process.spawn ?runner "service" ["nginx"; "restart"] |> Process.check
    in
    (* In order to pass the reverse proxy to the various Tezt helpers we
       need to pretend to be a DAL node. The simplest way to do so is to
       call Dal_node.Agent.create_from_endpoint with the appropriate
       rpc_port and never call Dal_node.run on the result.

       Since the DAL node never runs, it does not call it's L1 endpoint. *)
    let l1_node_endpoint : Endpoint.t = {host = ""; scheme = ""; port = 0} in
    let* dal_node =
      Dal_node.Agent.create_from_endpoint
        ~name:"bootstrap-dal-node"
        ~rpc_port:port
        agent
        ~l1_node_endpoint
    in
    return dal_node
end

type etherlink_configuration = {
  etherlink_sequencer : bool;
  etherlink_producers : int;
  (* Empty list means DAL FF is set to false. *)
  etherlink_dal_slots : int list;
  chain_id : int option;
}

type configuration = {
  with_dal : bool;
  stake : int list;
  bakers : string list; (* unencrypted secret keys *)
  stake_machine_type : string list option;
  dal_node_producers : int list; (* slot indices *)
  observer_slot_indices : int list;
  observer_pkhs : string list;
  protocol : Protocol.t;
  producer_machine_type : string option;
  (* The first argument is the deconnection frequency, the second is the
     reconnection delay *)
  disconnect : (int * int) option;
  network : Network.t;
  bootstrap : bool;
  teztale : bool;
  memtrace : bool;
  data_dir : string option;
  fundraiser : string option;
  blocks_history : int;
  metrics_retention : int;
  bootstrap_node_identity_file : string option;
  bootstrap_dal_node_identity_file : string option;
}

type bootstrap = {
  node : Node.t option;
  dal_node : Dal_node.t option;
  node_p2p_endpoint : string;
  node_rpc_endpoint : Endpoint.t;
  dal_node_p2p_endpoint : string option;
  client : Client.t;
}

type baker = {
  node : Node.t;
  dal_node : Dal_node.t option;
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
  slot_index : int;
}

type observer = {
  node : Node.t;
  dal_node : Dal_node.t;
  topic : [`Slot_index of int | `Pkh of string];
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

type public_key_hash = string

type commitment_info = {commitment : string; publisher_pkh : string}

type per_level_info = {
  level : int;
  published_commitments : (int, commitment_info) Hashtbl.t;
  attestations : (public_key_hash, Z.t option) Hashtbl.t;
  attested_commitments : Z.t;
  etherlink_operator_balance_sum : Tez.t;
}

type metrics = {
  level_first_commitment_published : int option;
  level_first_commitment_attested : int option;
  total_published_commitments : int;
  total_published_commitments_per_slot : (int, int) Hashtbl.t;
  (* A hash table mapping slot indices to their total number of
     published commitments. *)
  expected_published_commitments : int;
  total_attested_commitments : int;
  total_attested_commitments_per_slot : (int, int) Hashtbl.t;
  (* A hash table mapping slot indices to their total number of
     attested commitments. *)
  ratio_published_commitments : float;
  ratio_attested_commitments : float;
  ratio_published_commitments_last_level : float;
  ratio_attested_commitments_per_baker : (public_key_hash, float) Hashtbl.t;
  etherlink_operator_balance_sum : Tez.t;
}

let default_metrics =
  {
    level_first_commitment_published = None;
    level_first_commitment_attested = None;
    total_published_commitments = 0;
    total_published_commitments_per_slot = Hashtbl.create 32;
    expected_published_commitments = 0;
    total_attested_commitments = 0;
    total_attested_commitments_per_slot = Hashtbl.create 32;
    ratio_published_commitments = 0.;
    ratio_attested_commitments = 0.;
    ratio_published_commitments_last_level = 0.;
    ratio_attested_commitments_per_baker = Hashtbl.create 0;
    etherlink_operator_balance_sum = Tez.zero;
  }

type t = {
  configuration : configuration;
  cloud : Cloud.t;
  bootstrap : bootstrap;
  some_node_rpc_endpoint : Endpoint.t;
  (* endpoint to be used for get various information about L1; for testnets, it
     is a public endpoint only if no L1 node is run by the scenario, in contrast
     to [bootstrap.node_rpp_endpoint] which is a public endpoint when the
     '--bootstrap' argument is not provided *)
  bakers : baker list;
  producers : producer list;
  observers : observer list;
  etherlink : etherlink option;
  time_between_blocks : int;
  parameters : Dal_common.Parameters.t;
  infos : (int, per_level_info) Hashtbl.t;
  metrics : (int, metrics) Hashtbl.t;
  disconnection_state : Disconnect.t option;
  first_level : int;
  teztale : Teztale.t option;
  mutable aliases : (string, string) Hashtbl.t;
      (* mapping from baker addresses to their Tzkt aliases (if known)*)
  mutable versions : (string, string) Hashtbl.t;
      (* mapping from baker addresses to their octez versions (if known) *)
  otel : string option;
}

let pp_slot_metrics fmt xs =
  let open Format in
  fprintf
    fmt
    "[ %a ]"
    (pp_print_list
       (fun fmt (x, y) -> fprintf fmt "(%d -> %d)" x y)
       ~pp_sep:(fun fmt () -> fprintf fmt "; "))
    (List.of_seq xs
    |> List.filter (fun (_, n) -> n > 0)
    (* Sorting the list per slot index increasing order. *)
    |> List.sort (fun (idx1, _) (idx2, _) -> Int.compare idx1 idx2))

let pp_metrics t
    {
      level_first_commitment_published;
      level_first_commitment_attested;
      total_published_commitments;
      total_published_commitments_per_slot;
      expected_published_commitments;
      total_attested_commitments;
      total_attested_commitments_per_slot;
      ratio_published_commitments;
      ratio_attested_commitments;
      ratio_published_commitments_last_level;
      ratio_attested_commitments_per_baker;
      etherlink_operator_balance_sum;
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
             let alias =
               Hashtbl.find_opt t.aliases account.Account.public_key_hash
               |> Option.value ~default:account.Account.public_key_hash
             in
             Log.info "Ratio for %s (with stake %d): %f" alias stake ratio) ;
  Log.info
    "Sum of balances of the Etherlink operator: %s tez"
    (Tez.to_string etherlink_operator_balance_sum) ;
  Log.info
    "DAL slots: total published commitments per slot (<slot index> -> \
     <published commit.>).@.%a"
    pp_slot_metrics
    (Hashtbl.to_seq total_published_commitments_per_slot) ;
  Log.info
    "DAL slots: total attested commitments per slot (<slot index> -> <attested \
     commit.>).@.%a"
    pp_slot_metrics
    (Hashtbl.to_seq total_attested_commitments_per_slot)

let push_metrics t
    {
      level_first_commitment_published = _;
      level_first_commitment_attested = _;
      total_published_commitments;
      total_published_commitments_per_slot;
      expected_published_commitments;
      total_attested_commitments;
      total_attested_commitments_per_slot;
      ratio_published_commitments;
      ratio_attested_commitments;
      ratio_published_commitments_last_level;
      ratio_attested_commitments_per_baker;
      etherlink_operator_balance_sum;
    } =
  Hashtbl.to_seq ratio_attested_commitments_per_baker
  |> Seq.iter (fun (public_key_hash, value) ->
         (* Highly unoptimised since this is done everytime the metric is updated. *)
         let alias =
           Hashtbl.find_opt t.aliases public_key_hash
           |> Option.map (fun alias -> [("alias", alias)])
           |> Option.value ~default:[]
         in
         let version =
           Hashtbl.find_opt t.versions public_key_hash
           |> Option.map (fun version -> [("version", version)])
           |> Option.value ~default:[]
         in
         let labels = [("attester", public_key_hash)] @ alias @ version in
         Cloud.push_metric
           t.cloud
           ~help:
             "Ratio between the number of attested and expected commitments \
              per baker"
           ~typ:`Gauge
           ~labels
           ~name:"tezt_dal_commitments_attested_ratio"
           value) ;
  Hashtbl.iter
    (fun slot_index value ->
      let labels = [("slot_index", string_of_int slot_index)] in
      Cloud.push_metric
        t.cloud
        ~help:"Total published commitments per slot"
        ~typ:`Gauge
        ~labels
        ~name:"tezt_total_published_commitments_per_slot"
        (float value))
    total_published_commitments_per_slot ;
  Hashtbl.iter
    (fun slot_index value ->
      let labels = [("slot_index", string_of_int slot_index)] in
      Cloud.push_metric
        t.cloud
        ~help:"Total attested commitments per slot"
        ~typ:`Gauge
        ~labels
        ~name:"tezt_total_attested_commitments_per_slot"
        (float value))
    total_attested_commitments_per_slot ;
  Cloud.push_metric
    t.cloud
    ~help:"Ratio between the number of published and expected commitments"
    ~typ:`Gauge
    ~name:"tezt_dal_commitments_ratio"
    ~labels:[("kind", "published")]
    ratio_published_commitments ;
  Cloud.push_metric
    t.cloud
    ~help:"Ratio between the number of attested and expected commitments"
    ~typ:`Gauge
    ~name:"tezt_dal_commitments_ratio"
    ~labels:[("kind", "attested")]
    ratio_attested_commitments ;
  Cloud.push_metric
    t.cloud
    ~help:
      "Ratio between the number of attested and expected commitments per level"
    ~typ:`Gauge
    ~name:"tezt_dal_commitments_ratio"
    ~labels:[("kind", "published_last_level")]
    ratio_published_commitments_last_level ;
  Cloud.push_metric
    t.cloud
    ~help:"Number of commitments expected to be published"
    ~typ:`Counter
    ~name:"tezt_dal_commitments_total"
    ~labels:[("kind", "expected")]
    (float_of_int expected_published_commitments) ;
  Cloud.push_metric
    t.cloud
    ~help:"Number of published commitments "
    ~typ:`Counter
    ~name:"tezt_dal_commitments_total"
    ~labels:[("kind", "published")]
    (float_of_int total_published_commitments) ;
  Cloud.push_metric
    t.cloud
    ~help:"Number of  attested commitments"
    ~typ:`Counter
    ~name:"tezt_dal_commitments_total"
    ~labels:[("kind", "attested")]
    (float_of_int total_attested_commitments) ;
  Cloud.push_metric
    t.cloud
    ~help:"Sum of the balances of the etherlink operator"
    ~typ:`Gauge
    ~name:"tezt_etherlink_operator_balance_total"
    (Tez.to_float etherlink_operator_balance_sum)

let published_level_of_attested_level t level =
  level - t.parameters.attestation_lag

let update_level_first_commitment_published _t per_level_info metrics =
  match metrics.level_first_commitment_published with
  | None ->
      if Hashtbl.length per_level_info.published_commitments > 0 then
        Some per_level_info.level
      else None
  | Some l -> Some l

let update_level_first_commitment_attested t per_level_info metrics =
  match metrics.level_first_commitment_attested with
  | None ->
      if
        Z.popcount per_level_info.attested_commitments > 0
        && per_level_info.level >= t.first_level + t.parameters.attestation_lag
      then Some per_level_info.level
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
        min
          (List.length t.configuration.dal_node_producers)
          t.parameters.number_of_slots
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
        min
          (List.length t.configuration.dal_node_producers)
          t.parameters.number_of_slots
      in
      if producers = 0 then 100.
      else
        float_of_int (Hashtbl.length per_level_info.published_commitments)
        *. 100. /. float_of_int producers

let update_ratio_attested_commitments t per_level_info metrics =
  let published_level =
    published_level_of_attested_level t per_level_info.level
  in
  if published_level <= t.first_level then (
    Log.warn
      "Unable to retrieve information for published level %d because it \
       precedes the earliest available level (%d)."
      published_level
      t.first_level ;
    metrics.ratio_attested_commitments)
  else
    match Hashtbl.find_opt t.infos published_level with
    | None ->
        Log.warn
          "Unexpected error: The level %d is missing in the infos table"
          published_level ;
        metrics.ratio_attested_commitments
    | Some old_per_level_info ->
        let n = Hashtbl.length old_per_level_info.published_commitments in
        if n = 0 then metrics.ratio_attested_commitments
        else
          float (Z.popcount per_level_info.attested_commitments)
          *. 100. /. float n

let update_published_and_attested_commitments_per_slot t per_level_info
    total_published_commitments_per_slot total_attested_commitments_per_slot =
  let published_level =
    published_level_of_attested_level t per_level_info.level
  in
  if published_level <= t.first_level then (
    Log.warn
      "Unable to retrieve information for published level %d because it \
       precedes the earliest available level (%d)."
      published_level
      t.first_level ;
    (total_published_commitments_per_slot, total_attested_commitments_per_slot))
  else
    match Hashtbl.find_opt t.infos published_level with
    | None ->
        Log.warn
          "Unexpected error: The level %d is missing in the infos table"
          published_level ;
        ( total_published_commitments_per_slot,
          total_attested_commitments_per_slot )
    | Some old_per_level_info ->
        let published_commitments = old_per_level_info.published_commitments in
        for slot_index = 0 to pred t.parameters.number_of_slots do
          let is_published = Hashtbl.mem published_commitments slot_index in
          let total_published_commitments =
            Option.value
              ~default:0
              (Hashtbl.find_opt total_published_commitments_per_slot slot_index)
          in
          let new_total_published_commitments =
            if is_published then succ total_published_commitments
            else total_published_commitments
          in
          Hashtbl.replace
            total_published_commitments_per_slot
            slot_index
            new_total_published_commitments ;
          (* per_level_info.attested_commitments is a binary
             sequence of length parameters.number_of_slots
             (e.g. '00111111001110100010101011100101').
             For each index i:
             - 1 indicates the slot has been attested
             - 0 indicates the slot has not been attested. *)
          let is_attested =
            Z.testbit per_level_info.attested_commitments slot_index
          in
          let total_attested_commitments =
            Option.value
              ~default:0
              (Hashtbl.find_opt total_attested_commitments_per_slot slot_index)
          in
          let new_total_attested_commitments =
            if is_attested then succ total_attested_commitments
            else total_attested_commitments
          in
          Hashtbl.replace
            total_attested_commitments_per_slot
            slot_index
            new_total_attested_commitments
        done ;
        ( total_published_commitments_per_slot,
          total_attested_commitments_per_slot )

let update_ratio_attested_commitments_per_baker t per_level_info metrics =
  let default () =
    Hashtbl.to_seq_keys per_level_info.attestations
    |> Seq.map (fun key -> (key, 0.))
    |> Hashtbl.of_seq
  in
  match metrics.level_first_commitment_attested with
  | None -> default ()
  | Some level_first_commitment_attested -> (
      let published_level =
        published_level_of_attested_level t per_level_info.level
      in
      if published_level <= t.first_level then (
        Log.warn
          "Unable to retrieve information for published level %d because it \
           precedes the earliest available level (%d)."
          published_level
          t.first_level ;
        default ())
      else
        match Hashtbl.find_opt t.infos published_level with
        | None ->
            Log.warn
              "Unexpected error: The level %d is missing in the infos table"
              published_level ;
            default ()
        | Some old_per_level_info ->
            let n = Hashtbl.length old_per_level_info.published_commitments in
            let maximum_number_of_blocks =
              t.configuration.metrics_retention / t.time_between_blocks
            in
            let weight =
              min
                maximum_number_of_blocks
                (per_level_info.level - level_first_commitment_attested)
              |> float_of_int
            in
            let table =
              Hashtbl.copy metrics.ratio_attested_commitments_per_baker
            in
            Hashtbl.to_seq_keys per_level_info.attestations
            |> Seq.filter_map (fun public_key_hash ->
                   let bitset =
                     float_of_int
                     @@
                     match
                       Hashtbl.find_opt
                         per_level_info.attestations
                         public_key_hash
                     with
                     | None -> (* No attestation in block *) 0
                     | Some (Some z) when n = 0 ->
                         if z = Z.zero then (* No slot were published. *) 100
                         else (
                           Log.error
                             "Wow wow wait! It seems an invariant is broken. \
                              Either on the test side, or on the DAL node side" ;
                           100)
                     | Some (Some z) ->
                         (* Attestation with DAL payload *)
                         if n = 0 then 100 else Z.popcount z * 100 / n
                     | Some None ->
                         (* Attestation without DAL payload: no DAL rights. *)
                         100
                   in
                   match
                     Hashtbl.find_opt
                       metrics.ratio_attested_commitments_per_baker
                       public_key_hash
                   with
                   | None -> None
                   | Some _old_ratio when n = 0 -> None
                   | Some old_ratio ->
                       Some
                         ( public_key_hash,
                           ((old_ratio *. weight) +. bitset) /. (weight +. 1.)
                         ))
            |> Hashtbl.replace_seq table ;
            table)

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
  let total_published_commitments_per_slot, total_attested_commitments_per_slot
      =
    update_published_and_attested_commitments_per_slot
      t
      infos_per_level
      metrics.total_published_commitments_per_slot
      metrics.total_attested_commitments_per_slot
  in
  {
    level_first_commitment_published;
    level_first_commitment_attested;
    total_published_commitments;
    total_published_commitments_per_slot;
    expected_published_commitments;
    total_attested_commitments;
    total_attested_commitments_per_slot;
    ratio_published_commitments;
    ratio_attested_commitments;
    ratio_published_commitments_last_level;
    ratio_attested_commitments_per_baker;
    etherlink_operator_balance_sum =
      infos_per_level.etherlink_operator_balance_sum;
  }

let get_infos_per_level ~client ~endpoint ~level ~etherlink_operators =
  let block = string_of_int level in
  let* header =
    RPC_core.call endpoint @@ RPC.get_chain_block_header_shell ~block ()
  and* metadata =
    RPC_core.call endpoint @@ RPC.get_chain_block_metadata_raw ~block ()
  and* operations =
    RPC_core.call endpoint @@ RPC.get_chain_block_operations ~block ()
  in
  let level = JSON.(header |-> "level" |> as_int) in
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
  let* etherlink_operator_balance_sum =
    List.fold_left
      (fun acc (operator : Account.key) ->
        let* acc in
        let* balance =
          Client.get_full_balance_for ~account:operator.public_key_hash client
        in
        return Tez.(acc + balance))
      (return Tez.zero)
      etherlink_operators
  in
  Lwt.return
    {
      level;
      published_commitments;
      attestations;
      attested_commitments;
      etherlink_operator_balance_sum;
    }

let init_teztale (configuration : configuration) cloud agent =
  if configuration.teztale then init_teztale cloud agent |> Lwt.map Option.some
  else Lwt.return_none

let may_copy_dal_node_identity_file agent node = function
  | None -> Lwt.return_unit
  | Some source ->
      toplog "Copying the DAL node identity file" ;
      let* _ =
        Agent.copy agent ~source ~destination:(Dal_node.identity_file node)
      in
      Lwt.return_unit

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
        let () = toplog "Some agent given" in
        let () = toplog "Initializing the bootstrap node agent" in
        let* node =
          Node.init
            ?identity_file:configuration.bootstrap_node_identity_file
            ~name:"bootstrap-node"
            configuration.network
            agent
        in
        let* dal_node =
          if not configuration.with_dal then Lwt.return_none
          else
            let* dal_node =
              Dal_node.Agent.create ~name:"bootstrap-dal-node" agent ~node
            in

            let* () =
              Dal_node.init_config
                ~expected_pow:26.
                ~bootstrap_profile:true
                dal_node
            in
            let* () =
              may_copy_dal_node_identity_file
                agent
                dal_node
                configuration.bootstrap_dal_node_identity_file
            in
            let* () = Node.wait_for_ready node in
            let otel = Cloud.open_telemetry_endpoint cloud in
            let* () =
              Dal_node.Agent.run
                ?otel
                ~memtrace:configuration.memtrace
                ~event_level:`Notice
                dal_node
            in
            Lwt.return_some dal_node
        in
        let* () =
          add_prometheus_source cloud agent "bootstrap" ?dal_node ~node
        in
        let client = Client.create ~endpoint:(Node node) () in
        let node_rpc_endpoint =
          Endpoint.
            {
              scheme = "http";
              host =
                (match Agent.point agent with
                | None -> "127.0.0.1"
                | Some point -> fst point);
              port = Node.rpc_port node;
            }
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
                agent
                ~node_name:(Node.name node)
                ~node_port:(Node.rpc_port node)
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
      (List.length configuration.dal_node_producers)
      bootstrap.client
  in
  let* etherlink_rollup_operator_key =
    if etherlink_configuration <> None then
      let () = toplog "Generating a key pair for Etherlink operator" in
      Client.stresstest_gen_keys
        ~alias_prefix:"etherlink_operator"
        1
        bootstrap.client
    else Lwt.return_nil
  in
  let* etherlink_batching_operator_keys =
    if etherlink_configuration <> None then
      Client.stresstest_gen_keys
        ~alias_prefix:"etherlink_batching"
        20
        bootstrap.client
    else Lwt.return []
  in
  let accounts_to_fund =
    List.map (fun producer -> (producer, 10 * 1_000_000)) producer_accounts
    @ List.map
        (fun operator -> (operator, 11_000 * 1_000_000))
        etherlink_rollup_operator_key
    @ List.map
        (fun batcher -> (batcher, 10 * 1_000_000))
        etherlink_batching_operator_keys
  in
  let etherlink_rollup_operator_key =
    match etherlink_rollup_operator_key with key :: _ -> Some key | [] -> None
  in
  let* () =
    if List.length accounts_to_fund > 0 then
      let () = toplog "Funding the producer accounts" in
      let fundraiser_key =
        match configuration.fundraiser with
        | None ->
            Test.fail
              "No fundraiser key was specified. Please use either \
               `--fundraiser` or the variable environment \
               `TEZT_CLOUD_FUNDRAISER` to specified an unencrypted secret key \
               of an account having funds to run the scenario"
        | Some key -> key
      in
      let* () =
        Client.import_secret_key
          bootstrap.client
          (Unencrypted fundraiser_key)
          ~alias:"fundraiser"
      in
      let () = toplog "Revealing the fundraiser public key" in
      let* () =
        let*? process = Client.reveal ~src:"fundraiser" bootstrap.client in
        let* _ = Process.wait process in
        Lwt.return_unit
      in
      let* fundraiser =
        Client.show_address ~alias:"fundraiser" bootstrap.client
      in
      let () = toplog "Fetching fundraiser's counter" in
      let* counter =
        Operation.get_next_counter ~source:fundraiser bootstrap.client
      in
      let () = toplog "Fetching fundraiser's balance" in
      let* _balance =
        Client.get_balance_for ~account:"fundraiser" bootstrap.client
      in
      let () = toplog "Injecting the batch" in
      let* _op_hash =
        accounts_to_fund
        |> List.map (fun (dest, amount) ->
               Operation.Manager.transfer ~amount ~dest ())
        |> Operation.Manager.make_batch ~source:fundraiser ~counter
        |> Fun.flip (Operation.Manager.inject ~dont_wait:true) bootstrap.client
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
  in
  Lwt.return
    ( bootstrap,
      baker_accounts,
      producer_accounts,
      etherlink_rollup_operator_key,
      etherlink_batching_operator_keys )

let init_sandbox_and_activate_protocol cloud (configuration : configuration)
    ?(etherlink_configuration : etherlink_configuration option) agent =
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
  let* bootstrap_node =
    Node.init
      ?data_dir
      ?identity_file:configuration.bootstrap_node_identity_file
      ~dal_config
      ~name
      configuration.network
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
          agent
          ~node:bootstrap_node
      in
      Lwt.return_some dal_node
    else Lwt.return_none
  in
  let* () =
    Cloud.add_service
      cloud
      ~name:"Explorus"
      ~url:
        (sf "http://explorus.io?network=%s" (Node.rpc_endpoint bootstrap_node))
  in
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
      (List.length configuration.dal_node_producers)
      client
  in
  let* etherlink_rollup_operator_key =
    if etherlink_configuration <> None then
      Client.stresstest_gen_keys ~alias_prefix:"etherlink_operator" 1 client
    else Lwt.return_nil
  in
  let* etherlink_batching_operator_keys =
    if etherlink_configuration <> None then
      Client.stresstest_gen_keys ~alias_prefix:"etherlink_batching" 20 client
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
        (producer_accounts @ etherlink_rollup_operator_key
       @ etherlink_batching_operator_keys)
    in
    let overrides =
      if Cli.dal_incentives then
        [
          (["dal_parametric"; "incentives_enable"], `Bool true);
          (["dal_parametric"; "rewards_ratio"; "numerator"], `String "1");
          (["dal_parametric"; "rewards_ratio"; "denominator"], `String "10");
          (* This one is derived from the two constants above. *)
          (["issuance_weights"; "dal_rewards_weight"], `Int 5120);
          (["blocks_per_cycle"], `Int 8);
          (* This parameter should be lower than blocks_per_cycle *)
          (["nonce_revelation_threshold"], `Int 4);
        ]
      else []
    in
    Protocol.write_parameter_file
      ~bootstrap_accounts
      ~additional_bootstrap_accounts
      ~base
      overrides
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
          may_copy_dal_node_identity_file
            agent
            dal_bootstrap_node
            configuration.bootstrap_dal_node_identity_file
        in
        Dal_node.Agent.run
          ?otel
          ~memtrace:configuration.memtrace
          ~event_level:`Notice
          dal_bootstrap_node
  in
  let* () =
    add_prometheus_source
      ~node:bootstrap_node
      ?dal_node:dal_bootstrap_node
      cloud
      agent
      "bootstrap"
  in

  let node_rpc_endpoint =
    Endpoint.
      {
        scheme = "http";
        host =
          (match Agent.point agent with
          | None -> "127.0.0.1"
          | Some point -> fst point);
        port = Node.rpc_port bootstrap_node;
      }
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
      etherlink_batching_operator_keys )

let init_baker ?stake cloud (configuration : configuration) ~bootstrap teztale
    account i agent =
  let stake =
    match stake with
    | None -> List.nth configuration.stake i
    | Some stake -> stake
  in
  let name = Format.asprintf "baker-node-%d" i in
  let data_dir =
    configuration.data_dir |> Option.map (fun data_dir -> data_dir // name)
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
    if not configuration.with_dal then Lwt.return_none
    else
      let* dal_node =
        Dal_node.Agent.create
          ~name:(Format.asprintf "baker-dal-node-%d" i)
          ~node
          agent
      in
      let* () =
        Dal_node.init_config
          ~expected_pow:(Network.expected_pow configuration.network)
          ~attester_profiles:[account.Account.public_key_hash]
          ~peers:[bootstrap.dal_node_p2p_endpoint |> Option.get]
          (* Invariant: Option.get don't fail because t.configuration.dal is true *)
          dal_node
      in
      let otel = Cloud.open_telemetry_endpoint cloud in
      let* () =
        Dal_node.Agent.run
          ?otel
          ~memtrace:configuration.memtrace
          ~event_level:`Notice
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
            agent
            ~node_name:(Node.name node)
            ~node_port:(Node.rpc_port node)
        in
        Teztale.update_alias
          teztale
          ~address:account.Account.public_key_hash
          ~alias:account.Account.alias
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
      ~delegates:[account.Account.alias]
      ~protocol:configuration.protocol
      ~client
      ?dal_node
      node
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
  Lwt.return {node; dal_node; baker; account; stake}

let init_producer cloud configuration ~bootstrap teztale account i slot_index
    agent =
  let () = toplog "Initializing a DAL producer" in
  let name = Format.asprintf "producer-node-%i" i in
  let data_dir =
    configuration.data_dir |> Option.map (fun data_dir -> data_dir // name)
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
  let endpoint = Client.Node node in
  let () = toplog "Init producer: create client" in
  let* client = Client.Agent.create ~endpoint agent in
  let () = toplog "Init producer: import key" in
  let* () =
    Client.import_secret_key
      client
      ~endpoint
      account.Account.secret_key
      ~alias:account.Account.alias
  in
  let () = toplog "Init producer: reveal account" in
  let*! () = Client.reveal client ~endpoint ~src:account.Account.alias in
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
      ~expected_pow:(Network.expected_pow configuration.network)
      ~observer_profiles:[slot_index]
      ~peers:(Option.to_list bootstrap.dal_node_p2p_endpoint)
      (* If `--no-dal` is given with `--producers 2`, then DAL nodes
         are run for the producers. While this is weird in sandboxed
         mode, it can make sense on ghostnet for example. *)
      dal_node
  in
  let () = toplog "Init producer: add DAL node metrics" in
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
  let () = toplog "Init producer: wait for DAL node to be ready" in
  let otel = Cloud.open_telemetry_endpoint cloud in
  let is_ready =
    Dal_node.Agent.run
      ?otel
      ~memtrace:configuration.memtrace
      ~event_level:`Notice
      dal_node
  in
  let () = toplog "Init producer: DAL node is ready" in
  let* () =
    match teztale with
    | None -> Lwt.return_unit
    | Some teztale ->
        Teztale.add_archiver
          teztale
          agent
          ~node_name:(Node.name node)
          ~node_port:(Node.rpc_port node)
  in
  Lwt.return {client; node; dal_node; account; is_ready; slot_index}

let init_observer cloud configuration ~bootstrap teztale ~topic i agent =
  let name = Format.asprintf "observer-node-%i" i in
  let data_dir =
    configuration.data_dir |> Option.map (fun data_dir -> data_dir // name)
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
    match topic with
    | `Slot_index slot_index ->
        Dal_node.init_config
          ~expected_pow:(Network.expected_pow configuration.network)
          ~observer_profiles:[slot_index]
          ~peers:(Option.to_list bootstrap.dal_node_p2p_endpoint)
          dal_node
    | `Pkh pkh ->
        Dal_node.init_config
          ~expected_pow:(Network.expected_pow configuration.network)
          ~attester_profiles:[pkh]
          ~peers:(Option.to_list bootstrap.dal_node_p2p_endpoint)
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
      ?otel
      ~memtrace:configuration.memtrace
      ~event_level:`Notice
      dal_node
  in
  let* () =
    match teztale with
    | None -> Lwt.return_unit
    | Some teztale ->
        Teztale.add_archiver
          teztale
          agent
          ~node_name:(Node.name node)
          ~node_port:(Node.rpc_port node)
  in
  Lwt.return {node; dal_node; topic}

let init_etherlink_dal_node ~bootstrap ~next_agent ~dal_slots ~network ~otel
    ~memtrace =
  match dal_slots with
  | [] ->
      toplog "Etherlink will run without DAL support" ;
      none
  | [_] ->
      (* On a single DAL slot index, we launch a single DAL node for
         this index on a dedicated VM and give it directly as endpoint
         to the rollup node. *)
      toplog "Etherlink sequencer will run its own DAL node" ;
      let name = Format.asprintf "etherlink-dal-operator" in
      let* agent = next_agent ~name in
      let* node =
        Node.init
          ~name
          ~arguments:
            [Peer bootstrap.node_p2p_endpoint; History_mode (Rolling (Some 79))]
          network
          agent
      in
      let* dal_node = Dal_node.Agent.create ~name ~node agent in
      let* () =
        Dal_node.init_config
          ~expected_pow:(Network.expected_pow network)
          ~producer_profiles:dal_slots
          ~peers:(Option.to_list bootstrap.dal_node_p2p_endpoint)
          dal_node
      in
      let* () = Dal_node.Agent.run ?otel dal_node in
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
      let name = Format.asprintf "etherlink-dal-operator" in
      let* agent = next_agent ~name in
      let* node =
        Node.init
          ~name
          ~arguments:
            [Peer bootstrap.node_p2p_endpoint; History_mode (Rolling (Some 79))]
          network
          agent
      in
      let* default_dal_node = Dal_node.Agent.create ~name ~node agent in
      let* () =
        Dal_node.init_config
          ~expected_pow:(Network.expected_pow network)
          ~peers:(Option.to_list bootstrap.dal_node_p2p_endpoint)
          default_dal_node
      in
      let* () = Dal_node.Agent.run ?otel ~memtrace default_dal_node in
      let default_endpoint = Dal_node.rpc_endpoint default_dal_node in

      let* dal_slots_and_nodes =
        dal_slots
        |> Lwt_list.map_p (fun slot_index ->
               let name =
                 Format.asprintf "etherlink-dal-operator-%d" slot_index
               in
               let* agent = next_agent ~name in
               let* node =
                 Node.init
                   ~name
                   ~arguments:[Peer bootstrap.node_p2p_endpoint]
                   network
                   agent
               in
               let* dal_node = Dal_node.Agent.create ~name ~node agent in
               let* () =
                 Dal_node.init_config
                   ~expected_pow:(Network.expected_pow network)
                   ~producer_profiles:[slot_index]
                   ~peers:(Option.to_list bootstrap.dal_node_p2p_endpoint)
                   dal_node
               in
               let* () = Dal_node.Agent.run ?otel ~memtrace dal_node in
               return (slot_index, Dal_node.rpc_endpoint dal_node))
      in
      let* reverse_proxy_dal_node =
        Dal_reverse_proxy.init_reverse_proxy
          ~next_agent
          ~default_endpoint
          (List.to_seq dal_slots_and_nodes)
      in
      some reverse_proxy_dal_node

let init_etherlink_operator_setup cloud configuration etherlink_configuration
    name ~bootstrap ~dal_slots account batching_operators agent next_agent =
  let is_sequencer = etherlink_configuration.etherlink_sequencer in
  let data_dir =
    configuration.data_dir |> Option.map (fun data_dir -> data_dir // name)
  in
  let* node =
    Node.init
      ?data_dir
      ~name
      ~arguments:
        [Peer bootstrap.node_p2p_endpoint; History_mode (Rolling (Some 79))]
      configuration.network
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
  let output_config = Temp.file "config.yaml" in
  let bootstrap_accounts =
    Tezt_etherlink.Eth_account.bootstrap_accounts |> Array.to_list
    |> List.map (fun account -> account.Tezt_etherlink.Eth_account.address)
  in
  let*! () =
    let sequencer = if is_sequencer then Some account.public_key else None in
    let () = toplog "Init Etherlink: configuring the kernel" in
    Tezt_etherlink.Evm_node.make_kernel_installer_config
      ?sequencer
      ~bootstrap_accounts
      ~output:output_config
      ~enable_dal:(Option.is_some dal_slots)
      ?chain_id:etherlink_configuration.chain_id
      ?dal_slots
      ()
  in
  let otel = Cloud.open_telemetry_endpoint cloud in
  let* dal_node =
    init_etherlink_dal_node
      ~bootstrap
      ~next_agent
      ~dal_slots:etherlink_configuration.etherlink_dal_slots
      ~network:configuration.network
      ~otel
      ~memtrace:configuration.memtrace
  in
  let operators =
    List.map
      (fun account -> (Sc_rollup_node.Batching, account.Account.alias))
      batching_operators
  in
  let* sc_rollup_node =
    Sc_rollup_node.Agent.create
      ~name:(Format.asprintf "etherlink-%s-rollup-node" name)
      ~base_dir:(Client.base_dir client)
      ~default_operator:account.alias
      ~operators
      ?dal_node
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
  let () = toplog "Init Etherlink: originating the rollup" in
  let* sc_rollup_address =
    Sc_rollup_helpers.Agent.originate_sc_rollup
      ~kind:pvm_kind
      ~boot_sector:output
      ~parameters_ty:Tezt_etherlink.Helpers.evm_type
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
        |> JSON.update "experimental_features" (fun _ ->
               JSON.annotate ~origin:"patch-config:experimental_features"
               @@ `O [("enable_websocket", `Bool true)]))
      ~name:(Format.asprintf "etherlink-%s-evm-node" name)
      ~mode
      endpoint
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

let init_etherlink_producer_setup operator name ~bootstrap agent =
  let* node =
    Node.Agent.init
      ~rpc_external:Cli.node_external_rpc_server
      ~name:(Format.asprintf "etherlink-%s-node" name)
      ~arguments:[Peer bootstrap.node_p2p_endpoint; Synchronisation_threshold 0]
      agent
  in
  let endpoint = Client.Node node in
  let* client = Client.Agent.create ~endpoint agent in
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
        private_rpc_port = None;
        initial_kernel = output;
        preimages_dir = Some preimages_dir;
        rollup_node_endpoint = Sc_rollup_node.endpoint sc_rollup_node;
      }
  in
  let () = toplog "Init Etherlink: init producer %s" name in
  let endpoint = Evm_node.endpoint operator.evm_node in
  (* TODO: try using this local EVM node for Floodgate confirmations. *)
  let* evm_node =
    Evm_node.Agent.init
      ~name:(Format.asprintf "etherlink-%s-evm-node" name)
      ~mode
      endpoint
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
      ~max_active_eoa:150
      ~max_transaction_batch_length:50
      ~controller:Tezt_etherlink.Eth_account.bootstrap_accounts.(0)
      agent
  in
  return ()

let init_etherlink cloud configuration etherlink_configuration ~bootstrap
    etherlink_rollup_operator_key batching_operators ~dal_slots next_agent =
  let () = toplog "Initializing an Etherlink operator" in
  let* operator_agent = next_agent ~name:"etherlink-operator" in
  let* operator =
    init_etherlink_operator_setup
      cloud
      configuration
      etherlink_configuration
      ~dal_slots
      "operator"
      ~bootstrap
      etherlink_rollup_operator_key
      batching_operators
      operator_agent
      next_agent
  in
  let accounts = Tezt_etherlink.Eth_account.bootstrap_accounts in
  let* producers_agents =
    List.init etherlink_configuration.etherlink_producers (fun i ->
        let name = Format.asprintf "etherlink-producer-%d" i in
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
             ~bootstrap
             agent)
    |> Lwt.join
  in
  return {configuration = etherlink_configuration; operator; accounts}

let obtain_some_node_rpc_endpoint agent network (bootstrap : bootstrap)
    (bakers : baker list) (producers : producer list)
    (observers : observer list) etherlink =
  match (agent, network) with
  | None, (`Mainnet | `Ghostnet | `Nextnet _ | `Weeklynet _) -> (
      match (bakers, producers, observers, etherlink) with
      | baker :: _, _, _, _ -> Node.as_rpc_endpoint baker.node
      | [], producer :: _, _, _ -> Node.as_rpc_endpoint producer.node
      | [], [], observer :: _, _ -> Node.as_rpc_endpoint observer.node
      | [], [], [], Some etherlink ->
          Node.as_rpc_endpoint etherlink.operator.node
      | [], [], [], None -> bootstrap.node_rpc_endpoint)
  | _ -> bootstrap.node_rpc_endpoint

module Alert = struct
  open Tezt_cloud

  let receiver =
    match Cli.Alerts.dal_slack_webhook with
    | None -> Alert.null_receiver
    | Some api_url ->
        Alert.slack_receiver ~name:"slack-notifications" ~api_url ()

  let alert =
    Alert.make
      ~route:
        (Alert.route
           ~group_wait:"10s"
           ~group_interval:"10s"
           ~repeat_interval:"12h"
             (* In the future, probably we should make this value equals to Cli.metrics_retention *)
           receiver)
      ~group_name:"tezt"
      ~name:"LowDALAttestedCommitmentsRatio"
      ~for_:"5m"
      ~severity:Info
      ~expr:{|vector(1)|}
      ~summary:"DAL attested commitments ratio over the last 12 hours"
      ~description:
        {|'DAL attested commitments ratio over the last 12 hours: {{ with query "avg_over_time(tezt_dal_commitments_ratio{kind=\"attested\"}[12h])" }}{{ . | first | value | printf "%.2f%%" }}{{ end }}'|}
      ()

  let alerts = [alert]
end

let init ~(configuration : configuration) etherlink_configuration cloud
    next_agent =
  let () = toplog "Init" in
  let* bootstrap_agent =
    if configuration.bootstrap then
      let* agent = next_agent ~name:"bootstrap" in
      Lwt.return_some agent
    else Lwt.return_none
  in
  let* attesters_agents =
    configuration.stake
    |> List.mapi (fun i _stake ->
           let name = Format.asprintf "attester-%d" i in
           next_agent ~name)
    |> Lwt.all
  in
  let* bakers_agents =
    configuration.bakers
    |> List.mapi (fun i _stake ->
           let name = Format.asprintf "baker-%d" i in
           next_agent ~name)
    |> Lwt.all
  in
  let* producers_agents =
    configuration.dal_node_producers
    |> List.map (fun slot_index ->
           let name = Format.asprintf "dal-producer-%d" slot_index in
           let* name = next_agent ~name in
           return (name, slot_index))
    |> Lwt.all
  in
  let* observers_slot_index_agents =
    configuration.observer_slot_indices
    |> List.map (fun slot_index ->
           let name = Format.asprintf "dal-observer-%d" slot_index in
           let* agent = next_agent ~name in
           return (`Slot_index slot_index, agent))
    |> Lwt.all
  in
  let* observers_bakers_agents =
    configuration.observer_pkhs
    |> List.map (fun pkh ->
           let name = Format.asprintf "observer-%s" (String.sub pkh 0 8) in
           let* agent = next_agent ~name in
           return (`Pkh pkh, agent))
    |> Lwt.all
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
         etherlink_batching_operator_keys ) =
    match configuration.network with
    | `Sandbox ->
        let bootstrap_agent = Option.get bootstrap_agent in
        init_sandbox_and_activate_protocol
          cloud
          configuration
          ?etherlink_configuration
          bootstrap_agent
    | (`Ghostnet | `Nextnet _ | `Mainnet | `Weeklynet _) as network ->
        init_public_network
          cloud
          configuration
          etherlink_configuration
          teztale
          bootstrap_agent
          network
  in
  let* fresh_bakers =
    Lwt_list.mapi_p
      (fun i (agent, account) ->
        init_baker cloud configuration ~bootstrap teztale account i agent)
      (List.combine attesters_agents baker_accounts)
  in
  let* bakers_with_secret_keys =
    Lwt_list.mapi_p
      (fun i (agent, sk) ->
        let sk = Account.Unencrypted sk in
        let client = Client.create () in
        let alias = Format.asprintf "baker-%02d" i in
        let* () = Client.import_secret_key client sk ~alias in
        let* account = Client.show_address ~alias client in
        (* A bit random, to fix later. *)
        let stake = 1 in
        init_baker ~stake cloud configuration ~bootstrap teztale account i agent)
      (List.combine bakers_agents configuration.bakers)
  in
  let bakers = fresh_bakers @ bakers_with_secret_keys in
  let* constants =
    RPC_core.call
      bootstrap.node_rpc_endpoint
      (RPC.get_chain_block_context_constants_parametric ())
  in
  let time_between_blocks =
    JSON.(constants |-> "minimal_block_delay" |> as_int)
  in
  let* parameters =
    Dal_common.Parameters.from_endpoint bootstrap.node_rpc_endpoint
  in
  let () = toplog "Init: initializting producers and observers" in
  let* producers =
    Lwt_list.mapi_p
      (fun i ((agent, slot_index), account) ->
        init_producer
          cloud
          configuration
          ~bootstrap
          teztale
          account
          i
          slot_index
          agent)
      (List.combine producers_agents producer_accounts)
  and* observers =
    Lwt_list.mapi_p
      (fun i (topic, agent) ->
        init_observer cloud configuration ~bootstrap teztale ~topic i agent)
      (observers_slot_index_agents @ observers_bakers_agents)
  in
  let () = toplog "Init: all producers and observers have been initialized" in
  let* etherlink =
    match etherlink_configuration with
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
          toplog
            "Init: Etherlink+DAL feature flag: %b"
            (Option.is_some dal_slots)
        in
        let* etherlink =
          let () = toplog "Init: calling init_etherlink" in
          init_etherlink
            cloud
            configuration
            etherlink_configuration
            ~bootstrap
            etherlink_rollup_operator_key
            etherlink_batching_operator_keys
            next_agent
            ~dal_slots
        in
        some etherlink
    | None ->
        let () =
          toplog
            "Init: skipping Etherlink initialization because --etherlink was \
             not given on the CLI"
        in
        none
  in

  let infos = Hashtbl.create 101 in
  let metrics = Hashtbl.create 101 in
  let* first_level =
    match configuration.network with
    | `Sandbox -> Lwt.return 1
    | _ -> Network.get_level bootstrap.node_rpc_endpoint
  in
  Hashtbl.replace metrics first_level default_metrics ;
  let disconnection_state =
    Option.map Disconnect.init configuration.disconnect
  in
  let some_node_rpc_endpoint =
    obtain_some_node_rpc_endpoint
      bootstrap_agent
      configuration.network
      bootstrap
      bakers
      producers
      observers
      etherlink
  in
  let* aliases =
    let accounts = List.map (fun ({account; _} : baker) -> account) bakers in
    Network.aliases ~accounts configuration.network
  in
  let* versions = Network.versions configuration.network in
  let aliases = Option.value ~default:(Hashtbl.create 0) aliases in
  let versions = Option.value ~default:(Hashtbl.create 0) versions in
  let otel = Cloud.open_telemetry_endpoint cloud in
  (* Adds monitoring for all agents for octez-dal-node and octez-node
     TODO: monitor only specific agents for specific binaries *)
  let* () =
    Cloud.register_binary cloud ~group:"DAL" ~name:"octez-dal-node" ()
  in
  let* () = Cloud.register_binary cloud ~group:"L1" ~name:"octez-node" () in
  let* () = Cloud.register_binary cloud ~name:"main.exe" () in
  Lwt.return
    {
      cloud;
      configuration;
      bootstrap;
      some_node_rpc_endpoint;
      bakers;
      producers;
      observers;
      etherlink;
      time_between_blocks;
      parameters;
      infos;
      metrics;
      disconnection_state;
      first_level;
      teztale;
      aliases;
      versions;
      otel;
    }

let wait_for_level t level =
  match t.bootstrap.node with
  | None ->
      let rec loop () =
        let* head_level = Network.get_level t.bootstrap.node_rpc_endpoint in
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
  let* aliases =
    let accounts = List.map (fun ({account; _} : baker) -> account) t.bakers in
    Network.aliases ~accounts t.configuration.network
  in
  let* versions = Network.versions t.configuration.network in
  let aliases = Option.value ~default:t.aliases aliases in
  let versions = Option.value ~default:t.versions versions in
  t.aliases <- aliases ;
  t.versions <- versions ;
  Lwt.return_unit

let on_new_level t ?etherlink level =
  let* () = wait_for_level t level in
  toplog "Start process level %d" level ;
  clean_up t (level - t.configuration.blocks_history) ;
  let* () =
    if level mod 1_000 = 0 then update_bakers_infos t else Lwt.return_unit
  in
  let* infos_per_level =
    get_infos_per_level
      ~client:t.bootstrap.client
      ~endpoint:t.bootstrap.node_rpc_endpoint
      ~level
      ~etherlink_operators:
        (match etherlink with
        | None -> []
        | Some setup ->
            setup.operator.account :: setup.operator.batching_operators)
  in
  toplog "Level info processed" ;
  Hashtbl.replace t.infos level infos_per_level ;
  let metrics =
    get_metrics t infos_per_level (Hashtbl.find t.metrics (level - 1))
  in
  pp_metrics t metrics ;
  push_metrics t metrics ;
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
              Dal_node.terminate dal_node)
        in
        let* disconnection_state =
          Disconnect.reconnect disconnection_state level (fun b ->
              let baker_to_reconnect = List.nth t.bakers (b mod nb_bakers) in
              (* Invariant: Option.get don't fail because t.configuration.dal is true *)
              let dal_node = baker_to_reconnect.dal_node |> Option.get in
              Dal_node.Agent.run
                ?otel:t.otel
                ~memtrace:t.configuration.memtrace
                dal_node)
        in
        Lwt.return {t with disconnection_state = Some disconnection_state}
    | _ -> Lwt.return t
  in
  toplog "Level processed" ;
  Lwt.return t

let ensure_enough_funds t i =
  let producer = List.nth t.producers i in
  match t.configuration.network with
  | `Sandbox -> (* Producer has enough money *) Lwt.return_unit
  | _ ->
      let* balance =
        RPC_core.call t.bootstrap.node_rpc_endpoint
        @@ RPC.get_chain_block_context_contract_balance
             ~id:producer.account.public_key_hash
             ()
      in
      (* This is to prevent having to refund two producers at the same time and ensure it can produce at least one slot. *)
      let random = Random.int 5_000_000 + 10_000 in
      if balance < Tez.of_mutez_int random then (
        let* fundraiser =
          Client.show_address ~alias:"fundraiser" t.bootstrap.client
        in
        toplog "### transfer" ;
        let* _op_hash =
          Operation.Manager.transfer
            ~amount:10_000_000
            ~dest:producer.account
            ()
          |> Operation.Manager.make ~source:fundraiser
          |> Seq.return |> List.of_seq
          |> Fun.flip
               (Operation.Manager.inject ~dont_wait:true)
               t.bootstrap.client
        in
        Lwt.return_unit)
      else Lwt.return_unit

let produce_slot t level i =
  toplog "producing slots for level %d" level ;
  let* () = ensure_enough_funds t i in
  toplog "ensured enough funds are available" ;
  let producer = List.nth t.producers i in
  let index = producer.slot_index in
  let content =
    Format.asprintf "%d:%d" level index
    |> Helpers.make_slot
         ~padding:false
         ~slot_size:t.parameters.cryptobox.slot_size
  in
  let* _ = Node.wait_for_level producer.node level in
  let* _ =
    Helpers.publish_and_store_slot
      ~dont_wait:true
      producer.client
      producer.dal_node
      producer.account
      ~force:true
      ~index
      content
  in
  Log.info "publish slot" ;
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
    if producers_not_ready t then (
      toplog "producers not ready for level %d" level ;
      Lwt.return_unit)
    else
      Seq.ints 0
      |> Seq.take (List.length t.configuration.dal_node_producers)
      |> Seq.map (fun i -> produce_slot t level i)
      |> List.of_seq |> Lwt.join
  in
  let* t = p in
  loop t (level + 1)

let configuration, etherlink_configuration =
  let stake = Cli.stake in
  let stake_machine_type = Cli.stake_machine_type in
  let dal_node_producers =
    let last_index = ref 0 in
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
  let observer_pkhs = Cli.observer_pkhs in
  let protocol = Cli.protocol in
  let producer_machine_type = Cli.producer_machine_type in
  let etherlink = Cli.etherlink in
  let etherlink_sequencer = Cli.etherlink_sequencer in
  let etherlink_producers = Cli.etherlink_producers in
  let disconnect = Cli.disconnect in
  let network = Cli.network in
  let bootstrap = Cli.bootstrap in
  let etherlink_dal_slots = Cli.etherlink_dal_slots in
  let teztale = Cli.teztale in
  let memtrace = Cli.memtrace in
  let data_dir = Cli.data_dir in
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
        {
          etherlink_sequencer;
          etherlink_producers;
          etherlink_dal_slots;
          chain_id = etherlink_chain_id;
        }
    else None
  in
  let blocks_history = Cli.blocks_history in
  let metrics_retention = Cli.metrics_retention in
  let bootstrap_node_identity_file = Cli.bootstrap_node_identity_file in
  let bootstrap_dal_node_identity_file = Cli.bootstrap_dal_node_identity_file in
  let with_dal = Cli.with_dal in
  let bakers = Cli.bakers in
  let t =
    {
      with_dal;
      stake;
      bakers;
      stake_machine_type;
      dal_node_producers;
      observer_slot_indices;
      observer_pkhs;
      protocol;
      producer_machine_type;
      disconnect;
      network;
      bootstrap;
      teztale;
      memtrace;
      data_dir;
      fundraiser;
      blocks_history;
      metrics_retention;
      bootstrap_node_identity_file;
      bootstrap_dal_node_identity_file;
    }
  in
  (t, etherlink)

type agent_kind =
  | Bootstrap
  | Baker of int
  | Producer of int
  | Observer of [`Index of int | `Pkh of string]
  | Reverse_proxy
  | Etherlink_operator
  | Etherlink_dal_operator
  | Etherlink_dal_observer of {slot_index : int}
  | Etherlink_producer of int

let benchmark () =
  toplog "Parsing CLI done" ;
  let vms =
    [
      (if configuration.bootstrap then [Bootstrap] else []);
      List.mapi (fun i _ -> Baker i) configuration.stake;
      List.map (fun i -> Producer i) configuration.dal_node_producers;
      List.map
        (fun index -> Observer (`Index index))
        configuration.observer_slot_indices;
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
    ]
    |> List.concat
  in
  let docker_image =
    Option.map
      (fun tag -> Agent.Configuration.Octez_release {tag})
      Cli.octez_release
  in
  let default_vm_configuration ~name =
    Agent.Configuration.make ?docker_image ~name ()
  in
  let name_of = function
    | Bootstrap -> "bootstrap"
    | Baker i -> Format.asprintf "attester-%d" i
    | Producer i -> Format.asprintf "dal-producer-%d" i
    | Observer (`Index i) -> Format.asprintf "dal-observer-%d" i
    | Observer (`Pkh pkh) ->
        (* Shorting the pkh enables to get better logs. *)
        Format.asprintf "dal-observer-%s" (String.sub pkh 0 8)
    | Reverse_proxy -> "dal-reverse-proxy"
    | Etherlink_operator -> "etherlink-operator"
    | Etherlink_dal_operator -> Format.asprintf "etherlink-dal-operator"
    | Etherlink_dal_observer {slot_index} ->
        Format.asprintf "etherlink-dal-observer-%d" slot_index
    | Etherlink_producer i -> Format.asprintf "etherlink-producer-%d" i
  in
  let vms =
    vms
    |> List.map (fun agent_kind ->
           let name = name_of agent_kind in
           match agent_kind with
           | Bootstrap -> default_vm_configuration ~name
           | Baker i -> (
               match configuration.stake_machine_type with
               | None -> default_vm_configuration ~name
               | Some list -> (
                   try
                     let machine_type = List.nth list i in
                     Agent.Configuration.make
                       ?docker_image
                       ~machine_type
                       ~name
                       ()
                   with _ -> default_vm_configuration ~name))
           | Producer _ ->
               let machine_type = configuration.producer_machine_type in
               Agent.Configuration.make ?docker_image ?machine_type ~name ()
           | Observer _ | Etherlink_dal_operator | Etherlink_dal_observer _ ->
               Agent.Configuration.make ?docker_image ~name ()
           | Etherlink_operator -> default_vm_configuration ~name
           | Etherlink_producer _ -> default_vm_configuration ~name
           | Reverse_proxy -> default_vm_configuration ~name)
  in
  Cloud.register
  (* docker images are pushed before executing the test in case binaries are modified locally. This way we always use the latest ones. *)
    ~vms
    ~proxy_files:
      ([
         Format.asprintf
           "src/%s/parameters/mainnet-parameters.json"
           (Protocol.directory configuration.protocol);
         "octez-node";
       ]
      @ (if Cli.refresh_binaries then
           [
             "octez-dal-node";
             "octez-client";
             Tezt_wrapper.Uses.path (Protocol.baker configuration.protocol);
           ]
           @ (if Cli.etherlink then
                ["evm_kernel.wasm"; "octez-evm-node"; "octez-smart-rollup-node"]
              else [])
           @
           if Cli.teztale then
             ["octez-teztale-archiver"; "octez-teztale-server"]
           else []
         else [])
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
    ~tags:[Tag.cloud; "dal"; "benchmark"]
    ~alerts:Alert.alerts
    (fun cloud ->
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
      let* t = init ~configuration etherlink_configuration cloud next_agent in
      toplog "Starting main loop" ;
      loop t (t.first_level + 1))

let register () = benchmark ()
