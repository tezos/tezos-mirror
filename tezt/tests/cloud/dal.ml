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
  | Etherlink_dal_operator -> "etherlink-dal-operator"
  | Etherlink_dal_observer {slot_index} ->
      Format.asprintf "etherlink-dal-operator-%d" slot_index
  | Etherlink_producer i -> Format.asprintf "etherlink-producer-%d" i

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

  let init ?(arguments = []) ?data_dir ?identity_file ?dal_config ~rpc_external
      ~name network cloud agent =
    toplog "Inititializing an L1 node for %s" name ;
    match network with
    | #Network.public -> (
        let network = Network.to_public network in
        (* for public networks deployments, we listen on all interfaces on both
           ipv4 and ipv6 *)
        let net_addr = "[::]" in
        match data_dir with
        | Some data_dir ->
            let* node =
              Node.Agent.create
                ~rpc_external
                ~net_addr
                ~arguments
                ~data_dir
                ~name
                cloud
                agent
            in
            let* () = may_copy_node_identity_file agent node identity_file in
            let* () =
              Node.Agent.run
                node
                [Network (Network.to_octez_network_options network)]
            in
            let* () = Node.wait_for_ready node in
            Lwt.return node
        | None ->
            toplog
              "No data dir given, we will attempt to bootstrap the node from a \
               rolling snapshot." ;
            toplog "Creating the agent." ;
            let* node =
              Node.Agent.create
                ~rpc_external
                ~net_addr
                ~arguments:
                  [
                    Network (Network.to_octez_network_options network);
                    Expected_pow 26;
                    Cors_origin "*";
                  ]
                ?data_dir
                ~name
                cloud
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
                      let* _ =
                        Process.wait (Process.spawn "rm" ["snapshot_file"])
                      in
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
              Node.Agent.run
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
    | _ (* private network *) -> (
        (* For sandbox deployments, we only listen on local interface, hence
           no connection could be made to us from outside networks *)
        let net_addr = "127.0.0.1" in
        match data_dir with
        | None ->
            let* node =
              Node.Agent.create ~net_addr ~rpc_external ~name cloud agent
            in
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
              Node.Agent.run
                node
                ([
                   No_bootstrap_peers;
                   Synchronisation_threshold 0;
                   Cors_origin "*";
                   Force_history_mode_switch;
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
            let* node =
              Node.Agent.create
                ~rpc_external
                ~net_addr
                ~arguments
                ~data_dir
                ~name
                cloud
                agent
            in
            let* () = may_copy_node_identity_file agent node identity_file in
            let* () = Node.Agent.run node [] in
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

  let init_reverse_proxy cloud ~next_agent ~default_endpoint
      (proxified_dal_nodes : (int * string) Seq.t) =
    (* A NGINX reverse proxy which balances load between producer DAL
       nodes based on the requested slot index. *)
    let name = name_of Reverse_proxy in
    let* agent = next_agent ~name in
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
    let l1_node_endpoint = Endpoint.make ~host:"" ~scheme:"" ~port:0 () in
    let* dal_node =
      Dal_node.Agent.create_from_endpoint
        ~name:"bootstrap-dal-node"
        ~rpc_port:port
        cloud
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

type monitor_app_configuration = {
  slack_bot_token : string;
  slack_channel_id : string;
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
  producer_key : string option;
  fundraiser : string option;
  producers_delay : int;
  blocks_history : int;
  bootstrap_node_identity_file : string option;
  bootstrap_dal_node_identity_file : string option;
  external_rpc : bool;
  dal_incentives : bool;
  monitor_app_configuration : monitor_app_configuration option;
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
  baker : Agnostic_baker.t;
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

type dal_status = With_DAL of Z.t | Without_DAL | Out_of_committee

type per_level_info = {
  level : int;
  published_commitments : (int, commitment_info) Hashtbl.t;
  attestations : (public_key_hash, dal_status) Hashtbl.t;
  attested_commitments : Z.t;
  etherlink_operator_balance_sum : Tez.t;
}

type per_baker_dal_summary = {
  published_slots : int;
  attested_slots : int;
  in_committee : bool;
  attestation_with_dal : bool;
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
  ratio_attested_commitments_per_baker :
    (public_key_hash, per_baker_dal_summary) Hashtbl.t;
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
     to [bootstrap.node_rpc_endpoint] which is a public endpoint when the
     '--bootstrap' argument is not provided *)
  bakers : baker list;
  producers : producer list; (* NOTE: they have the observer profile*)
  observers : observer list;
  etherlink : etherlink option;
  time_between_blocks : int;
  parameters : Dal_common.Parameters.t;
  infos : (int, per_level_info) Hashtbl.t;
  metrics : (int, metrics) Hashtbl.t;
  disconnection_state : Disconnect.t option;
  first_level : int;
  teztale : Teztale.t option;
  mutable versions : (string, string) Hashtbl.t;
      (* mapping from baker addresses to their octez versions (if known) *)
  otel : string option;
}

let aliases =
  Hashtbl.create
    50 (* mapping from baker addresses to their Tzkt aliases (if known)*)

let merge_aliases =
  Option.iter (fun new_aliases ->
      Hashtbl.iter
        (fun key alias -> Hashtbl.replace aliases key alias)
        new_aliases)

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
  let pp_ratio fmt (num, div) =
    if div = 0 then Format.fprintf fmt "Not a number: %d/0" num
    else Format.fprintf fmt "%.2f" (float_of_int num *. 100. /. float_of_int div)
  in
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
  t.bakers
  |> List.iter (fun {account; stake; _} ->
         let pkh = account.Account.public_key_hash in
         match Hashtbl.find_opt ratio_attested_commitments_per_baker pkh with
         | None -> Log.info "We lack information about %s" pkh
         | Some {published_slots; attested_slots; _} ->
             let alias =
               Hashtbl.find_opt aliases account.Account.public_key_hash
               |> Option.value ~default:account.Account.public_key_hash
             in
             Log.info
               "Ratio for %s (with stake %d): %a"
               alias
               stake
               pp_ratio
               (attested_slots, published_slots)) ;
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
  let get_labels public_key_hash =
    let alias =
      Hashtbl.find_opt aliases public_key_hash
      |> Option.map (fun alias -> [("alias", alias)])
      |> Option.value ~default:[]
    in
    let version =
      Hashtbl.find_opt t.versions public_key_hash
      |> Option.map (fun version -> [("version", version)])
      |> Option.value ~default:[]
    in
    [("attester", public_key_hash)] @ alias @ version
  in
  let push_attested ~labels value =
    Cloud.push_metric
      t.cloud
      ~help:"Number of attested commitments per baker"
      ~typ:`Gauge
      ~labels
      ~name:"tezt_dal_commitments_attested"
      (float_of_int value)
  in
  let push_published ~labels value =
    Cloud.push_metric
      t.cloud
      ~help:"Number of published commitments per baker"
      ~typ:`Gauge
      ~labels
      ~name:"tezt_dal_commitments_published"
      (float_of_int value)
  in
  let push_dal_attestation_sent ~labels value =
    Cloud.push_metric
      t.cloud
      ~help:
        "Did the baker sent a DAL attestation when they had the opportunity to"
      ~typ:`Gauge
      ~labels
      ~name:"tezt_dal_attestation_sent"
      (if value then 1. else 0.)
  in
  Hashtbl.iter
    (fun public_key_hash
         {attested_slots; published_slots; in_committee; attestation_with_dal} ->
      if in_committee then (
        let labels = get_labels public_key_hash in
        push_attested ~labels attested_slots ;
        push_published ~labels published_slots ;
        push_dal_attestation_sent ~labels attestation_with_dal)
      else ())
    ratio_attested_commitments_per_baker ;
  Hashtbl.iter
    (fun slot_index value ->
      let labels = [("slot_index", string_of_int slot_index)] in
      Cloud.push_metric
        t.cloud
        ~help:"Total published commitments per slot"
        ~typ:`Counter
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
        ~typ:`Counter
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
    ~help:"Number of attested commitments"
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

let update_ratio_attested_commitments_per_baker t per_level_info =
  let default () = Hashtbl.create 0 in
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
    | Some published_level_info ->
        (* Retrieves the number of published commitments *)
        let published_slots =
          Hashtbl.length published_level_info.published_commitments
        in
        let table = Hashtbl.(create (length per_level_info.attestations)) in
        Hashtbl.to_seq per_level_info.attestations
        |> Seq.map (fun (public_key_hash, status) ->
               ( public_key_hash,
                 match status with
                 | With_DAL z ->
                     {
                       published_slots;
                       attested_slots = Z.popcount z;
                       in_committee = true;
                       attestation_with_dal = true;
                     }
                 | Out_of_committee ->
                     {
                       published_slots;
                       attested_slots = 0;
                       in_committee = false;
                       attestation_with_dal = false;
                     }
                 | Without_DAL ->
                     {
                       published_slots;
                       attested_slots = 0;
                       in_committee = true;
                       attestation_with_dal = false;
                     } ))
        |> Hashtbl.add_seq table ;
        table

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
    update_ratio_attested_commitments_per_baker t infos_per_level
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

module Monitoring_app = struct
  let pp_delegate fmt delegate_pkh =
    match Hashtbl.find_opt aliases delegate_pkh with
    | None -> Format.fprintf fmt "`%s`" delegate_pkh
    | Some alias ->
        Format.fprintf fmt "`%s` : `%s`" (String.sub delegate_pkh 0 7) alias

  (** [network_to_image_url network] return an image for each monitored network. *)
  let network_to_image_url : Network.t -> string = function
    | `Rionet ->
        "https://gitlab.com/tezos/tezos/-/raw/master/tezt/lib_cloud/assets/rionet.png"
    | `Mainnet ->
        "https://gitlab.com/tezos/tezos/-/raw/master/tezt/lib_cloud/assets/mainnet.png"
    | `Ghostnet ->
        "https://gitlab.com/tezos/tezos/-/raw/master/tezt/lib_cloud/assets/ghostnet.png"
    | `Sandbox | `Weeklynet _ | `Nextnet _ -> "no_image_yet"

  let endpoint_of_webhook webhook =
    let host =
      (* Default to slack hooks host. *)
      Option.value ~default:"hooks.slack.com" (Uri.host webhook)
    in
    let scheme =
      (* Default to https scheme. *)
      Option.value ~default:"https" (Uri.scheme webhook)
    in
    let port =
      (* Default to https default https port. *)
      match scheme with "https" -> 443 | "http" -> 80 | _ -> 443
    in
    Endpoint.make ~host ~scheme ~port ()

  module Format_app = struct
    (* Helper for Slack App message format block-kit
       See: https://api.slack.com/reference/block-kit/
    *)

    let image ~url ~alt =
      let open Ezjsonm in
      `O
        [
          ("type", string "image");
          ("image_url", string url);
          ("alt_text", string alt);
        ]

    (* [section content ?accessory] creates Slack App message blocks
       with an optional accessory.

       The function joins content strings with newlines, formats them
       using mrkdwn, and returns properly structured JSON objects for
       Slack's Block Kit. *)
    let section content ?accessory () =
      let open Ezjsonm in
      if List.is_empty content then []
      else
        let text = String.concat "\n" content in
        [
          `O
            (("type", string "section")
            :: ("text", `O [("type", string "mrkdwn"); ("text", string text)])
            :: Option.fold
                 ~none:[]
                 ~some:(fun data -> [("accessory", data)])
                 accessory);
        ]

    let make_payload ~slack_channel_id ?ts content =
      let open Ezjsonm in
      `O
        (("channel", string slack_channel_id)
        :: ("blocks", `A content)
        :: Option.fold ~none:[] ~some:(fun ts -> [("thread_ts", string ts)]) ts
        )
  end

  let post_message ?ts ~slack_channel_id ~slack_bot_token data =
    let data = Format_app.make_payload ?ts ~slack_channel_id data in
    let slack_endpoint =
      Endpoint.make ~scheme:"https" ~host:"slack.com" ~port:443 ()
    in
    let rpc =
      RPC_core.make
        ~data:(Data data)
        POST
        ["api"; "chat.postMessage"]
        (fun _json -> ())
    in
    let* response =
      RPC_core.call_raw
        ~extra_headers:
          [("Authorization", Format.sprintf "Bearer %s" slack_bot_token)]
        slack_endpoint
        rpc
    in
    let thread_id =
      let open JSON in
      parse ~origin:"DAL.Monitoring_app.chat_postMessage" response.body
      |-> "ts" |> as_string
    in
    return thread_id

  module Tasks = struct
    let endpoint_from_prometheus_query ~query =
      let fail ~uri_part =
        Test.fail
          "DAL.Monitoring_app.Tasks.endpoint_from_prometheus_query: expecting \
           a prometheus %s"
          uri_part
      in
      let uri =
        match Prometheus.get_query_endpoint ~query with
        | None -> fail ~uri_part:"endpoint"
        | Some endpoint -> endpoint
      in
      let scheme, host, port =
        match Uri.(scheme uri, host uri, port uri) with
        | Some scheme, Some host, Some port -> (scheme, host, port)
        | None, _, _ -> fail ~uri_part:"scheme"
        | _, None, _ -> fail ~uri_part:"host"
        | _, _, None -> fail ~uri_part:"port"
      in
      let query_string =
        (* Fixme: warn about `k` being dropped in the second case. We
           need to keep only list of size 1 because of the way RPC_core
           is implemented. Should probably be fixed soon or later. *)
        List.filter_map
          (function k, [v] -> Some (k, v) | _k, _ -> None)
          (Uri.query uri)
      in
      let path = String.split_on_char '/' (Uri.path uri) in
      let endpoint = Endpoint.make ~host ~scheme ~port () in
      (`endpoint endpoint, `query query_string, `path path)

    let fetch ~origin ~query ~decoder =
      let open RPC_core in
      let `endpoint endpoint, `query query_string, `path path =
        endpoint_from_prometheus_query ~query
      in
      let rpc = make ~query_string GET path decoder in
      Lwt.catch
        (fun () ->
          let* response = call_raw endpoint rpc in
          Lwt.return (decode_raw ~origin rpc response.body))
        (fun exn ->
          Log.warn
            "Unexpected error while fetching prometheus query (origin : %s): \
             '%s'"
            origin
            (Printexc.to_string exn) ;
          Lwt.return_none)

    let decoder_prometheus_float json =
      let open JSON in
      let status = json |-> "status" |> as_string in
      if not (String.equal status "success") then (* fixme: warning *)
        None
      else
        let opt =
          json |-> "data" |-> "result" |> as_list |> Fun.flip List.nth_opt 0
        in
        match opt with
        | None -> None
        | Some x ->
            x |-> "value" |> as_list |> Fun.flip List.nth_opt 1
            |> Option.map as_float

    let view_ratio_attested_over_published
        (`attested attested, `published published) =
      let open Format in
      match (attested, published) with
      | Some 0., Some 0. -> None
      | None, None -> None
      | Some attested, Some 0. ->
          let s = sprintf "`unk` (`%d`/`0`)" (Int.of_float attested) in
          Some s
      | Some attested, None ->
          let s = sprintf "`unk` (`%d`/`?`)" (Int.of_float attested) in
          Some s
      | None, Some published ->
          let s = sprintf "`unk` (`?`/`%d`)" (Int.of_float published) in
          Some s
      | Some attested, Some published ->
          let ratio = attested /. published *. 100. in
          let s =
            sprintf
              "`%s` (`%d/%d`)"
              (sprintf "%.2f%%" ratio)
              (Int.of_float attested)
              (Int.of_float published)
          in
          Some s

    let fetch_slot_info ~slot_index =
      let query s =
        Format.sprintf
          "increase(tezt_total_%s_commitments_per_slot{slot_index=\"%d\"}[6h])"
          s
          slot_index
      in
      let decoder = decoder_prometheus_float in
      let* attested =
        fetch
          ~origin:"fetch_slot_info.attested"
          ~decoder
          ~query:(query "attested")
      in
      let* published =
        fetch
          ~origin:"fetch_slot_info.published"
          ~decoder
          ~query:(query "published")
      in
      Lwt.return
        (`slot_index slot_index, `attested attested, `published published)

    let view_slot_info slot_info =
      let slots_info =
        List.filter_map
          (fun (`slot_index slot_index, attested, published) ->
            view_ratio_attested_over_published (attested, published)
            |> Option.map
                 (Format.sprintf ":black_small_square: `%02d` : %s" slot_index))
          slot_info
      in
      if List.is_empty slots_info then []
      else
        "• Percentage of attested over published DAL commitments per slot:"
        :: slots_info

    let fetch_dal_commitments_total_info () =
      let query s =
        Format.sprintf {|increase(tezt_dal_commitments_total{kind="%s"}[6h])|} s
      in
      let decoder = decoder_prometheus_float in
      let* attested =
        fetch
          ~origin:"fetch_dal_commitments_total.attested"
          ~decoder
          ~query:(query "attested")
      in
      let* published =
        fetch
          ~origin:"fetch_dal_commitments_total.published"
          ~decoder
          ~query:(query "published")
      in
      let ratio =
        view_ratio_attested_over_published
          (`attested attested, `published published)
      in
      let view =
        (Format.sprintf
           "• Percentage of attested over published DAL commitments: %s")
          (Option.value ~default:"unk" ratio)
      in
      Lwt.return view

    let pp_stake fmt stake_ratio =
      Format.fprintf fmt "`%.2f%%` stake" (stake_ratio *. 100.)

    let display_delegate (`address address, _, stake_ratio) =
      Format.asprintf "%a (%a)" pp_delegate address pp_stake stake_ratio

    let view_bakers bakers =
      List.map
        (fun ((_, (value, mentions_dal), _) as baker) ->
          Format.sprintf
            ":black_small_square: %s - %s (%s)"
            (Option.fold
               ~none:"Never was in committee when slots were produced"
               ~some:(fun value -> Format.sprintf "`%.2f%%`" (value *. 100.))
               value)
            (display_delegate baker)
            (match mentions_dal with
            | None -> "Never sent attestations while in DAL committee"
            | Some 0. -> "OFF"
            | Some 1. -> "ON"
            | Some x -> Format.sprintf "ACTIVE %.0f%% of the time" (x *. 100.)))
        bakers

    let fetch_baker_info ~tz1 ~origin =
      let query =
        Format.sprintf
          "sum_over_time(tezt_dal_commitments_attested{attester=\"%s\"}[6h])"
          tz1
      in
      let* attested = fetch ~decoder:decoder_prometheus_float ~query ~origin in
      let query =
        Format.sprintf
          "sum_over_time(tezt_dal_commitments_published{attester=\"%s\"}[6h])"
          tz1
      in
      let* published = fetch ~decoder:decoder_prometheus_float ~query ~origin in
      let query =
        Format.sprintf
          "avg_over_time(tezt_dal_attestation_sent{attester=\"%s\"}[6h])"
          tz1
      in
      let* dal_attestation_ratio =
        fetch ~decoder:decoder_prometheus_float ~query ~origin
      in
      match (attested, published) with
      | None, _ | _, None | _, Some 0. -> return (None, dal_attestation_ratio)
      | Some attested, Some published ->
          return (Some (attested /. published), dal_attestation_ratio)

    (* fixme: use the stdlib List.take once we use OCaml 5.3 *)
    let take n l =
      let rec loop acc = function
        | 0, _ | _, [] -> List.rev acc
        | n, x :: xs -> loop (x :: acc) (pred n, xs)
      in
      loop [] (n, l)

    let get_current_cycle endpoint =
      let* {cycle; _} =
        RPC_core.call endpoint (RPC.get_chain_block_helper_current_level ())
      in
      Lwt.return cycle

    let get_bakers_with_staking_power endpoint cycle =
      RPC_core.call endpoint (RPC.get_stake_distribution ~cycle ())

    let fetch_bakers_info endpoint =
      let* cycle = get_current_cycle endpoint in
      let* bakers = get_bakers_with_staking_power endpoint cycle in
      let total_baking_power =
        List.fold_left
          (fun acc RPC.{baking_power; _} -> acc + baking_power)
          0
          bakers
      in
      let* bakers_info =
        Lwt_list.filter_map_p
          (fun RPC.{delegate; baking_power} ->
            let* info =
              fetch_baker_info
                ~origin:(Format.sprintf "fetch_baker_info.%s" delegate)
                ~tz1:delegate
            in
            let baking_ratio =
              float_of_int baking_power /. float_of_int total_baking_power
            in
            Lwt.return_some (`address delegate, info, baking_ratio))
          bakers
      in
      let rec classify_bakers mute_bakers dal_on dal_off = function
        | [] -> (mute_bakers, dal_on, dal_off)
        | ((_, (_, dal_endorsement), _) as baker) :: tl -> (
            match dal_endorsement with
            | None -> classify_bakers (baker :: mute_bakers) dal_on dal_off tl
            | Some 0. ->
                classify_bakers mute_bakers dal_on (baker :: dal_off) tl
            | _ -> classify_bakers mute_bakers (baker :: dal_on) dal_off tl)
      in
      let mute_bakers, dal_on, dal_off = classify_bakers [] [] [] bakers_info in
      let ( >> ) cmp1 cmp2 x y =
        match cmp1 x y with 0 -> cmp2 x y | cmp -> cmp
      in
      let stake_descending (_, (_, _), x_stake) (_, (_, _), y_stake) =
        Float.compare y_stake x_stake
      in
      let attestation_rate_ascending (_, (x_attestation, _), _)
          (_, (y_attestation, _), _) =
        Option.compare Float.compare x_attestation y_attestation
      in
      let dal_mention_perf_ascending (_, (_, x_dal_mention), _)
          (_, (_, y_dal_mention), _) =
        Option.compare Float.compare x_dal_mention y_dal_mention
      in
      let mute_bakers = List.sort stake_descending mute_bakers in
      let dal_on =
        List.sort
          (attestation_rate_ascending >> dal_mention_perf_ascending
         >> stake_descending)
          dal_on
      in
      let dal_off = List.sort stake_descending dal_off in
      (* `group_by n l` outputs the list of lists with the same elements as `l`
         but with `n` elements per list (except the last one).
         For instance
         `group_by 4 [a_1, ..., a_10] = [[a_1, ..., a_4], [a_5, ..., a_8],  [a_9, a_10]]`
      *)
      let group_by n =
        let rec bis local_acc main_acc k = function
          | [] -> List.rev (List.rev local_acc :: main_acc)
          | l when k = 0 -> bis [] (List.rev local_acc :: main_acc) n l
          | hd :: tl -> bis (hd :: local_acc) main_acc (k - 1) tl
        in
        bis [] [] n
      in
      let agglomerate_infos bakers =
        let nb, stake =
          List.fold_left
            (fun (nb, stake_acc) (_, _, stake_ratio) ->
              (nb + 1, stake_acc +. stake_ratio))
            (0, 0.)
            bakers
        in
        Format.sprintf
          "They are %d representing %.2f%% of the stake"
          nb
          (stake *. 100.)
      in
      let display_muted =
        match mute_bakers with
        | [] -> []
        | _ ->
            group_by
              20
              (":alert: *Those bakers never sent attestations while in DAL \
                committee, this is quite unexpected. They probably have an \
                issue.*"
              :: agglomerate_infos mute_bakers
              :: List.map display_delegate mute_bakers)
      in
      let display_on =
        group_by
          20
          (":white_check_mark: *Those bakers sent attestations mentioning DAL.*"
         :: agglomerate_infos dal_on :: view_bakers dal_on)
      in
      let display_off =
        group_by
          20
          (":x: *Those bakers never turned their DAL on.*"
         :: agglomerate_infos dal_off
          :: List.map display_delegate dal_off)
      in
      Lwt.return
        ((["Details of bakers performance:"] :: display_muted)
        @ display_on @ display_off)

    let fetch_slots_info () =
      let* data =
        (* fixme: should use protocol parameterized number of slots *)
        Lwt_list.map_p
          (fun slot_index -> fetch_slot_info ~slot_index)
          (List.init 32 Fun.id)
      in
      Lwt.return (view_slot_info data)

    let action ~slack_bot_token ~slack_channel_id ~configuration endpoint () =
      let* endpoint in
      let network = configuration.network in
      let title_info =
        Format.sprintf
          "*DAL report* for the *%s* network over the last 6 hours."
          (String.capitalize_ascii (Network.to_string network))
      in
      let network_info =
        Format.sprintf
          "• Network: %s"
          (String.capitalize_ascii (Network.to_string network))
      in
      let* ratio_dal_commitments_total_info =
        fetch_dal_commitments_total_info ()
      in
      let* slots_info = fetch_slots_info () in
      let network_overview_info =
        network_info :: ratio_dal_commitments_total_info :: slots_info
      in
      let data =
        let open Format_app in
        section [title_info] ()
        @ section ["*Network overview*"] ()
        @ section
            network_overview_info
            ~accessory:
              (Format_app.image
                 ~url:(network_to_image_url network)
                 ~alt:(Network.to_string network))
            ()
      in
      let* thread_id = post_message ~slack_channel_id ~slack_bot_token data in
      let* bakers_info = fetch_bakers_info endpoint in
      Lwt_list.iter_s
        (fun to_post ->
          let data =
            let open Format_app in
            section to_post ()
          in
          let* _ts =
            post_message ~ts:thread_id ~slack_channel_id ~slack_bot_token data
          in
          Lwt.return_unit)
        bakers_info

    (* Relies on UTC (Universal Time Coordinated).
       Paris operates on Central European Time (CET), which is UTC+1
       during standard time (winter months). During daylight saving
       time (summer months), Paris switches to Central European Summer
       Time (CEST), which is UTC+2.
    *)
    let tasks ~configuration endpoint =
      match configuration.monitor_app_configuration with
      | None -> []
      | Some {slack_bot_token; slack_channel_id; _} ->
          let action () =
            action ~slack_bot_token ~slack_channel_id ~configuration endpoint ()
          in
          [
            Chronos.task ~name:"network-overview" ~tm:"0 0-23/6 * * *" ~action ();
          ]
  end

  module Alert = struct
    let report_lost_dal_rewards ~slack_channel_id ~slack_bot_token ~network
        ~level ~cycle ~lost_dal_rewards =
      let data =
        let header =
          Format.sprintf
            "*[lost-dal-rewards]* On network `%s`, delegates have lost DAL \
             rewards at cycle `%d`, level `%d`. \
             <https://%s.tzkt.io/%d/implicit_operations/dal_attestation_reward \
             |See online>"
            (Network.to_string network)
            cycle
            level
            (Network.to_string network)
            level
        in
        let content =
          List.map
            (fun (`delegate delegate, `change change) ->
              Format.asprintf
                ":black_small_square: %a has missed ~%.1f tez DAL attestation \
                 rewards"
                pp_delegate
                delegate
                (float_of_int change /. 1_000_000.))
            lost_dal_rewards
        in
        Format_app.section (header :: content) ()
      in
      let* _ts = post_message ~slack_channel_id ~slack_bot_token data in
      Lwt.return_unit

    let check_for_lost_dal_rewards t ~metadata =
      match t.configuration.monitor_app_configuration with
      | None -> unit
      | Some {slack_channel_id; slack_bot_token; _} ->
          let cycle = JSON.(metadata |-> "level_info" |-> "cycle" |> as_int) in
          let level = JSON.(metadata |-> "level_info" |-> "level" |> as_int) in
          let balance_updates =
            JSON.(metadata |-> "balance_updates" |> as_list)
          in
          let lost_dal_rewards =
            List.filter_map
              (fun balance_update ->
                let category =
                  JSON.(balance_update |-> "category" |> as_string_opt)
                in
                match category with
                | None -> None
                | Some category ->
                    if String.equal category "lost DAL attesting rewards" then
                      let delegate =
                        JSON.(balance_update |-> "delegate" |> as_string)
                      in
                      let change =
                        JSON.(balance_update |-> "change" |> as_int)
                      in
                      Some (`delegate delegate, `change change)
                    else None)
              balance_updates
          in
          if List.is_empty lost_dal_rewards then unit
          else
            let network = t.configuration.network in
            report_lost_dal_rewards
              ~slack_channel_id
              ~slack_bot_token
              ~network
              ~level
              ~cycle
              ~lost_dal_rewards

    let check_for_lost_dal_rewards t ~metadata =
      Lwt.catch
        (fun () -> check_for_lost_dal_rewards t ~metadata)
        (fun exn ->
          Log.warn
            "Monitor_app.Alert.check_for_lost_dal_rewards: unexpected error: \
             '%s'"
            (Printexc.to_string exn) ;
          unit)

    let report_dal_accusations ~slack_channel_id ~slack_bot_token ~network
        ~level ~cycle dal_accusations =
      let data =
        let header =
          Format.sprintf
            "*[dal-accusations]* On network `%s`, delegates have been accused \
             at cycle `%d`, level `%d`."
            (Network.to_string network)
            cycle
            level
        in
        let content =
          List.map
            (fun ( `attestation_level attestation_level,
                   `slot_index slot_index,
                   `delegate delegate,
                   `op_hash hash ) ->
              Format.asprintf
                ":black_small_square: %a for attesting slot index `%d` at \
                 level `%d`. <https://%s.tzkt.io/%s|See online>"
                pp_delegate
                delegate
                slot_index
                attestation_level
                (Network.to_string network)
                hash)
            dal_accusations
        in
        Format_app.section (header :: content) ()
      in
      let* _ts = post_message ~slack_channel_id ~slack_bot_token data in
      Lwt.return_unit

    let check_for_dal_accusations t ~cycle ~level ~operations ~endpoint =
      match t.configuration.monitor_app_configuration with
      | None -> unit
      | Some {slack_channel_id; slack_bot_token; _} ->
          let open JSON in
          let accusations =
            operations |> as_list |> Fun.flip List.nth 2 |> as_list
          in
          let dal_entrapments =
            List.filter_map
              (fun accusation ->
                let contents =
                  accusation |-> "contents" |> as_list |> Fun.flip List.nth 0
                in
                match contents |-> "kind" |> as_string_opt with
                | Some "dal_entrapment_evidence" ->
                    let attestation_level =
                      contents |-> "attestation" |-> "operations" |-> "level"
                      |> as_int
                    in
                    let slot_index = contents |-> "slot_index" |> as_int in
                    let shard =
                      contents |-> "shard_with_proof" |-> "shard" |> as_list
                      |> Fun.flip List.nth 0 |> as_int
                    in
                    let hash = accusation |-> "hash" |> as_string in
                    Some
                      ( `attestation_level attestation_level,
                        `shard shard,
                        `slot_index slot_index,
                        `op_hash hash )
                | None | Some _ -> None)
              accusations
          in
          (* todo: optimize to avoid too many RPC calls (?) *)
          let* dal_accusations =
            Lwt_list.map_p
              (fun ( `attestation_level attestation_level,
                     `shard shard,
                     `slot_index slot_index,
                     `op_hash hash ) ->
                let* json =
                  RPC_core.call
                    endpoint
                    (RPC.get_chain_block_context_dal_shards
                       ~level:attestation_level
                       ())
                in
                let assignment =
                  let open JSON in
                  List.find_opt
                    (fun assignment ->
                      let indexes =
                        assignment |-> "indexes" |> as_list |> List.map as_int
                      in
                      List.mem shard indexes)
                    (json |> as_list)
                in
                let delegate =
                  Option.fold
                    ~none:"should_not_happen"
                    ~some:(fun x -> x |-> "delegate" |> as_string)
                    assignment
                in
                return
                  ( `attestation_level attestation_level,
                    `slot_index slot_index,
                    `delegate delegate,
                    `op_hash hash ))
              dal_entrapments
          in
          if List.is_empty dal_accusations then unit
          else
            let network = t.configuration.network in
            report_dal_accusations
              ~slack_channel_id
              ~slack_bot_token
              ~network
              ~cycle
              ~level
              dal_accusations

    let check_for_dal_accusations t ~cycle ~level ~operations ~endpoint =
      Lwt.catch
        (fun () ->
          check_for_dal_accusations t ~cycle ~level ~operations ~endpoint)
        (fun exn ->
          Log.warn
            "Monitor_app.Alert.check_for_dal_accusations: unexpected error: \
             '%s'"
            (Printexc.to_string exn) ;
          unit)

    type attestation_transition = Stopped_attesting | Restarted_attesting

    let report_attestation_transition ~monitor_app_configuration ~network ~level
        ~pkh ~transition ~attestation_percentage =
      let {slack_channel_id; slack_bot_token} = monitor_app_configuration in
      let data =
        let header =
          Format.asprintf
            (match transition with
            | Restarted_attesting ->
                "*[dal-attester-is-back]* On network `%s` at level `%d`, \
                 delegate %a who was under the reward threshold is again above \
                 threshold. New attestation rate is %.2f%%."
            | Stopped_attesting ->
                "*[dal-attester-dropped]* On network `%s` at level `%d`, \
                 delegate %a DAL attestation rate dropped under the reward \
                 threshold. New attestation rate is %.2f%%.")
            (Network.to_string network)
            level
            pp_delegate
            pkh
            attestation_percentage
        in
        Format_app.section [header] ()
      in
      let* _ts = post_message ~slack_channel_id ~slack_bot_token data in
      Lwt.return_unit

    let take_and_drop n l =
      let rec bis acc n = function
        | [] -> (List.rev acc, [])
        | hd :: tl as l ->
            if n = 0 then (List.rev acc, l) else bis (hd :: acc) (n - 1) tl
      in
      bis [] n l

    let check_for_recently_missed_a_lot =
      (* TODO: This correspond to the number of bakers which activated DAL on mainnet.
         We expect more bakers to run the DAL, this initial size might be then increased
         to avoid rescaling of the hash table.
      *)
      let prev_was_enough = Hashtbl.create 50 in
      let to_treat_delegates = ref [] in
      fun t ~level ~metadata ->
        match t.configuration.monitor_app_configuration with
        | None -> unit
        | Some monitor_app_configuration -> (
            let cycle_position =
              JSON.(metadata |-> "level_info" |-> "cycle_position" |> as_int)
            in
            (* We do not run this function during the first 20 levels of a cycle,
               since the attestation rate are not relevant when so few levels happened.
               Especially since 0 attestation out of 0 attestable slots is considered
               as 100% attestation rate. *)
            if cycle_position < 20 then unit
            else
              let endpoint = t.some_node_rpc_endpoint in
              let treat_delegate pkh =
                let* infos =
                  RPC_core.call endpoint
                  @@ RPC.get_chain_block_context_delegate pkh
                in
                let dal_participation = JSON.(infos |-> "dal_participation") in
                let attest_enough =
                  JSON.(
                    dal_participation |-> "sufficient_dal_participation"
                    |> as_bool)
                in
                match Hashtbl.find_opt prev_was_enough pkh with
                | None ->
                    let () = Hashtbl.add prev_was_enough pkh attest_enough in
                    unit
                | Some prev_status ->
                    if prev_status = attest_enough then unit
                    else
                      let attestable =
                        JSON.(
                          dal_participation |-> "delegate_attestable_dal_slots"
                          |> as_float)
                      in
                      (* If no slots was attestatble, the value of [attest_enough]
                         is [true], but it is quite meaningless, since we do not want
                          to state that the DAL node is working well again simply
                          because there was nothing to attest. *)
                      if attestable = 0. then unit
                      else (
                        Hashtbl.add prev_was_enough pkh attest_enough ;
                        let network = t.configuration.network in
                        let attested =
                          JSON.(
                            dal_participation |-> "delegate_attested_dal_slots"
                            |> as_float)
                        in
                        let attestation_percentage =
                          100. *. attested /. attestable
                        in
                        let transition =
                          if attest_enough then Restarted_attesting
                          else Stopped_attesting
                        in
                        report_attestation_transition
                          ~monitor_app_configuration
                          ~network
                          ~level
                          ~pkh
                          ~transition
                          ~attestation_percentage)
              in
              let refill_delegates_to_treat () =
                let query_string = [("active", "true")] in
                let* delegates =
                  RPC_core.call endpoint
                  @@ RPC.get_chain_block_context_delegates ~query_string ()
                in
                to_treat_delegates := delegates ;
                unit
              in
              (* To avoid spawning a high number of RPCs simultaneously, we treat 2 delegates at each level. *)
              let to_treat_now, treat_later =
                take_and_drop 2 !to_treat_delegates
              in
              let* () = Lwt_list.iter_p treat_delegate to_treat_now in
              match treat_later with
              | [] -> refill_delegates_to_treat ()
              | remaining_delegates ->
                  to_treat_delegates := remaining_delegates ;
                  unit)

    let check_for_recently_missed_a_lot t ~level ~metadata =
      Lwt.catch
        (fun () -> check_for_recently_missed_a_lot t ~level ~metadata)
        (fun exn ->
          Log.warn
            "Monitor_app.Alert.check_for_recently_missed_a_lot: unexpected \
             error: '%s'"
            (Printexc.to_string exn) ;
          unit)
  end
end

let get_infos_per_level t ~level ~metadata =
  let client = t.bootstrap.client in
  let endpoint = t.some_node_rpc_endpoint in
  let etherlink_operators =
    match t.etherlink with
    | None -> []
    | Some setup -> setup.operator.account :: setup.operator.batching_operators
  in
  let block = string_of_int level in
  let* operations =
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
    let first_slot =
      JSON.(operation |-> "contents" |=> 0 |-> "slot" |> as_int)
    in
    if first_slot >= 512 then Out_of_committee
    else if is_dal_attestation operation then
      With_DAL
        JSON.(
          operation |-> "contents" |=> 0 |-> "dal_attestation" |> as_string
          |> Z.of_string)
    else Without_DAL
  in
  let cycle = JSON.(metadata |-> "level_info" |-> "cycle" |> as_int) in
  let attestations =
    consensus_operations |> List.to_seq
    |> Seq.map (fun operation ->
           let public_key_hash = get_public_key_hash operation in
           let dal_attestation = get_dal_attestation operation in
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
  (* None of these actions are performed if `--dal-slack-webhook` is
     not provided. *)
  let* () =
    let open Monitoring_app.Alert in
    let* () =
      check_for_dal_accusations
        t
        ~cycle
        ~level
        ~operations
        ~endpoint:t.some_node_rpc_endpoint
    in
    let* () = check_for_recently_missed_a_lot t ~level ~metadata in
    unit
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
            ~rpc_external:configuration.external_rpc
            ~name:"bootstrap-node"
            configuration.network
            cloud
            agent
        in
        let* dal_node =
          if not configuration.with_dal then Lwt.return_none
          else
            let* dal_node =
              Dal_node.Agent.create ~name:"bootstrap-dal-node" cloud agent ~node
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
  let* baker_accounts =
    Client.stresstest_gen_keys
      ~alias_prefix:"baker"
      (List.length configuration.stake)
      bootstrap.client
  in
  let () = toplog "Initializing the producers" in
  let* producer_accounts =
    match configuration.producer_key with
    | None ->
        Client.stresstest_gen_keys
          ~alias_prefix:"dal_producer"
          (List.length configuration.dal_node_producers)
          bootstrap.client
    | Some producer_key -> (
        match configuration.dal_node_producers with
        | [_] ->
            let* () =
              Client.import_secret_key
                bootstrap.client
                (Unencrypted producer_key)
                ~alias:"producer_key"
            in
            let* account =
              Client.show_address ~alias:"producer_key" bootstrap.client
            in
            return [account]
        | _ ->
            Test.fail
              "A producer key can only be used if there is exactly one slot on \
               which data are produced.")
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
      ~rpc_external:configuration.external_rpc
      ~dal_config
      ~name
      configuration.network
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
      if configuration.dal_incentives then
        [
          (["dal_parametric"; "incentives_enable"], `Bool true);
          (["dal_parametric"; "rewards_ratio"; "numerator"], `String "1");
          (["dal_parametric"; "rewards_ratio"; "denominator"], `String "10");
          (* This one is derived from the two constants above. *)
          (["issuance_weights"; "dal_rewards_weight"], `Int 5120);
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
      ~rpc_external:configuration.external_rpc
      configuration.network
      cloud
      agent
  in
  let* dal_node =
    if not configuration.with_dal then Lwt.return_none
    else
      let* dal_node =
        Dal_node.Agent.create
          ~name:(Format.asprintf "baker-dal-node-%d" i)
          ~node
          cloud
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
            cloud
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
    let dal_node_rpc_endpoint = Option.map Dal_node.as_rpc_endpoint dal_node in
    Agnostic_baker.Agent.init
      ~name:(Format.asprintf "baker-%d" i)
      ~delegates:[account.Account.alias]
      ~client
      ?dal_node_rpc_endpoint
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
      ~rpc_external:configuration.external_rpc
      configuration.network
      cloud
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
  let*? process = Client.reveal client ~endpoint ~src:account.Account.alias in
  let* _ = Process.wait process in
  let () = toplog "Init producer: create agent" in
  let* dal_node =
    Dal_node.Agent.create
      ~name:(Format.asprintf "producer-dal-node-%i" i)
      ~node
      cloud
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
          cloud
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
      ~rpc_external:configuration.external_rpc
      configuration.network
      cloud
      agent
  in
  let* dal_node =
    Dal_node.Agent.create
      ~name:(Format.asprintf "observer-dal-node-%i" i)
      ~node
      cloud
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
          cloud
          agent
          ~node_name:(Node.name node)
          ~node_port:(Node.rpc_port node)
  in
  Lwt.return {node; dal_node; topic}

let init_etherlink_dal_node ~bootstrap ~next_agent ~dal_slots ~network ~otel
    ~memtrace ~rpc_external ~cloud =
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
      let* node =
        Node.init
          ~name
          ~arguments:
            [Peer bootstrap.node_p2p_endpoint; History_mode (Rolling (Some 79))]
          ~rpc_external
          network
          cloud
          agent
      in
      let* dal_node = Dal_node.Agent.create ~name ~node cloud agent in
      let* () =
        Dal_node.init_config
          ~expected_pow:(Network.expected_pow network)
          ~operator_profiles:dal_slots
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
      let name = name_of Etherlink_dal_operator in
      let* agent = next_agent ~name in
      let* node =
        Node.init
          ~name
          ~arguments:
            [Peer bootstrap.node_p2p_endpoint; History_mode (Rolling (Some 79))]
          ~rpc_external
          network
          cloud
          agent
      in
      let* default_dal_node = Dal_node.Agent.create ~name ~node cloud agent in
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
               let name = name_of (Etherlink_dal_observer {slot_index}) in
               let* agent = next_agent ~name in
               let* node =
                 Node.init
                   ~name
                   ~arguments:
                     [
                       Peer bootstrap.node_p2p_endpoint;
                       History_mode (Rolling (Some 79));
                     ]
                   ~rpc_external
                   network
                   cloud
                   agent
               in
               let* dal_node = Dal_node.Agent.create ~name ~node cloud agent in
               let* () =
                 Dal_node.init_config
                   ~expected_pow:(Network.expected_pow network)
                   ~operator_profiles:[slot_index]
                   ~peers:(Option.to_list bootstrap.dal_node_p2p_endpoint)
                   dal_node
               in
               let* () = Dal_node.Agent.run ?otel ~memtrace dal_node in
               return (slot_index, Dal_node.rpc_endpoint dal_node))
      in
      let* reverse_proxy_dal_node =
        Dal_reverse_proxy.init_reverse_proxy
          cloud
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
      ~rpc_external:configuration.external_rpc
      configuration.network
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
  let output_config = Temp.file "config.yaml" in
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
      ~rpc_external:configuration.external_rpc
      ~otel
      ~memtrace:configuration.memtrace
      ~cloud
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
      cloud
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
      ~parameters_ty:Tezt_etherlink.Test_helpers.evm_type
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
        max_blueprints_lag = Some 300;
        max_blueprints_ahead = Some 2000;
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
        |> Evm_node.patch_config_with_experimental_feature
             ~enable_websocket:true
             ~drop_duplicate_when_injection:true
             ~blueprints_publisher_order_enabled:true
             ())
      ~name:(Format.asprintf "etherlink-%s-evm-node" name)
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

let init_etherlink_producer_setup operator name ~bootstrap ~rpc_external cloud
    agent =
  let* node =
    Node.Agent.init
      ~rpc_external
      ~name:(Format.asprintf "etherlink-%s-node" name)
      ~arguments:[Peer bootstrap.node_p2p_endpoint; Synchronisation_threshold 0]
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
      ~name:(Format.asprintf "etherlink-%s-rollup-node" name)
      ~base_dir:(Client.base_dir client)
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
  return ()

let init_etherlink cloud configuration etherlink_configuration ~bootstrap
    etherlink_rollup_operator_key batching_operators ~dal_slots next_agent =
  let () = toplog "Initializing an Etherlink operator" in
  let name = name_of Etherlink_operator in
  let* operator_agent = next_agent ~name in
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
             ~bootstrap
             ~rpc_external:configuration.external_rpc
             cloud
             agent)
    |> Lwt.join
  in
  return {configuration = etherlink_configuration; operator; accounts}

let obtain_some_node_rpc_endpoint agent network (bootstrap : bootstrap)
    (bakers : baker list) (producers : producer list)
    (observers : observer list) etherlink =
  match (agent, network) with
  | None, #Network.public -> (
      match (bakers, producers, observers, etherlink) with
      | baker :: _, _, _, _ -> Node.as_rpc_endpoint baker.node
      | [], producer :: _, _, _ -> Node.as_rpc_endpoint producer.node
      | [], [], observer :: _, _ -> Node.as_rpc_endpoint observer.node
      | [], [], [], Some etherlink ->
          Node.as_rpc_endpoint etherlink.operator.node
      | [], [], [], None -> bootstrap.node_rpc_endpoint)
  | _ -> bootstrap.node_rpc_endpoint

let init ~(configuration : configuration) etherlink_configuration cloud
    next_agent =
  let () = toplog "Init" in
  let* bootstrap_agent =
    if configuration.bootstrap then
      let name = name_of Bootstrap in
      let* agent = next_agent ~name in
      Lwt.return_some agent
    else Lwt.return_none
  in
  let* attesters_agents =
    configuration.stake
    |> List.mapi (fun i _stake ->
           let name = name_of (Baker i) in
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
           let name = name_of (Producer slot_index) in
           let* agent = next_agent ~name in
           return (agent, slot_index))
    |> Lwt.all
  in
  let* observers_slot_index_agents =
    configuration.observer_slot_indices
    |> List.map (fun slot_index ->
           let name = name_of (Observer (`Index slot_index)) in
           let* agent = next_agent ~name in
           return (`Slot_index slot_index, agent))
    |> Lwt.all
  in
  let* observers_bakers_agents =
    configuration.observer_pkhs
    |> List.map (fun pkh ->
           let name = name_of (Observer (`Pkh pkh)) in
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
    | `Sandbox -> Lwt.return 1
    | _ -> Network.get_level some_node_rpc_endpoint
  in
  Hashtbl.replace metrics first_level default_metrics ;
  let disconnection_state =
    Option.map Disconnect.init configuration.disconnect
  in
  let* init_aliases =
    let accounts = List.map (fun ({account; _} : baker) -> account) bakers in
    Network.aliases ~accounts configuration.network
  in
  let* versions = Network.versions configuration.network in
  merge_aliases init_aliases ;
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
  let* new_aliases =
    let accounts = List.map (fun ({account; _} : baker) -> account) t.bakers in
    Network.aliases ~accounts t.configuration.network
  in
  let* versions = Network.versions t.configuration.network in
  merge_aliases new_aliases ;
  let versions = Option.value ~default:t.versions versions in
  t.versions <- versions ;
  Lwt.return_unit

let on_new_level t level ~metadata =
  toplog "Start process level %d" level ;
  clean_up t (level - t.configuration.blocks_history) ;
  let* () =
    if level mod 1_000 = 0 then update_bakers_infos t else Lwt.return_unit
  in
  let* infos_per_level = get_infos_per_level t ~level ~metadata in
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
              Dal_node.Agent.terminate dal_node)
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

let on_new_cycle t ~level =
  let endpoint = t.some_node_rpc_endpoint in
  let last_block_of_prev_cycle = string_of_int (level - 1) in
  let* metadata =
    RPC_core.call endpoint
    @@ RPC.get_chain_block_metadata_raw ~block:last_block_of_prev_cycle ()
  in
  (* This action is performed only if `--dal-slack-webhook` is provided. *)
  Monitoring_app.Alert.check_for_lost_dal_rewards t ~metadata

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

let ensure_enough_funds t i =
  let producer = List.nth t.producers i in
  match (t.configuration.network, t.configuration.producer_key) with
  | `Sandbox, _ -> (* Producer has enough money *) Lwt.return_unit
  | _, Some _ ->
      (* Producer key is assumed to have enough money. We simply check that it is the case,
         but do not refund it. *)
      let* balance =
        RPC_core.call t.some_node_rpc_endpoint
        @@ RPC.get_chain_block_context_contract_balance
             ~id:producer.account.public_key_hash
             ()
      in
      if balance < Tez.of_mutez_int 520 then
        Lwt.fail_with
          "Producer key has not enough money anymore to publish slots"
      else Lwt.return_unit
  | _ ->
      let* balance =
        RPC_core.call t.some_node_rpc_endpoint
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
  if level mod t.configuration.producers_delay = 0 then (
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
      (* A dry-run of the "publish dal commitment" command outputs fees of 516µtz and
         1333 gas consumed. We added a (quite small) margin to it. *)
      Helpers.publish_and_store_slot
        ~fee:520
        ~gas_limit:1400
        ~dont_wait:true
        producer.client
        producer.dal_node
        producer.account
        ~force:true
        ~index
        content
    in
    Log.info "publish slot" ;
    Lwt.return_unit)
  else Lwt.return_unit

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
  let p = on_new_block t ~level in
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

let register (module Cli : Scenarios_cli.Dal) =
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
    let producer_key = Cli.producer_key in
    let producers_delay = Cli.producers_delay in
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
    let bootstrap_node_identity_file = Cli.bootstrap_node_identity_file in
    let bootstrap_dal_node_identity_file =
      Cli.bootstrap_dal_node_identity_file
    in
    let with_dal = Cli.with_dal in
    let bakers = Cli.bakers in
    let external_rpc = Cli.node_external_rpc_server in
    let dal_incentives = Cli.dal_incentives in
    let monitor_app_configuration =
      match Cli.Monitoring_app.(slack_bot_token, slack_channel_id) with
      | None, None -> None
      | Some _, None ->
          Log.warn
            "A Slack bot token has been provided but no Slack channel id. No \
             reports or alerts will be sent." ;
          None
      | None, Some _ ->
          Log.warn
            "A Slack channel ID has been provided but no Slack bot token. No \
             reports or alerts will be sent." ;
          None
      | Some slack_bot_token, Some slack_channel_id ->
          Some {slack_channel_id; slack_bot_token}
    in
    let t =
      {
        with_dal;
        bakers;
        stake;
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
        producer_key;
        producers_delay;
        blocks_history;
        bootstrap_node_identity_file;
        bootstrap_dal_node_identity_file;
        external_rpc;
        dal_incentives;
        monitor_app_configuration;
      }
    in
    (t, etherlink)
  in
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
  let endpoint, resolver_endpoint = Lwt.wait () in
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
    ~tasks:(Monitoring_app.Tasks.tasks ~configuration endpoint)
    ~tags:[]
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
      Lwt.wakeup resolver_endpoint t.some_node_rpc_endpoint ;
      toplog "Starting main loop" ;
      loop t (t.first_level + 1))
