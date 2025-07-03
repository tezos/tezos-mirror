(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Prerequisite:

   In order to be able to run the following test successfully, you need to make
   sure that your environment is well configured. To do so, have a look at the
   tezt/lib_cloud/README.md documentation.

   Additionally, if you are running the test you must ensure that:
   - DAL_TRUSTED_SETUP_PATH contains the expected data -- this can be done by
     running `./scripts/install_dal_trusted_setup.sh`
   - smart rollup binaries are available -- requires to run `make -f kernels.mk
     build-deps` and `make -f kernels.mk kernel_sdk`
   - floodgate binaries are available -- `make build-floodgate`
*)

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
  | Echo_rollup_operator
  | Echo_rollup_dal_observer of {slot_index : int}

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
  | Echo_rollup_operator -> "echo-rollup-operator"
  | Echo_rollup_dal_observer {slot_index} ->
      Format.sprintf "echo-rollup-dal-node-%d" slot_index

type snapshot_config =
  | Docker_embedded of string
  | Local_file of string
  | No_snapshot

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

  let yes_wallet agent =
    let name = Tezt_cloud.Agent.name agent ^ "-yes-wallet" in
    Yes_wallet.Agent.create ~name agent

  let yes_crypto_env =
    String_map.singleton
      Tezos_crypto.Helpers.yes_crypto_environment_variable
      "y"

  let import_snapshot ?(delete_snapshot_file = false) ~name node
      snapshot_file_path =
    toplog "Importing the snapshot for %s" name ;
    let* () =
      try
        let* () = Node.snapshot_import ~no_check:true node snapshot_file_path in
        let () = toplog "Snapshot import succeeded for %s." name in
        let* () =
          if delete_snapshot_file then (
            (* Delete the snapshot downloaded locally *)
            toplog "Deleting downloaded snapshot (%s)" snapshot_file_path ;
            let* (_ignored_exit_status : Unix.process_status) =
              Process.wait (Process.spawn "rm" [snapshot_file_path])
            in
            Lwt.return_unit)
          else Lwt.return_unit
        in
        Lwt.return_unit
      with _ ->
        (* Failing to import the snapshot could happen on a very young
           Weeklynet, before the first snapshot is available. In this
           case bootstrapping from the genesis block is OK. *)
        let () =
          toplog
            "Snapshot import failed for %s, the node will be bootstrapped from \
             genesis."
            name
        in
        Lwt.return_unit
    in
    Lwt.return_unit

  let get_snapshot_info_network node snapshot_path =
    let* info = Node.snapshot_info node ~json:true snapshot_path in
    let json = JSON.parse ~origin:"snapshot_info" info in
    (match JSON.(json |-> "snapshot_header" |-> "chain_name" |> as_string) with
    | "TEZOS_ITHACANET_2022-01-25T15:00:00Z" -> "ghostnet"
    | "TEZOS_RIONET_2025-02-19T12:45:00Z" -> "rionet"
    | "TEZOS_MAINNET" -> "mainnet"
    | "TEZOS" | _ -> "sandbox")
    |> Lwt.return

  let init ?(arguments = []) ?data_dir ?identity_file ?dal_config ?env
      ~rpc_external ~name network ~with_yes_crypto ~snapshot ?ppx_profiling
      cloud agent =
    toplog "Initializing an L1 node for %s" name ;
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
                ?ppx_profiling
                ?env
                node
                [Network (Network.to_octez_network_options network)]
            in
            let* () = Node.wait_for_ready node in
            Lwt.return node
        | None ->
            toplog
              "No data dir given, we will attempt to bootstrap the node from a \
               rolling snapshot." ;
            toplog "Creating the agent %s." name ;
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
            toplog "Initializing node configuration for %s" name ;
            let* () = Node.config_init node [] in
            let* snapshot_file_path =
              match snapshot with
              | Docker_embedded snapshot_path ->
                  let () =
                    toplog
                      "Using locally stored snapshot file: %s"
                      snapshot_path
                  in
                  Lwt.return snapshot_path
              | Local_file snapshot_path ->
                  let () = toplog "Copying snapshot to destination" in
                  let* snapshot_path =
                    Tezt_cloud.Agent.copy
                      agent
                      ~destination:snapshot_path
                      ~source:snapshot_path
                  in
                  Lwt.return snapshot_path
              | No_snapshot ->
                  toplog "Trying to download a rolling snapshot for %s" name ;
                  let downloaded_snapshot_file_path = "snapshot_file" in
                  let* exit_status =
                    Process.spawn
                      ?runner:(runner_of_agent agent)
                      "wget"
                      [
                        "-O";
                        downloaded_snapshot_file_path;
                        sf "%s/rolling" (Network.snapshot_service network);
                      ]
                    |> Process.wait
                  in
                  let* () =
                    match exit_status with
                    | WEXITED 0 -> Lwt.return_unit
                    | WEXITED code ->
                        toplog
                          "Could not download the snapshot for %s: wget exit \
                           code: %d\n\
                           Starting without snapshot. It could last long \
                           before the node is bootstrapped"
                          name
                          code ;
                        Lwt.return_unit
                    | status -> (
                        match Process.validate_status status with
                        | Ok () -> Lwt.return_unit
                        | Error (`Invalid_status reason) ->
                            failwith @@ Format.sprintf "wget: %s" reason)
                  in
                  return downloaded_snapshot_file_path
            in
            let* () =
              import_snapshot
                ~delete_snapshot_file:(snapshot = No_snapshot)
                ~name
                node
                snapshot_file_path
            in
            toplog "Launching the node %s." name ;
            let* () =
              Node.Agent.run
                ?ppx_profiling
                ?env
                node
                (Force_history_mode_switch :: Synchronisation_threshold 1
               :: arguments)
            in
            toplog "Waiting for the node %s to be ready." name ;
            let* () = wait_for_ready node in
            toplog "Node %s is ready." name ;
            let* () = Node.wait_for_synchronisation ~statuses:["synced"] node in
            toplog "Node %s is bootstrapped" name ;
            Lwt.return node)
    | _ (* private network *) -> (
        (* For sandbox deployments, we only listen on local interface, hence
           no connection could be made to us from outside networks *)
        let net_addr = "127.0.0.1" in
        let yes_crypto_arg =
          if with_yes_crypto then [Allow_yes_crypto] else []
        in
        match data_dir with
        | None ->
            let* node =
              Node.Agent.create ~net_addr ~rpc_external ~name cloud agent
            in
            let* () = Node.config_init node [Cors_origin "*"] in
            let* snapshot_path =
              match snapshot with
              | Docker_embedded snapshot_path ->
                  let () =
                    toplog
                      "Using locally stored snapshot file: %s"
                      snapshot_path
                  in
                  Lwt.return_some snapshot_path
              | Local_file snapshot_path ->
                  let () = toplog "Copying snapshot to destination" in
                  let* snapshot_path =
                    Tezt_cloud.Agent.copy
                      agent
                      ~destination:snapshot_path
                      ~source:snapshot_path
                  in
                  Lwt.return_some snapshot_path
              | No_snapshot -> Lwt.return_none
            in
            let* snapshot_network =
              match snapshot_path with
              | Some path ->
                  let* network = get_snapshot_info_network node path in
                  Lwt.return_some network
              | None -> Lwt.return_none
            in
            (* Set network *)
            let* () =
              Node.Config_file.update
                node
                (match snapshot_network with
                | Some "mainnet" -> Node.Config_file.set_mainnet_network ()
                | Some "ghostnet" -> Node.Config_file.set_ghostnet_network ()
                | Some "rionet" -> Node.Config_file.set_rionet_network ()
                | _ -> Node.Config_file.set_sandbox_network)
            in
            let* () =
              match dal_config with
              | None -> Lwt.return_unit
              | Some config ->
                  Node.Config_file.update
                    node
                    (Node.Config_file.set_network_with_dal_config config)
            in
            let* () = may_copy_node_identity_file agent node identity_file in
            let* () =
              match snapshot_path with
              | Some snapshot_path -> import_snapshot ~name node snapshot_path
              | None -> Lwt.return_unit
            in
            let* () =
              Node.Agent.run
                ?ppx_profiling
                ?env
                node
                ([
                   No_bootstrap_peers;
                   Synchronisation_threshold 0;
                   Cors_origin "*";
                   Force_history_mode_switch;
                 ]
                @ yes_crypto_arg @ arguments)
            in
            let* () = wait_for_ready node in
            Lwt.return node
        | Some data_dir ->
            let arguments =
              [No_bootstrap_peers; Synchronisation_threshold 0; Cors_origin "*"]
              @ yes_crypto_arg @ arguments
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
            let* () = Node.Agent.run ?env ?ppx_profiling node arguments in
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
                              "if ($query_string ~ \"slot_index=%d\")"
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
  tezlink : bool;
}

type configuration = {
  with_dal : bool;
  stake : int list Lwt.t;
  bakers : string list; (* unencrypted secret keys *)
  stake_machine_type : string list option;
  dal_node_producers : int list; (* slot indices *)
  observer_slot_indices : int list;
  observer_pkhs : string list;
  protocol : Protocol.t;
  producer_machine_type : string option;
  (* The first argument is the deconnection frequency, the second is the
     reconnection delay *)
  echo_rollup : bool;
  disconnect : (int * int) option;
  network : Network.t;
  simulate_network : Cli.network_simulation_config;
  snapshot : snapshot_config;
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
  ignore_pkhs : string list;
  ppx_profiling : bool;
  ppx_profiling_backends : string list;
  network_health_monitoring : bool;
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
  accounts : Account.key list;
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

type echo_operator = {
  node : Node.t;
  client : Client.t;
  sc_rollup_node : Sc_rollup_node.t;
  sc_rollup_address : string;
  operator : Account.key;
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

type public_key_hash = PKH of string

type commitment_info = {commitment : string; publisher_pkh : string}

(* "Status" of an attester at some level.
   There are 5 cases:
   - The attester is in the DAL committee and sent a dal_attestation -> With_DAL
   - The attester is in the DAL committee and sent an attestation without DAL -> Without_DAL
   - The attester is in the DAL committee and sent no attestation -> Expected_to_DAL_attest
   - The attester is out of the DAL committee (but in the Tenderbake committee) and
     sent an attestation -> Out_of_committee
   - The attester is out of the DAL committee and did not send an attestation
     (this case can happen either because they are out of the Tenderbake committee or
     because their baker had an issue at this level) -> Those bakers will not be in the
     `attestations` field of the `per_level_infos` crafted at the current level.
*)
type dal_status =
  | With_DAL of Z.t
  | Without_DAL
  | Out_of_committee
  | Expected_to_DAL_attest

type per_level_info = {
  level : int;
  published_commitments : (int, commitment_info) Hashtbl.t;
  attestations : (public_key_hash, dal_status) Hashtbl.t;
  attested_commitments : Z.t;
  etherlink_operator_balance_sum : Tez.t;
}

type per_baker_dal_summary = {
  attestable_slots : int;
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
  echo_rollup : echo_operator option;
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
  List.iter
    (fun {accounts; stake; baker; _} ->
      let baker_name = Agnostic_baker.name baker in
      List.iter
        (fun account ->
          let pkh = account.Account.public_key_hash in
          match
            Hashtbl.find_opt ratio_attested_commitments_per_baker (PKH pkh)
          with
          | None -> Log.info "We lack information about %s" pkh
          | Some {attestable_slots; attested_slots; _} ->
              let alias =
                Hashtbl.find_opt aliases account.Account.public_key_hash
                |> Option.value ~default:account.Account.public_key_hash
              in
              Log.info
                "%s: Ratio for %s (with stake %d): %a"
                baker_name
                alias
                stake
                pp_ratio
                (attested_slots, attestable_slots))
        accounts)
    t.bakers ;
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
  let push_attestable ~labels value =
    Cloud.push_metric
      t.cloud
      ~help:
        "Number of attestable commitments per baker (ie published when the \
         baker is in the DAL committee at attestation level)"
      ~typ:`Gauge
      ~labels
      ~name:"tezt_dal_commitments_attestable"
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
  let push_metric_out_attestation_sent ~labels () =
    Cloud.push_metric
      t.cloud
      ~help:"The baker sent an attestation while out of the DAL committee"
      ~typ:`Gauge
      ~labels
      ~name:"tezt_attestation_sent_when_out_of_dal_committee"
      1.
  in
  Hashtbl.iter
    (fun (PKH public_key_hash)
         {attested_slots; attestable_slots; in_committee; attestation_with_dal} ->
      if in_committee then (
        let labels = get_labels public_key_hash in
        push_attested ~labels attested_slots ;
        push_attestable ~labels attestable_slots ;
        push_dal_attestation_sent ~labels attestation_with_dal)
      else
        let labels = get_labels public_key_hash in
        push_metric_out_attestation_sent ~labels ())
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
        let attestable_slots =
          Hashtbl.length published_level_info.published_commitments
        in
        let table = Hashtbl.(create (length per_level_info.attestations)) in
        Hashtbl.to_seq per_level_info.attestations
        |> Seq.map (fun (public_key_hash, status) ->
               ( public_key_hash,
                 match status with
                 (* The baker is in the DAL committee and sent an attestation_with_dal. *)
                 | With_DAL attestation_bitset ->
                     {
                       attestable_slots;
                       attested_slots = Z.popcount attestation_bitset;
                       in_committee = true;
                       attestation_with_dal = true;
                     }
                 (* The baker is out of the DAL committee and sent an attestation_with_dal. *)
                 | Out_of_committee ->
                     {
                       attestable_slots;
                       attested_slots = 0;
                       in_committee = false;
                       attestation_with_dal = false;
                     }
                 (* The baker is in the DAL committee but sent either an attestation without DAL, or no attestations. *)
                 | Without_DAL | Expected_to_DAL_attest ->
                     {
                       attestable_slots;
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
  (* time interval in hours at which to submit report *)
  let report_interval = 6

  let pp_delegate fmt delegate_pkh =
    match Hashtbl.find_opt aliases delegate_pkh with
    | None -> Format.fprintf fmt "%s" delegate_pkh
    | Some alias ->
        Format.fprintf fmt "%s : %-26s" (String.sub delegate_pkh 0 7) alias

  (** [network_to_image_url network] return an image for each monitored network. *)
  let network_to_image_url : Network.t -> string = function
    | `Rionet ->
        "https://gitlab.com/tezos/tezos/-/raw/master/tezt/lib_cloud/assets/rionet.png"
    | `Mainnet ->
        "https://gitlab.com/tezos/tezos/-/raw/master/tezt/lib_cloud/assets/mainnet.png"
    | `Ghostnet ->
        "https://gitlab.com/tezos/tezos/-/raw/master/tezt/lib_cloud/assets/ghostnet.png"
    | `Nextnet _ ->
        "https://gitlab.com/tezos/tezos/-/raw/master/tezt/lib_cloud/assets/seoulnet.png"
    | `Sandbox | `Weeklynet _ -> "no_image_yet"

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
          "increase(tezt_total_%s_commitments_per_slot{slot_index=\"%d\"}[%dh])"
          s
          slot_index
          report_interval
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
        Format.sprintf
          {|increase(tezt_dal_commitments_total{kind="%s"}[%dh])|}
          s
          report_interval
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
      let ratio_view =
        (Format.sprintf
           "• Percentage of attested over published DAL commitments: %s")
          (Option.value ~default:"unk" ratio)
      in
      let slot_size = 126_944 (* TODO: do not hard-code this *) in
      let bandwidth =
        Option.map
          (fun x ->
            Format.sprintf
              "%.2f"
              (x *. float_of_int slot_size
              /. float_of_int (1024 * report_interval * 3600)))
          attested
      in
      let bandwidth_view =
        Format.sprintf
          "• Bandwidth: %s KiB/s"
          (Option.value ~default:"unk" bandwidth)
      in
      Lwt.return (ratio_view, bandwidth_view)

    let pp_stake fmt stake_ratio =
      Format.fprintf fmt "%.2f%% stake" (stake_ratio *. 100.)

    type baker_attestation_numbers = {
      (* The rate of attested/attestable slots for this baker.
         An attestatble slot is a published slot for which the baker is in
         the DAL committee at attestation level.
      *)
      slot_attestation_rate : float option;
      (* The rate of attestation_with_dal when the baker is in the DAL committee.
         This ratio is especially useful for small bakers which are rarely in
         the DAL committee, hence have very few opportunities to attest slots
         when publication is quite rare.
      *)
      dal_content_rate : float option;
      (* This boolean is true if the baker sent at least one attestation while
         out of the DAL committee. This is useful to detect bakers who attest
         but have a broken DAL setup preventing them to send attestations while
         in DAL committee.
      *)
      attests_while_out_of_dal_committee : bool;
    }

    type baker_infos = {
      address : public_key_hash;
      attest_infos : baker_attestation_numbers;
      stake_fraction : float;
    }

    let pp_baker_light fmt {address = PKH delegate_pkh; stake_fraction; _} =
      Format.fprintf
        fmt
        "%a (%a)"
        pp_delegate
        delegate_pkh
        pp_stake
        stake_fraction

    let pp_baker_dal_status fmt baker =
      Format.fprintf
        fmt
        "%a (%s)"
        pp_baker_light
        baker
        (match baker.attest_infos.dal_content_rate with
        | None -> "Never sent attestations while in DAL committee"
        | Some 0. -> "OFF"
        | Some 1. -> "ON"
        | Some x -> Format.sprintf "ACTIVE %.0f%% of the time" (x *. 100.))

    let pp_bakers_all_infos fmt baker =
      Format.fprintf
        fmt
        "%s - %a"
        (Option.fold
           ~none:"Never was in committee when slots were produced"
           ~some:(fun value ->
             let percentage = value *. 100. in
             Format.sprintf "%.2f%%" percentage)
           baker.attest_infos.slot_attestation_rate)
        pp_baker_dal_status
        baker

    let fetch_baker_info ~tz1 ~origin =
      let query =
        Format.sprintf
          "sum_over_time(tezt_dal_commitments_attested{attester=\"%s\"}[%dh])"
          tz1
          report_interval
      in
      let* attested = fetch ~decoder:decoder_prometheus_float ~query ~origin in
      let query =
        Format.sprintf
          "sum_over_time(tezt_dal_commitments_attestable{attester=\"%s\"}[%dh])"
          tz1
          report_interval
      in
      let* attestable =
        fetch ~decoder:decoder_prometheus_float ~query ~origin
      in
      let query =
        Format.sprintf
          "avg_over_time(tezt_dal_attestation_sent{attester=\"%s\"}[%dh])"
          tz1
          report_interval
      in
      let* dal_content_rate =
        fetch ~decoder:decoder_prometheus_float ~query ~origin
      in
      let query =
        Format.sprintf
          "sum_over_time(tezt_attestation_sent_when_out_of_dal_committee{attester=\"%s\"}[6h])"
          tz1
      in
      let* out_attestations =
        fetch ~decoder:decoder_prometheus_float ~query ~origin
      in
      let attests_while_out_of_dal_committee =
        Option.is_some out_attestations
      in
      let slot_attestation_rate =
        match (attested, attestable) with
        | None, _ | _, None | _, Some 0. -> None
        | Some attested, Some attestable -> Some (attested /. attestable)
      in
      return
        {
          slot_attestation_rate;
          dal_content_rate;
          attests_while_out_of_dal_committee;
        }

    let get_current_cycle endpoint =
      let* {cycle; _} =
        RPC_core.call endpoint (RPC.get_chain_block_helper_current_level ())
      in
      Lwt.return cycle

    let get_bakers_with_staking_power endpoint cycle =
      RPC_core.call endpoint (RPC.get_stake_distribution ~cycle ())

    type classified_bakers = {
      mute_bakers : baker_infos list;
      muted_by_dal : baker_infos list;
      dal_zero : baker_infos list;
      dal_on : baker_infos list;
      no_shards : baker_infos list;
      dal_off : baker_infos list;
    }

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
            let* attest_infos =
              fetch_baker_info
                ~origin:(Format.sprintf "fetch_baker_info.%s" delegate)
                ~tz1:delegate
            in
            let stake_fraction =
              float_of_int baking_power /. float_of_int total_baking_power
            in
            Lwt.return_some
              {address = PKH delegate; attest_infos; stake_fraction})
          bakers
      in
      let rec classify_bakers already_classified = function
        | [] -> already_classified
        | ({attest_infos; _} as baker) :: tl -> (
            match attest_infos.dal_content_rate with
            | None ->
                if attest_infos.attests_while_out_of_dal_committee then
                  classify_bakers
                    {
                      already_classified with
                      muted_by_dal = baker :: already_classified.muted_by_dal;
                    }
                    tl
                else
                  classify_bakers
                    {
                      already_classified with
                      mute_bakers = baker :: already_classified.mute_bakers;
                    }
                    tl
            | Some 0. ->
                classify_bakers
                  {
                    already_classified with
                    dal_off = baker :: already_classified.dal_off;
                  }
                  tl
            | _ -> (
                match attest_infos.slot_attestation_rate with
                | None ->
                    classify_bakers
                      {
                        already_classified with
                        no_shards = baker :: already_classified.no_shards;
                      }
                      tl
                | Some 0. ->
                    classify_bakers
                      {
                        already_classified with
                        dal_zero = baker :: already_classified.dal_zero;
                      }
                      tl
                | _ ->
                    classify_bakers
                      {
                        already_classified with
                        dal_on = baker :: already_classified.dal_on;
                      }
                      tl))
      in
      let {mute_bakers; muted_by_dal; dal_zero; dal_on; no_shards; dal_off} =
        classify_bakers
          {
            mute_bakers = [];
            muted_by_dal = [];
            dal_zero = [];
            dal_on = [];
            no_shards = [];
            dal_off = [];
          }
          bakers_info
      in
      let ( >> ) cmp1 cmp2 x y =
        match cmp1 x y with 0 -> cmp2 x y | cmp -> cmp
      in
      let stake_descending x y =
        Float.compare y.stake_fraction x.stake_fraction
      in
      let attestation_rate_ascending x y =
        Option.compare
          Float.compare
          x.attest_infos.slot_attestation_rate
          y.attest_infos.slot_attestation_rate
      in
      let dal_mention_perf_ascending x y =
        Option.compare
          Float.compare
          x.attest_infos.dal_content_rate
          y.attest_infos.dal_content_rate
      in
      let mute_bakers = List.sort stake_descending mute_bakers in
      let muted_by_dal = List.sort stake_descending muted_by_dal in
      let dal_zero =
        List.sort (stake_descending >> dal_mention_perf_ascending) dal_zero
      in
      let no_shards =
        List.sort (stake_descending >> dal_mention_perf_ascending) no_shards
      in
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
            (fun (nb, stake_acc) {stake_fraction; _} ->
              (nb + 1, stake_acc +. stake_fraction))
            (0, 0.)
            bakers
        in
        Format.sprintf
          "They are %d representing %.2f%% of the stake"
          nb
          (stake *. 100.)
      in
      let encapsulate_in_code_block strings = ("```" :: strings) @ ["```"] in
      let display catch_phrase printer bakers =
        if bakers = [] then []
        else
          [catch_phrase; agglomerate_infos bakers]
          :: List.map
               encapsulate_in_code_block
               (group_by 20 (List.map (Format.asprintf "%a" printer) bakers))
      in
      let display_muted =
        display
          ":alert: *Those bakers never sent attestations.*"
          pp_baker_light
          mute_bakers
      in
      let display_muted_by_DAL =
        display
          ":alert: *Those bakers never sent attestation while in DAL \
           committee, however they sent attestations when they are out of it. \
           This is quite unexpected. They probably have an issue.*"
          pp_baker_light
          muted_by_dal
      in
      let display_zero =
        display
          ":triangular_flag_on_post: *Those bakers have a broken DAL node. \
           They send attestations with a DAL content, but do not attest any \
           slots.*"
          pp_baker_dal_status
          dal_zero
      in
      let display_on =
        display
          ":white_check_mark: *Those bakers sent attestations with a DAL \
           content.*"
          pp_bakers_all_infos
          dal_on
      in
      let display_off =
        display
          ":x: *Those bakers never turned their DAL on.*"
          pp_baker_light
          dal_off
      in
      let display_no_shards =
        display
          ":microscope: *Those bakers seems to have a working DAL node, but \
           never were in committee when slots were produced, hence we cannot \
           say much about how well it works.*"
          pp_baker_light
          no_shards
      in
      Lwt.return
        ((["Details of bakers performance:"] :: display_muted_by_DAL)
        @ display_zero @ display_on @ display_no_shards @ display_off
        @ display_muted)

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
          "*DAL report* for the *%s* network over the last %d hours."
          (String.capitalize_ascii (Network.to_string network))
          report_interval
      in
      let* ratio_dal_commitments_total_info, bandwidth_info =
        fetch_dal_commitments_total_info ()
      in
      let* slots_info = fetch_slots_info () in
      let network_overview_info =
        bandwidth_info :: ratio_dal_commitments_total_info :: slots_info
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
    let register_chronos_task cloud ~configuration endpoint =
      match Cloud.notifier cloud with
      | Notifier_null -> ()
      | Notifier_slack {slack_bot_token; slack_channel_id; _} ->
          let task =
            let action () =
              action
                ~slack_bot_token
                ~slack_channel_id
                ~configuration
                endpoint
                ()
            in
            let tm = Format.sprintf "0 0-23/%d * * *" report_interval in
            Chronos.task ~name:"network-overview" ~tm ~action ()
          in
          Cloud.register_chronos_task cloud task
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
      match Cloud.notifier t.cloud with
      | Notifier_null -> unit
      | Notifier_slack {slack_channel_id; slack_bot_token; _} ->
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
      match Cloud.notifier t.cloud with
      | Notifier_null -> unit
      | Notifier_slack {slack_channel_id; slack_bot_token; _} ->
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

    let report_attestation_transition ~slack_channel_id ~slack_bot_token
        ~network ~level ~pkh ~transition ~attestation_percentage =
      let data =
        let header =
          Format.asprintf
            (match transition with
            | Restarted_attesting ->
                "*[dal-attester-is-back]* On network `%s` at level `%d`, \
                 delegate `%a` who was under the reward threshold is again \
                 above threshold. New attestation rate is %.2f%%."
            | Stopped_attesting ->
                "*[dal-attester-dropped]* On network `%s` at level `%d`, \
                 delegate `%a` DAL attestation rate dropped under the reward \
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

    let check_for_recently_missed_a_lot =
      (* TODO: This correspond to the number of bakers which activated DAL on mainnet.
         We expect more bakers to run the DAL, this initial size might be then increased
         to avoid rescaling of the hash table.
      *)
      let prev_was_enough = Hashtbl.create 50 in
      let to_treat_delegates = ref [] in
      fun t ~level ~metadata ->
        match Cloud.notifier t.cloud with
        | Notifier_null -> unit
        | Notifier_slack {slack_bot_token; slack_channel_id; _} -> (
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
                          ~slack_channel_id
                          ~slack_bot_token
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
                Tezos_stdlib.TzList.split_n 2 !to_treat_delegates
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
    if t.configuration.network_health_monitoring then
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
    else Lwt.return_unit
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
      toplog "Copying the DAL node identity file for %s" (Agent.name agent) ;
      let* _ =
        Agent.copy agent ~source ~destination:(Dal_node.identity_file node)
      in
      Lwt.return_unit

let fund_producers_accounts (bootstrap : bootstrap) configuration
    accounts_to_fund =
  if List.length accounts_to_fund > 0 then
    let () = toplog "Funding the producer accounts" in
    let fundraiser_key =
      match configuration.fundraiser with
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

let init_producer_accounts (bootstrap : bootstrap) configuration =
  let () = toplog "Initializing the producers" in
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

let init_echo_rollup_account (bootstrap : bootstrap)
    (configuration : configuration) =
  if configuration.echo_rollup then
    let () = toplog "Initializing the echo rollup key" in
    let* key =
      Client.stresstest_gen_keys
        ~alias_prefix:"echo_operator"
        1
        bootstrap.client
    in
    Lwt.return_some (List.hd key)
  else Lwt.return_none

let init_etherlink_operators (bootstrap : bootstrap) etherlink_configuration =
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
  return (etherlink_rollup_operator_key, etherlink_batching_operator_keys)

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
          match configuration.simulate_network with
          | Scatter _ | Map _ -> true
          | Disabled -> false
        in
        let* node =
          Node.init
            ?identity_file:configuration.bootstrap_node_identity_file
            ~rpc_external:configuration.external_rpc
            ~name:"bootstrap-node"
            configuration.network
            ~with_yes_crypto
            ~snapshot:configuration.snapshot
            ~ppx_profiling:configuration.ppx_profiling
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
              may_copy_dal_node_identity_file
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
                ~ppx_profiling:configuration.ppx_profiling
                ~ppx_profiling_backends:configuration.ppx_profiling_backends
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
  let* stake = configuration.stake in
  let* baker_accounts =
    Lwt_list.mapi_s
      (fun i _stake ->
        (* We assume that a baker holds only one key. *)
        Client.stresstest_gen_keys
          ~alias_prefix:(Format.sprintf "baker-%d" i)
          1
          bootstrap.client)
      stake
  in
  let* producer_accounts = init_producer_accounts bootstrap configuration in
  let* etherlink_rollup_operator_key, etherlink_batching_operator_keys =
    init_etherlink_operators bootstrap etherlink_configuration
  in
  let* echo_rollup_key = init_echo_rollup_account bootstrap configuration in
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
    @ Option.fold
        ~none:[]
        ~some:(fun operator -> [(operator, 11_000 * 1_000_000)])
        echo_rollup_key
  in
  let* () = fund_producers_accounts bootstrap configuration accounts_to_fund in
  let etherlink_rollup_operator_key =
    match etherlink_rollup_operator_key with key :: _ -> Some key | [] -> None
  in
  Lwt.return
    ( bootstrap,
      baker_accounts,
      producer_accounts,
      etherlink_rollup_operator_key,
      etherlink_batching_operator_keys,
      echo_rollup_key )

let round_robin_split m lst =
  assert (m > 0) ;
  let buckets = Array.make m [] in
  List.iteri
    (fun idx x ->
      let bucket = idx mod m in
      buckets.(bucket) <- x :: buckets.(bucket))
    (List.rev lst) ;
  Array.to_list buckets |> List.rev

let may_set_yes_crypto = function
  | Cli.Scatter _ | Map _ -> (Some Node.yes_crypto_env, true)
  | Disabled -> (None, false)

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
  let env, with_yes_crypto =
    may_set_yes_crypto configuration.simulate_network
  in
  let* bootstrap_node =
    Node.init
      ?env
      ?data_dir
      ?identity_file:configuration.bootstrap_node_identity_file
      ~rpc_external:configuration.external_rpc
      ~dal_config
      ~name
      configuration.network
      ~with_yes_crypto
      ~snapshot:configuration.snapshot
      ~ppx_profiling:configuration.ppx_profiling
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
      let* yes_wallet = Node.yes_wallet agent in
      let* snapshot_network =
        match configuration.snapshot with
        | Docker_embedded path | Local_file path ->
            let* network = Node.get_snapshot_info_network bootstrap_node path in
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
  let* simulated_delegates =
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
    | Disabled -> Lwt.return_nil
  in
  let* generated_baker_accounts =
    Lwt_list.mapi_s
      (fun i _stake ->
        (* We assume that a baker holds only one key. *)
        Client.stresstest_gen_keys
          ~alias_prefix:(Format.sprintf "baker-%d" i)
          1
          client)
      stake
  in
  let* simulated_bakers =
    match configuration.simulate_network with
    | Disabled -> return []
    | Map _ | Scatter _ ->
        (* Substitute consensus pkh with delegate pkh *)
        let* yw = Node.yes_wallet agent in
        let* ckm = Yes_wallet.load_consensus_key_mapping yw ~client in
        List.map
          (fun l ->
            List.map
              (fun a ->
                try
                  let ck =
                    List.find
                      (fun {Yes_wallet.public_key_hash; _} ->
                        public_key_hash = a.Account.public_key_hash)
                      ckm
                  in
                  {
                    a with
                    public_key_hash = ck.consensus_public_key_hash;
                    public_key = ck.consensus_public_key;
                  }
                with Not_found -> a)
              l)
          simulated_delegates
        |> return
  in
  (* [baker_accounts] stands for the list of keys that are actually used for
     baking. Meaning that if a baker uses a consensus key, the baker account
     will be that consensus key (and not the registered baker one). This aims to
     be used by bakers and attesters daemons. *)
  let baker_accounts = generated_baker_accounts @ simulated_bakers in
  (* [delegate_accounts] stands for the list of delegates keys (registered
     baking account) that will be used by the producers daemons. Indeed, the
     producers requires an account holding funds as they aim to inject
     publishments. *)
  let delegate_accounts = generated_baker_accounts @ simulated_delegates in
  List.iteri
    (fun i l ->
      toplog
        "Baker agent %d will run with: %a"
        i
        (Format.pp_print_list
           ~pp_sep:(fun out () -> Format.fprintf out ",")
           (fun fmt (a : Account.key) -> Format.fprintf fmt "%s" a.alias))
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
  let* echo_rollup_key =
    if configuration.echo_rollup then
      let* keys =
        Client.stresstest_gen_keys ~alias_prefix:"echo_rollup_key" 1 client
      in
      Lwt.return_some (List.hd keys)
    else Lwt.return_none
  in

  let* () =
    if configuration.simulate_network = Disabled then
      let* parameter_file =
        let base =
          Either.right (configuration.protocol, Some Protocol.Constants_mainnet)
        in
        let bootstrap_accounts =
          List.mapi
            (fun i key -> (key, Some (List.nth stake i * 1_000_000_000_000)))
            (List.flatten baker_accounts)
        in
        let additional_bootstrap_accounts =
          List.map
            (fun key -> (key, Some 1_000_000_000_000, false))
            (producer_accounts @ etherlink_rollup_operator_key
           @ etherlink_batching_operator_keys
            @ Option.fold ~none:[] ~some:(fun k -> [k]) echo_rollup_key)
        in
        let overrides = [] in
        Protocol.write_parameter_file
          ~bootstrap_accounts
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
          may_copy_dal_node_identity_file
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
          ~ppx_profiling:configuration.ppx_profiling
          ~ppx_profiling_backends:configuration.ppx_profiling_backends
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
      etherlink_batching_operator_keys,
      echo_rollup_key )

let init_baker ?stake cloud (configuration : configuration) ~bootstrap teztale
    ~baker_accounts i agent =
  let* stake =
    (* As simulate_network and stake are mutually exclusive, the stake is used
       only when the simulation is Disabled. *)
    match configuration.simulate_network with
    | Disabled -> (
        match stake with
        | None ->
            let* stake = configuration.stake in
            return (List.nth stake i)
        | Some stake -> return stake)
    | Scatter _ | Map _ -> Lwt.return 0
  in
  let name = Format.asprintf "baker-node-%d" i in
  let data_dir =
    configuration.data_dir |> Option.map (fun data_dir -> data_dir // name)
  in
  let env, with_yes_crypto =
    may_set_yes_crypto configuration.simulate_network
  in
  let* node =
    Node.init
      ?env
      ?data_dir
      ~arguments:Node.[Peer bootstrap.node_p2p_endpoint]
      ~name:(Format.asprintf "baker-node-%d" i)
      ~rpc_external:configuration.external_rpc
      configuration.network
      ~with_yes_crypto
      ~snapshot:configuration.snapshot
      ~ppx_profiling:configuration.ppx_profiling
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
          ~disable_shard_validation:configuration.disable_shard_validation
          cloud
          agent
      in
      let attester_profiles =
        List.map (fun account -> account.Account.public_key_hash) baker_accounts
      in
      let* () =
        Dal_node.init_config
          ~expected_pow:(Network.expected_pow configuration.network)
          ~attester_profiles
          ~peers:[bootstrap.dal_node_p2p_endpoint |> Option.get]
          (* Invariant: Option.get don't fail because t.configuration.dal is true *)
          dal_node
      in
      let otel = Cloud.open_telemetry_endpoint cloud in
      let* () =
        Dal_node.Agent.run
          ~prometheus:Tezt_cloud_cli.prometheus
          ?otel
          ~memtrace:configuration.memtrace
          ~event_level:`Notice
          ~disable_shard_validation:configuration.disable_shard_validation
          ~ppx_profiling:configuration.ppx_profiling
          ~ppx_profiling_backends:configuration.ppx_profiling_backends
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
          (fun account ->
            Teztale.update_alias
              teztale
              ~address:account.Account.public_key_hash
              ~alias:account.Account.alias)
          baker_accounts
  in
  let* client = Client.Agent.create ~endpoint:(Node node) agent in
  let* () =
    match configuration.simulate_network with
    | Scatter _ | Map _ ->
        let* yes_wallet = Node.yes_wallet agent in
        let* () =
          Lwt_list.iter_s
            (fun (account : Account.key) ->
              Client.import_public_key
                client
                ~public_key:account.public_key
                ~alias:account.alias)
            baker_accounts
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
          baker_accounts
  in
  let delegates =
    List.map (fun account -> account.Account.alias) baker_accounts
  in
  let* baker =
    let dal_node_rpc_endpoint = Option.map Dal_node.as_rpc_endpoint dal_node in
    Agnostic_baker.Agent.init
      ?env
      ~name:(Format.asprintf "baker-%d" i)
      ~delegates
      ~client
      ?dal_node_rpc_endpoint
      ~ppx_profiling:configuration.ppx_profiling
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

let init_producer cloud configuration ~bootstrap teztale account i slot_index
    agent =
  let name = Format.asprintf "producer-node-%i" i in
  let () = toplog "Initializing the DAL producer %s" name in
  let data_dir =
    configuration.data_dir |> Option.map (fun data_dir -> data_dir // name)
  in
  let () = toplog "Init producer %s: init L1 node" name in
  let env, with_yes_crypto =
    may_set_yes_crypto configuration.simulate_network
  in
  let* node =
    Node.init
      ?env
      ?data_dir
      ~name
      ~arguments:Node.[Peer bootstrap.node_p2p_endpoint]
      ~rpc_external:configuration.external_rpc
      configuration.network
      ~with_yes_crypto
      ~snapshot:configuration.snapshot
      ~ppx_profiling:configuration.ppx_profiling
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
    let ignore_pkhs =
      if configuration.ignore_pkhs = [] then None
      else Some configuration.ignore_pkhs
    in
    Dal_node.Agent.create
      ~name:(Format.asprintf "producer-dal-node-%i" i)
      ~node
      ~disable_shard_validation:configuration.disable_shard_validation
      ?ignore_pkhs
      cloud
      agent
  in
  let () = toplog "Init producer %s: init DAL node config" name in
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
    let ignore_pkhs =
      if configuration.ignore_pkhs = [] then None
      else Some configuration.ignore_pkhs
    in
    Dal_node.Agent.run
      ~prometheus:Tezt_cloud_cli.prometheus
      ?otel
      ~memtrace:configuration.memtrace
      ~event_level:`Notice
      ~disable_shard_validation:configuration.disable_shard_validation
      ?ignore_pkhs
      ~ppx_profiling:configuration.ppx_profiling
      ~ppx_profiling_backends:configuration.ppx_profiling_backends
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

let init_observer cloud configuration ~bootstrap teztale ~topic i agent =
  let name = Format.asprintf "observer-node-%i" i in
  let data_dir =
    configuration.data_dir |> Option.map (fun data_dir -> data_dir // name)
  in
  let env, with_yes_crypto =
    may_set_yes_crypto configuration.simulate_network
  in
  let* node =
    Node.init
      ?env
      ?data_dir
      ~name
      ~arguments:[Peer bootstrap.node_p2p_endpoint]
      ~rpc_external:configuration.external_rpc
      configuration.network
      ~with_yes_crypto
      ~snapshot:configuration.snapshot
      ~ppx_profiling:configuration.ppx_profiling
      cloud
      agent
  in
  let* dal_node =
    Dal_node.Agent.create
      ~name:(Format.asprintf "observer-dal-node-%i" i)
      ~node
      ~disable_shard_validation:configuration.disable_shard_validation
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
      ~prometheus:Tezt_cloud_cli.prometheus
      ?otel
      ~memtrace:configuration.memtrace
      ~event_level:`Notice
      ~disable_shard_validation:configuration.disable_shard_validation
      ~ppx_profiling:configuration.ppx_profiling
      ~ppx_profiling_backends:configuration.ppx_profiling_backends
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

let init_dal_reverse_proxy_observers
    {
      external_rpc;
      network;
      snapshot;
      ppx_profiling;
      ppx_profiling_backends;
      memtrace;
      simulate_network;
      _;
    } ~name_of ~default_endpoint ~bootstrap ~dal_slots ~next_agent ~otel ~cloud
    =
  if dal_slots = [] then failwith "Expected at least a DAL slot." ;
  let* dal_slots_and_nodes =
    dal_slots
    |> Lwt_list.map_p (fun slot_index ->
           let name = name_of slot_index in
           let* agent = next_agent ~name in
           let env, with_yes_crypto = may_set_yes_crypto simulate_network in
           let* node =
             Node.init
               ?env
               ~name
               ~arguments:
                 [
                   Peer bootstrap.node_p2p_endpoint;
                   History_mode (Rolling (Some 79));
                 ]
               ~rpc_external:external_rpc
               network
               ~with_yes_crypto
               ~snapshot
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
           let* () =
             Dal_node.Agent.run
               ?otel
               ~memtrace
               ~ppx_profiling
               ~ppx_profiling_backends
               dal_node
           in
           return (slot_index, Dal_node.rpc_endpoint dal_node))
  in
  let default_endpoint =
    match default_endpoint with
    | Some e -> e
    | None ->
        let _, first_observer_endpoint = List.hd dal_slots_and_nodes in
        first_observer_endpoint
  in
  Dal_reverse_proxy.init_reverse_proxy
    cloud
    ~next_agent
    ~default_endpoint
    (List.to_seq dal_slots_and_nodes)

let init_etherlink_dal_node
    ({
       external_rpc;
       network;
       snapshot;
       ppx_profiling;
       ppx_profiling_backends;
       memtrace;
       simulate_network;
       _;
     } as configuration) ~bootstrap ~dal_slots ~next_agent ~otel ~cloud =
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
      let env, with_yes_crypto = may_set_yes_crypto simulate_network in
      let* node =
        Node.init
          ?env
          ~name
          ~arguments:
            [Peer bootstrap.node_p2p_endpoint; History_mode (Rolling (Some 79))]
          ~rpc_external:external_rpc
          network
          ~with_yes_crypto
          ~snapshot
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
      let* () =
        Dal_node.Agent.run ?otel ~ppx_profiling ~ppx_profiling_backends dal_node
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
      let env, with_yes_crypto = may_set_yes_crypto simulate_network in
      let* node =
        Node.init
          ?env
          ~name
          ~arguments:
            [Peer bootstrap.node_p2p_endpoint; History_mode (Rolling (Some 79))]
          ~rpc_external:external_rpc
          network
          ~with_yes_crypto
          ~snapshot
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
      let* () =
        Dal_node.Agent.run
          ?otel
          ~memtrace
          ~ppx_profiling
          ~ppx_profiling_backends
          default_dal_node
      in
      let default_endpoint = Dal_node.rpc_endpoint default_dal_node in
      let* reverse_proxy_dal_node =
        init_dal_reverse_proxy_observers
          configuration
          ~name_of:(fun slot_index ->
            name_of (Etherlink_dal_observer {slot_index}))
          ~default_endpoint:(Some default_endpoint)
          ~bootstrap
          ~dal_slots
          ~next_agent
          ~otel
          ~cloud
      in
      some reverse_proxy_dal_node

let init_etherlink_operator_setup cloud configuration etherlink_configuration
    name ~bootstrap ~dal_slots ~tezlink account batching_operators agent
    next_agent =
  let chain_id = Option.value ~default:1 etherlink_configuration.chain_id in
  let is_sequencer = etherlink_configuration.etherlink_sequencer in
  let data_dir =
    configuration.data_dir |> Option.map (fun data_dir -> data_dir // name)
  in
  let env, with_yes_crypto =
    may_set_yes_crypto configuration.simulate_network
  in
  let* node =
    Node.init
      ?env
      ?data_dir
      ~name
      ~arguments:
        [Peer bootstrap.node_p2p_endpoint; History_mode (Rolling (Some 79))]
      ~rpc_external:configuration.external_rpc
      configuration.network
      ~with_yes_crypto
      ~snapshot:configuration.snapshot
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
      configuration
      ~bootstrap
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
    etherlink_rollup_operator_key batching_operators ~dal_slots ~tezlink
    next_agent =
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
             ~bootstrap
             ~rpc_external:configuration.external_rpc
             cloud
             agent)
    |> Lwt.join
  in
  return {configuration = etherlink_configuration; operator; accounts}

let init_echo_rollup cloud configuration ~bootstrap operator dal_slots
    next_agent =
  let name = name_of Echo_rollup_operator in
  let* agent = next_agent ~name in
  let data_dir =
    configuration.data_dir |> Option.map (fun data_dir -> data_dir // name)
  in
  let with_yes_crypto =
    match configuration.simulate_network with
    | Scatter _ | Map _ -> true
    | Disabled -> false
  in
  let* node =
    Node.init
      ?data_dir
      ~name
      ~arguments:
        (* Keeping enough data for refutation games -- might be adjusted if need. *)
        [Peer bootstrap.node_p2p_endpoint; History_mode (Rolling (Some 79))]
      ~rpc_external:configuration.external_rpc
      configuration.network
      ~snapshot:configuration.snapshot
      ~with_yes_crypto
      cloud
      agent
  in
  let endpoint = Client.Node node in
  let* client = Client.Agent.create ~endpoint agent in
  let () = toplog "Init Echo rollup: importing the operator secret key" in
  let* () =
    Client.import_secret_key
      client
      ~endpoint
      operator.Account.secret_key
      ~alias:operator.Account.alias
  in

  let l = Node.get_last_seen_level node in
  let () = toplog "Init Echo rollup: revealing the operator account" in
  let*! () = Client.reveal client ~endpoint ~src:operator.Account.alias in
  let () = toplog "Init Echo rollup: waiting for level %d" (l + 2) in
  let* _ = Node.wait_for_level node (l + 2) in
  let () = toplog "Init Echo rollup: waiting for level %d: done" (l + 2) in

  let otel = Cloud.open_telemetry_endpoint cloud in
  let* dal_node =
    match dal_slots with
    | [] ->
        toplog "Echo rollup doesn't follow any slot" ;
        none
    | [slot_index] ->
        let name = name_of (Echo_rollup_dal_observer {slot_index}) in
        let* agent = next_agent ~name in
        let* dal_node = Dal_node.Agent.create ~name ~node cloud agent in
        let* () =
          Dal_node.init_config
            ~expected_pow:(Network.expected_pow configuration.network)
            ~observer_profiles:dal_slots
            ~peers:(Option.to_list bootstrap.dal_node_p2p_endpoint)
            dal_node
        in
        let* () =
          Dal_node.Agent.run
            ?otel
            ~ppx_profiling:configuration.ppx_profiling
            ~ppx_profiling_backends:configuration.ppx_profiling_backends
            dal_node
        in
        some dal_node
    | _ ->
        let* dal_reverse_proxy_with_observers =
          init_dal_reverse_proxy_observers
            configuration
            ~name_of:(fun slot_index ->
              name_of (Echo_rollup_dal_observer {slot_index}))
            ~default_endpoint:None
            ~bootstrap
            ~dal_slots
            ~next_agent
            ~otel
            ~cloud
        in
        some dal_reverse_proxy_with_observers
  in
  let* sc_rollup_node =
    Sc_rollup_node.Agent.create
      ~name:(Format.asprintf "%s-rollup-node" name)
      ~base_dir:(Client.base_dir client)
      ~default_operator:operator.alias
      ~operators:[(Sc_rollup_node.Operating, operator.Account.alias)]
      ?dal_node
      cloud
      agent
      Operator
      node
  in
  let preimages_dir =
    Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) "wasm_2_0_0"
  in
  let slots_bitvector =
    List.fold_left
      (fun vec slot -> Z.logor vec (Z.of_int (1 lsl slot)))
      Z.zero
      dal_slots
  in
  let config =
    Format.sprintf
      {|instructions:
- set:
    value: %s
    to: /slots
      |}
      (Z.to_bits slots_bitvector |> Hex.of_string |> Hex.show)
  in
  let output_config = Temp.file "config.yaml" in
  write_file output_config ~contents:config ;
  let* remote_output_config = Agent.copy agent ~source:output_config in
  let* {output; _} =
    Sc_rollup_helpers.Agent.prepare_installer_kernel
      ~config:(`Path remote_output_config)
      ~preimages_dir
      Constant.WASM.dal_echo_kernel_bandwidth
      agent
  in
  let pvm_kind = "wasm_2_0_0" in
  let l = Node.get_last_seen_level node in
  let () = toplog "Init Echo rollup: originating the rollup" in
  let* sc_rollup_address =
    Sc_rollup_helpers.Agent.originate_sc_rollup
      ~kind:pvm_kind
      ~boot_sector:output
      ~parameters_ty:"unit"
      ~src:operator.alias
      client
  in
  let () = toplog "Init Echo rollup: waiting again, for level %d" (l + 2) in
  let* _ = Node.wait_for_level node (l + 2) in
  let () =
    toplog "Init Echo rollup: waiting again, for level %d: done" (l + 2)
  in
  let () = toplog "Init Echo rollup: launching the rollup node" in
  let* () =
    Sc_rollup_node.run sc_rollup_node sc_rollup_address [Log_kernel_debug]
  in
  let () = toplog "Init Echo rollup: launching the rollup node: done" in
  let operator : echo_operator =
    {node; client; sc_rollup_node; operator; sc_rollup_address}
  in
  let* () =
    add_prometheus_source
      ?dal_node
      ~node
      ~sc_rollup_node
      cloud
      agent
      (Format.asprintf "echo-%s" name)
  in
  return operator

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
  let () = toplog "Agents preparation" in
  let* bootstrap_agent =
    if configuration.bootstrap then
      let name = name_of Bootstrap in
      let* agent = next_agent ~name in
      Lwt.return_some agent
    else Lwt.return_none
  in
  let* stake = configuration.stake in
  let* attesters_agents =
    (* As simulate_network and stake are mutually exclusive, the stake is used
       only when the simulation is Disabled. *)
    match configuration.simulate_network with
    | Scatter (_, baker_count) ->
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
      (match configuration.simulate_network with
      | Scatter (_selected_baker_count, baker_daemon_count) ->
          List.init baker_daemon_count string_of_int
      | Map
          ( _selected_baker_count,
            single_baker_daemon_count,
            multiple_baker_daemon_count ) ->
          List.init
            (single_baker_daemon_count + multiple_baker_daemon_count)
            string_of_int
      | Disabled -> configuration.bakers)
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
        let name = name_of (Observer (`Index slot_index)) in
        let* agent = next_agent ~name in
        return (`Slot_index slot_index, agent))
      configuration.observer_slot_indices
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
         echo_rollup_key ) =
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
    match configuration.simulate_network with
    | Scatter _ | Map _ ->
        Lwt_list.mapi_p
          (fun i (agent, accounts) ->
            init_baker
              cloud
              configuration
              ~bootstrap
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
                cloud
                configuration
                ~bootstrap
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
                  (fun (alias, _) -> Client.show_address ~alias client)
                  addresses
              in
              (* A bit random, to fix later. *)
              let stake = 1 in
              init_baker
                ~stake
                cloud
                configuration
                ~bootstrap
                teztale
                ~baker_accounts:accounts
                i
                agent)
            (List.combine bakers_agents configuration.bakers)
        in
        Lwt.return (fresh_bakers @ bakers_with_secret_keys)
  in
  let () = toplog "Init: initializing producers and observers" in
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
  let* echo_rollup =
    match echo_rollup_key with
    | Some operator ->
        let dal_slots = List.map (fun p -> p.slot_index) producers in
        let* echo_rollup =
          init_echo_rollup
            cloud
            configuration
            ~bootstrap
            operator
            dal_slots
            next_agent
        in
        Lwt.return_some echo_rollup
    | _ -> Lwt.return_none
  in

  let () = toplog "Init: Echo rollup has been initialized" in
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
            ~tezlink:etherlink_configuration.tezlink
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
    | `Sandbox -> (
        match configuration.simulate_network with
        | Disabled -> Lwt.return 1
        | Scatter _ | Map _ -> Network.get_level some_node_rpc_endpoint)
    | _ -> Network.get_level some_node_rpc_endpoint
  in
  Hashtbl.replace metrics first_level default_metrics ;
  let disconnection_state =
    Option.map Disconnect.init configuration.disconnect
  in
  let* init_aliases =
    let accounts =
      List.concat_map (fun ({accounts; _} : baker) -> accounts) bakers
    in
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
      echo_rollup;
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
    let accounts =
      List.(concat_map (fun ({accounts; _} : baker) -> accounts) t.bakers)
    in
    Network.aliases ~accounts t.configuration.network
  in
  let* versions = Network.versions t.configuration.network in
  merge_aliases new_aliases ;
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
                ~prometheus:Tezt_cloud_cli.prometheus
                ?otel:t.otel
                ~memtrace:t.configuration.memtrace
                ~ppx_profiling:t.configuration.ppx_profiling
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
    Monitoring_app.Alert.check_for_lost_dal_rewards t ~metadata
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

let ensure_enough_funds ~fee t i =
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
      if balance < Tez.of_mutez_int fee then
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
      if balance < Tez.of_mutez_int random then
        let* fundraiser =
          Client.show_address ~alias:"fundraiser" t.bootstrap.client
        in
        let* _op_hash =
          Operation.Manager.transfer
            ~amount:10_000_000
            ~dest:producer.account
            ()
          |> Operation.Manager.make ~fee ~source:fundraiser
          |> Seq.return |> List.of_seq
          |> Fun.flip
               (Operation.Manager.inject ~dont_wait:true)
               t.bootstrap.client
        in
        Lwt.return_unit
      else Lwt.return_unit

let produce_slot t level i =
  if level mod t.configuration.producers_delay = 0 then (
    let all_start = Unix.gettimeofday () in
    let producer = List.nth t.producers i in
    toplog
      "Producing a slot for index %d for level %d"
      producer.slot_index
      level ;
    let fee = 800 in
    let* () = ensure_enough_funds ~fee t i in
    toplog "Ensured enough funds are available" ;
    let index = producer.slot_index in
    let content =
      Format.asprintf "%d:%d" level index
      |> Helpers.make_slot
           ~padding:false
           ~slot_size:t.parameters.cryptobox.slot_size
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
      toplog "Producers not ready for level %d" level ;
      Lwt.return_unit)
    else
      Seq.ints 0
      |> Seq.take (List.length t.configuration.dal_node_producers)
      |> Seq.map (fun i -> produce_slot t level i)
      |> List.of_seq |> Lwt.join
  in
  let* t = p in
  loop t (level + 1)

let parse_snapshot_arg snapshot_arg =
  let fail path =
    Test.fail
      "wrong snapshot argument (--snapshot) [%s].@.Use:@.- \
       \"file:path/to/file\" to use a local file that will be uploaded to each \
       agent,@.- \"docker\" to use the docker_embedded_snapshot_file, that \
       must be located in the local path, to embed the snapshot file into the \
       docker image."
      path
  in
  match snapshot_arg with
  | Some v -> (
      match v with
      | "docker" ->
          (* This hardcoded path must be defined as the location path of the snapshot
             embedded in the docker image. See the associated dockerfile. *)
          Docker_embedded "/tmp/docker_embedded_snapshot_file"
      | s when String.starts_with ~prefix:"file:" s -> (
          match String.split_on_char ':' s with
          | [_; path] -> Local_file path
          | _ -> fail s)
      | _ -> fail v)
  | None -> No_snapshot

let yes_wallet_exe = Uses.path Constant.yes_wallet

let parse_stake_arg ~stake_arg ~simulation_arg =
  let open Network in
  match simulation_arg with
  | Cli.Disabled -> (
      match stake_arg with
      | Custom distrib -> return distrib
      | Mimic {network; max_nb_bakers} ->
          let network_string =
            match network with
            | `Mainnet | `Ghostnet | `Rionet -> to_string network
            | _ ->
                failwith
                  (Format.sprintf
                     "Cannot get stake distribution for %s"
                     (to_string network))
          in
          let endpoint =
            Endpoint.make ~host:"rpc.tzkt.io" ~scheme:"https" ~port:443 ()
          in
          let decoder json = JSON.(json |-> "cycle" |> as_int) in
          let rpc =
            RPC_core.(
              make
                GET
                [
                  network_string;
                  "chains";
                  "main";
                  "blocks";
                  "head";
                  "helpers";
                  "current_level";
                ]
                decoder)
          in
          let* response = RPC_core.call_raw endpoint rpc in
          let cycle =
            RPC_core.decode_raw ~origin:"Network.cycle" rpc response.body
          in
          let get_stake_in_ktez stake =
            JSON.(
              (stake |-> "frozen" |> as_int) + (stake |-> "delegated" |> as_int))
            / 1_000_000_000
          in
          let decoder json =
            json |> JSON.as_list
            |> List.map (fun json_account ->
                   let active_stake = JSON.(json_account |-> "active_stake") in
                   get_stake_in_ktez active_stake)
          in
          let rpc =
            RPC_core.(
              make
                GET
                [
                  network_string;
                  "chains";
                  "main";
                  "blocks";
                  "head";
                  "context";
                  "raw";
                  "json";
                  "cycle";
                  string_of_int cycle;
                  "selected_stake_distribution";
                ]
                decoder)
          in
          let* response = RPC_core.call_raw endpoint rpc in
          let distribution =
            RPC_core.decode_raw ~origin:"Network.cycle" rpc response.body
          in
          let distribution =
            match max_nb_bakers with
            | None -> distribution
            | Some n -> Tezos_stdlib.TzList.take_n n distribution
          in
          return distribution)
  | Scatter _ | Map _ -> (
      match stake_arg with
      | Custom [] ->
          (* As simulate_network and stake are mutually exclusive, only empty
             stake option is allowed. *)
          Lwt.return []
      | _ ->
          Test.fail
            "Options --simulate and --stake are mutually exclusive. We cannot \
             set baker stake while using baking power of bakers from a \
             simulated network.")

let register (module Cli : Scenarios_cli.Dal) =
  let simulate_network = Cli.simulate_network in
  let stake =
    parse_stake_arg ~stake_arg:Cli.stake ~simulation_arg:simulate_network
  in
  let configuration, etherlink_configuration =
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
    let echo_rollup = Cli.echo_rollup in
    let disconnect = Cli.disconnect in
    let network = Cli.network in
    let snapshot = parse_snapshot_arg Cli.snapshot in
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
    let ppx_profiling = Cli.ppx_profiling in
    let ppx_profiling_backends = Cli.ppx_profiling_backends in
    let network_health_monitoring = Cli.enable_network_health_monitoring in
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
        echo_rollup;
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
        ignore_pkhs;
        ppx_profiling;
        ppx_profiling_backends;
        network_health_monitoring;
      }
    in
    (t, etherlink)
  in
  toplog "Parsing CLI done" ;
  let baker_daemon_count =
    match simulate_network with
    | Scenarios_cli.Disabled -> 0
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
           (if configuration.echo_rollup then
              Echo_rollup_operator :: Reverse_proxy
              :: List.map
                   (fun slot_index -> Echo_rollup_dal_observer {slot_index})
                   configuration.dal_node_producers
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
  let vms =
    let* vms in
    return
    @@ List.map
         (fun agent_kind ->
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
           | Observer _ | Etherlink_dal_operator | Etherlink_dal_observer _
           | Echo_rollup_dal_observer _ ->
               Agent.Configuration.make ?docker_image ~name ()
           | Echo_rollup_operator -> default_vm_configuration ~name
           | Etherlink_operator -> default_vm_configuration ~name
           | Etherlink_producer _ -> default_vm_configuration ~name
           | Reverse_proxy -> default_vm_configuration ~name)
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
        Monitoring_app.Tasks.register_chronos_task cloud ~configuration endpoint ;
      let* t = init ~configuration etherlink_configuration cloud next_agent in
      Lwt.wakeup resolver_endpoint t.some_node_rpc_endpoint ;
      toplog "Starting main loop" ;
      loop t (t.first_level + 1))
