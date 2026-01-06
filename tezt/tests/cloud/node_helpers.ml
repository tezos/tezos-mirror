(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Network
open Scenarios_helpers
open Snapshot_helpers
open Tezos

let may_copy_node_identity_file agent node = function
  | None -> Lwt.return_unit
  | Some source ->
      toplog "Copying the node identity file" ;
      let* _ =
        Agent.copy agent ~source ~destination:(Node.identity_file node)
      in
      Lwt.return_unit

let yes_wallet agent =
  let name = Tezt_cloud.Agent.name agent ^ "-yes-wallet" in
  Yes_wallet.Agent.create ~name agent

(** We are running a private network with yes-crypto enabled.
    We don't want to connect with the real network. *)
let isolated_config ~auto_synchronisation_threshold ~auto_connections
    ~no_bootstrap_peers ~peers ~network ~delay =
  Node.
    [
      Network Network.(to_octez_network_options network);
      Expected_pow 0;
      Cors_origin "*";
      Storage_maintenance_delay (string_of_int delay);
    ]
  @ (if auto_synchronisation_threshold then []
     else
       [Node.Synchronisation_threshold (if List.length peers < 2 then 1 else 2)])
  @ (if auto_connections then []
     else [Node.Connections (min 100 (List.length peers))])
  @ if no_bootstrap_peers then [No_bootstrap_peers] else []

(** [--private-mode] is mainly useful for the bootstrap node
    because it is used first to bootstrap a node with real network peers
    before being disconnected.
    For the other node, it's an extra security but their ip/identity should
    not be advertised to the external world anyway. *)
let isolated_args ~private_mode peers =
  Node.(
    List.fold_left
      (fun acc peer -> Peer peer :: acc)
      ([Allow_yes_crypto; Force_history_mode_switch]
      @ if private_mode then [Private_mode] else [])
      peers)

let may_add_migration_offset_to_config node snapshot
    ~(migration_offset : Protocol_migration.offset option) ~network =
  let* level = get_snapshot_info_level node snapshot in
  match migration_offset with
  | None -> Lwt.return_unit
  | Some (Cycle_offset _) ->
      (* Migration_offset with Cycle should have been replaced by the proper level
       at the beginning of the experiment *)
      assert false
  | Some (Level_offset migration_offset) ->
      let* network_config =
        match network with
        | `Mainnet -> Lwt.return Node.Config_file.mainnet_network_config
        | `Ghostnet -> Lwt.return Node.Config_file.ghostnet_network_config
        | _ ->
            Lwt.fail_with
              "Migration scenarios are only supported for Mainnet and Ghostnet."
      in
      let migration_level = level + migration_offset in
      toplog "Add UAU entry for level : %d" migration_level ;
      Node.Config_file.update node (fun json ->
          JSON.put
            ( "network",
              JSON.annotate
                ~origin:"add_migration_offset_to_config"
                network_config )
            json
          |> Node.Config_file.update_network_with_user_activated_upgrades
               [(migration_level, Network.next_protocol network)])

let may_init_from_snapshot node ?data_dir ?dal_config ~network ~snapshot
    ?migration_offset ~name agent =
  match data_dir with
  | Some _ -> Lwt.return_unit
  | None -> (
      toplog
        "No data dir given, we will attempt to bootstrap the node from a \
         rolling snapshot." ;
      toplog "Initializing node configuration for %s" name ;
      let* () = Node.config_init node [Cors_origin "*"] in
      let* snapshot_file_path =
        if is_public network then
          let* snapshot_file_path =
            ensure_snapshot ~agent ~name ~network:(to_public network) snapshot
          in
          Lwt.return_some snapshot_file_path
        else
          let* snapshot_file_path = ensure_snapshot_opt ~agent ~name snapshot in
          let* snapshot_network =
            match snapshot_file_path with
            | Some path ->
                let* network = get_snapshot_info_network node path in
                Lwt.return_some network
            | None -> Lwt.return_none
          in
          (* Set network *)
          let* () =
            Node.Config_file.(
              update
                node
                (match snapshot_network with
                | Some "mainnet" -> set_mainnet_network ()
                | Some "ghostnet" -> set_ghostnet_network ()
                | Some "shadownet" -> set_shadownet_network ()
                | Some "seoulnet" -> set_seoulnet_network ()
                | Some "tallinnnet" -> set_tallinnnet_network ()
                | _ -> set_sandbox_network))
          in
          let* () =
            match dal_config with
            | None -> Lwt.return_unit
            | Some config ->
                Node.Config_file.(
                  update node (set_network_with_dal_config config))
          in
          Lwt.return snapshot_file_path
      in
      match snapshot_file_path with
      | Some snapshot_file_path ->
          let* () =
            may_add_migration_offset_to_config
              ~migration_offset
              ~network
              node
              snapshot_file_path
          in
          import_snapshot
            ~delete_snapshot_file:(snapshot = No_snapshot)
            ~no_check:true
            ~name
            node
            snapshot_file_path
      | None -> Lwt.return_unit)

let init ?(arguments = []) ?data_dir ?identity_file ?dal_config ?env
    ?migration_offset ~rpc_external ~name network ~with_yes_crypto ~snapshot
    ~ppx_profiling_verbosity ~ppx_profiling_backends cloud agent =
  toplog "Initializing an L1 node for %s" name ;
  let net_addr =
    if is_public network then
      (* for public networks deployments, we listen on all interfaces on both
         ipv4 and ipv6 *)
      "[::]"
      (* For sandbox deployments, we only listen on local interface, hence
         no connection could be made to us from outside networks. *)
    else "127.0.0.1"
  in
  let arguments =
    if is_public network then
      Node.Network (to_octez_network_options network) :: arguments
    else arguments
  in
  toplog "Creating the agent %s." name ;
  let* node =
    Node.Agent.create
      ~rpc_external
      ~net_addr
      ~arguments
      ?data_dir
      ~name
      cloud
      agent
  in
  let* () =
    may_init_from_snapshot
      node
      ?data_dir
      ?dal_config
      ~network
      ~snapshot
      ?migration_offset
      ~name
      agent
  in
  let* () = may_copy_node_identity_file agent node identity_file in
  let arguments =
    if Option.is_none data_dir then
      (* We've just imported a rolling snapshot keeping few history.
         To switch to the configured history mode, which may have
         longer history, we need the --force-history-mode-switch
         option. *)
      Node.Force_history_mode_switch :: arguments
    else arguments
  in
  let arguments =
    if is_public network then
      Node.[Synchronisation_threshold 1; Cors_origin "*"; Expected_pow 26]
      @ arguments
    else
      let yes_crypto_arg =
        if with_yes_crypto then Node.[Allow_yes_crypto] else []
      in
      Node.[No_bootstrap_peers; Synchronisation_threshold 0; Cors_origin "*"]
      @ yes_crypto_arg @ arguments
  in
  toplog "Launching the node %s." name ;
  let* () =
    Node.Agent.run
      ~ppx_profiling_verbosity
      ~ppx_profiling_backends
      ?env
      node
      arguments
  in
  toplog "Waiting for the node %s to be ready." name ;
  let* () = Node.wait_for_ready node in
  toplog "Node %s is ready." name ;
  let* () =
    if is_public network && Option.is_none data_dir then
      Node.wait_for_synchronisation ~statuses:["synced"] node
    else Lwt.return_unit
  in
  Lwt.return node
