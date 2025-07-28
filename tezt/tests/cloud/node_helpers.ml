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

let may_add_migration_offset_to_config node snapshot ~migration_offset ~network
    =
  let* level = get_snapshot_info_level node snapshot in
  match migration_offset with
  | None -> Lwt.return_unit
  | Some migration_offset ->
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

let init ?(arguments = []) ?data_dir ?identity_file ?dal_config ?env
    ~rpc_external ~name network ~with_yes_crypto ~snapshot ?ppx_profiling cloud
    agent =
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
      Node.Network (to_octez_network_options @@ to_public network) :: arguments
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
  match network with
  | #Network.public -> (
      let network = Network.to_public network in
      match data_dir with
      | Some _ ->
          let* () = may_copy_node_identity_file agent node identity_file in
          let* () = Node.Agent.run ?ppx_profiling ?env node arguments in
          let* () = Node.wait_for_ready node in
          Lwt.return node
      | None ->
          toplog
            "No data dir given, we will attempt to bootstrap the node from a \
             rolling snapshot." ;
          let* () = may_copy_node_identity_file agent node identity_file in
          toplog "Initializing node configuration for %s" name ;
          let* () = Node.config_init node [] in
          let* snapshot_file_path =
            ensure_snapshot ~agent ~name ~network snapshot
          in
          let* () =
            import_snapshot
              ~delete_snapshot_file:(snapshot = No_snapshot)
              ~no_check:true
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
              (* We've just imported a rolling snapshot keeping few history.
                 To switch to the configured history mode, which may have
                 longer history, we need the --force-history-mode-switch
                 option. *)
              (Force_history_mode_switch :: Synchronisation_threshold 1
             :: Expected_pow 26 :: Cors_origin "*" :: arguments)
          in
          toplog "Waiting for the node %s to be ready." name ;
          let* () = Node.wait_for_ready node in
          toplog "Node %s is ready." name ;
          let* () = Node.wait_for_synchronisation ~statuses:["synced"] node in
          toplog "Node %s is bootstrapped" name ;
          Lwt.return node)
  | _ (* private network *) -> (
      let yes_crypto_arg =
        if with_yes_crypto then [Node.Allow_yes_crypto] else []
      in
      match data_dir with
      | None ->
          let* () = Node.config_init node [Cors_origin "*"] in
          let* snapshot_path = ensure_snapshot_opt ~agent ~name snapshot in
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
              | Some "seoulnet" -> Node.Config_file.set_seoulnet_network ()
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
            | Some snapshot_path ->
                import_snapshot ~no_check:true ~name node snapshot_path
            | None -> Lwt.return_unit
          in
          let* () =
            Node.Agent.run
              ?ppx_profiling
              ?env
              node
              (Node.
                 [
                   No_bootstrap_peers;
                   Synchronisation_threshold 0;
                   Cors_origin "*";
                   (* We've just imported a rolling snapshot keeping few
                      history. To switch to the configured history mode, which
                      may have longer history, we need the
                      --force-history-mode-switch option. *)
                   Force_history_mode_switch;
                 ]
              @ yes_crypto_arg @ arguments)
          in
          let* () = Node.wait_for_ready node in
          Lwt.return node
      | Some _ ->
          let arguments =
            Node.
              [No_bootstrap_peers; Synchronisation_threshold 0; Cors_origin "*"]
            @ yes_crypto_arg @ arguments
          in
          let* () = may_copy_node_identity_file agent node identity_file in
          let* () = Node.Agent.run ?env ?ppx_profiling node arguments in
          let* () = Node.wait_for_ready node in
          Lwt.return node)
