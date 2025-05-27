(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

module Event = struct
  include Internal_event.Simple

  let section = ["p2p-node"; "main"]

  let disabled_discovery_addr =
    declare_0
      ~section
      ~name:"disabled_discovery_addr"
      ~msg:"disabled local peer discovery"
      ~level:Notice
      ()

  let starting_p2p_node =
    declare_2
      ~section
      ~name:"starting_p2p_node"
      ~msg:"starting the Octez P2P node {version} ({git_info})"
      ~level:Notice
      ~pp1:Tezos_version.Version.pp_simple
      ("version", Tezos_version.Octez_node_version.version_encoding)
      ("git_info", Data_encoding.string)

  let p2p_node_is_ready =
    declare_0
      ~section
      ~alternative_color:Internal_event.Green
      ~name:"p2p_node_is_ready"
      ~msg:"the Tezos P2P node is now running"
      ~level:Notice
      ()

  let bye =
    (* Note that "exit_code" may be negative in case of signals. *)
    declare_1
      ~section
      ~name:"bye"
      ~msg:"bye"
      ~level:Notice
      (* may be negative in case of signals *)
      ("exit_code", Data_encoding.int31)
end

let init_p2p_node ~listen_addr ?peers ?discovery_addr () =
  let open Lwt_result_syntax in
  let* discovery_addr, discovery_port =
    match discovery_addr with
    | None ->
        let*! () = Event.(emit disabled_discovery_addr) () in
        return (None, None)
    | Some addr -> (
        let* addrs = Config_file.resolve_discovery_addrs addr in
        match addrs with
        | [] -> failwith "Cannot resolve P2P discovery address: %S" addr
        | (addr, port) :: _ -> return (Some addr, Some port))
  in
  let* listening_addr, listening_port =
    let* addrs = Config_file.resolve_listening_addrs listen_addr in
    match addrs with
    | [] -> failwith "Cannot resolve P2P listening address: %S" listen_addr
    | (addr, port) :: _ -> return (Some addr, Some port)
  in
  let* trusted_points =
    peers |> Option.value ~default:[]
    |> List.map_es (fun peer ->
           let* addrs = Config_file.resolve_listening_addrs peer in
           match addrs with
           | [] -> failwith "Cannot resolve peer address: %S" peer
           | peer :: _ -> return (peer, None))
  in
  (* Lower the targetted connections. *)
  let limits =
    P2p_limits.{default with min_connections = 1; expected_connections = 1}
  in
  let p2p_params : P2p.config * P2p_limits.t =
    ( {
        listening_addr;
        listening_port;
        advertised_port = listening_port;
        discovery_addr;
        discovery_port;
        trusted_points;
        peers_file = "";
        private_mode = false;
        reconnection_config = Point_reconnection_config.default;
        identity = P2p_identity.generate_with_pow_target_0 ();
        proof_of_work_target = Tezos_crypto.Crypto_box.make_pow_target 0.;
        trust_discovered_peers = true;
        disable_peer_discovery = Option.is_none discovery_addr;
      },
      limits )
  in
  P2p_node.create p2p_params

let run ~listen_addr ?peers ?discovery_addr () =
  let open Lwt_result_syntax in
  (* Main loop *)
  let*! () = Tezos_base_unix.Internal_event_unix.init () in
  let*! () =
    Event.(emit starting_p2p_node)
      ( Tezos_version_value.Current_git_info.octez_version,
        Tezos_version_value.Current_git_info.abbreviated_commit_hash )
  in
  let* _p2p_node = init_p2p_node ~listen_addr ?peers ?discovery_addr () in
  let _ =
    Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun exit_status ->
        let*! () = Event.(emit bye) exit_status in
        Tezos_base_unix.Internal_event_unix.close ())
  in
  let*! () = Event.(emit p2p_node_is_ready) () in
  let*! () = Lwt_utils.never_ending () in
  return_unit
