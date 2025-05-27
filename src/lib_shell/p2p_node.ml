(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

type error += Failed_to_init_P2P_node

let () =
  register_error_kind
    `Permanent
    ~id:"main.run.failed_to_init_p2p_node"
    ~title:"Cannot start P2P node: P2P initialization failed"
    ~description:
      "Tezos P2P node could not be started because of a network problem while \
       initializing P2P."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Tezos P2P node could not be started because of a network problem.")
    Data_encoding.(obj1 @@ req "error" @@ constant "Failed_to_init_P2P")
    (function Failed_to_init_P2P_node -> Some () | _ -> None)
    (fun () -> Failed_to_init_P2P_node)

(* Peer metadata configuration *)

let peer_metadata_cfg =
  P2p_params.
    {
      peer_meta_encoding = Peer_metadata.encoding;
      peer_meta_initial = Peer_metadata.empty;
      score = Peer_metadata.score;
    }

(* Connection metadata configuration*)

let connection_metadata_cfg cfg =
  P2p_params.
    {
      conn_meta_encoding = Connection_metadata.encoding;
      private_node = (fun {private_node; _} -> private_node);
      conn_meta_value = (fun () -> cfg);
    }

let init_connection_metadata opt disable_mempool =
  let open Connection_metadata in
  match opt with
  | None -> {disable_mempool = false; private_node = false}
  | Some c -> {disable_mempool; private_node = c.P2p.private_mode}

(* Message configuration *)

type message = Text of string | Bytes of bytes

let message_encoding =
  let open Data_encoding in
  let case ?max_length ~tag ~title encoding unwrap wrap =
    P2p_params.Encoding {tag; title; encoding; wrap; unwrap; max_length}
  in
  [
    case
      ~tag:0x10
      ~title:"Text"
      (obj1 (req "content" string))
      (function Text s -> Some s | _ -> None)
      (fun s -> Text s);
    case
      ~tag:0x11
      ~title:"Bytes"
      (obj1 (req "content" bytes))
      (function Bytes b -> Some b | _ -> None)
      (fun b -> Bytes b);
  ]

let message_cfg =
  P2p_params.
    {
      encoding = message_encoding;
      chain_name = Distributed_db_version.Name.of_string "";
      distributed_db_versions = [Distributed_db_version.zero];
    }

type t = {
  p2p : (message, Peer_metadata.t, Connection_metadata.t) P2p.t;
  shutdown : unit -> unit Lwt.t;
}

let log_connections p2p =
  let incoming, _ = P2p.watcher p2p in
  Lwt_stream.iter_p
    (fun ev ->
      match ev with
      | P2p_connection.P2p_event.Incoming_connection point_id ->
          P2p_node_event.(emit connection_incoming) point_id
      | Connection_established (point_id, peer) ->
          P2p_node_event.(emit connection_established)
            (peer, P2p_connection.Id.to_string point_id)
      | Disconnection peer -> P2p_node_event.(emit disconnected) peer
      | _ -> Lwt.return_unit)
    incoming
  |> ignore

let init_p2p (p2p_config, limits) =
  let open Lwt_result_syntax in
  let c_meta = init_connection_metadata (Some p2p_config) true in
  let conn_metadata_cfg = connection_metadata_cfg c_meta in
  let*! () = P2p_node_event.(emit bootstrapping) () in
  let* p2p =
    P2p.create
      ~config:p2p_config
      ~limits
      peer_metadata_cfg
      conn_metadata_cfg
      message_cfg
  in
  let*! () = P2p_node_event.(emit maintenance_started) () in

  let () = P2p.activate p2p in
  let () = log_connections p2p in

  return p2p |> trace Failed_to_init_P2P_node

let create p2p_params =
  let open Lwt_result_syntax in
  let* p2p = init_p2p p2p_params in
  let shutdown () =
    let*! () = P2p_node_event.(emit shutdown) () in
    let*! () = P2p.shutdown p2p in
    Lwt.return_unit
  in
  return {p2p; shutdown}

let shutdown p2p_node = p2p_node.shutdown ()
