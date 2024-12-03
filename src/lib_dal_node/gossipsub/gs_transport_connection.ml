(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Functori,     <contact@functori.com>                   *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

module Worker = Gs_interface.Worker_instance
open Gs_interface.Worker_instance

module Events = struct
  include Internal_event.Simple

  let section = ["gossipsub"; "transport"; "event"]

  let prefix =
    let prefix = String.concat "_" section in
    fun s -> prefix ^ "-" ^ s

  let no_connection_for_peer =
    declare_1
      ~section
      ~name:(prefix "no_connection_for_peer")
      ~msg:"No running connection found for peer {peer}"
      ~level:Notice
      ~pp1:P2p_peer.Id.pp
      ("peer", P2p_peer.Id.encoding)

  let message_notified_to_app =
    declare_1
      ~section
      ~name:(prefix "message_notified_to_app")
      ~msg:"Successfully notified message id {message_id} to the application"
      ~level:Info
      ~pp1:Worker.GS.Message_id.pp
      ("message_id", Types.Message_id.encoding)

  let app_message_callback_failed =
    declare_2
      ~section
      ~name:(prefix "app_message_callback_failed")
      ~msg:"Callback failed for message id {message_id}. Failure is {failure}"
      ~level:Warning
      ~pp1:Worker.GS.Message_id.pp
      ~pp2:pp_print_trace
      ("message_id", Types.Message_id.encoding)
      ("failure", trace_encoding)

  let send_p2p_message_failed =
    declare_2
      ~section
      ~name:(prefix "p2p_send_failed")
      ~msg:"Sending P2P message to {peer} failed with error {failure}"
      ~level:Warning
      ~pp1:P2p_peer.Id.pp
      ~pp2:pp_print_trace
      ("peer", P2p_peer.Id.encoding)
      ("failure", trace_encoding)

  let send_p2p_message =
    declare_2
      ~section
      ~name:(prefix "p2p_send")
      ~msg:"Sending to {peer} P2P message {message}"
      ~level:Debug
      ~pp1:P2p_peer.Id.pp
      ~pp2:Transport_layer_interface.pp_p2p_message
      ("peer", P2p_peer.Id.encoding)
      ("message", Transport_layer_interface.p2p_message_encoding)
end

let peer_of_connection p2p_layer conn =
  let open Types.P2P.Metadata.Connection in
  let {P2p_connection.Info.peer_id; id_point; remote_metadata; _} =
    P2p.connection_info p2p_layer conn
  in
  let default_port =
    Transport_layer_default_parameters.P2p_config.listening_port
  in
  let addr =
    Option.value remote_metadata.advertised_net_addr ~default:(fst id_point)
  in
  let port =
    Option.value remote_metadata.advertised_net_port ~default:default_port
  in
  let maybe_reachable_point = (addr, port) in
  Types.Peer.{peer_id; maybe_reachable_point}

(** This handler forwards information about connections established by the P2P
    layer to the Gossipsub worker.

    Note that, a connection is considered [outbound] only if we initiated it and
    we trust the point or the peer we are connecting to. Consequently, PX peers
    are not considered outgoing connections by default, as they are not trusted
    unless explicitly specified otherwise.

    Indeed, Gossipsub tries to maintain a threshold of outbound connections per
    topic. So, we don't automatically set connections we initiate to PX peers as
    outbound to avoid possible love bombing attacks. The Rust version also
    implements a way to mitigate this risk, but not the Go implementation.
*)
let new_connections_handler gs_worker p2p_layer peer_id conn =
  let P2p_connection.Info.{id_point = addr, port_opt; _} =
    P2p.connection_info p2p_layer conn
  in
  let Types.P2P.Metadata.Connection.{is_bootstrap_peer = bootstrap; _} =
    P2p.connection_remote_metadata p2p_layer conn
  in
  let pool_opt = P2p.pool p2p_layer in
  let fold_pool_opt f arg =
    Option.fold
      pool_opt
      ~none:true (* It doesn't matter in fake networks where pool is None *)
      ~some:(fun pool -> f pool arg)
  in
  let trusted_peer = fold_pool_opt P2p_pool.Peers.get_trusted peer_id in
  let trusted_point =
    Option.fold port_opt ~none:false ~some:(fun port ->
        fold_pool_opt P2p_pool.Points.get_trusted (addr, port))
  in
  let trusted = trusted_peer || trusted_point in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/5584

     Add the ability to have direct peers. *)
  let direct = false in
  let peer = peer_of_connection p2p_layer conn in
  Worker.(
    New_connection {peer; direct; trusted; bootstrap} |> p2p_input gs_worker)

(** This handler forwards information about P2P disconnections to the Gossipsub
    worker. *)
let disconnections_handler gs_worker peer_id =
  let open GS.Introspection in
  (* When this callback is called, we only have the [peer_id] and not
     the [maybe_reachable_point].

     It can be reconstructed in many ways:

     - We could do it via the octez-p2p

     - We can find it in the automaton state (choosen option)

     Last option does not have the good complexity but is simple enough.
  *)
  let view = Worker.state gs_worker in
  let {connections; _} = view in
  (* Complexity is wrong, but the number of connections should not be
     too large, so it should be ok in practice. *)
  let bindings = Connections.bindings connections in
  let value =
    List.find_opt
      (fun (peer, _) -> P2p_peer.Id.equal peer.Types.Peer.peer_id peer_id)
      bindings
  in
  match value with
  | None -> (* Something is off, we should log something probably. *) ()
  | Some (peer, _) -> Worker.(Disconnection {peer} |> p2p_input gs_worker)

(* This function translates a Worker p2p_message to the type of messages sent
   via the P2P layer. The two types don't coincide because of Prune. *)
let wrap_p2p_message _p2p_layer =
  let module W = Worker in
  let open Transport_layer_interface in
  function
  | W.Graft {topic} -> Graft {topic}
  | W.Prune {topic; px; backoff} -> Prune {topic; px; backoff}
  | W.IHave {topic; message_ids} -> IHave {topic; message_ids}
  | W.IWant {message_ids} -> IWant {message_ids}
  | W.Subscribe {topic} -> Subscribe {topic}
  | W.Unsubscribe {topic} -> Unsubscribe {topic}
  | W.Message_with_header {message; topic; message_id} ->
      Message_with_header {message; topic; message_id}

(* This function translates a message received via the P2P layer to a Worker
   p2p_message. The two types don't coincide because of Prune. *)
let unwrap_p2p_message _p2p_layer ~from_peer:_ =
  let open Worker in
  let module I = Transport_layer_interface in
  function
  | I.Graft {topic} -> Graft {topic}
  | I.Prune {topic; px; backoff} -> Prune {topic; px; backoff}
  | I.IHave {topic; message_ids} -> IHave {topic; message_ids}
  | I.IWant {message_ids} -> IWant {message_ids}
  | I.Subscribe {topic} -> Subscribe {topic}
  | I.Unsubscribe {topic} -> Unsubscribe {topic}
  | I.Message_with_header {message; topic; message_id} ->
      Message_with_header {message; topic; message_id}

let try_connect ?expected_peer_id p2p_layer point =
  let open Lwt_syntax in
  (* We don't wait for the promise to resolve here, because if the
     advertised peer is not reachable or is not responding, we might block
     until connection timeout is reached (we observed a timeout of 10
     seconds in some case). Blocking here means that processing of other
     messages from p2p_output_stream (including shards propagation) will
     be delayed. *)
  Lwt.dont_wait
    (fun () ->
      let* (_ : _ P2p.connection tzresult) =
        P2p.connect ?expected_peer_id p2p_layer point
      in
      return_unit)
    (fun exn ->
      Format.eprintf
        "Warning: got an exception while trying to connect to %a: %s@."
        Point.pp
        point
        (Printexc.to_string exn))
  |> return

(** This handler pops and processes the items put by the worker in the p2p
    output stream. The out messages are sent to the corresponding peers and the
    directives to the P2P layer to connect or disconnect peers are handled. *)
let gs_worker_p2p_output_handler gs_worker p2p_layer =
  let open Lwt_syntax in
  (* only log sending of GS control messages  *)
  let log_sending_message = function
    | Message_with_header _ -> false
    | _ -> true
  in
  let rec loop output_stream =
    let* p2p_output = Worker.Stream.pop output_stream in
    let* () =
      match p2p_output with
      | Worker.Out_message {to_peer; p2p_message} -> (
          let conn = P2p.find_connection_by_peer_id p2p_layer to_peer.peer_id in
          match conn with
          | None ->
              (* This could happen when the peer is disconnected or the
                 connection is accepted but not running (authenticated) yet. *)
              (* TODO: https://gitlab.com/tezos/tezos/-/issues/5649

                 Are there weird cases in which there is no connection
                 associated to the peer, but the peer is still registered as
                 connected on the GS side? *)
              Events.(emit no_connection_for_peer to_peer.peer_id)
          | Some conn -> (
              let* (res : unit tzresult) =
                let msg = wrap_p2p_message p2p_layer p2p_message in
                let* () =
                  if log_sending_message p2p_message then
                    Events.(emit send_p2p_message (to_peer.peer_id, msg))
                  else return_unit
                in
                P2p.send p2p_layer conn msg
              in
              match res with
              | Ok () -> return_unit
              | Error err ->
                  Events.(emit send_p2p_message_failed (to_peer.peer_id, err))))
      | Disconnect {peer} ->
          P2p.find_connection_by_peer_id p2p_layer peer.peer_id
          |> Option.iter_s
               (P2p.disconnect ~reason:"disconnected by Gossipsub" p2p_layer)
      | Connect {peer; origin = _} ->
          let Types.Peer.{maybe_reachable_point; peer_id} = peer in
          try_connect ~expected_peer_id:peer_id p2p_layer maybe_reachable_point
      | Connect_point {point} -> try_connect p2p_layer point
      | Forget _ -> return_unit
      | Kick {peer} ->
          P2p.pool p2p_layer
          |> Option.iter_s (fun pool -> P2p_pool.Peers.ban pool peer.peer_id)
    in
    loop output_stream
  in
  Worker.p2p_output_stream gs_worker |> loop

(** This handler forwards p2p messages received via Octez p2p to the Gossipsub
    worker. *)
let transport_layer_inputs_handler gs_worker p2p_layer =
  let open Lwt_syntax in
  let rec loop () =
    let* conn, msg = P2p.recv_any p2p_layer in
    let from_peer = peer_of_connection p2p_layer conn in
    Worker.(
      In_message
        {from_peer; p2p_message = unwrap_p2p_message p2p_layer ~from_peer msg}
      |> p2p_input gs_worker) ;
    loop ()
  in
  loop ()

(** This loop pops messages from application output stream and calls the given
    [app_messages_callback] on them. *)
let app_messages_handler gs_worker ~app_messages_callback =
  let open Lwt_syntax in
  let rec loop app_output_stream =
    let* Worker.{message; message_id; topic = _} =
      Worker.Stream.pop app_output_stream
    in
    let* res = app_messages_callback message message_id in
    let* () =
      match res with
      | Ok () -> Events.(emit message_notified_to_app message_id)
      | Error err -> Events.(emit app_message_callback_failed (message_id, err))
    in
    loop app_output_stream
  in
  Worker.app_output_stream gs_worker |> loop

let activate gs_worker p2p_layer ~app_messages_callback =
  (* Register a handler to notify new P2P connections to GS. *)
  let () =
    new_connections_handler gs_worker p2p_layer
    |> P2p.on_new_connection p2p_layer
  in
  (* Register a handler to notify P2P disconnections to GS. *)
  let () = disconnections_handler gs_worker |> P2p.on_disconnection p2p_layer in
  Lwt.join
    [
      gs_worker_p2p_output_handler gs_worker p2p_layer;
      transport_layer_inputs_handler gs_worker p2p_layer;
      app_messages_handler gs_worker ~app_messages_callback;
    ]
