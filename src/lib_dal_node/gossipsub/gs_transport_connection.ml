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
end

(** This module implements a cache of alternative peers (alternative PX)
    advertised by P2P neighbors within Prune messages.

    The cache associates a P2P point to each pair made of the advertised peer
    and the peer that advertised it. In case of redundancy, the last advertised
    point is kept in the cache.

    The cache is in theory not bounded. However, some invariants on the way
    {!insert} and {!drop} are used below ensures that we remove the added entries very
    quickly.
*)
module PX_cache : sig
  (** The cache data structure for advertised alternative PXs. *)
  type t

  type origin = Worker.peer_origin

  (** Create a new cache data structure. The [size] parameter is an indication
      on the size of internal table storing data. Its default value is
      [2048]. *)
  val create : ?size:int -> unit -> t

  (** [insert t ~origin ~px point] associates the given [point] to [(origin,
      px)] pair of peers. If a point already exists for the pair, it is
      overwritten.  *)
  val insert : t -> origin:origin -> px:P2p_peer.Id.t -> P2p_point.Id.t -> unit

  (** [find_opt t ~origin ~px] returns the content associated to the entry
      [(origin, px)] in the cache, if any. *)
  val find_opt : t -> origin:origin -> px:P2p_peer.Id.t -> P2p_point.Id.t option

  (** [drop t ~origin ~px] drops the entry [(origin, px)] from the cache. *)
  val drop : t -> origin:origin -> px:P2p_peer.Id.t -> unit
end = struct
  type origin = Worker.peer_origin

  type key = {origin : origin; px : P2p_peer.Id.t}

  module Table = Hashtbl.Make (struct
    type t = key

    let equal {origin; px} k2 =
      P2p_peer.Id.equal px k2.px
      &&
      match (origin, k2.origin) with
      | PX peer1, PX peer2 -> P2p_peer.Id.equal peer1 peer2
      | Trusted, Trusted -> true
      | PX _, _ | Trusted, _ -> false

    let hash {origin; px} =
      match origin with
      | PX peer -> (P2p_peer.Id.hash px + P2p_peer.Id.hash peer) * 2
      | Trusted -> P2p_peer.Id.hash px * 3
  end)

  type t = P2p_point.Id.t Table.t

  let create ?(size = 2048) () = Table.create size

  let insert table ~origin ~px point = Table.replace table {px; origin} point

  let drop table ~origin ~px = Table.remove table {origin; px}

  let find_opt table ~origin ~px = Table.find_opt table {origin; px}
end

(* [px_of_peer p2p_layer peer] returns the public IP address and port at which
   [peer] could be reached. For that, it first inspects information transmitted
   via connection metadata by the remote [peer]. If the address or port are
   missing from the connection's metadata, the function inspects the Internet
   connection link's information. It returns [None] if it does manage to get
   those information with both methods. *)
let px_of_peer p2p_layer peer =
  let open Option_syntax in
  let open Transport_layer_interface in
  let* conn = P2p.find_connection_by_peer_id p2p_layer peer in
  (* In general, people either provide an address and a port, or just a port.
     In any case, we use `P2p.connection_remote_metadata` to get the address and
     the port of the provided values, and we fall back to the values given by
     `P2p.connection_info` if the former are not available
     (respectively/independently for the address and the port).  The first case
     is covered by {!P2p.connection_remote_metadata}. But if the IP address is
     not explicitly given, we rely on the function {!P2p.connection_info
     p2p_layer conn}. *)
  let Types.P2P.Metadata.Connection.
        {advertised_net_addr; advertised_net_port; is_bootstrap_peer = _} =
    P2p.connection_remote_metadata p2p_layer conn
  in
  let {P2p_connection.Info.id_point = conn_addr, conn_port_opt; _} =
    P2p.connection_info p2p_layer conn
  in
  let addr = Option.value advertised_net_addr ~default:conn_addr in
  let* port = Option.either advertised_net_port conn_port_opt in
  return {point = (addr, port); peer}

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
let new_connections_handler gs_worker p2p_layer peer conn =
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
  let trusted_peer = fold_pool_opt P2p_pool.Peers.get_trusted peer in
  let trusted_point =
    Option.fold port_opt ~none:false ~some:(fun port ->
        fold_pool_opt P2p_pool.Points.get_trusted (addr, port))
  in
  let trusted = trusted_peer || trusted_point in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/5584

     Add the ability to have direct peers. *)
  let direct = false in
  Worker.(
    New_connection {peer; direct; trusted; bootstrap} |> p2p_input gs_worker)

(** This handler forwards information about P2P disconnections to the Gossipsub
    worker. *)
let disconnections_handler gs_worker peer =
  Worker.(Disconnection {peer} |> p2p_input gs_worker)

(* This function translates a Worker p2p_message to the type of messages sent
   via the P2P layer. The two types don't coincide because of Prune. *)
let wrap_p2p_message p2p_layer =
  let module W = Worker in
  let open Transport_layer_interface in
  function
  | W.Graft {topic} -> Graft {topic}
  | W.Prune {topic; px; backoff} ->
      let px = Seq.filter_map (fun peer -> px_of_peer p2p_layer peer) px in
      Prune {topic; px; backoff}
  | W.IHave {topic; message_ids} -> IHave {topic; message_ids}
  | W.IWant {message_ids} -> IWant {message_ids}
  | W.Subscribe {topic} -> Subscribe {topic}
  | W.Unsubscribe {topic} -> Unsubscribe {topic}
  | W.Message_with_header {message; topic; message_id} ->
      Message_with_header {message; topic; message_id}

(* This function translates a message received via the P2P layer to a Worker
   p2p_message. The two types don't coincide because of Prune. *)
let unwrap_p2p_message p2p_layer ~from_peer px_cache =
  let open Worker in
  let module I = Transport_layer_interface in
  function
  | I.Graft {topic} -> Graft {topic}
  | I.Prune {topic; px; backoff} ->
      let px =
        Seq.map
          (fun I.{point; peer} ->
            if Option.is_none @@ P2p.find_connection_by_peer_id p2p_layer peer
            then PX_cache.insert px_cache ~origin:(PX from_peer) ~px:peer point ;
            peer)
          px
      in
      Prune {topic; px; backoff}
  | I.IHave {topic; message_ids} -> IHave {topic; message_ids}
  | I.IWant {message_ids} -> IWant {message_ids}
  | I.Subscribe {topic} -> Subscribe {topic}
  | I.Unsubscribe {topic} -> Unsubscribe {topic}
  | I.Message_with_header {message; topic; message_id} ->
      Message_with_header {message; topic; message_id}

let try_connect_to_peer p2p_layer px_cache ~px ~origin =
  let open Lwt_syntax in
  (* If there is some [point] associated to [px] and advertised by [origin]
     on the [px_cache], we will try to connect to it. *)
  match PX_cache.find_opt px_cache ~px ~origin with
  | Some point ->
      (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5799

         We may have an issues as described by the following scenario:
         - A legit pair [(point, peer)] is already known by P2P, but the remote
         peer is disconnected for some reason;
         - Some (malicious) peer advertises a fake px [(point, peer')], where
         [peer'] is not the real peer id associated to the [point];
         - We try to connect to [point], setting at the same time that [peer']
         is the expected peer id for this point;
         - The connection fails, but the old (correct) association [(point,
         peer)] is lost.

         We may want to revert [peer'] association to [point]. But we sill have
         an issue in case [(point, peer)] is actually the fake info and [(point,
         peer')] the legit association but the node is disconnected when trying
         to connect to it.

         This implementation will be hardened once we add the notion of "signed
         records" found, e.g., in Rust version, to check that the advertised
         (peer, point) pair alongside a timestamp are not faked. *)
      let* (_ : _ P2p.connection tzresult) =
        P2p.connect ~expected_peer_id:px p2p_layer point
      in
      (match origin with
      | Trusted -> () (* Don't drop trusted points. *)
      | PX _ -> PX_cache.drop px_cache ~px ~origin) ;
      return_unit
  | _ -> return_unit

(** This handler pops and processes the items put by the worker in the p2p
    output stream. The out messages are sent to the corresponding peers and the
    directives to the P2P layer to connect or disconnect peers are handled. *)
let gs_worker_p2p_output_handler gs_worker p2p_layer px_cache =
  let open Lwt_syntax in
  let rec loop output_stream =
    let* p2p_output = Worker.Stream.pop output_stream in
    let* () =
      match p2p_output with
      | Worker.Out_message {to_peer; p2p_message} -> (
          let conn = P2p.find_connection_by_peer_id p2p_layer to_peer in
          match conn with
          | None ->
              (* This could happen when the peer is disconnected or the
                 connection is accepted but not running (authenticated) yet. *)
              (* TODO: https://gitlab.com/tezos/tezos/-/issues/5649

                 Are there weird cases in which there is no connection
                 associated to the peer, but the peer is still registered as
                 connected on the GS side? *)
              Events.(emit no_connection_for_peer to_peer)
          | Some conn -> (
              let* (res : unit tzresult) =
                wrap_p2p_message p2p_layer p2p_message
                |> P2p.send p2p_layer conn
              in
              match res with
              | Ok () -> return_unit
              | Error err ->
                  Events.(emit send_p2p_message_failed (to_peer, err))))
      | Disconnect {peer} ->
          P2p.find_connection_by_peer_id p2p_layer peer
          |> Option.iter_s
               (P2p.disconnect ~reason:"disconnected by Gossipsub" p2p_layer)
      | Connect {px; origin} ->
          try_connect_to_peer p2p_layer px_cache ~px ~origin
      | Forget {px; origin} ->
          PX_cache.drop px_cache ~px ~origin:(PX origin) ;
          return_unit
      | Kick {peer} ->
          P2p.pool p2p_layer
          |> Option.iter_s (fun pool -> P2p_pool.Peers.ban pool peer)
    in
    loop output_stream
  in
  Worker.p2p_output_stream gs_worker |> loop

(** This handler forwards p2p messages received via Octez p2p to the Gossipsub
    worker. *)
let transport_layer_inputs_handler gs_worker p2p_layer advertised_px_cache =
  let open Lwt_syntax in
  let rec loop () =
    let* conn, msg = P2p.recv_any p2p_layer in
    let {P2p_connection.Info.peer_id = from_peer; _} =
      P2p.connection_info p2p_layer conn
    in
    Worker.(
      In_message
        {
          from_peer;
          p2p_message =
            unwrap_p2p_message p2p_layer ~from_peer advertised_px_cache msg;
        }
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
  let px_cache = PX_cache.create () in
  (* Register a handler to notify new P2P connections to GS. *)
  let () =
    new_connections_handler gs_worker p2p_layer
    |> P2p.on_new_connection p2p_layer
  in
  (* Register a handler to notify P2P disconnections to GS. *)
  let () = disconnections_handler gs_worker |> P2p.on_disconnection p2p_layer in
  Lwt.join
    [
      gs_worker_p2p_output_handler gs_worker p2p_layer px_cache;
      transport_layer_inputs_handler gs_worker p2p_layer px_cache;
      app_messages_handler gs_worker ~app_messages_callback;
    ]
