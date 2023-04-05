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

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/5165

   Add coverage unit tests *)

module Make (C : Gossipsub_intf.WORKER_CONFIGURATION) :
  Gossipsub_intf.WORKER with module GS = C.GS = struct
  open C
  module GS = GS
  module View = GS.Introspection
  module Topic = GS.Topic
  module Peer = GS.Peer
  module Message_id = GS.Message_id
  module Message = GS.Message

  (** A worker has one of the following statuses:
     - [Starting] in case it is initialized with {!make} but not started yet.
     - [Running] in case the function [start] has been called. *)
  type worker_status =
    | Starting
    | Running of {
        heartbeat_handle : unit Monad.t;
        event_loop_handle : unit Monad.t;
      }

  type full_message = {
    message : Message.t;
    topic : Topic.t;
    message_id : Message_id.t;
  }

  type p2p_message =
    | Graft of {topic : Topic.t}
    | Prune of {topic : Topic.t; px : Peer.t Seq.t}
    | IHave of {topic : Topic.t; message_ids : Message_id.t list}
    | IWant of {message_ids : Message_id.t list}
    | Subscribe of {topic : Topic.t}
    | Unsubscribe of {topic : Topic.t}
    | Publish of full_message
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5323

     The payloads of the variants about are quite simular to those of GS (types
     graft, prune, ...). We could reuse them if we move the peer field
     outside. *)

  type p2p_input =
    | In_message of {from_peer : Peer.t; p2p_message : p2p_message}
    | New_connection of {peer : Peer.t; direct : bool; outbound : bool}
    | Disconnection of {peer : Peer.t}

  type app_input =
    | Inject_message of full_message
    | Join of Topic.t
    | Leave of Topic.t

  type p2p_output =
    | Out_message of {to_peer : Peer.t; p2p_message : p2p_message}
    | Disconnect of {peer : Peer.t}
    | Kick of {peer : Peer.t}

  type app_output = full_message

  (** The different kinds of events the Gossipsub worker handles. *)
  type event = Heartbeat | P2P_input of p2p_input | App_input of app_input

  (** The worker's state is made of its status, the gossipsub automaton's state,
      and a stream of events to process. It also has two output streams to
      communicate with the application and P2P layers. *)
  type t = {
    gossip_state : GS.state;
    status : worker_status;
    events_stream : event Stream.t;
    p2p_output_stream : p2p_output Stream.t;
    app_output_stream : app_output Stream.t;
  }

  type collection_kind = List of Peer.t list | Set of Peer.Set.t

  let send_p2p_message ~emit_p2p_msg p2p_message =
    let emit to_peer = emit_p2p_msg @@ Out_message {to_peer; p2p_message} in
    function
    | List list -> List.iter emit list | Set set -> Peer.Set.iter emit set

  let message_is_from_network publish = Option.is_some publish.GS.sender

  (** From the worker's perspective, handling a full message consists in:
      - Sending it to peers returned in [to_publish] field of
      [GS.Publish_message] output);
      - Notifying the application layer if it is a network message it is
      interesed in.

      Note that it's the responsability of the automaton modules to filter out
      peers based on various criteria (bad score, connection expired, ...). *)
  let handle_full_message ~emit_p2p_msg ~emit_app_msg publish = function
    | gstate, GS.Publish_message {to_publish} ->
        let {GS.sender = _; topic; message_id; message} = publish in
        let full_message = {message; topic; message_id} in
        let p2p_message = Publish full_message in
        send_p2p_message ~emit_p2p_msg p2p_message (Set to_publish) ;
        let has_joined = View.(has_joined topic @@ view gstate) in
        if message_is_from_network publish && has_joined then
          emit_app_msg full_message ;
        gstate
    | gstate, GS.Already_published -> gstate

  (** From the worker's perspective, the outcome of joining a new topic from the
      application layer are:
      - Sending [Subscribe] messages to connected peers with that topic;
      - Sending [Graft] messages to the newly construced topic's mesh. *)
  let handle_join ~emit_p2p_msg topic = function
    | gstate, GS.Already_joined -> gstate
    | gstate, Joining_topic {to_graft} ->
        let filters = View.[Not_expired] in
        let peers = View.(view gstate |> get_connected_peers ~filters) in
        (* It's important to send [Subscribe] before [Graft], as the other peer
           would ignore the [Graft] message if we did not subscribe to the
           topic first. *)
        send_p2p_message ~emit_p2p_msg (Subscribe {topic}) (List peers) ;
        send_p2p_message ~emit_p2p_msg (Graft {topic}) (Set to_graft) ;
        gstate

  (** From the worker's perspective, the outcome of leaving a topic by the
      application layer are:
      - Sending [Prune] messages to the topic's mesh;
      - Sending [Unsubscribe] messages to connected peers. *)
  let handle_leave ~emit_p2p_msg topic = function
    | gstate, GS.Not_joined -> gstate
    | gstate, Leaving_topic {to_prune} ->
        let filters = View.[Not_expired] in
        let peers = View.(view gstate |> get_connected_peers ~filters) in
        (* Sending [Prune] before [Unsubscribe] to let full-connection peers
           clean their mesh before handling [Unsubscribe] message. *)
        let prune = Prune {topic; px = Seq.empty} in
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5350

           Send a list of alternative peers when pruning a link for a topic. *)
        send_p2p_message ~emit_p2p_msg prune (Set to_prune) ;
        send_p2p_message ~emit_p2p_msg (Unsubscribe {topic}) (List peers) ;
        gstate

  (** When a new peer is connected, the worker will send a [Subscribe] message
      to that peer for each topic the local peer tracks. *)
  let handle_new_connection ~emit_p2p_msg peer = function
    | gstate, GS.Peer_already_known -> gstate
    | gstate, Peer_added ->
        let peers = List [peer] in
        View.(view gstate |> get_our_topics)
        |> List.iter (fun topic ->
               send_p2p_message ~emit_p2p_msg (Subscribe {topic}) peers) ;
        gstate

  (** Handling application events. *)
  let apply_app_event ~emit_p2p_msg ~emit_app_msg gossip_state = function
    | Inject_message {message; message_id; topic} ->
        let publish = {GS.sender = None; topic; message_id; message} in
        GS.publish publish gossip_state
        |> handle_full_message ~emit_p2p_msg ~emit_app_msg publish
    | Join topic ->
        GS.join {topic} gossip_state |> handle_join ~emit_p2p_msg topic
    | Leave topic ->
        GS.leave {topic} gossip_state |> handle_leave ~emit_p2p_msg topic

  (** Handling messages received from the P2P network. *)
  let apply_p2p_message ~emit_p2p_msg ~emit_app_msg gossip_state from_peer =
    function
    | Publish {message; topic; message_id} ->
        let publish =
          {GS.sender = Some from_peer; topic; message_id; message}
        in
        GS.publish publish gossip_state
        |> handle_full_message ~emit_p2p_msg ~emit_app_msg publish
    | _ ->
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5164

           Handle received p2p messages. *)
        assert false

  (** Handling events received from P2P layer. *)
  let apply_p2p_event ~emit_p2p_msg ~emit_app_msg gossip_state = function
    | New_connection {peer; direct; outbound} ->
        GS.add_peer {direct; outbound; peer} gossip_state
        |> handle_new_connection ~emit_p2p_msg peer
    | Disconnection {peer} ->
        let gossip_state, output = GS.remove_peer {peer} gossip_state in
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5161

           Handle disconnection's output *)
        ignore output ;
        gossip_state
    | In_message {from_peer; p2p_message} ->
        apply_p2p_message
          ~emit_p2p_msg
          ~emit_app_msg
          gossip_state
          from_peer
          p2p_message

  (** This is the main function of the worker. It interacts with the Gossipsub
      automaton given an event. The function possibly sends messages to the P2P
      and application layers via the functions [emit_p2p_msg] and [emit_app_msg]
      and returns the new automaton's state. *)
  let apply_event ~emit_p2p_msg ~emit_app_msg gossip_state = function
    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5326

       Notify the GS worker about the status of messages sent to peers. *)
    | Heartbeat ->
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/5170

           Do we want to detect cases where two successive [Heartbeat] events
           would be handled (e.g. because the first one is late)? *)
        let gossip_state, output = GS.heartbeat gossip_state in
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5159

           Handle Heartbeat's output *)
        ignore output ;
        gossip_state
    | P2P_input event ->
        apply_p2p_event ~emit_p2p_msg ~emit_app_msg gossip_state event
    | App_input event ->
        apply_app_event ~emit_p2p_msg ~emit_app_msg gossip_state event

  (** A helper function that pushes events in the state *)
  let push e t = Stream.push e t.events_stream

  let app_input t input = push (App_input input) t

  let p2p_input t input = push (P2P_input input) t

  (** This function returns a never-ending loop that periodically pushes
      [Heartbeat] events in the stream.  *)
  let heartbeat_events_producer ~heartbeat_span t =
    let stream = t.events_stream in
    let rec loop () =
      let open Monad in
      let* () = Monad.sleep heartbeat_span in
      Stream.push Heartbeat stream ;
      loop ()
    in
    loop ()

  (** This function returns a never-ending loop that processes the events of the
      worker's stream. *)
  let event_loop t =
    let rev_push stream e = Stream.push e stream in
    let emit_p2p_msg = rev_push t.p2p_output_stream in
    let emit_app_msg = rev_push t.app_output_stream in
    let rec loop t =
      let open Monad in
      let* event = Stream.pop t.events_stream in
      let gossip_state =
        apply_event ~emit_p2p_msg ~emit_app_msg t.gossip_state event
      in
      loop {t with gossip_state}
    in
    loop t

  let start ~heartbeat_span topics t =
    match t.status with
    | Starting ->
        let heartbeat_handle = heartbeat_events_producer ~heartbeat_span t in
        let event_loop_handle = event_loop t in
        let status = Running {heartbeat_handle; event_loop_handle} in
        let t = {t with status} in
        List.iter (fun topic -> app_input t (Join topic)) topics ;
        t
    | Running _ ->
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5166

           Better error handling *)
        Format.eprintf "A worker is already running for this state!@." ;
        assert false

  let shutdown state =
    match state.status with
    | Starting -> ()
    | Running _ ->
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5171

           Implement worker shutdown.
           Should we unsubscribe from the callbacks called in start? *)
        ()

  let make rng limits parameters =
    {
      gossip_state = GS.make rng limits parameters;
      status = Starting;
      events_stream = Stream.empty;
      p2p_output_stream = Stream.empty;
      app_output_stream = Stream.empty;
    }
end
