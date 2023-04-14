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
  Gossipsub_intf.WORKER
    with module GS = C.GS
     and module Monad = C.Monad
     and module Stream = C.Stream = struct
  module GS = C.GS
  module Monad = C.Monad
  module Stream = C.Stream
  module View = GS.Introspection
  module Topic = GS.Topic
  module Peer = GS.Peer
  module Message_id = GS.Message_id
  module Message = GS.Message

  type 'a cancellation_handle = {
    schedule_cancellation : unit -> 'a Monad.t;
        (** A handle for a cancellable looping monad. When
            [schedule_cancellation ()] is called, the internal monad is
            returned. Details on how the returned monad resovles or is cancelled
            depends on the loop's implementation. *)
  }

  (** A worker has one of the following statuses:
     - [Starting] in case it is initialized with {!make} but not started yet.
     - [Running] in case the function [start] has been called. *)
  type worker_status =
    | Starting
    | Running of {
        heartbeat_handle : unit cancellation_handle;
        event_loop_handle : unit cancellation_handle;
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
    | Connect of {peer : Peer.t}

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

  let send_p2p_output ~emit_p2p_output ~mk_output =
    let emit to_peer = emit_p2p_output @@ mk_output to_peer in
    Seq.iter emit

  let send_p2p_message ~emit_p2p_output p2p_message =
    send_p2p_output ~emit_p2p_output ~mk_output:(fun to_peer ->
        Out_message {to_peer; p2p_message})

  let message_is_from_network publish = Option.is_some publish.GS.sender

  (** From the worker's perspective, handling a full message consists in:
      - Sending it to peers returned in [to_publish] field of
      [GS.Publish_message] output);
      - Notifying the application layer if it is a network message it is
      interesed in.

      Note that it's the responsability of the automaton modules to filter out
      peers based on various criteria (bad score, connection expired, ...). *)
  let handle_full_message ~emit_p2p_output ~emit_app_output publish = function
    | gstate, GS.Publish_message {to_publish} ->
        let {GS.sender = _; topic; message_id; message} = publish in
        let full_message = {message; topic; message_id} in
        let p2p_message = Publish full_message in
        send_p2p_message
          ~emit_p2p_output
          p2p_message
          (Peer.Set.to_seq to_publish) ;
        let has_joined = View.(has_joined topic @@ view gstate) in
        if message_is_from_network publish && has_joined then
          emit_app_output full_message ;
        gstate
    | gstate, GS.Already_published -> gstate

  (** From the worker's perspective, the outcome of joining a new topic from the
      application layer are:
      - Sending [Subscribe] messages to connected peers with that topic;
      - Sending [Graft] messages to the newly construced topic's mesh. *)
  let handle_join ~emit_p2p_output topic = function
    | gstate, GS.Already_joined -> gstate
    | gstate, Joining_topic {to_graft} ->
        let peers = View.(view gstate |> get_connected_peers) in
        (* It's important to send [Subscribe] before [Graft], as the other peer
           would ignore the [Graft] message if we did not subscribe to the
           topic first. *)
        send_p2p_message
          ~emit_p2p_output
          (Subscribe {topic})
          (List.to_seq peers) ;
        send_p2p_message
          ~emit_p2p_output
          (Graft {topic})
          (Peer.Set.to_seq to_graft) ;
        gstate

  (** From the worker's perspective, the outcome of leaving a topic by the
      application layer are:
      - Sending [Prune] messages to the topic's mesh;
      - Sending [Unsubscribe] messages to connected peers. *)
  let handle_leave ~emit_p2p_output topic = function
    | gstate, GS.Not_joined -> gstate
    | gstate, Leaving_topic {to_prune} ->
        let peers = View.(view gstate |> get_connected_peers) in
        (* Sending [Prune] before [Unsubscribe] to let full-connection peers
           clean their mesh before handling [Unsubscribe] message. *)
        let prune = Prune {topic; px = Seq.empty} in
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5350

           Send a list of alternative peers when pruning a link for a topic. *)
        send_p2p_message ~emit_p2p_output prune (Peer.Set.to_seq to_prune) ;
        send_p2p_message
          ~emit_p2p_output
          (Unsubscribe {topic})
          (List.to_seq peers) ;
        gstate

  (** When a new peer is connected, the worker will send a [Subscribe] message
      to that peer for each topic the local peer tracks. *)
  let handle_new_connection ~emit_p2p_output peer = function
    | gstate, GS.Peer_already_known -> gstate
    | gstate, Peer_added ->
        View.(view gstate |> get_our_topics)
        |> List.iter (fun topic ->
               send_p2p_message
                 ~emit_p2p_output
                 (Subscribe {topic})
                 (Seq.return peer)) ;
        gstate

  (** When a peer is disconnected, the worker has nothing to do, as:
      - It cannot send [Prune] or [Unsubscribe] to the remote peer because the
        connection is closed;
      - [Prune] or [Unsubscribe] are already handled when calling
        {!GS.remove_peer}. *)
  let handle_disconnection = function gstate, GS.Removing_peer -> gstate

  (** When a [Graft] request from a remote peer is received, the worker just
      forwards it to the automaton. There is nothing else to do. *)
  let handle_graft = function
    | ( gstate,
        ( GS.Peer_filtered | Unknown_topic | Peer_already_in_mesh
        | Grafting_direct_peer | Unexpected_grafting_peer
        | Grafting_peer_with_negative_score | Grafting_successfully
        | Peer_backed_off | Mesh_full ) ) ->
        gstate

  (** When a [Subscribe] request from a remote peer is received, the worker just
      forwards it to the automaton. There is nothing else to do. *)
  let handle_subscribe = function
    | gstate, (GS.Subscribed | Subscribe_to_unknown_peer) -> gstate

  (** When a [Unsubscribe] request from a remote peer is received, the worker just
      forwards it to the automaton. There is nothing else to do. *)
  let handle_unsubscribe = function
    | gstate, (GS.Unsubscribed | Unsubscribe_from_unknown_peer) -> gstate

  (** When an [IHave] control message from a remote peer is received, the
      automaton checks whether it is interested is some full messages whose ids
      are given. If so, the worker requests them from the remote peer via an
      [IWant] message. *)
  let handle_ihave ~emit_p2p_output ({peer; _} : GS.ihave) = function
    | gstate, GS.Message_requested_message_ids message_ids ->
        if not (List.is_empty message_ids) then
          send_p2p_message
            ~emit_p2p_output
            (IWant {message_ids})
            (Seq.return peer) ;
        gstate
    | ( gstate,
        ( GS.Ihave_from_peer_with_low_score _ | Too_many_recv_ihave_messages _
        | Too_many_sent_iwant_messages _ | Message_topic_not_tracked ) ) ->
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5424

           Penalize peers with negative score or non expected behaviour
           (revisit all the outputs in different apply event functions). *)
        gstate

  (** When an [IWant] control message from a remote peer is received, the
      automaton checks which full messages it can send back (based on message
      availability and on the number of received requests). Selected messages,
      if any, are transmitted to the remote peer via [Publish]. *)
  let handle_iwant ~emit_p2p_output ({peer; _} : GS.iwant) = function
    | gstate, GS.On_iwant_messages_to_route {routed_message_ids} ->
        Message_id.Map.iter
          (fun message_id status ->
            match status with
            | `Message message ->
                let topic = Message_id.get_topic message_id in
                (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5415

                   Don't provide a topic when it can be inferred (e.g. from
                   Message_id.t). This also applies for Publish and IHave. *)
                let full_message = {message; topic; message_id} in
                let p2p_message = Publish full_message in
                send_p2p_message ~emit_p2p_output p2p_message (Seq.return peer)
            | _ -> ())
          routed_message_ids ;
        gstate
    | gstate, GS.Iwant_from_peer_with_low_score _ ->
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5424

           Penalize peers with negative score or non expected behaviour
           (revisit all the outputs in different apply event functions). *)
        gstate

  (** When a [Prune] control message from a remote peer is received, the
      automaton removes that peer from the given topic's mesh. It also filters
      the given collection of alternative peers to connect to. The worker then
      asks the P2P part to connect to those peeers. *)
  let handle_prune ~emit_p2p_output = function
    | gstate, (GS.No_peer_in_mesh | Ignore_PX_score_too_low _ | No_PX) -> gstate
    | gstate, GS.PX peers ->
        send_p2p_output
          ~emit_p2p_output
          ~mk_output:(fun to_peer -> Connect {peer = to_peer})
          (Peer.Set.to_seq peers) ;
        gstate
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5425

     The automaton doesn't filter out the alternative peers we are already
     connected to. *)
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5426

     The automaton doesn't filter out the alternative peers we are already
     connected to. *)

  (** On a [Heartbeat] events, the worker sends graft and prune messages
      following the automaton's output. It also sends [IHave] messages (computed
      by the automaton as well). *)
  let handle_heartheat ~emit_p2p_output = function
    | gstate, GS.Heartbeat {to_graft; to_prune; noPX_peers} ->
        let iter pmap mk_msg =
          Peer.Map.iter
            (fun peer topicset ->
              Topic.Set.iter
                (fun topic ->
                  send_p2p_message
                    ~emit_p2p_output
                    (mk_msg peer topic)
                    (Seq.return peer))
                topicset)
            pmap
        in
        (* Send Graft messages. *)
        iter to_graft (fun _peer topic -> Graft {topic}) ;
        (* Send Prune messages with adequate px. *)
        iter to_prune (fun peer_to_prune topic ->
            let px =
              GS.select_px_peers gstate ~peer_to_prune topic ~noPX_peers
              |> List.to_seq
            in
            Prune {topic; px}) ;
        (* Send IHave messages. *)
        GS.select_gossip_messages gstate
        |> List.iter (fun GS.{peer; topic; message_ids} ->
               if not (List.is_empty message_ids) then
                 send_p2p_message
                   ~emit_p2p_output
                   (IHave {topic; message_ids})
                   (Seq.return peer)) ;
        gstate

  (** Handling application events. *)
  let apply_app_event ~emit_p2p_output ~emit_app_output gossip_state = function
    | Inject_message {message; message_id; topic} ->
        let publish = {GS.sender = None; topic; message_id; message} in
        GS.publish publish gossip_state
        |> handle_full_message ~emit_p2p_output ~emit_app_output publish
    | Join topic ->
        GS.join {topic} gossip_state |> handle_join ~emit_p2p_output topic
    | Leave topic ->
        GS.leave {topic} gossip_state |> handle_leave ~emit_p2p_output topic

  (** Handling messages received from the P2P network. *)
  let apply_p2p_message ~emit_p2p_output ~emit_app_output gossip_state from_peer
      = function
    | Publish {message; topic; message_id} ->
        let publish =
          {GS.sender = Some from_peer; topic; message_id; message}
        in
        GS.publish publish gossip_state
        |> handle_full_message ~emit_p2p_output ~emit_app_output publish
    | Graft {topic} ->
        let graft : GS.graft = {peer = from_peer; topic} in
        GS.handle_graft graft gossip_state |> handle_graft
    | Subscribe {topic} ->
        let subscribe : GS.subscribe = {peer = from_peer; topic} in
        GS.handle_subscribe subscribe gossip_state |> handle_subscribe
    | Unsubscribe {topic} ->
        let unsubscribe : GS.unsubscribe = {peer = from_peer; topic} in
        GS.handle_unsubscribe unsubscribe gossip_state |> handle_unsubscribe
    | IHave {topic; message_ids} ->
        (* The automaton should guarantee that the list is not empty. *)
        let ihave : GS.ihave = {peer = from_peer; topic; message_ids} in
        GS.handle_ihave ihave gossip_state
        |> handle_ihave ~emit_p2p_output ihave
    | IWant {message_ids} ->
        (* The automaton should guarantee that the list is not empty. *)
        let iwant : GS.iwant = {peer = from_peer; message_ids} in
        GS.handle_iwant iwant gossip_state
        |> handle_iwant ~emit_p2p_output iwant
    | Prune {topic; px} ->
        let backoff = View.((view gossip_state).limits.prune_backoff) in
        let prune : GS.prune = {peer = from_peer; topic; px; backoff} in
        GS.handle_prune prune gossip_state |> handle_prune ~emit_p2p_output

  (** Handling events received from P2P layer. *)
  let apply_p2p_event ~emit_p2p_output ~emit_app_output gossip_state = function
    | New_connection {peer; direct; outbound} ->
        GS.add_peer {direct; outbound; peer} gossip_state
        |> handle_new_connection ~emit_p2p_output peer
    | Disconnection {peer} ->
        GS.remove_peer {peer} gossip_state |> handle_disconnection
    | In_message {from_peer; p2p_message} ->
        apply_p2p_message
          ~emit_p2p_output
          ~emit_app_output
          gossip_state
          from_peer
          p2p_message

  (** This is the main function of the worker. It interacts with the Gossipsub
      automaton given an event. The function possibly sends messages to the P2P
      and application layers via the functions [emit_p2p_output] and [emit_app_output]
      and returns the new automaton's state. *)
  let apply_event ~emit_p2p_output ~emit_app_output gossip_state = function
    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5326

       Notify the GS worker about the status of messages sent to peers. *)
    | Heartbeat ->
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/5170

           Do we want to detect cases where two successive [Heartbeat] events
           would be handled (e.g. because the first one is late)? *)
        GS.heartbeat gossip_state |> handle_heartheat ~emit_p2p_output
    | P2P_input event ->
        apply_p2p_event ~emit_p2p_output ~emit_app_output gossip_state event
    | App_input event ->
        apply_app_event ~emit_p2p_output ~emit_app_output gossip_state event

  (** A helper function that pushes events in the state *)
  let push e t = Stream.push e t.events_stream

  let app_input t input = push (App_input input) t

  let p2p_input t input = push (P2P_input input) t

  (** This function returns a {!cancellation_handle} for a looping monad
      that pushes [Heartbeat] events in the [t.events_stream] every
      [heartbeat_span].

      When the loop is canceled, the returned monad may take up to
      [heartbeat_span] to resolve. *)
  let heartbeat_events_producer ~heartbeat_span t =
    let open Monad in
    let shutdown = ref false in
    let stream = t.events_stream in
    let rec loop () =
      let* () = Monad.sleep heartbeat_span in
      Stream.push Heartbeat stream ;
      if !shutdown then return () else loop ()
    in
    let promise = loop () in
    let schedule_cancellation () =
      shutdown := true ;
      promise
    in
    {schedule_cancellation}

  (** This function returns a {!cancellation_handle} for a looping monad
      that consumes pushed events in [t.events_stream], if any.

      When the loop is canceled, the returned monad will need an additional
      extra event to consume in order to resolve. *)
  let event_loop t =
    let open Monad in
    let shutdown = ref false in
    let rev_push stream e = Stream.push e stream in
    let emit_p2p_output = rev_push t.p2p_output_stream in
    let emit_app_output = rev_push t.app_output_stream in
    let events_stream = t.events_stream in
    let rec loop gossip_state =
      let* event = Stream.pop events_stream in
      if !shutdown then return ()
      else
        loop @@ apply_event ~emit_p2p_output ~emit_app_output gossip_state event
    in
    let promise = loop t.gossip_state in
    let schedule_cancellation () =
      shutdown := true ;
      promise
    in
    {schedule_cancellation}

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

  (** Shutting down may require waiting up [heartbeat_span] to stop the
      heartbeat event loop. *)
  let shutdown state =
    let open C.Monad in
    match state.status with
    | Starting -> return ()
    | Running {heartbeat_handle; event_loop_handle; _} ->
        (* First, we schedule event_loop cancellation without waiting for the
           promise to resolve (i.e. for an input in the stream to be read). *)
        let event_loop_promise = event_loop_handle.schedule_cancellation () in
        (* Then, we schedule heartbeat cancellation, which will push a last
           [Heartbeat] event in the stream before its returned promise is
           resolved. *)
        let* () = heartbeat_handle.schedule_cancellation () in
        (* Now, we are sure that an event has been pushed to the event stream
           after [event_loop] cancellation. So, [event_loop_promise] can be
           resolved. *)
        event_loop_promise

  let make rng limits parameters =
    {
      gossip_state = GS.make rng limits parameters;
      status = Starting;
      events_stream = Stream.empty ();
      p2p_output_stream = Stream.empty ();
      app_output_stream = Stream.empty ();
    }

  let p2p_output_stream t = t.p2p_output_stream

  let app_output_stream t = t.app_output_stream
end
