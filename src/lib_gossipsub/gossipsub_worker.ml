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

  module Introspection = struct
    type stats = {
      mutable count_topics : int64;
      mutable count_connections : int64;
      mutable count_bootstrap_connections : int64;
      mutable count_sent_app_messages : int64;
      mutable count_sent_grafts : int64;
      mutable count_sent_prunes : int64;
      mutable count_sent_ihaves : int64;
      mutable count_sent_iwants : int64;
      mutable count_recv_valid_app_messages : int64;
      mutable count_recv_invalid_app_messages : int64;
      mutable count_recv_unknown_validity_app_messages : int64;
      mutable count_recv_grafts : int64;
      mutable count_recv_prunes : int64;
      mutable count_recv_ihaves : int64;
      mutable count_recv_iwants : int64;
    }

    let empty_stats () =
      {
        count_topics = 0L;
        count_connections = 0L;
        count_bootstrap_connections = 0L;
        count_sent_app_messages = 0L;
        count_sent_grafts = 0L;
        count_sent_prunes = 0L;
        count_sent_ihaves = 0L;
        count_sent_iwants = 0L;
        count_recv_valid_app_messages = 0L;
        count_recv_invalid_app_messages = 0L;
        count_recv_unknown_validity_app_messages = 0L;
        count_recv_grafts = 0L;
        count_recv_prunes = 0L;
        count_recv_ihaves = 0L;
        count_recv_iwants = 0L;
      }

    let counter_update = function
      | `Incr -> 1L
      | `Decr -> -1L
      | `Plus n -> Int64.of_int n
      | `Minus n -> Int64.of_int (-n)

    let ( ++ ) = Int64.add

    let update_count_topics t delta =
      t.count_topics <- t.count_topics ++ counter_update delta

    let update_count_connections t delta =
      t.count_connections <- t.count_connections ++ counter_update delta

    let update_count_bootstrap_connections t delta =
      t.count_bootstrap_connections <-
        t.count_bootstrap_connections ++ counter_update delta

    let update_count_recv_grafts t delta =
      t.count_recv_grafts <- t.count_recv_grafts ++ counter_update delta

    let update_count_recv_prunes t delta =
      t.count_recv_prunes <- t.count_recv_prunes ++ counter_update delta

    let update_count_recv_ihaves t delta =
      t.count_recv_ihaves <- t.count_recv_ihaves ++ counter_update delta

    let update_count_recv_iwants t delta =
      t.count_recv_iwants <- t.count_recv_iwants ++ counter_update delta

    let update_count_recv_invalid_app_messages t delta =
      t.count_recv_invalid_app_messages <-
        t.count_recv_invalid_app_messages ++ counter_update delta

    let update_count_recv_unknown_validity_app_messages t delta =
      t.count_recv_unknown_validity_app_messages <-
        t.count_recv_unknown_validity_app_messages ++ counter_update delta

    let update_count_recv_valid_app_messages t delta =
      t.count_recv_valid_app_messages <-
        t.count_recv_valid_app_messages ++ counter_update delta

    let update_count_sent_grafts t delta =
      t.count_sent_grafts <- t.count_sent_grafts ++ counter_update delta

    let update_count_sent_prunes t delta =
      t.count_sent_prunes <- t.count_sent_prunes ++ counter_update delta

    let update_count_sent_ihaves t delta =
      t.count_sent_ihaves <- t.count_sent_ihaves ++ counter_update delta

    let update_count_sent_iwants t delta =
      t.count_sent_iwants <- t.count_sent_iwants ++ counter_update delta

    let update_count_sent_app_messages t delta =
      t.count_sent_app_messages <-
        t.count_sent_app_messages ++ counter_update delta

    (* *)
  end

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

  type message_with_header = {
    message : Message.t;
    topic : Topic.t;
    message_id : Message_id.t;
  }

  type p2p_message =
    | Graft of {topic : Topic.t}
    | Prune of {topic : Topic.t; px : Peer.t Seq.t; backoff : GS.Span.t}
    | IHave of {topic : Topic.t; message_ids : Message_id.t list}
    | IWant of {message_ids : Message_id.t list}
    | Subscribe of {topic : Topic.t}
    | Unsubscribe of {topic : Topic.t}
    | Message_with_header of message_with_header

  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5323

     The payloads of the variants about are quite simular to those of GS (types
     graft, prune, ...). We could reuse them if we move the peer field
     outside. *)

  type p2p_input =
    | In_message of {from_peer : Peer.t; p2p_message : p2p_message}
    | New_connection of {
        peer : Peer.t;
        direct : bool;
        trusted : bool;
        bootstrap : bool;
      }
    | Disconnection of {peer : Peer.t}

  type app_input =
    | Publish_message of message_with_header
    | Join of Topic.t
    | Leave of Topic.t

  type peer_origin = PX of Peer.t | Trusted

  type p2p_output =
    | Out_message of {to_peer : Peer.t; p2p_message : p2p_message}
    | Disconnect of {peer : Peer.t}
    | Kick of {peer : Peer.t}
    | Connect of {px : Peer.t; origin : peer_origin}
    | Forget of {px : Peer.t; origin : Peer.t}

  type app_output = message_with_header

  (** The different kinds of events the Gossipsub worker handles. *)
  type event = Heartbeat | P2P_input of p2p_input | App_input of app_input

  (** The worker's state is made of the gossipsub automaton's state,
      and a stream of events to process. It also has two output streams to
      communicate with the application and P2P layers. *)
  type worker_state = {
    stats : Introspection.stats;
    gossip_state : GS.state;
    trusted_peers : Peer.Set.t;
    connected_bootstrap_peers : Peer.Set.t;
    events_stream : event Stream.t;
    p2p_output_stream : p2p_output Stream.t;
    app_output_stream : app_output Stream.t;
    events_logging : event -> unit Monad.t;
  }

  (** A worker instance is made of its status and state. *)
  type t = {mutable status : worker_status; mutable state : worker_state}

  let state {state; _} = GS.Introspection.view state.gossip_state

  let emit_app_output state e = Stream.push e state.app_output_stream

  let emit_p2p_output =
    let update_sent_stats stats = function
      | Out_message {p2p_message; to_peer = _} -> (
          match p2p_message with
          | Graft _ -> Introspection.update_count_sent_grafts stats `Incr
          | Prune _ -> Introspection.update_count_sent_prunes stats `Incr
          | IHave _ -> Introspection.update_count_sent_ihaves stats `Incr
          | IWant _ -> Introspection.update_count_sent_iwants stats `Incr
          | Message_with_header _ ->
              Introspection.update_count_sent_app_messages stats `Incr
          | Subscribe _ | Unsubscribe _ -> ())
      | Connect _ | Disconnect _ | Forget _ | Kick _ -> ()
    in
    fun {connected_bootstrap_peers; p2p_output_stream; stats; _} ~mk_output ->
      let maybe_emit to_peer =
        let message = mk_output to_peer in
        let do_emit =
          (not (Peer.Set.mem to_peer connected_bootstrap_peers))
          ||
          match message with
          | Out_message {p2p_message; to_peer = _} -> (
              match p2p_message with
              | Message_with_header _ | IHave _ | IWant _ ->
                  (* Don't emit app messages, send IHave messages or respond to
                     IWant if the remote peer has a bootstrap profile. *)
                  false
              | Graft _ | Prune _ | Subscribe _ | Unsubscribe _ -> true)
          | Connect _ | Disconnect _ | Forget _ | Kick _ -> true
        in
        if do_emit then (
          update_sent_stats stats message ;
          Stream.push message p2p_output_stream)
      in
      Seq.iter maybe_emit

  let emit_p2p_message state p2p_message =
    emit_p2p_output state ~mk_output:(fun to_peer ->
        Out_message {to_peer; p2p_message})

  (** From the worker's perspective, handling publishing a message consists in:
      - Sending it to peers returned in [to_publish] field of
      [GS.Publish_message] output);

      Note that it's the responsability of the automaton modules to filter out
      peers based on various criteria (bad score, connection expired, ...). *)
  let handle_publish_message publish_message = function
    | state, GS.Publish_message {to_publish} ->
        let ({topic; message_id; message} : GS.publish_message) =
          publish_message
        in
        let message_with_header = {message; topic; message_id} in
        let p2p_message = Message_with_header message_with_header in
        emit_p2p_message state p2p_message (Peer.Set.to_seq to_publish) ;
        state
    | state, GS.Already_published -> state

  (** From the worker's perspective, handling receiving a message consists in:
      - Sending it to peers returned in [to_route] field of
      [GS.Route_message] output);
      - Notifying the application layer if it is interesed in it.

      Note that it's the responsability of the automaton modules to filter out
      peers based on various criteria (bad score, connection expired, ...). *)
  let handle_receive_message received_message = function
    | state, GS.Route_message {to_route} ->
        Introspection.update_count_recv_valid_app_messages state.stats `Incr ;
        let ({sender = _; topic; message_id; message} : GS.receive_message) =
          received_message
        in
        let message_with_header = {message; topic; message_id} in
        let p2p_message = Message_with_header message_with_header in
        emit_p2p_message state p2p_message (Peer.Set.to_seq to_route) ;
        let has_joined = View.(has_joined topic @@ view state.gossip_state) in
        if has_joined then emit_app_output state message_with_header ;
        state
    | state, GS.Already_received | state, GS.Not_subscribed -> state
    | state, GS.Unknown_validity ->
        Introspection.update_count_recv_unknown_validity_app_messages
          state.stats
          `Incr ;
        state
    | state, GS.Invalid_message ->
        Introspection.update_count_recv_invalid_app_messages state.stats `Incr ;
        state

  (** From the worker's perspective, the outcome of joining a new topic from the
      application layer are:
      - Sending [Subscribe] messages to connected peers with that topic;
      - Sending [Graft] messages to the newly construced topic's mesh. *)
  let handle_join topic = function
    | state, GS.Already_joined -> state
    | state, Joining_topic {to_graft} ->
        Introspection.update_count_topics state.stats `Incr ;
        let peers = View.(view state.gossip_state |> get_connected_peers) in
        (* It's important to send [Subscribe] before [Graft], as the other peer
           would ignore the [Graft] message if we did not subscribe to the
           topic first. *)
        emit_p2p_message state (Subscribe {topic}) (List.to_seq peers) ;
        emit_p2p_message state (Graft {topic}) (Peer.Set.to_seq to_graft) ;
        state

  (** From the worker's perspective, the outcome of leaving a topic by the
      application layer are:
      - Sending [Prune] messages to the topic's mesh;
      - Sending [Unsubscribe] messages to connected peers. *)
  let handle_leave topic = function
    | state, GS.Not_joined -> state
    | state, Leaving_topic {to_prune; noPX_peers} ->
        Introspection.update_count_topics state.stats `Decr ;
        let gstate = state.gossip_state in
        let view = View.view gstate in
        let peers = View.get_connected_peers view in
        let backoff = (View.limits view).unsubscribe_backoff in
        (* Sending [Prune] before [Unsubscribe] to let full-connection peers
           clean their mesh before handling [Unsubscribe] message. *)
        Peer.Set.iter
          (fun peer_to_prune ->
            (* Send Prune messages with adequate px. *)
            let prune =
              let px =
                GS.select_px_peers gstate ~peer_to_prune topic ~noPX_peers
                |> List.to_seq
              in
              Prune {topic; px; backoff}
            in
            emit_p2p_message state prune (Seq.return peer_to_prune))
          to_prune ;
        emit_p2p_message state (Unsubscribe {topic}) (List.to_seq peers) ;
        state

  (** When a new peer is connected, the worker will send a [Subscribe] message
      to that peer for each topic the local peer tracks. *)
  let handle_new_connection peer ~bootstrap ~trusted = function
    | state, GS.Peer_already_known -> state
    | state, Peer_added ->
        Introspection.update_count_connections state.stats `Incr ;
        let connected_bootstrap_peers =
          if bootstrap then (
            Introspection.update_count_bootstrap_connections state.stats `Incr ;
            Peer.Set.add peer state.connected_bootstrap_peers)
          else state.connected_bootstrap_peers
        in
        let trusted_peers =
          if trusted then Peer.Set.add peer state.trusted_peers
          else state.trusted_peers
        in
        let state = {state with connected_bootstrap_peers; trusted_peers} in
        View.(view state.gossip_state |> get_our_topics)
        |> List.iter (fun topic ->
               emit_p2p_message state (Subscribe {topic}) (Seq.return peer)) ;
        state

  (** When a peer is disconnected, the worker has nothing to do, as:
      - It cannot send [Prune] or [Unsubscribe] to the remote peer because the
        connection is closed;
      - [Prune] or [Unsubscribe] are already handled when calling
        {!GS.remove_peer}. *)
  let handle_disconnection peer = function
    | state, GS.Removing_peer ->
        Introspection.update_count_connections state.stats `Decr ;
        if Peer.Set.mem peer state.connected_bootstrap_peers then
          Introspection.update_count_bootstrap_connections state.stats `Decr ;
        {
          state with
          connected_bootstrap_peers =
            Peer.Set.remove peer state.connected_bootstrap_peers;
        }

  (** When a [Graft] request from a remote peer is received, the worker forwards
      it to the automaton. In certain cases the peer needs to be pruned. Other
      than that there is nothing else to do. *)
  let handle_graft peer topic (state, output) =
    let gstate = state.gossip_state in
    let backoff = View.(view gstate |> limits).prune_backoff in
    let do_prune ~do_px =
      let prune =
        let px =
          if do_px then
            GS.select_px_peers
              gstate
              ~peer_to_prune:peer
              topic
              ~noPX_peers:Peer.Set.empty
            |> List.to_seq
          else Seq.empty
        in
        Prune {topic; px; backoff}
      in
      emit_p2p_message state prune (Seq.return peer)
    in
    (* NOTE: if the cases when we send a Prune change, be sure to also update
       the backoffs accordingly in the automaton. *)
    let () =
      match output with
      | GS.Peer_already_in_mesh | Unexpected_grafting_peer -> ()
      | Grafting_successfully ->
          Introspection.update_count_recv_grafts state.stats `Incr ;
          ()
      | Peer_filtered | Unsubscribed_topic | Grafting_direct_peer
      | Grafting_peer_with_negative_score | Peer_backed_off ->
          do_prune ~do_px:false
      | Mesh_full ->
          (* We consider this case as a successful graft, although the peer is
             pruned immediately. *)
          Introspection.update_count_recv_grafts state.stats `Incr ;
          do_prune ~do_px:true
    in
    state

  (** When a [Subscribe] request from a remote peer is received, the worker just
      forwards it to the automaton. There is nothing else to do. *)
  let handle_subscribe = function
    | state, (GS.Subscribed | Subscribe_to_unknown_peer) -> state

  (** When a [Unsubscribe] request from a remote peer is received, the worker just
      forwards it to the automaton. There is nothing else to do. *)
  let handle_unsubscribe = function
    | state, (GS.Unsubscribed | Unsubscribe_from_unknown_peer) -> state

  (** When an [IHave] control message from a remote peer is received, the
      automaton checks whether it is interested is some full messages whose ids
      are given. If so, the worker requests them from the remote peer via an
      [IWant] message. *)
  let handle_ihave ({peer; _} : GS.ihave) = function
    | state, GS.Message_requested_message_ids message_ids ->
        Introspection.update_count_recv_ihaves state.stats
        @@ `Plus (List.length message_ids) ;
        if not (List.is_empty message_ids) then
          emit_p2p_message state (IWant {message_ids}) (Seq.return peer) ;
        state
    | ( state,
        ( GS.Ihave_from_peer_with_low_score _ | Too_many_recv_ihave_messages _
        | Too_many_sent_iwant_messages _ | Message_topic_not_tracked
        | Invalid_message_id ) ) ->
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5424

           Penalize peers with negative score or non expected behaviour
           (revisit all the outputs in different apply event functions). *)
        state

  (** When an [IWant] control message from a remote peer is received, the
      automaton checks which full messages it can send back (based on message
      availability and on the number of received requests). Selected messages,
      if any, are transmitted to the remote peer via [Message_with_header]. *)
  let handle_iwant ({peer; _} : GS.iwant) = function
    | state, GS.On_iwant_messages_to_route {routed_message_ids} ->
        Message_id.Map.iter
          (fun message_id status ->
            match status with
            | `Message message ->
                Introspection.update_count_recv_iwants state.stats `Incr ;
                let topic = Message_id.get_topic message_id in
                (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5415

                   Don't provide a topic when it can be inferred (e.g. from
                   Message_id.t). This also applies for Message_with_header and IHave. *)
                let message_with_header = {message; topic; message_id} in
                let p2p_message = Message_with_header message_with_header in
                emit_p2p_message state p2p_message (Seq.return peer)
            | _ -> ())
          routed_message_ids ;
        state
    | state, GS.Iwant_from_peer_with_low_score _ ->
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5424

           Penalize peers with negative score or non expected behaviour
           (revisit all the outputs in different apply event functions). *)
        state

  (** When a [Prune] control message from a remote peer is received, the
      automaton removes that peer from the given topic's mesh. It also filters
      the given collection of alternative peers to connect to. The worker then
      asks the P2P part to connect to those peeers. *)
  let handle_prune ~from_peer input_px =
    let forget_all state =
      emit_p2p_output
        state
        ~mk_output:(fun to_peer -> Forget {px = to_peer; origin = from_peer})
        input_px
    in
    function
    | state, (GS.Prune_topic_not_tracked | Peer_not_in_mesh) ->
        forget_all state ;
        state
    | state, (Ignore_PX_score_too_low _ | No_PX) ->
        Introspection.update_count_recv_prunes state.stats `Incr ;
        forget_all state ;
        state
    | state, GS.PX peers ->
        Introspection.update_count_recv_prunes state.stats `Incr ;
        emit_p2p_output
          state
          ~mk_output:(fun to_peer ->
            Connect {px = to_peer; origin = PX from_peer})
          (Peer.Set.to_seq peers) ;
        (* Forget peers that were filtered out by the automaton. *)
        emit_p2p_output
          state
          ~mk_output:(fun to_peer -> Forget {px = to_peer; origin = from_peer})
          (Peer.Set.to_seq @@ Peer.Set.(diff (of_seq input_px) peers)) ;
        state
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5426

     Add more verification/attacks protections as done in Rust. *)

  (** On a [Heartbeat] events, the worker sends graft and prune messages
      following the automaton's output. It also sends [IHave] messages (computed
      by the automaton as well). *)
  let handle_heartheat = function
    | state, GS.Heartbeat {to_graft; to_prune; noPX_peers} ->
        let gstate = state.gossip_state in
        let gstate_view = View.view gstate in
        let iter pmap mk_msg =
          Peer.Map.iter
            (fun peer topicset ->
              Topic.Set.iter
                (fun topic ->
                  emit_p2p_message state (mk_msg peer topic) (Seq.return peer))
                topicset)
            pmap
        in
        (* Send Graft messages. *)
        iter to_graft (fun _peer topic -> Graft {topic}) ;
        (* Send Prune messages with adequate px. *)
        let backoff = View.(limits gstate_view).prune_backoff in
        iter to_prune (fun peer_to_prune topic ->
            let px =
              GS.select_px_peers gstate ~peer_to_prune topic ~noPX_peers
              |> List.to_seq
            in
            Prune {topic; px; backoff}) ;
        (* Send IHave messages. *)
        GS.select_gossip_messages gstate
        |> List.iter (fun GS.{peer; topic; message_ids} ->
               if not (List.is_empty message_ids) then
                 emit_p2p_message
                   state
                   (IHave {topic; message_ids})
                   (Seq.return peer)) ;
        (* Once every 15 hearbreat ticks, try to reconnect to trusted peers if
           they are disconnected. *)
        if Int64.(equal (rem gstate_view.heartbeat_ticks 15L) 0L) then
          (* TODO: https://gitlab.com/tezos/tezos/-/issues/6636

             Put the value [15L] as a parameter. *)
          Peer.Set.fold
            (fun trusted_peer seq ->
              if View.Connections.mem trusted_peer gstate_view.View.connections
              then seq
              else Seq.cons trusted_peer seq)
            state.trusted_peers
            Seq.empty
          |> emit_p2p_output state ~mk_output:(fun trusted_peer ->
                 Connect {px = trusted_peer; origin = Trusted}) ;
        state

  let update_gossip_state state (gossip_state, output) =
    ({state with gossip_state}, output)

  (** Handling application events. *)
  let apply_app_event ({gossip_state; _} as state) = function
    | Publish_message {message; message_id; topic} ->
        let publish_message = GS.{topic; message_id; message} in
        GS.publish_message publish_message gossip_state
        |> update_gossip_state state
        |> handle_publish_message publish_message
    | Join topic ->
        GS.join {topic} gossip_state
        |> update_gossip_state state |> handle_join topic
    | Leave topic ->
        GS.leave {topic} gossip_state
        |> update_gossip_state state |> handle_leave topic

  (** Handling messages received from the P2P network. *)
  let apply_p2p_message ({gossip_state; _} as state) from_peer = function
    | Message_with_header {message; topic; message_id} ->
        let receive_message =
          {GS.sender = from_peer; topic; message_id; message}
        in
        GS.handle_receive_message receive_message gossip_state
        |> update_gossip_state state
        |> handle_receive_message receive_message
    | Graft {topic} ->
        let graft : GS.graft = {peer = from_peer; topic} in
        GS.handle_graft graft gossip_state
        |> update_gossip_state state
        |> handle_graft from_peer topic
    | Subscribe {topic} ->
        let subscribe : GS.subscribe = {peer = from_peer; topic} in
        GS.handle_subscribe subscribe gossip_state
        |> update_gossip_state state |> handle_subscribe
    | Unsubscribe {topic} ->
        let unsubscribe : GS.unsubscribe = {peer = from_peer; topic} in
        GS.handle_unsubscribe unsubscribe gossip_state
        |> update_gossip_state state |> handle_unsubscribe
    | IHave {topic; message_ids} ->
        (* The automaton should guarantee that the list is not empty. *)
        let ihave : GS.ihave = {peer = from_peer; topic; message_ids} in
        GS.handle_ihave ihave gossip_state
        |> update_gossip_state state |> handle_ihave ihave
    | IWant {message_ids} ->
        (* The automaton should guarantee that the list is not empty. *)
        let iwant : GS.iwant = {peer = from_peer; message_ids} in
        GS.handle_iwant iwant gossip_state
        |> update_gossip_state state |> handle_iwant iwant
    | Prune {topic; px; backoff} ->
        let prune : GS.prune = {peer = from_peer; topic; px; backoff} in
        GS.handle_prune prune gossip_state
        |> update_gossip_state state |> handle_prune ~from_peer px

  (** Handling events received from P2P layer. *)
  let apply_p2p_event ({gossip_state; _} as state) = function
    | New_connection {peer; direct; trusted; bootstrap} ->
        GS.add_peer {direct; outbound = trusted; peer} gossip_state
        |> update_gossip_state state
        |> handle_new_connection peer ~bootstrap ~trusted
    | Disconnection {peer} ->
        GS.remove_peer {peer} gossip_state
        |> update_gossip_state state |> handle_disconnection peer
    | In_message {from_peer; p2p_message} ->
        apply_p2p_message state from_peer p2p_message

  (** This is the main function of the worker. It interacts with the Gossipsub
      automaton given an event. The function possibly sends messages to the P2P
      and application layers and returns the new worker's state. *)
  let apply_event ({gossip_state; _} as state) = function
    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5326

       Notify the GS worker about the status of messages sent to peers. *)
    | Heartbeat ->
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/5170

           Do we want to detect cases where two successive [Heartbeat] events
           would be handled (e.g. because the first one is late)? *)
        GS.heartbeat gossip_state |> update_gossip_state state
        |> handle_heartheat
    | P2P_input event -> apply_p2p_event state event
    | App_input event -> apply_app_event state event

  (** A helper function that pushes events in the state *)
  let push e {status = _; state} = Stream.push e state.events_stream

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
    let stream = t.state.events_stream in
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
    let events_stream = t.state.events_stream in
    let events_logging = t.state.events_logging in
    let rec loop t =
      let* event = Stream.pop events_stream in
      if !shutdown then return ()
      else
        let* () = events_logging event in
        t.state <- apply_event t.state event ;
        loop t
    in
    let promise = loop t in
    let schedule_cancellation () =
      shutdown := true ;
      promise
    in
    {schedule_cancellation}

  let start topics t =
    match t.status with
    | Starting ->
        let heartbeat_span =
          View.((view t.state.gossip_state).limits.heartbeat_interval)
        in
        let heartbeat_handle = heartbeat_events_producer ~heartbeat_span t in
        let event_loop_handle = event_loop t in
        let status = Running {heartbeat_handle; event_loop_handle} in
        t.status <- status ;
        List.iter (fun topic -> app_input t (Join topic)) topics
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

  let make ?(events_logging = fun _event -> Monad.return ()) rng limits
      parameters =
    {
      status = Starting;
      state =
        {
          stats = Introspection.empty_stats ();
          gossip_state = GS.make rng limits parameters;
          trusted_peers = Peer.Set.empty;
          connected_bootstrap_peers = Peer.Set.empty;
          events_stream = Stream.empty ();
          p2p_output_stream = Stream.empty ();
          app_output_stream = Stream.empty ();
          events_logging;
        };
    }

  let stats t = t.state.stats

  let p2p_output_stream t = t.state.p2p_output_stream

  let app_output_stream t = t.state.app_output_stream

  let input_events_stream t = t.state.events_stream

  let is_subscribed t topic =
    GS.Introspection.(has_joined topic (view t.state.gossip_state))

  let pp_list pp_elt =
    Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ") pp_elt

  let pp_message_with_header fmt {message; message_id; topic} =
    Format.fprintf
      fmt
      "Message_with_header{message=%a, message_id=%a, topic=%a}"
      Message.pp
      message
      Message_id.pp
      message_id
      Topic.pp
      topic

  let pp_p2p_message fmt = function
    | Graft {topic} -> Format.fprintf fmt "Graft{topic=%a}" Topic.pp topic
    | Prune {topic; px; backoff} ->
        Format.fprintf
          fmt
          "Prune{topic=%a, px=%a, backoff=%a}"
          Topic.pp
          topic
          (pp_list Peer.pp)
          (List.of_seq px)
          GS.Span.pp
          backoff
    | IHave {topic; message_ids} ->
        Format.fprintf
          fmt
          "IHave{topic=%a, message_ids=%a}"
          Topic.pp
          topic
          (pp_list Message_id.pp)
          message_ids
    | IWant {message_ids} ->
        Format.fprintf
          fmt
          "IWant{message_ids=%a}"
          (pp_list Message_id.pp)
          message_ids
    | Subscribe {topic} ->
        Format.fprintf fmt "Subscribe{topic=%a}" Topic.pp topic
    | Unsubscribe {topic} ->
        Format.fprintf fmt "Unsubscribe{topic=%a}" Topic.pp topic
    | Message_with_header message_with_header ->
        pp_message_with_header fmt message_with_header

  let pp_p2p_output fmt = function
    | Disconnect {peer} -> Format.fprintf fmt "Disconnect{peer=%a}" Peer.pp peer
    | Kick {peer} -> Format.fprintf fmt "Kick{peer=%a}" Peer.pp peer
    | Connect {px; origin} ->
        Format.fprintf
          fmt
          "Connect{px=%a; origin=%a}"
          Peer.pp
          px
          (fun fmt origin ->
            match origin with
            | PX peer -> Peer.pp fmt peer
            | Trusted -> Format.fprintf fmt "(trusted)")
          origin
    | Forget {px; origin} ->
        Format.fprintf fmt "Forget{px=%a; origin=%a}" Peer.pp px Peer.pp origin
    | Out_message {to_peer; p2p_message} ->
        Format.fprintf
          fmt
          "Out_message{peer=%a, p2p_message=%a}"
          Peer.pp
          to_peer
          pp_p2p_message
          p2p_message

  let pp_app_output = pp_message_with_header
end
