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

module Profiler = (val Profiler.wrap Gossipsub_profiler.gossipsub_profiler)

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/5165

   Add coverage unit tests *)

module Make (C : Gossipsub_intf.WORKER_CONFIGURATION) :
  Gossipsub_intf.WORKER
    with module GS = C.GS
     and module Monad = C.Monad
     and module Stream = C.Stream
     and module Point = C.Point = struct
  module GS = C.GS
  module Monad = C.Monad
  module Stream = C.Stream
  module View = GS.Introspection
  module Topic = GS.Topic
  module Peer = GS.Peer
  module Message_id = GS.Message_id
  module Message = GS.Message
  module Point = C.Point

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
      mutable count_recv_outdated_app_messages : int64;
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
        count_recv_outdated_app_messages = 0L;
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

    let update_count_recv_outdated_app_messages t delta =
      t.count_recv_outdated_app_messages <-
        t.count_recv_outdated_app_messages ++ counter_update delta

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
    | Crashed

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
    | Ping

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
    | Connect of {peer : Peer.t; origin : peer_origin}
    | Connect_point of {point : Point.t}
    | Forget of {peer : Peer.t; origin : Peer.t}

  type app_output = message_with_header

  (** The different kinds of events the Gossipsub worker handles. *)
  type event =
    | Heartbeat
    | P2P_input of p2p_input
    | App_input of app_input
    | Check_unknown_messages
    | Process_batch of (GS.receive_message * Peer.Set.t) list

  module Bounded_message_map = struct
    (* We maintain the invariant that:
       1. [size <= capacity]
       2. [Map.cardinal map = size]
    *)
    type t = {
      capacity : int;
      size : int;
      map : GS.receive_message Message_id.Map.t;
    }

    let make ~capacity =
      assert (capacity >= 0) ;
      {capacity; size = 0; map = Message_id.Map.empty}

    let add message t =
      let key = message.GS.message_id in
      match Message_id.Map.find_opt key t.map with
      | Some _ -> t
      | None ->
          if t.size < t.capacity then
            {
              t with
              size = t.size + 1;
              map = Message_id.Map.add key message t.map;
            }
          else
            let map =
              Message_id.Map.max_binding t.map
              |> Option.map (fun (key, _) -> Message_id.Map.remove key t.map)
              |> Option.value ~default:t.map
            in
            {t with map = Message_id.Map.add key message map}

    let remove_min t =
      match Message_id.Map.min_binding_opt t.map with
      | None -> None
      | Some (key, value) ->
          let t =
            {t with size = t.size - 1; map = Message_id.Map.remove key t.map}
          in
          Some (value, t)
  end

  module ReceiveMsgMap = Map.Make (struct
    type t = GS.receive_message

    let compare (m1 : t) (m2 : t) =
      Compare.or_else
        (Compare.or_else (Topic.compare m1.topic m2.topic) (fun () ->
             Message_id.compare m1.message_id m2.message_id))
        (fun () -> Message.compare m1.message m2.message)
  end)

  type batch_state = Pending | Accumulating of Peer.Set.t ReceiveMsgMap.t

  (** The worker's state is made of the gossipsub automaton's state,
      and a stream of events to process. It also has two output streams to
      communicate with the application and P2P layers. *)
  type worker_state = {
    stats : Introspection.stats;
    gossip_state : GS.state;
    persistent_points : unit -> Point.t list;
    trusted_peers : Peer.Set.t;
    connected_bootstrap_peers : Peer.Set.t;
    events_stream : event Stream.t;
    p2p_output_stream : p2p_output Stream.t;
    app_output_stream : app_output Stream.t;
    events_logging : event -> unit Monad.t;
    unknown_validity_messages : Bounded_message_map.t;
    unreachable_points : int64 Point.Map.t;
        (* For each point, stores the next heartbeat tick when we can try to recontact this point again. *)
    message_handling : GS.message_handling;
  }

  (** A worker instance is made of its status and state. *)
  type t = {
    mutable status : worker_status;
    mutable state : worker_state;
    self : Peer.t;
    main_loop_promise : unit Lwt.t * unit Lwt.u;
  }

  let maybe_reachable_point = C.maybe_reachable_point

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
          | Subscribe _ | Unsubscribe _ | Ping -> ())
      | Connect _ | Connect_point _ | Disconnect _ | Forget _ | Kick _ -> ()
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
              | Graft _ | Prune _ | Subscribe _ | Unsubscribe _ | Ping -> true)
          | Connect _ | Connect_point _ | Disconnect _ | Forget _ | Kick _ ->
              true
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
      - Notifying the application layer if it is interested in it.

      Note that it's the responsibility of the automaton modules to filter out
      peers based on various criteria (bad score, connection expired, ...). *)
  let handle_receive_message (received_message : GS.receive_message) :
      worker_state * [`Receive_message] GS.output -> worker_state = function
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
    | state, GS.Already_received
    | state, GS.Not_subscribed
    | state, GS.To_include_in_batch _ ->
        state
    | state, GS.Unknown_validity ->
        Introspection.update_count_recv_unknown_validity_app_messages
          state.stats
          `Incr ;
        let unknown_validity_messages =
          Bounded_message_map.add
            received_message
            state.unknown_validity_messages
        in
        let state = {state with unknown_validity_messages} in
        state
    | state, GS.Outdated ->
        Introspection.update_count_recv_outdated_app_messages state.stats `Incr ;
        state
    | state, GS.Invalid_message ->
        Introspection.update_count_recv_invalid_app_messages state.stats `Incr ;
        state

  (** From the worker's perspective, the outcome of joining a new topic from the
      application layer are:
      - Sending [Subscribe] messages to connected peers with that topic;
      - Sending [Graft] messages to the newly constructed topic's mesh. *)
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
  let handle_prune ~self ~from_peer input_px =
    let forget_all state =
      emit_p2p_output
        state
        ~mk_output:(fun to_peer -> Forget {peer = to_peer; origin = from_peer})
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
        let peers =
          let GS.Introspection.
                {connections = current_connections; heartbeat_ticks; _} =
            GS.Introspection.(view state.gossip_state)
          in
          let can_be_contacted peer =
            let point = maybe_reachable_point peer in
            match Point.Map.find_opt point state.unreachable_points with
            | None -> true
            | Some next_attempt ->
                Int64.compare heartbeat_ticks next_attempt >= 0
          in
          Peer.Set.filter
            (fun peer ->
              (not (GS.Introspection.Connections.mem peer current_connections))
              && (not (Peer.equal peer self))
              && can_be_contacted peer)
            peers
        in
        emit_p2p_output
          state
          ~mk_output:(fun to_peer ->
            Connect {peer = to_peer; origin = PX from_peer})
          (Peer.Set.to_seq peers) ;
        (* Forget peers that were filtered out by the automaton. *)
        emit_p2p_output
          state
          ~mk_output:(fun to_peer ->
            Forget {peer = to_peer; origin = from_peer})
          (Peer.Set.to_seq @@ Peer.Set.(diff (of_seq input_px) peers)) ;
        state
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5426

     Add more verification/attacks protections as done in Rust. *)

  let may_emit_pings state =
    let gstate = state.gossip_state in
    let gstate_view = View.view gstate in
    let heartbeat_ping_interval =
      Int64.of_int gstate_view.limits.heartbeat_ping_interval
    in
    let heartbeat_ticks = gstate_view.heartbeat_ticks in
    let rem = Int64.rem heartbeat_ticks heartbeat_ping_interval in
    if Int64.equal rem 0L then
      GS.Introspection.Connections.iter
        (fun peer _connection -> emit_p2p_message state Ping (Seq.return peer))
        gstate_view.connections

  (** On a [Heartbeat] events, the worker sends graft and prune
      messages following the automaton's output. It also sends [IHave]
      messages (computed by the automaton as well). It also send ping
      messages to its for each peer in its connections every
      [heartbeat_ping_interval] ticks. *)
  let handle_heartbeat = function
    | state, GS.Heartbeat {to_graft; to_prune; noPX_peers} ->
        let gstate = state.gossip_state in
        let gstate_view = View.view gstate in
        may_emit_pings state ;
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
        let point_can_be_contacted point =
          match Point.Map.find_opt point state.unreachable_points with
          | None -> true
          | Some next_attempt ->
              Int64.compare gstate_view.heartbeat_ticks next_attempt >= 0
        in
        let peer_can_be_contacted peer =
          let point = maybe_reachable_point peer in
          point_can_be_contacted point
        in
        (* Once every 15 hearbreat ticks, try to reconnect to trusted peers if
           they are disconnected. Also try to reconnect to bootstrap points. *)
        if Int64.(equal (rem gstate_view.heartbeat_ticks 15L) 0L) then (
          (* TODO: https://gitlab.com/tezos/tezos/-/issues/6636

             Put the value [15L] as a parameter. *)
          Peer.Set.fold
            (fun trusted_peer seq ->
              if View.Connections.mem trusted_peer gstate_view.View.connections
              then seq
              else Seq.cons trusted_peer seq)
            state.trusted_peers
            Seq.empty
          |> Seq.filter peer_can_be_contacted
          |> emit_p2p_output state ~mk_output:(fun trusted_peer ->
                 Connect {peer = trusted_peer; origin = Trusted}) ;
          let p2p_output_stream = state.p2p_output_stream in
          let persistent_points =
            state.persistent_points ()
            |> List.filter point_can_be_contacted
            |> Point.Set.of_list
          in
          Point.Set.iter
            (fun point -> Stream.push (Connect_point {point}) p2p_output_stream)
            persistent_points) ;
        let state =
          (* We reset the map every 6h. This prevents this map to contain outdated points. *)
          if Int64.(equal (rem gstate_view.heartbeat_ticks 21600L) 0L) then
            {state with unreachable_points = Point.Map.empty}
          else state
        in
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

  let handle_batch (state, GS.Batch_result batch) =
    List.fold_left
      (fun state (msg, out) -> handle_receive_message msg (state, out))
      state
      batch

  let apply_batch_event ({gossip_state; _} as state) batch =
    GS.apply_batch batch gossip_state
    |> update_gossip_state state |> handle_batch

  (* This function uses Lwt.async, making the gossipsub library not independent
     of the concurrency model anymore.
     If independence from [Lwt] is considered useful to recover, a proposed
     implementation could be:
     https://gitlab.com/tezos/tezos/-/commit/a361dd827b43b601eb9de89e7187f9bcf7fc64d1
     This implementation is not the current implementation, because it has the
     disadvantage of having a "ticking clock" and accumulate whatever arrives
     between two ticks. It was considered nicer to have the accumulation to
     start at the reception of the first shard, because due to network latencies,
     the reception will most probably not align well with the clock.

     Even with current design, it happens to have the shards for a commitment
     to be splitted between two batches. Increasing the frequency of this event
     would probably not be an issue. *)
  let batch_accumulator =
    let current_batch = ref Pending in
    fun output time_interval events_stream ->
      match output with
      | GS.To_include_in_batch (msg, peers) -> (
          match !current_batch with
          | Pending ->
              let open Lwt_syntax in
              Lwt.async (fun () ->
                  let* () = Lwt_unix.sleep (GS.Span.to_float_s time_interval) in
                  let batch =
                    match !current_batch with
                    | Accumulating batch -> ReceiveMsgMap.bindings batch
                    | Pending -> []
                  in
                  current_batch := Pending ;
                  Stream.push (Process_batch batch) events_stream ;
                  return_unit) ;
              let content_map = ReceiveMsgMap.singleton msg peers in
              current_batch := Accumulating content_map
          | Accumulating prev_contents ->
              let new_contents =
                ReceiveMsgMap.update
                  msg
                  (function
                    | None -> Some peers
                    | Some prev_peers -> Some (Peer.Set.inter peers prev_peers))
                  prev_contents
              in
              current_batch := Accumulating new_contents)
      | _ -> ()

  (** Handles applicative (business-level) messages received from the P2P network,
      i.e. messages that are not control ones like GRAFT, IWANT, SUBSCRIBE... *)
  let handle_app_message state message =
    let batching_configuration = state.message_handling in
    let new_state, output =
      GS.handle_receive_message
        ~batching_configuration
        message
        state.gossip_state
    in
    (match batching_configuration with
    | Sequentially -> ()
    | In_batches {time_interval} ->
        batch_accumulator output time_interval state.events_stream) ;
    update_gossip_state state (new_state, output)
    |> handle_receive_message message

  (** Handling messages received from the P2P network. *)
  let apply_p2p_message ~self ({gossip_state; _} as state) from_peer = function
    | Message_with_header {message; topic; message_id} ->
        (let receive_message =
           {GS.sender = from_peer; topic; message_id; message}
         in
         handle_app_message state receive_message)
        [@profiler.span_f
          {verbosity = Notice}
            ["apply_event"; "P2P_input"; "In_message"; "Message_with_header"]]
    | Graft {topic} ->
        let graft : GS.graft = {peer = from_peer; topic} in
        (GS.handle_graft graft gossip_state
        |> update_gossip_state state
        |> handle_graft from_peer topic)
        [@profiler.span_f
          {verbosity = Notice}
            ["apply_event"; "P2P_input"; "In_message"; "Graft"]]
    | Subscribe {topic} ->
        let subscribe : GS.subscribe = {peer = from_peer; topic} in
        (GS.handle_subscribe subscribe gossip_state
        |> update_gossip_state state |> handle_subscribe)
        [@profiler.span_f
          {verbosity = Notice}
            ["apply_event"; "P2P_input"; "In_message"; "Subscribe"]]
    | Unsubscribe {topic} ->
        let unsubscribe : GS.unsubscribe = {peer = from_peer; topic} in
        (GS.handle_unsubscribe unsubscribe gossip_state
        |> update_gossip_state state |> handle_unsubscribe)
        [@profiler.span_f
          {verbosity = Notice}
            ["apply_event"; "P2P_input"; "In_message"; "Unsubscribe"]]
    | IHave {topic; message_ids} ->
        (* The automaton should guarantee that the list is not empty. *)
        let ihave : GS.ihave = {peer = from_peer; topic; message_ids} in
        (GS.handle_ihave ihave gossip_state
        |> update_gossip_state state |> handle_ihave ihave)
        [@profiler.span_f
          {verbosity = Notice}
            ["apply_event"; "P2P_input"; "In_message"; "IHave"]]
    | IWant {message_ids} ->
        (* The automaton should guarantee that the list is not empty. *)
        let iwant : GS.iwant = {peer = from_peer; message_ids} in
        (GS.handle_iwant iwant gossip_state
        |> update_gossip_state state |> handle_iwant iwant)
        [@profiler.span_f
          {verbosity = Notice}
            ["apply_event"; "P2P_input"; "In_message"; "IWant"]]
    | Prune {topic; px; backoff} ->
        let prune : GS.prune = {peer = from_peer; topic; px; backoff} in
        (GS.handle_prune prune gossip_state
        |> update_gossip_state state
        |> handle_prune ~self ~from_peer px)
        [@profiler.span_f
          {verbosity = Notice}
            ["apply_event"; "P2P_input"; "In_message"; "Prune"]]
    | Ping ->
        (* We treat [Ping] message as a no-op and return the current [state]. *)
        state
        [@profiler.span_f
          {verbosity = Notice}
            ["apply_event"; "P2P_input"; "In_message"; "Ping"]]

  (** Handling events received from P2P layer. *)
  let apply_p2p_event ~self ({gossip_state; _} as state) = function
    | New_connection {peer; direct; trusted; bootstrap} ->
        ((GS.add_peer {direct; outbound = trusted; peer; bootstrap} gossip_state
         |> update_gossip_state state
         |> handle_new_connection peer ~bootstrap ~trusted)
         [@profiler.span_f
           {verbosity = Notice} ["apply_event"; "P2P_input"; "New_connection"]])
    | Disconnection {peer} ->
        ((GS.remove_peer {peer} gossip_state
         |> update_gossip_state state |> handle_disconnection peer)
         [@profiler.span_f
           {verbosity = Notice} ["apply_event"; "P2P_input"; "Disconnection"]])
    | In_message {from_peer; p2p_message} ->
        apply_p2p_message
          ~self
          state
          from_peer
          p2p_message
        [@profiler.span_f
          {verbosity = Notice} ["apply_event"; "P2P_input"; "In_message"]]

  let rec check_unknown_messages_id state =
    match Bounded_message_map.remove_min state.unknown_validity_messages with
    | None -> state
    | Some (message, unknown_validity_messages) -> (
        (* Any message whose validity status of its id is known is processed.
           The automaton needs to process only valid and invalid messages.
           Outdated messages are dropped.

           We check the status of the first message to know whether we should
           process more messages. Indeed, a message has an unknown validity only
           if it is from a future level, and if the first message is from the
           future, then all other messages are from the future as well (given
           that the order used by `Bounded_message_map` is the order on levels),
           so they do not need to be checked.

           The same message may have been sent by multiple senders, but it will
           appear only once in the map. As a consequence, it will be handle by
           the automaton only once. This is different from the case where the
           validity of the message is known the first time it is received.

           This also mean score-wise that only the first sender will be
           impacted. This may introduce a small biais which should have a
           relatively small impact on the positive case. If the message is
           invalid, then only the first one will be punished. It is not that bad
           since this state is transient.
        *)
        match GS.Message_id.valid message.message_id with
        | `Valid | `Invalid ->
            let state = {state with unknown_validity_messages} in
            handle_app_message state message
            |>
            (* Other messages are processed recursively *)
            check_unknown_messages_id
        | `Unknown -> state
        | `Outdated ->
            let state = {state with unknown_validity_messages} in
            check_unknown_messages_id state)

  (** This is the main function of the worker. It interacts with the Gossipsub
      automaton given an event. The function possibly sends messages to the P2P
      and application layers and returns the new worker's state. *)
  let apply_event ~self ({gossip_state; _} as state) = function
    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5326

       Notify the GS worker about the status of messages sent to peers. *)
    | Heartbeat ->
        (((* TODO: https://gitlab.com/tezos/tezos/-/issues/5170

             Do we want to detect cases where two successive [Heartbeat] events
             would be handled (e.g. because the first one is late)? *)
          GS.heartbeat gossip_state
         |> update_gossip_state state |> handle_heartbeat)
         [@profiler.span_f {verbosity = Notice} ["apply_event"; "Heartbeat"]])
    | P2P_input event ->
        apply_p2p_event
          ~self
          state
          event
        [@profiler.span_f {verbosity = Notice} ["apply_event"; "P2P_input"]]
    | App_input event ->
        apply_app_event
          state
          event
        [@profiler.span_f {verbosity = Notice} ["apply_event"; "App_input"]]
    | Check_unknown_messages ->
        check_unknown_messages_id
          state
        [@profiler.span_f
          {verbosity = Notice} ["apply_event"; "Check_unknown_messages"]]
    | Process_batch batch -> apply_batch_event state batch

  (** A helper function that pushes events in the state *)
  let push e {status = _; state; self = _; _} =
    Stream.push e state.events_stream

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
      Stream.push Check_unknown_messages stream ;
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
      extra event to consume in order to resolve.

      When the loop catches an exception, it is raised to the main_loop promise
      to help the handling of a failing gossipsub_worker. *)
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
        try
          let new_state =
            (apply_event
               ~self:t.self
               t.state
               event [@profiler.span_f {verbosity = Notice} ["apply_event"]])
          in
          t.state <- new_state ;
          loop t
        with exn ->
          t.status <- Crashed ;
          Lwt.wakeup_exn (snd t.main_loop_promise) exn ;
          return ()
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
    | Running _ | Crashed ->
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5166

           Better error handling *)
        Format.eprintf "A worker is already running for this state!@." ;
        assert false

  (** Shutting down may require waiting up [heartbeat_span] to stop the
      heartbeat event loop. *)
  let shutdown state =
    let open C.Monad in
    match state.status with
    | Starting | Crashed -> return ()
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

  let make ?(events_logging = fun _event -> Monad.return ())
      ?(initial_points = fun () -> []) ?batching_interval ~self rng limits
      parameters =
    {
      self;
      status = Starting;
      state =
        {
          persistent_points = initial_points;
          stats = Introspection.empty_stats ();
          gossip_state = GS.make rng limits parameters;
          trusted_peers = Peer.Set.empty;
          connected_bootstrap_peers = Peer.Set.empty;
          events_stream = Stream.empty ();
          p2p_output_stream = Stream.empty ();
          app_output_stream = Stream.empty ();
          events_logging;
          unknown_validity_messages = Bounded_message_map.make ~capacity:10_000;
          unreachable_points = Point.Map.empty;
          message_handling =
            (match batching_interval with
            | None -> Sequentially
            | Some time_interval -> In_batches {time_interval});
        };
      main_loop_promise = Lwt.wait ();
    }

  let stats t = t.state.stats

  let main_loop_promise t = fst t.main_loop_promise

  let p2p_output_stream t = t.state.p2p_output_stream

  let app_output_stream t = t.state.app_output_stream

  let input_events_stream t = t.state.events_stream

  let reconnection_delays {state = {unreachable_points; gossip_state; _}; _} =
    let heartbeat_span = View.((view gossip_state).limits.heartbeat_interval) in
    let View.{heartbeat_ticks; _} = View.(view gossip_state) in
    unreachable_points |> Point.Map.to_seq
    |> Seq.map (fun (point, next_heartbeat_reconnection_tick) ->
           let how_many_ticks_left =
             Int64.(sub next_heartbeat_reconnection_tick heartbeat_ticks)
             |> Int64.to_int
           in
           let span = GS.Span.mul heartbeat_span how_many_ticks_left in
           (point, span))
    |> List.of_seq

  let is_subscribed t topic =
    GS.Introspection.(has_joined topic (view t.state.gossip_state))

  let set_unreachable_point t point =
    let GS.Introspection.{heartbeat_ticks; _} =
      GS.Introspection.(view t.state.gossip_state)
    in
    let unreachable_points =
      Point.Map.update
        point
        (function
          | None -> Some (Int64.add 5L heartbeat_ticks)
          | Some x ->
              Some
                (Int64.mul x 2L |> Int64.min 300L |> Int64.add heartbeat_ticks))
        t.state.unreachable_points
    in
    t.state <- {t.state with unreachable_points}

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
    | Ping -> Format.fprintf fmt "Ping"

  let pp_p2p_output fmt = function
    | Disconnect {peer} -> Format.fprintf fmt "Disconnect{peer=%a}" Peer.pp peer
    | Kick {peer} -> Format.fprintf fmt "Kick{peer=%a}" Peer.pp peer
    | Connect {peer; origin} ->
        Format.fprintf
          fmt
          "Connect{peer=%a; origin=%a}"
          Peer.pp
          peer
          (fun fmt origin ->
            match origin with
            | PX peer -> Peer.pp fmt peer
            | Trusted -> Format.fprintf fmt "(trusted)")
          origin
    | Connect_point {point} ->
        Format.fprintf fmt "Connect_point{point=%a}" Point.pp point
    | Forget {peer; origin} ->
        Format.fprintf
          fmt
          "Forget{peer=%a; origin=%a}"
          Peer.pp
          peer
          Peer.pp
          origin
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
