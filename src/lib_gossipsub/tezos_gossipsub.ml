(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* FIXME https://gitlab.com/tezos/tezos/-/issues/4966

   We should decide whether we want to implement a tracing mechanism
   as in the go implementation.
*)

module Gossipsub_intf = Gossipsub_intf
open Gossipsub_intf
module Worker = Gossipsub_worker.Make

module Make (C : AUTOMATON_CONFIG) :
  AUTOMATON
    with type Time.t = C.Time.t
     and module Span = C.Span
     and module Peer = C.Subconfig.Peer
     and module Topic = C.Subconfig.Topic
     and module Message_id = C.Subconfig.Message_id
     and module Message = C.Subconfig.Message = struct
  module Peer = C.Subconfig.Peer
  module Topic = C.Subconfig.Topic
  module Message_id = C.Subconfig.Message_id
  module Message = C.Subconfig.Message
  module Span = C.Span
  module Time = C.Time
  module Score = Peers_score.Make (Span) (Time) (Topic)

  type message = Message.t

  type time = Time.t

  type span = Time.span

  type nonrec limits = (Topic.t, Peer.t, Message_id.t, span) limits

  type nonrec parameters = (Peer.t, Message_id.t) parameters

  type add_peer = {direct : bool; outbound : bool; peer : Peer.t}

  type remove_peer = {peer : Peer.t}

  type ihave = {peer : Peer.t; topic : Topic.t; message_ids : Message_id.t list}

  type iwant = {peer : Peer.t; message_ids : Message_id.t list}

  type graft = {peer : Peer.t; topic : Topic.t}

  type prune = {
    peer : Peer.t;
    topic : Topic.t;
    px : Peer.t Seq.t;
    backoff : span;
  }

  type publish_message = {
    topic : Topic.t;
    message_id : Message_id.t;
    message : message;
  }

  type receive_message = {
    sender : Peer.t;
    topic : Topic.t;
    message_id : Message_id.t;
    message : message;
  }

  type join = {topic : Topic.t}

  type leave = {topic : Topic.t}

  type subscribe = {topic : Topic.t; peer : Peer.t}

  type unsubscribe = {topic : Topic.t; peer : Peer.t}

  type set_application_score = {peer : Peer.t; score : float}

  (* FIXME not sure subtyping for output is useful. If it is, it is
     probably for few ouputs and could be removed. *)
  type _ output =
    | Ihave_from_peer_with_low_score : {
        score : Score.t;
        threshold : float;
      }
        -> [`IHave] output
    | Too_many_recv_ihave_messages : {count : int; max : int} -> [`IHave] output
    | Too_many_sent_iwant_messages : {count : int; max : int} -> [`IHave] output
    | Message_topic_not_tracked : [`IHave] output
    | Message_requested_message_ids : Message_id.t list -> [`IHave] output
    | Iwant_from_peer_with_low_score : {
        score : Score.t;
        threshold : float;
      }
        -> [`IWant] output
    | On_iwant_messages_to_route : {
        routed_message_ids :
          [`Ignored | `Not_found | `Too_many_requests | `Message of message]
          Message_id.Map.t;
      }
        -> [`IWant] output
    | Peer_filtered : [`Graft] output
    | Unsubscribed_topic : [`Graft] output
    | Peer_already_in_mesh : [`Graft] output
    | Grafting_direct_peer : [`Graft] output
    | Unexpected_grafting_peer : [`Graft] output
    | Grafting_peer_with_negative_score : [`Graft] output
    | Grafting_successfully : [`Graft] output
    | Peer_backed_off : [`Graft] output
    | Mesh_full : [`Graft] output
    | Prune_topic_not_tracked : [`Prune] output
    | Peer_not_in_mesh : [`Prune] output
    | Ignore_PX_score_too_low : Score.t -> [`Prune] output
    | No_PX : [`Prune] output
    | PX : Peer.Set.t -> [`Prune] output
    | Publish_message : {to_publish : Peer.Set.t} -> [`Publish_message] output
    | Already_published : [`Publish_message] output
    | Route_message : {to_route : Peer.Set.t} -> [`Receive_message] output
    | Already_received : [`Receive_message] output
    | Not_subscribed : [`Receive_message] output
    | Invalid_message : [`Receive_message] output
    | Unknown_validity : [`Receive_message] output
    | Already_joined : [`Join] output
    | Joining_topic : {to_graft : Peer.Set.t} -> [`Join] output
    | Not_joined : [`Leave] output
    | Leaving_topic : {
        to_prune : Peer.Set.t;
        noPX_peers : Peer.Set.t;
      }
        -> [`Leave] output
    | Heartbeat : {
        to_graft : Topic.Set.t Peer.Map.t;
        to_prune : Topic.Set.t Peer.Map.t;
        noPX_peers : Peer.Set.t;
      }
        -> [`Heartbeat] output
    | Peer_added : [`Add_peer] output
    | Peer_already_known : [`Add_peer] output
    | Removing_peer : [`Remove_peer] output
    | Subscribed : [`Subscribe] output
    | Subscribe_to_unknown_peer : [`Subscribe] output
    | Unsubscribed : [`Unsubscribe] output
    | Unsubscribe_from_unknown_peer : [`Unsubscribe] output
    | Set_application_score : [`Set_application_score] output

  type connection = {
    topics : Topic.Set.t;  (** The set of topics the peer subscribed to. *)
    direct : bool;
        (** A direct (aka explicit) connection is a connection to which we
            forward all the messages. *)
    outbound : bool;  (** An outbound connection is a connection we initiated. *)
  }

  type fanout_peers = {peers : Peer.Set.t; last_published_time : time}

  module Message_cache = Message_cache.Make (C.Subconfig) (Time)

  type state = {
    limits : limits;  (** Statically known parameters of the algorithm. *)
    parameters : parameters;  (** Other parameters of the algorithm. *)
    connections : connection Peer.Map.t;
        (** [connections] is the set of active connections. A connection is added
            through the `Add_peer` message and removed through the `Remove_peer`
            message. *)
    scores : Score.t Peer.Map.t;
        (** The scores are used to drive peer selection mechanisms. Scores are kept
            for at least [retain_duration] after a connection is removed, hence they
            can't be stored in {!connections}. Any peer having an active connection
            is associated to a score. *)
    ihave_per_heartbeat : int Peer.Map.t;
        (** Mapping tracking for each peer the number of IHave messages received from
            that peer between two heartbeats. *)
    iwant_per_heartbeat : int Peer.Map.t;
        (** Mapping tracking for each peer the number of messages ids sent in
            IWant messages to that peer between two heartbeats. *)
    mesh : Peer.Set.t Topic.Map.t;
        (** The mesh for a topic is a random subset of the connected peers
            subscribed to that topic and that have non-negative score, are not
            backed-off, and are not direct peers. The local peer routes
            full-messages to these peers. A topic is in the domain of the mesh
            iff the local peer has joined the topic (we also say that it tracks
            the topic). *)
    fanout : fanout_peers Topic.Map.t;
        (** The fanout for a topic is a random subset of the connected peers
            subscribed to that topic and that are not direct and have a score
            above some given threshold. The local peer routes full-messages to
            these peers. In contrast to the mesh, the fanout map contains topics
            the local peer has not joined. *)
    backoff : time Peer.Map.t Topic.Map.t;
        (** The backoff times associated to a topic and a peer. When a backoff is
             set, we refuse any `graft` from this peer. As a consequence, if a
             backoff is set for a peer and a topic, this peer is not expected
             to be in the mesh of the given topic.  *)
    message_cache : Message_cache.t;
        (** A sliding window cache that stores published messages and their first
        seen time. *)
    rng : Random.State.t;  (** The state of the PRNG algorithm. *)
    heartbeat_ticks : int64;
        (** A counter of the number of elapsed heartbeat ticks.  *)
  }
  (* Invariants:

     - Forall t, Topic.Map.mem t mesh <=> not (Map.mem t fanout)

     - Forall p c t, Peer.Map.find connections p = Some ({ connection = Expires _}) ->
         Topic.Map.find t mesh = None &&
         Topic.Map.find t fanout = None

     - Forall p t, Peer.Map.find connections p = None ->
         Topic.Map.find t mesh = None &&
         Topic.Map.find t fanout = None

     - Forall p. Peer.Map.mem connections p -> Peer.Map.mem scores p

     - Forall p, Peer.Map.mem connections p <-> Peer.Map.find scores p = Some {peer_status = Connected; _}
  *)

  (* FIXME https://gitlab.com/tezos/tezos/-/issues/4984

     Test the those invariants
  *)
  module Monad = struct
    type 'a t = (state, 'a) State_monad.t

    type ('pass, 'fail) check = (state, 'pass, 'fail) State_monad.check

    include State_monad.M
  end

  let assert_in_unit_interval v = assert (v >= 0.0 && v <= 1.0)

  let check_per_topic_score_limits tsp =
    assert (tsp.time_in_mesh_weight >= 0.0) ;
    assert (tsp.time_in_mesh_cap >= 0.0) ;
    assert (tsp.time_in_mesh_quantum > 0.0) ;
    assert (tsp.first_message_deliveries_weight >= 0.0) ;
    assert (tsp.first_message_deliveries_cap >= 0) ;
    assert_in_unit_interval tsp.first_message_deliveries_decay ;
    assert (tsp.mesh_message_deliveries_weight <= 0.0) ;
    assert (Span.(tsp.mesh_message_deliveries_activation >= of_int_s 1)) ;
    assert (tsp.mesh_message_deliveries_cap >= 0) ;
    assert (tsp.mesh_message_deliveries_threshold > 0) ;
    assert_in_unit_interval tsp.mesh_message_deliveries_decay ;
    assert (tsp.mesh_failure_penalty_weight <= 0.0) ;
    assert_in_unit_interval tsp.mesh_failure_penalty_decay ;
    assert (tsp.invalid_message_deliveries_weight <= 0.0) ;
    assert_in_unit_interval tsp.invalid_message_deliveries_decay

  let check_score_limits (sp : _ score_limits) =
    (match sp.topics with
    | Topic_score_limits_single tp -> check_per_topic_score_limits tp
    | Topic_score_limits_family {all_topics; parameters; weights = _} ->
        Seq.map parameters all_topics |> Seq.iter check_per_topic_score_limits) ;
    Option.iter (fun cap -> assert (cap >= 0.0)) sp.topic_score_cap ;
    assert (sp.behaviour_penalty_weight <= 0.0) ;
    assert (sp.behaviour_penalty_threshold >= 0.0) ;
    assert_in_unit_interval sp.behaviour_penalty_decay ;
    assert (sp.decay_zero >= 0.0)

  let check_limits l =
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/5129
       Replace the asserts by something more informative. *)
    assert (l.degree_low > 0) ;
    assert (l.degree_out >= 0) ;
    assert (l.degree_score >= 0) ;
    assert (l.degree_low <= l.degree_optimal) ;
    assert (l.degree_high >= l.degree_optimal) ;
    assert (l.backoff_cleanup_ticks > 0) ;
    assert (l.score_cleanup_ticks > 0) ;
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/5052
       This requirement is not imposed in the spec/Go implementation. Relax this
       requirement or delete the todo. *)
    assert (l.degree_score + l.degree_out <= l.degree_optimal) ;
    assert (l.degree_out <= l.degree_low) ;
    assert (l.degree_out <= l.degree_optimal / 2) ;
    assert (l.history_gossip_length > 0) ;
    assert (l.history_gossip_length <= l.history_length) ;
    check_score_limits l.score_limits

  let make : Random.State.t -> limits -> parameters -> state =
   fun rng limits parameters ->
    check_limits limits ;
    {
      limits;
      parameters;
      connections = Peer.Map.empty;
      scores = Peer.Map.empty;
      ihave_per_heartbeat = Peer.Map.empty;
      iwant_per_heartbeat = Peer.Map.empty;
      mesh = Topic.Map.empty;
      fanout = Topic.Map.empty;
      backoff = Topic.Map.empty;
      message_cache =
        Message_cache.create
          ~history_slots:limits.history_length
          ~gossip_slots:limits.history_gossip_length
          ~seen_message_slots:limits.seen_history_length;
      rng;
      heartbeat_ticks = 0L;
    }

  module Helpers = struct
    let fail_if cond output_err =
      let open Monad.Syntax in
      if cond then fail output_err else unit

    let fail_if_not cond output_err = fail_if (not cond) output_err

    (* These projections enable let-punning. *)
    let max_recv_ihave_per_heartbeat state =
      state.limits.max_recv_ihave_per_heartbeat

    let max_sent_iwant_per_heartbeat state =
      state.limits.max_sent_iwant_per_heartbeat

    let degree_optimal state = state.limits.degree_optimal

    let publish_threshold state = state.limits.publish_threshold

    let do_px state = state.limits.do_px

    let peers_to_px state = state.limits.peers_to_px

    let accept_px_threshold state = state.limits.accept_px_threshold

    let prune_backoff state = state.limits.prune_backoff

    let unsubscribe_backoff state = state.limits.unsubscribe_backoff

    let graft_flood_backoff state = state.limits.graft_flood_backoff

    let retain_duration state = state.limits.retain_duration

    let fanout_ttl state = state.limits.fanout_ttl

    let heartbeat_interval state = state.limits.heartbeat_interval

    let backoff_cleanup_ticks state = state.limits.backoff_cleanup_ticks

    let score_cleanup_ticks state = state.limits.score_cleanup_ticks

    let degree_low state = state.limits.degree_low

    let degree_high state = state.limits.degree_high

    let degree_score state = state.limits.degree_score

    let degree_out state = state.limits.degree_out

    let max_gossip_retransmission state = state.limits.max_gossip_retransmission

    let gossip_threshold state = state.limits.gossip_threshold

    let opportunistic_graft_ticks state = state.limits.opportunistic_graft_ticks

    let opportunistic_graft_peers state = state.limits.opportunistic_graft_peers

    let opportunistic_graft_threshold state =
      state.limits.opportunistic_graft_threshold

    let mesh state = state.mesh

    let fanout state = state.fanout

    let backoff state = state.backoff

    let connections state = state.connections

    let scores state = state.scores

    let peer_filter state = state.parameters.peer_filter

    let message_cache state = state.message_cache

    let rng state = state.rng

    let score_limits state = state.limits.score_limits

    let update ?(delta = 1) key map =
      Peer.Map.update
        key
        (function None -> Some delta | Some n -> Some (n + delta))
        map

    let update_and_get ?(delta = 1) key map =
      let res = ref delta in
      Peer.Map.update
        key
        (function
          | None -> Some delta
          | Some n ->
              let value = n + delta in
              res := value ;
              Some value)
        map
      |> fun x -> (x, !res)

    let update_and_get_ihave_per_heartbeat ?delta key state =
      let ihave_per_heartbeat, res =
        update_and_get ?delta key state.ihave_per_heartbeat
      in
      let state = {state with ihave_per_heartbeat} in
      (state, res)

    let update_iwant_per_heartbeat ?delta key state =
      let iwant_per_heartbeat = update ?delta key state.iwant_per_heartbeat in
      let state = {state with iwant_per_heartbeat} in
      (state, ())

    let find ?(default = 0) key map =
      match Peer.Map.find key map with None -> default | Some n -> n

    let find_iwant_per_heartbeat ?default key state =
      find ?default key state.iwant_per_heartbeat

    let reset_ihave_per_heartbeat state =
      ({state with ihave_per_heartbeat = Peer.Map.empty}, ())

    let reset_iwant_per_heartbeat state =
      ({state with iwant_per_heartbeat = Peer.Map.empty}, ())

    let heartbeat_ticks state = state.heartbeat_ticks

    let set_heartbeat_ticks heartbeat_ticks state =
      ({state with heartbeat_ticks}, ())

    let set_connections connections state = ({state with connections}, ())

    let set_scores scores state = ({state with scores}, ())

    let topic_is_tracked topic state =
      let {mesh; _} = state in
      match Topic.Map.find topic mesh with None -> false | Some _ -> true

    let set_message_cache message_cache state = ({state with message_cache}, ())

    let get_scores_score scores peer = Peer.Map.find peer scores

    let get_scores_score_or_zero scores peer =
      get_scores_score scores peer
      |> Option.fold ~none:Score.zero ~some:Score.value

    let get_score peer state = get_scores_score state.scores peer

    let get_score_or_zero peer state =
      get_scores_score_or_zero state.scores peer

    let peer_has_outbound_connection peers ~default peer =
      Peer.Map.find_opt peer peers
      |> Option.map (fun connection -> connection.outbound)
      |> Option.value ~default

    (* TODO: https://gitlab.com/tezos/tezos/-/issues/5391
       Optimize by having a topic to peers map *)
    let select_connections_peers connections scores rng topic ~filter ~max =
      Peer.Map.bindings connections
      |> List.filter_map (fun (peer, connection) ->
             let score = get_scores_score_or_zero scores peer in
             let topics = connection.topics in
             if filter peer connection score && Topic.Set.mem topic topics then
               Some peer
             else None)
      |> List.shuffle ~rng |> List.take_n max

    let select_peers topic ~filter ~max =
      let open Monad.Syntax in
      let*! connections in
      let*! scores in
      let*! rng in
      select_connections_peers connections scores rng topic ~filter ~max
      |> Peer.Set.of_list |> return

    let get_direct_peers topic =
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/5010

         Have a dedicated structure for direct peers? *)
      let filter _peer {direct; _} _score = direct in
      select_peers topic ~filter ~max:max_int

    let set_mesh_topic topic peers state =
      let state = {state with mesh = Topic.Map.add topic peers state.mesh} in
      (state, ())

    let set_mesh mesh state =
      let state = {state with mesh} in
      (state, ())

    let find_mesh topic state = Topic.Map.find topic state.mesh

    let find_fanout topic state = Topic.Map.find topic state.fanout

    let set_fanout_topic topic last_published_time peers state =
      if Peer.Set.is_empty peers then (state, ())
      else
        let state =
          {
            state with
            fanout =
              Topic.Map.add topic {peers; last_published_time} state.fanout;
          }
        in
        (state, ())

    let set_fanout fanout state =
      let state = {state with fanout} in
      (state, ())

    let delete_mesh topic state =
      let state = {state with mesh = Topic.Map.remove topic state.mesh} in
      (state, ())

    let delete_fanout topic state =
      let state = {state with fanout = Topic.Map.remove topic state.fanout} in
      (state, ())

    let put_message_in_cache message_id message topic state =
      let state =
        {
          state with
          message_cache =
            Message_cache.add_message
              message_id
              message
              topic
              state.message_cache;
        }
      in
      (state, ())

    let get_backoff topic peer backoff =
      Option.bind (Topic.Map.find topic backoff) (fun per_peer_backoff ->
          Peer.Map.find peer per_peer_backoff)

    let exists_backoff topic peer backoff =
      get_backoff topic peer backoff |> Option.is_some

    let set_backoff backoff state =
      let state = {state with backoff} in
      (state, ())

    let add_backoff_for_peer delay topic peer backoffs =
      let now = Time.now () in
      let backoff_expire = Time.add now delay in
      Topic.Map.update
        topic
        (function
          | None -> Some (Peer.Map.singleton peer backoff_expire)
          | Some peer_backoff ->
              Peer.Map.update
                peer
                (function
                  | None -> Some backoff_expire
                  | Some old_backoff ->
                      if Time.(old_backoff < backoff_expire) then
                        Some backoff_expire
                      else Some old_backoff)
                peer_backoff
              |> Option.some)
        backoffs

    let set_backoff_for_peer delay topic peer =
      let open Monad.Syntax in
      let*! backoff in
      add_backoff_for_peer delay topic peer backoff |> set_backoff

    let update_scores_score peer f scores =
      Peer.Map.update peer (Option.map f) scores

    let update_score peer f =
      let open Monad.Syntax in
      let*! scores in
      let scores = update_scores_score peer f scores in
      set_scores scores

    let with_score (opt : Score.t option) f =
      let open Monad.Syntax in
      match opt with None -> pass () | Some v -> f v

    let add_score peer score = update_score peer (Fun.const score)

    let check_score peer threshold ~on_failure =
      let open Monad.Syntax in
      let*! peer_score_opt = get_score peer in
      with_score peer_score_opt @@ fun peer_score ->
      fail_if Score.(value peer_score < threshold) @@ on_failure peer_score
  end

  include Helpers

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/5148
     Consider merging subcribe/unsubscribe with join/leave. *)
  module Subscribe = struct
    let handle topic peer =
      let open Monad.Syntax in
      let*! connections in
      match Peer.Map.find peer connections with
      | None -> return Subscribe_to_unknown_peer
      | Some connection ->
          let connection =
            {connection with topics = Topic.Set.add topic connection.topics}
          in
          let connections = Peer.Map.add peer connection connections in
          let* () = set_connections connections in
          (* TODO: https://gitlab.com/tezos/tezos/-/issues/5143
             rust-libp2p adds the peer to the mesh if needed here. *)
          return Subscribed
  end

  let handle_subscribe : subscribe -> [`Subscribe] output Monad.t =
   fun {topic; peer} -> Subscribe.handle topic peer

  module Unsubscribe = struct
    let remove_peer_from_mesh topic peer =
      let open Monad.Syntax in
      let*! mesh in
      let mesh =
        Topic.Map.update
          topic
          (Option.map (fun peers -> Peer.Set.remove peer peers))
          mesh
      in
      set_mesh mesh

    let remove_peer_from_fanout topic peer =
      let open Monad.Syntax in
      let*! fanout in
      let fanout =
        Topic.Map.update
          topic
          (Option.map (fun fanout_peers ->
               {
                 fanout_peers with
                 peers = Peer.Set.remove peer fanout_peers.peers;
               }))
          fanout
      in
      set_fanout fanout

    let handle topic peer =
      let open Monad.Syntax in
      let*! connections in
      match Peer.Map.find peer connections with
      | None -> return @@ Unsubscribe_from_unknown_peer
      | Some connection ->
          let connection =
            {connection with topics = Topic.Set.remove topic connection.topics}
          in
          let peers = Peer.Map.add peer connection connections in
          let* () = set_connections peers in
          (* Remove the peer from mesh as done in the rust implementation
             but not go implementation. *)
          let* () = remove_peer_from_mesh topic peer in
          (* Remove the peer from fanout. This is not done in go/rust implementation but
             we do it here as it is more natural and consistent with the above mesh cleaning. *)
          let* () = remove_peer_from_fanout topic peer in
          return Unsubscribed
  end

  let handle_unsubscribe : unsubscribe -> [`Unsubscribe] output Monad.t =
   fun {topic; peer} -> Unsubscribe.handle topic peer

  module IHave = struct
    let check_too_many_recv_ihave_message count =
      let open Monad.Syntax in
      let*! max_recv_ihave_per_heartbeat in
      fail_if (count > max_recv_ihave_per_heartbeat)
      @@ Too_many_recv_ihave_messages
           {count; max = max_recv_ihave_per_heartbeat}

    (* FIXME https://gitlab.com/tezos/tezos/-/issues/5016

       This check is not correct if the distant peer uses a different
       value for [max_recv_ihave_per_heartbeat] then our value for
       [max_sent_iwant_per_heartbeat]. *)
    let check_too_many_sent_iwant_message count =
      let open Monad.Syntax in
      let*! max_sent_iwant_per_heartbeat in
      fail_if (count >= max_sent_iwant_per_heartbeat)
      @@ Too_many_sent_iwant_messages
           {count; max = max_sent_iwant_per_heartbeat}

    let check_topic_tracked topic =
      let open Monad.Syntax in
      let*! is_topic_tracked = topic_is_tracked topic in
      fail_if_not is_topic_tracked Message_topic_not_tracked

    let check_not_empty iwant_message_ids =
      fail_if (List.is_empty iwant_message_ids)
      @@ Message_requested_message_ids []

    let filter peer message_ids : Message_id.t list Monad.t =
      let open Monad.Syntax in
      let*! peer_filter in
      let*! message_cache in
      let should_handle_message_id message_id : bool =
        (not (Message_cache.seen_message message_id message_cache))
        && peer_filter peer (`IHave message_id)
      in
      List.filter should_handle_message_id message_ids |> return

    let shuffle_and_trunc message_ids ~limit : (int * Message_id.t list) Monad.t
        =
      let open Monad.Syntax in
      let*! rng in
      let iwant_message_ids_len = List.length message_ids in
      (* Do not send more than [max_sent_iwant_per_heartbeat] message ids. *)
      let iwant_ids_to_send_n = min iwant_message_ids_len limit in
      let shuffle_iwant_ids = List.shuffle ~rng message_ids in
      let requested_message_ids =
        List.take_n iwant_ids_to_send_n shuffle_iwant_ids
      in
      return (iwant_ids_to_send_n, requested_message_ids)

    let handle peer topic message_ids : [`IHave] output Monad.t =
      let open Monad.Syntax in
      let*! gossip_threshold in
      let*? () =
        check_score
          peer
          (Score.of_float gossip_threshold)
          ~on_failure:(fun score ->
            Ihave_from_peer_with_low_score {score; threshold = gossip_threshold})
      in
      let* count_ihave_received = update_and_get_ihave_per_heartbeat peer in
      let*? () = check_too_many_recv_ihave_message count_ihave_received in
      let*! count_iwant_sent = find_iwant_per_heartbeat peer in
      let*? () = check_too_many_sent_iwant_message count_iwant_sent in
      let*? () = check_topic_tracked topic in
      let* iwant_message_ids = filter peer message_ids in
      let*? () = check_not_empty iwant_message_ids in
      let*! max_sent_iwant_per_heartbeat in
      let limit = max_sent_iwant_per_heartbeat - count_iwant_sent in
      (* Invariant: limit > 0 *)
      let* length, requested_message_ids =
        shuffle_and_trunc iwant_message_ids ~limit
      in
      let* () = update_iwant_per_heartbeat ~delta:length peer in
      (* FIXME https://gitlab.com/tezos/tezos/-/issues/5532

         The go implementation traces some of the messages
         requested. *)
      Message_requested_message_ids requested_message_ids |> return
  end

  let handle_ihave : ihave -> [`IHave] output Monad.t =
   fun {peer; topic; message_ids} -> IHave.handle peer topic message_ids

  module IWant = struct
    let handle peer message_ids : [`IWant] output Monad.t =
      let open Monad.Syntax in
      let*! gossip_threshold in
      let*? () =
        check_score
          peer
          (Score.of_float gossip_threshold)
          ~on_failure:(fun score ->
            Iwant_from_peer_with_low_score {score; threshold = gossip_threshold})
      in
      let routed_message_ids = Message_id.Map.empty in
      let*! message_cache in
      let*! peer_filter in
      let*! max_gossip_retransmission in
      let message_cache, routed_message_ids =
        List.fold_left
          (fun (message_cache, messages) message_id ->
            let message_cache, info =
              match
                Message_cache.get_message_for_peer peer message_id message_cache
              with
              | None -> (message_cache, `Not_found)
              | Some (message_cache, message, access_counter) ->
                  ( message_cache,
                    if access_counter > max_gossip_retransmission then
                      `Too_many_requests
                    else if peer_filter peer (`IWant message_id) then
                      `Message message
                    else `Ignored )
            in
            (message_cache, Message_id.Map.add message_id info messages))
          (message_cache, routed_message_ids)
          message_ids
      in
      let* () = set_message_cache message_cache in
      On_iwant_messages_to_route {routed_message_ids} |> return
  end

  let handle_iwant : iwant -> [`IWant] output Monad.t =
   fun {peer; message_ids} -> IWant.handle peer message_ids

  module Graft = struct
    let check_filter peer topic =
      let open Monad.Syntax in
      let*! peer_filter in
      if peer_filter peer `Graft then pass ()
      else
        let*! prune_backoff in
        let* () = set_backoff_for_peer prune_backoff topic peer in
        Peer_filtered |> fail

    let check_subscribed peer topic =
      let open Monad.Syntax in
      let*! mesh_opt = find_mesh topic in
      match mesh_opt with
      | None ->
          let*! prune_backoff in
          let* () = set_backoff_for_peer prune_backoff topic peer in
          Unsubscribed_topic |> fail
      | Some mesh -> pass mesh

    let check_not_in_mesh mesh peer =
      fail_if (Peer.Set.mem peer mesh) Peer_already_in_mesh

    let check_active peer =
      let open Monad.Syntax in
      let*! connections in
      match Peer.Map.find peer connections with
      | None -> Unexpected_grafting_peer |> fail
      | Some connection -> pass connection

    let check_not_direct connection peer topic =
      let open Monad.Syntax in
      if connection.direct then
        let*! prune_backoff in
        let* () = set_backoff_for_peer prune_backoff topic peer in
        Grafting_direct_peer |> fail
      else pass ()

    let check_score peer topic score =
      let open Monad.Syntax in
      if Score.(value score >= zero) then unit
      else
        let*! prune_backoff in
        let* () = set_backoff_for_peer prune_backoff topic peer in
        Grafting_peer_with_negative_score |> fail

    let check_mesh_size mesh connection peer topic =
      let open Monad.Syntax in
      let*! degree_high in
      (* Check the number of mesh peers; if it is at (or over) [degree_high], we
         only accept grafts from peers with outbound connections; this is a
         defensive check to restrict potential mesh takeover attacks combined
         with love bombing *)
      if (not connection.outbound) && Peer.Set.cardinal mesh >= degree_high then
        let*! prune_backoff in
        let* () = set_backoff_for_peer prune_backoff topic peer in
        Mesh_full |> fail
      else pass ()

    let check_backoff peer topic score =
      let open Monad.Syntax in
      let*! backoff in
      match get_backoff topic peer backoff with
      | None -> unit
      | Some backoff_expire ->
          let current = Time.now () in
          if Time.(current >= backoff_expire) then unit
          else
            let score = Score.penalty score 1 in
            let*! graft_flood_backoff in
            let score =
              if Time.(current < add backoff_expire graft_flood_backoff) then
                Score.penalty score 1
              else score
            in
            let*! prune_backoff in
            let* () = set_backoff_for_peer prune_backoff topic peer in
            let* () = add_score peer score in
            fail Peer_backed_off

    (* NOTE: It is the worker who builds the Prune message in (some of the)
       cases of failed/rejected graft requests. The cases should be the same as
       those for which we set a backoff, which is done here; be sure the two
       modules match! *)
    let handle peer topic =
      let open Monad.Syntax in
      let*? () = check_filter peer topic in
      let*? mesh = check_subscribed peer topic in
      let*? () = check_not_in_mesh mesh peer in
      let*? connection = check_active peer in
      let*? () = check_not_direct connection peer topic in
      let*! score_opt = get_score peer in
      (* Global invariant: peer in mesh implies that score exists *)
      let*? () = with_score score_opt @@ check_backoff peer topic in
      let*? () = with_score score_opt @@ check_score peer topic in
      let*? () = check_mesh_size mesh connection peer topic in
      let* () = update_score peer (fun s -> Score.graft s topic) in
      let* () = set_mesh_topic topic (Peer.Set.add peer mesh) in
      (* Call [handle_subscribe] to ensure the invariant where all grafted peers subscribed. *)
      let* output = handle_subscribe {topic; peer} in
      (match output with
      | Subscribe_to_unknown_peer ->
          (* Not possible due to invariant. *)
          ()
      | Subscribed ->
          (* Expected case. *)
          ()) ;
      Grafting_successfully |> return
  end

  let handle_graft : graft -> [`Graft] output Monad.t =
   fun {peer; topic} -> Graft.handle peer topic

  module Prune = struct
    let check_px_score peer =
      let open Monad.Syntax in
      let*! accept_px_threshold in
      let*! score_opt = get_score peer in
      (* Global invariant: peer in mesh => connection exists => score exists *)
      with_score score_opt @@ fun score ->
      fail_if Score.(value score < of_float accept_px_threshold)
      @@ Ignore_PX_score_too_low score

    let check_topic_tracked topic =
      let open Monad.Syntax in
      let*! mesh_opt = find_mesh topic in
      match mesh_opt with
      | None -> Prune_topic_not_tracked |> fail
      | Some mesh -> pass mesh

    let handle peer topic ~px ~backoff =
      let open Monad.Syntax in
      let*? mesh = check_topic_tracked topic in
      let*? () = fail_if_not (Peer.Set.mem peer mesh) Peer_not_in_mesh in
      let mesh = Peer.Set.remove peer mesh in
      let* () = set_mesh_topic topic mesh in
      let* () = set_backoff_for_peer backoff topic peer in
      let* () = update_score peer (fun s -> Score.prune s topic) in
      let px = Peer.Set.of_seq px in
      if Peer.Set.is_empty px then No_PX |> return
      else
        let*? () = check_px_score peer in
        return (PX px)
  end

  let handle_prune : prune -> [`Prune] output Monad.t =
   fun {peer; topic; px; backoff} -> Prune.handle peer topic ~px ~backoff

  module Receive_message = struct
    let check_valid sender topic message message_id =
      let open Monad.Syntax in
      match Message.valid message message_id with
      | `Valid -> unit
      | `Unknown ->
          (* FIXME https://gitlab.com/tezos/tezos/-/issues/5486
             It is not clear yet what we should do here. *)
          fail Unknown_validity
      | `Invalid ->
          let* () =
            update_score sender (fun stats ->
                Score.invalid_message_delivered stats topic)
          in
          fail Invalid_message

    let handle sender topic message_id message =
      let open Monad.Syntax in
      let*? () =
        let*! message_cache in
        match Message_cache.get_first_seen_time message_id message_cache with
        | None -> unit
        | Some validated ->
            let* () =
              update_score sender (fun stats ->
                  Score.duplicate_message_delivered stats topic validated)
            in
            fail Already_received
      in
      let*? () = check_valid sender topic message message_id in
      let*! mesh_opt = find_mesh topic in
      let*? peers_in_mesh =
        match mesh_opt with
        | Some peers -> pass peers
        | None -> fail Not_subscribed
      in
      let peers = Peer.Set.remove sender peers_in_mesh in
      let* () = put_message_in_cache message_id message topic in
      let* () =
        update_score sender (fun stats ->
            Score.first_message_delivered stats topic)
      in
      let* direct_peers = get_direct_peers topic in
      let to_route = Peer.Set.union peers direct_peers in
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/5272

         Filter out peers from which we already received the message, or an
         IHave message? *)
      Route_message {to_route} |> return
  end

  let handle_receive_message :
      receive_message -> [`Receive_message] output Monad.t =
   fun {sender; topic; message_id; message} ->
    Receive_message.handle sender topic message_id message

  module Publish_message = struct
    let check_not_seen message_id =
      let open Monad.Syntax in
      let*! message_cache in
      match Message_cache.get_first_seen_time message_id message_cache with
      | None -> unit
      | Some _validated -> fail Already_published

    let get_peers_for_unsubscribed_topic topic =
      let open Monad.Syntax in
      let*! publish_threshold in
      let*! degree_optimal in
      let now = Time.now () in
      let*! fanout_opt = find_fanout topic in
      match fanout_opt with
      | None ->
          let filter_by_score score =
            Score.(score >= of_float publish_threshold)
          in
          let filter _peer {direct; _} score =
            (not direct) && filter_by_score score
          in
          let* not_direct_peers =
            select_peers topic ~filter ~max:degree_optimal
          in
          let* () = set_fanout_topic topic now not_direct_peers in
          return not_direct_peers
      | Some fanout ->
          let* () = set_fanout_topic topic now fanout.peers in
          return fanout.peers

    let handle topic message_id message : [`Publish_message] output Monad.t =
      let open Monad.Syntax in
      let*? () = check_not_seen message_id in
      let* () = put_message_in_cache message_id message topic in
      let*! mesh_opt = find_mesh topic in
      let* peers =
        match mesh_opt with
        | Some peers -> return peers
        | None -> get_peers_for_unsubscribed_topic topic
      in
      let* direct_peers = get_direct_peers topic in
      let to_publish = Peer.Set.union peers direct_peers in
      Publish_message {to_publish} |> return
  end

  let publish_message : publish_message -> [`Publish_message] output Monad.t =
   fun {topic; message_id; message} ->
    Publish_message.handle topic message_id message

  module Join = struct
    let check_is_not_subscribed topic : (unit, [`Join] output) Monad.check =
      let open Monad.Syntax in
      let*! mesh in
      fail_if (Topic.Map.mem topic mesh) Already_joined

    let init_mesh topic : [`Join] output Monad.t =
      let open Monad.Syntax in
      let*! degree_optimal in
      let*! connections in
      let*! backoff in
      let*! scores in
      let is_valid peer =
        match Peer.Map.find peer connections with
        | None ->
            (* FIXME https://gitlab.com/tezos/tezos/-/issues/5005

               Not supposed to happen. But maybe it is better to
               return a value for defensive programming. *)
            false
        | Some _peer_info ->
            let score = get_scores_score_or_zero scores peer in
            let backed_off = exists_backoff topic peer backoff in
            not (backed_off || Score.(score < zero))
      in
      let*! fanout in
      let valid_fanout_peers =
        match Topic.Map.find topic fanout with
        | None -> Peer.Set.empty
        | Some fanout_peers -> Peer.Set.filter is_valid fanout_peers.peers
      in
      let* peers =
        (* We prioritize fanout peers to be in the mesh for this
           topic. If we need more peers, we look at all our peers
           subscribed to this topic. *)
        let valid_fanout_peers_len = Peer.Set.cardinal valid_fanout_peers in
        if valid_fanout_peers_len >= degree_optimal then
          return valid_fanout_peers
        else
          let max = max 0 (degree_optimal - valid_fanout_peers_len) in
          let* more_peers =
            let filter peer {direct; _} score =
              let backed_off = exists_backoff topic peer backoff in
              not
                (direct || backed_off
                || Score.(score < zero)
                || Peer.Set.mem peer valid_fanout_peers)
            in
            select_peers topic ~filter ~max
          in
          return (Peer.Set.union more_peers valid_fanout_peers)
      in
      (* Notify scoring about the graft. *)
      let scores =
        Peer.Set.fold
          (fun peer scores ->
            update_scores_score peer (fun s -> Score.graft s topic) scores)
          peers
          scores
      in
      let* () = set_scores scores in
      let* () = set_mesh_topic topic peers in
      let* () = delete_fanout topic in
      Joining_topic {to_graft = peers} |> return

    let handle topic : [`Join] output Monad.t =
      let open Monad.Syntax in
      let*? () = check_is_not_subscribed topic in
      init_mesh topic
  end

  let join : join -> [`Join] output Monad.t = fun {topic} -> Join.handle topic

  module Leave = struct
    type mesh = Peer.Set.t

    let check_already_subscribed topic : (mesh, [`Leave] output) Monad.check =
      let open Monad.Syntax in
      let*! mesh in
      match Topic.Map.find topic mesh with
      | None -> Not_joined |> fail
      | Some mesh -> pass mesh

    let handle_mesh topic mesh : [`Leave] output Monad.t =
      let open Monad.Syntax in
      let*! unsubscribe_backoff in
      let*! backoff in
      let*! scores in
      let* () =
        Peer.Set.fold
          (fun peer backoff ->
            add_backoff_for_peer unsubscribe_backoff topic peer backoff)
          mesh
          backoff
        |> set_backoff
      in
      let noPX_peers =
        (* no PX for peers with negative score. *)
        let get_score = get_scores_score_or_zero scores in
        Peer.Set.filter (fun peer -> Score.(get_score peer < zero)) mesh
      in
      Leaving_topic {to_prune = mesh; noPX_peers} |> return

    let handle topic : [`Leave] output Monad.t =
      let open Monad.Syntax in
      let*? mesh = check_already_subscribed topic in
      let* () = delete_mesh topic in
      handle_mesh topic mesh
  end

  let leave : leave -> [`Leave] output Monad.t =
   fun {topic} -> Leave.handle topic

  let set_application_score :
      set_application_score -> [`Set_application_score] output Monad.t =
   fun {peer; score} ->
    let open Monad.Syntax in
    let* () =
      update_score peer (fun s -> Score.set_application_score s score)
    in
    return Set_application_score

  module Heartbeat = struct
    let clear_backoff =
      let open Monad.Syntax in
      let*! heartbeat_ticks in
      let*! heartbeat_interval in
      (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5455

         Move the heartbeat interval/span outside the automaton to the worker. *)
      let*! backoff_cleanup_ticks in
      (* NOTE: Probably the cleanup can also be done lazily: at use, if a
         backoff time is expired, then remove it *)
      (* We only clear once every [backoff_cleanup_ticks] ticks to avoid
         iterating over the map(s) too much *)
      if Int64.(rem heartbeat_ticks (of_int backoff_cleanup_ticks)) <> 0L then
        return ()
      else
        let current = Time.now () in
        let current_with_slack =
          (* Subtract some slack time to the current time to account for
             the message latency; for details, see
             https://github.com/libp2p/go-libp2p-pubsub/issues/368 *)
          Time.sub current (Span.mul heartbeat_interval 2)
        in
        let*! backoff in
        Topic.Map.filter_map
          (fun _topic peer_backoff ->
            let peer_backoff =
              Peer.Map.filter
                (fun _peer expire -> Time.(expire > current_with_slack))
                peer_backoff
            in
            if Peer.Map.is_empty peer_backoff then None else Some peer_backoff)
          backoff
        |> set_backoff

    let clear_or_refresh_scores =
      let open Monad.Syntax in
      let*! heartbeat_ticks in
      let*! score_cleanup_ticks in
      let*! scores in
      (* We only refresh once every [score_cleanup_ticks] ticks to avoid
         iterating over the map(s) too much *)
      if Int64.(rem heartbeat_ticks (of_int score_cleanup_ticks)) <> 0L then
        return ()
      else
        Peer.Map.filter_map (fun _peer score -> Score.refresh score) scores
        |> set_scores

    let cleanup =
      let open Monad.Syntax in
      (* Clean up expired backoffs *)
      let* () = clear_backoff in

      (* Clean up or refresh scores *)
      let* () = clear_or_refresh_scores in

      (* Clean up IHave and IWant counters *)
      let* () = reset_ihave_per_heartbeat in
      let* () = reset_iwant_per_heartbeat in

      (* TODO: https://gitlab.com/tezos/tezos/-/issues/5532
         Apply IWANT request penalties *)
      return ()

    (* Update mesh for grafted and pruned peers. Note that in the Go
       implementation this update is done on-the-fly. Contrary to the Go
       implementation, we explicitly ensure that a peer cannot be grafted and
       then pruned or vice-versa. The reasoning for why that is the case is as
       follows. There are four blocks of updates in the code above:

         1) graft peers if [d_mesh < degree_low]
         2) prune peers if [d_mesh > degree_high]
         3) graft peers if [d_mesh > degree_low] and not enough outbound peers
            ([d_mesh] denotes [Peer.Set.cardinal peers])
         4) graft peers with opportunistic grafting, and remove them from the
            previously pruned peers (if they were indeed meant to be pruned).

       The condition for 1) is mutually exclusive with ones for 2) and 3).
       Now, a peer p cannot be pruned in 2) and then grafted in 3) because in 2):
         A) Either we have enough outbound peers, then we don't execute 3).
         B) Or we don't have enough outbound peers, so p is not outbound and in 3)
            we only graft outbound peers.

       Finally, block 4) ensures the property explicitly.
    *)
    let update_mesh mesh ~to_graft ~to_prune =
      let update f =
        Peer.Map.fold (fun peer topicset mesh ->
            Topic.Set.fold
              (fun topic mesh -> Topic.Map.update topic (f peer) mesh)
              topicset
              mesh)
      in
      let add_peer peer = function
        | None -> Peer.Set.singleton peer |> Option.some
        | Some peers ->
            (* Note: [peer] should not be in [peers] already *)
            Peer.Set.add peer peers |> Option.some
      in
      let remove_peer peer = function
        | None ->
            (* Note: this should not occur *)
            None
        | Some peers -> Peer.Set.remove peer peers |> Option.some
      in
      mesh |> update add_peer to_graft
      |> update remove_peer to_prune
      |> set_mesh

    (* Mesh maintenance. For each topic, do in order:

       - Prune all peers with negative score, do not enable peer exchange for
       these peers.

       - If the number of remaining peers in the topic mesh is less than
       [degree_low], then select as many random peers (not already in the mesh
       topic) to graft as possible so that to have [degree_optimal] in the topic
       mesh. The selected peers should have a non-negative score, should not be
       backed off, and should not be direct peers (and should be subscribed to
       the topic).

       - If the number of remaining peers in the topic mesh is higher than
       [degree_high], then select as many peers (not already in the mesh topic)
       to prune as possible so that to have [degree_optimal] in the topic mesh.
       See [select_peers_to_prune] to see how the selection is performed.

       - If the number of remaining peers in the topic mesh is higher than
       [degree_low] and the number of outbound peers therein is smaller than
       [degree_out], then select an additional number of peers to graft (with
       the same conditions for grafting) to ensure that there are at least
       [degree_out] outbound peers in the mesh.

       - Attempt opportunistic grafting: if the median score of the remaining
       peers is below a given threshold, then graft a few peers at random with
       score above the median and remove them from the peers to be pruned (if
       they were indeed meant to be pruned).

       Finally, for pruned peers, back them off for [prune_backoff] time.
    *)
    let maintain_mesh =
      let open Monad.Syntax in
      let*! connections in
      let*! scores in
      let*! backoff in
      let*! rng in
      let*! prune_backoff in
      let*! degree_optimal in
      let*! degree_low in
      let*! degree_high in
      let*! degree_score in
      let*! degree_out in
      let*! heartbeat_ticks in
      let*! opportunistic_graft_ticks in
      let*! opportunistic_graft_peers in
      let*! opportunistic_graft_threshold in

      let has_outbound_connection =
        peer_has_outbound_connection connections ~default:false
      in

      let get_score = get_scores_score_or_zero scores in

      (* [add_to_peers_topic_set peers_topicset topic peers] adds [topic] to
         each [peer] from the [peers] list into the [peers_topicset], which is a
         peer to topicset map. It returns the new map.

         Note that the following invariant is maintained by the caller: the
         topicset for [peer] does not already contain [topic]. *)
      let add_to_peers_topic_set map topic peers =
        List.fold_left
          (fun acc peer ->
            Peer.Map.update
              peer
              (function
                | None -> Some (Topic.Set.singleton topic)
                | Some topics -> Some (Topic.Set.add topic topics))
              acc)
          map
          peers
      in

      (* Update [to_prune] and [old_peers] as follows:
         - In [to_prune], add [topic] for each of the peers in [new_peers].
         - Remove [new_peers] from [old_peers]. *)
      let prune topic to_prune ~old_peers new_peers =
        let to_prune = add_to_peers_topic_set to_prune topic new_peers in
        let peers =
          List.fold_left
            (fun acc peer -> Peer.Set.remove peer acc)
            old_peers
            new_peers
        in
        (to_prune, peers)
      in

      let opportunistic_grafting topic peers =
        if Int64.rem heartbeat_ticks opportunistic_graft_ticks = 0L then
          let num_peers = List.length peers in
          if num_peers > 1 then
            (* Opportunistic grafting works as follows: we check the median score
               of peers in the mesh; if this score is below the
               [opportunistic_graft_threshold], we select a few peers at random
               with score over the median.

               The intention is to (slowly) improve an underperforming mesh by
               introducing good scoring peers that may have been gossiping at
               us. This allows us to get out of sticky situations where we are
               stuck with poor peers and also recover from churn of good peers. *)

            (* Compute the median peer score in the mesh. *)
            let median_score =
              let sorted_scores =
                peers |> List.rev_map get_score |> Array.of_list
              in
              Array.sort Score.compare sorted_scores ;
              sorted_scores.(num_peers / 2)
            in
            if Score.(median_score < of_float opportunistic_graft_threshold)
            then
              let peers_set = Peer.Set.of_list peers in
              let filter peer connection score =
                let in_mesh = Peer.Set.mem peer peers_set in
                let backed_off = exists_backoff topic peer backoff in
                let above_median = Score.(score > median_score) in
                (not in_mesh) && (not backed_off) && (not connection.direct)
                && above_median
              in
              select_connections_peers
                connections
                scores
                rng
                topic
                ~filter
                ~max:opportunistic_graft_peers
            else []
          else []
        else []
      in

      (* Keep the first [degree_score] peers by score and the remaining up to
         [degree_optimal] randomly, under the constraint that we keep
         [degree_out] peers in the mesh (if we have that many). *)
      let select_peers_to_prune peers =
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/5052
           Consider first selecting [degree_out] peers and then ordering on the score. *)
        (* Sort descendingly by score, but shuffle first for the case we
           don't use the score. Head of list has highest score. *)
        let peers =
          peers |> Peer.Set.elements |> List.shuffle ~rng
          |> List.sort (fun peer1 peer2 ->
                 let s1 = get_score peer1 in
                 let s2 = get_score peer2 in
                 (* Global invariant:
                    peers in mesh => peer in connections => peer in scores *)
                 Score.compare s2 s1)
        in

        let peers_high_score, peers_low_score =
          List.split_n degree_score peers
        in
        let peers_low_score = List.shuffle ~rng peers_low_score in
        let peers_to_keep, peers_to_prune =
          let to_keep, peers_to_prune =
            (* Recall that [degree_score <= degree_optimal] *)
            List.split_n (degree_optimal - degree_score) peers_low_score
          in
          (peers_high_score @ to_keep, peers_to_prune)
        in

        (* Count the outbound peers we are keeping. *)
        let outbound_peers_to_keep, inbound_peers_to_keep =
          List.partition has_outbound_connection peers_to_keep
        in
        let num_outbound_to_keep = List.length outbound_peers_to_keep in

        (* If [num_outbound] is less than [degree_out], swap some
           outbound peers from the peers to prune with inbound peers
           from the peers to keep. *)
        if num_outbound_to_keep < degree_out then
          let outbound_peers_to_prune, inbound_peers_to_prune =
            List.partition has_outbound_connection peers_to_prune
          in
          let num_outbound_to_prune = List.length outbound_peers_to_prune in
          let num_inbound_to_keep = List.length inbound_peers_to_keep in
          let num_to_swap =
            min
              (max 0 (degree_out - num_outbound_to_keep))
              (min num_outbound_to_prune num_inbound_to_keep)
          in
          if num_to_swap > 0 then
            (* We additionally prune the [num_to_swap] inbound peers among the ones with
               a low score (or were shuffled); that's why we revert [inbound_peers_to_keep]. *)
            let inbound_peers_to_prune =
              List.take_n num_to_swap (List.rev inbound_peers_to_keep)
              @ inbound_peers_to_prune
            in
            (* Actually keep [num_to_swap] outbound peers. *)
            let outbound_peers_to_prune =
              List.drop_n num_to_swap outbound_peers_to_prune
            in
            inbound_peers_to_prune @ outbound_peers_to_prune
          else peers_to_prune
        else peers_to_prune
      in

      (* [maintain_topic_mesh topic peers (to_prune, to_graft, noPX_peers)]
         maintains the mesh for [topic] where [peers] are the original [peers]
         in this topic. [to_prune], [to_graft], [noPX_peers] are the peers to be
         pruned, grafted, and those for which no peer exchange should be done
         performed, accumulated from the maintenance for other mesh topics. *)
      let maintain_topic_mesh topic peers
          (`To_prune to_prune, `To_graft to_graft, noPX_peers) =
        let to_prune, peers, noPX_peers =
          (* Drop all peers with negative score, without PX *)
          Peer.Set.fold
            (fun peer (to_prune, peers, noPX_peers) ->
              if Score.(get_score peer < zero) then
                let to_prune, peers =
                  prune topic to_prune ~old_peers:peers [peer]
                in
                let noPX_peers = Peer.Set.add peer noPX_peers in
                (to_prune, peers, noPX_peers)
              else (to_prune, peers, noPX_peers))
            peers
            (to_prune, peers, noPX_peers)
        in

        (* Do we have too few peers? *)
        let num_peers = Peer.Set.cardinal peers in
        let to_graft =
          if num_peers < degree_low then
            let max = degree_optimal - num_peers in
            (* Filter out our current and direct peers, peers we are backing
               off, and peers with negative score. *)
            let filter peer connection score =
              let in_mesh = Peer.Set.mem peer peers in
              let backed_off = exists_backoff topic peer backoff in
              (not in_mesh) && (not backed_off) && (not connection.direct)
              && Score.(score >= zero)
            in
            select_connections_peers connections scores rng topic ~filter ~max
            |> add_to_peers_topic_set to_graft topic
          else to_graft
        in

        (* Do we have too many peers? *)
        let to_prune, peers =
          if num_peers > degree_high then
            (* We'll prune [num_peers - degree_optimal] peers. *)
            select_peers_to_prune peers |> prune topic to_prune ~old_peers:peers
          else (to_prune, peers)
        in

        (* Do we have enough outbound peers? *)
        let num_peers = Peer.Set.cardinal peers in
        let to_graft, peers =
          if num_peers >= degree_low then
            let num_outbound =
              Peer.Set.fold
                (fun peer count ->
                  if has_outbound_connection peer then count + 1 else count)
                peers
                0
            in
            if num_outbound < degree_out then
              let max = degree_out - num_outbound in
              (* Filter out our current and direct peers, peers we are backing
                 off, and peers with negative score *)
              let filter peer connection score =
                let in_mesh = Peer.Set.mem peer peers in
                let backed_off = exists_backoff topic peer backoff in
                (not in_mesh) && (not backed_off) && (not connection.direct)
                && Score.(score >= zero)
                && has_outbound_connection peer
              in
              let new_peers =
                select_connections_peers
                  connections
                  scores
                  rng
                  topic
                  ~filter
                  ~max
              in
              let to_graft = add_to_peers_topic_set to_graft topic new_peers in
              (to_graft, new_peers)
            else (to_graft, Peer.Set.elements peers)
          else (to_graft, Peer.Set.elements peers)
        in

        (* Attempt opportunistic grafting. *)
        let to_graft, to_prune =
          let peers_to_graft = opportunistic_grafting topic peers in
          let to_graft = add_to_peers_topic_set to_graft topic peers_to_graft in
          (* Do not actually prune the [peers_to_graft]. *)
          let to_prune =
            List.fold_left
              (fun to_prune peer_to_graft ->
                Peer.Map.update
                  peer_to_graft
                  (function
                    | None -> None
                    | Some topicset ->
                        let topicset = Topic.Set.remove topic topicset in
                        if Topic.Set.is_empty topicset then None
                        else Some topicset)
                  to_prune)
              to_prune
              peers_to_graft
          in
          (to_graft, to_prune)
        in

        (`To_prune to_prune, `To_graft to_graft, noPX_peers)
      in

      let*! mesh in
      let `To_prune to_prune, `To_graft to_graft, noPX_peers =
        Topic.Map.fold
          maintain_topic_mesh
          mesh
          (`To_prune Peer.Map.empty, `To_graft Peer.Map.empty, Peer.Set.empty)
      in

      (* Update backoff for pruned peers. *)
      let* () =
        Peer.Map.fold
          (fun peer topicset backoff ->
            Topic.Set.fold
              (fun topic backoff ->
                add_backoff_for_peer prune_backoff topic peer backoff)
              topicset
              backoff)
          to_prune
          backoff
        |> set_backoff
      in
      (* Notify scoring about the grafts. *)
      let scores =
        Peer.Map.fold
          (fun peer ->
            Topic.Set.fold (fun topic ->
                update_scores_score peer (fun s -> Score.graft s topic)))
          to_graft
          scores
      in
      let* () = set_scores scores in
      (* Update mesh for grafted and pruned peers *)
      let* () = update_mesh mesh ~to_graft ~to_prune in
      return (to_graft, to_prune, noPX_peers)

    let update_fanout fanout ~to_add ~to_remove =
      let update f topic_peers_list fanout =
        List.fold_left
          (fun fanout (topic, peers) -> Topic.Map.update topic (f peers) fanout)
          fanout
          topic_peers_list
      in
      let add_peers peers_to_add = function
        | None ->
            (* impossible: in [maintain_fanout] we only consider topics that are in
               the domain of the fanout map *)
            assert false
        | Some v ->
            let peers =
              List.fold_left
                (fun peers peer -> Peer.Set.add peer peers)
                v.peers
                peers_to_add
            in
            Some {v with peers}
      in
      let remove_peers peers_to_remove = function
        | None -> (* impossible, as in the previous case *) assert false
        | Some v ->
            let peers =
              List.fold_left
                (fun peers peer -> Peer.Set.remove peer peers)
                v.peers
                peers_to_remove
            in
            Some {v with peers}
      in
      fanout |> update add_peers to_add
      |> update remove_peers to_remove
      |> set_fanout

    (* Maintain the fanout map as follows:

       - Remove topics to which the local peer has not published in the
       [fanout_ttl] time.

       - Remove peers that are not subscribed anymore or that have a score below
       [publish_threshold].

       - Remove peers that are expiring.

       - If for a topic the set of fanout peers is below [degree_optimal], then
       try to fill the map so that to have [degree_optimal] in the topic
       fanout. The selected peers should have a score above [publish_threshold]
       and should not be direct peers (and should be subscribed to the topic).
    *)
    let maintain_fanout =
      let open Monad.Syntax in
      let*! connections in
      let*! scores in
      let*! rng in
      let*! degree_optimal in
      let*! publish_threshold in
      let*! fanout_ttl in

      let expire_fanout =
        let current = Time.now () in
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/5184
           Optimize by having a min and a max last published time to avoid
           traversing the map when not needed? *)
        Topic.Map.filter (fun _topic {last_published_time; peers = _} ->
            Time.(add last_published_time fanout_ttl >= current))
      in

      let maintain_topic_fanout topic {peers; _} (to_add, to_remove) =
        (* Check whether our peers are still in the topic and have a score
           above the publish threshold *)
        let peers_to_keep, peers_to_remove =
          Peer.Set.fold
            (fun peer acc ->
              match Peer.Map.find peer connections with
              | None ->
                  (* impossible, given the global invariants on the state *)
                  assert false
              | Some connection ->
                  let score = get_scores_score_or_zero scores peer in
                  if
                    Topic.Set.mem topic connection.topics
                    && Score.(score >= of_float publish_threshold)
                  then acc
                  else
                    let peers_to_keep, peers_to_remove = acc in
                    (Peer.Set.remove peer peers_to_keep, peer :: peers_to_remove))
            peers
            (peers, [])
        in
        let to_remove = (topic, peers_to_remove) :: to_remove in

        (* Do we need more peers? *)
        let num_peers = Peer.Set.cardinal peers_to_keep in
        if num_peers < degree_optimal then
          let ineed = degree_optimal - num_peers in
          (* Filter our current and direct peers and peers with score above
             the publish threshold *)
          let filter peer connection score =
            let in_fanout = Peer.Set.mem peer peers_to_keep in
            (not in_fanout) && (not connection.direct)
            && Score.(score >= of_float publish_threshold)
          in
          let new_peers =
            select_connections_peers
              connections
              scores
              rng
              topic
              ~filter
              ~max:ineed
          in
          let to_add = (topic, new_peers) :: to_add in
          (to_add, to_remove)
        else (to_add, to_remove)
      in

      let*! fanout in
      (* Expire fanout for topics we haven't published to in a while. *)
      let fanout = expire_fanout fanout in
      let to_add, to_remove =
        Topic.Map.fold maintain_topic_fanout fanout ([], [])
      in
      (* Update the fanout map. *)
      update_fanout fanout ~to_add ~to_remove

    let handle =
      let open Monad.Syntax in
      let*! heartbeat_ticks in
      let* () = set_heartbeat_ticks (Int64.succ heartbeat_ticks) in
      (* cleaning up *)
      let* () = cleanup in

      (* Maintain the mesh for topics we have joined. Concretely, in case the
         number of peers per topic (in the mesh) is lower than [degree_low] or
         higher than [degree_high], then select peers to graft or prune
         respectively, so that the new number of peers per topic becomes
         [degree_optimal]. *)
      let* to_graft, to_prune, noPX_peers = maintain_mesh in

      (* Maintain our fanout for topics we are publishing to, but we have not
         joined. *)
      let* () = maintain_fanout in

      (* Advance the message history sliding window. *)
      let*! message_cache in
      let* () = Message_cache.shift message_cache |> set_message_cache in

      Heartbeat {to_graft; to_prune; noPX_peers} |> return
  end

  let heartbeat : [`Heartbeat] output Monad.t = Heartbeat.handle

  module Add_peer = struct
    let handle ~direct ~outbound peer : [`Add_peer] output Monad.t =
      let open Monad.Syntax in
      let*! connections in
      let*! scores in
      let*! score_limits in
      match Peer.Map.find peer connections with
      | None ->
          let connection = {direct; topics = Topic.Set.empty; outbound} in
          let connections = Peer.Map.add peer connection connections in
          let scores =
            Peer.Map.update
              peer
              (function
                | None -> Some (Score.newly_connected score_limits)
                | Some score -> Some (Score.set_connected score))
              scores
          in
          let* () = set_connections connections in
          let* () = set_scores scores in
          return Peer_added
      | Some _ -> return Peer_already_known
  end

  let add_peer : add_peer -> [`Add_peer] output Monad.t =
   fun {direct; outbound; peer} -> Add_peer.handle ~direct ~outbound peer

  module Remove_peer = struct
    let handle peer : [`Remove_peer] output Monad.t =
      let open Monad.Syntax in
      let*! mesh in
      let mesh = Topic.Map.map (fun peers -> Peer.Set.remove peer peers) mesh in
      let* () = set_mesh mesh in
      let*! fanout in
      let fanout =
        Topic.Map.map
          (fun fanout_peers ->
            {fanout_peers with peers = Peer.Set.remove peer fanout_peers.peers})
          fanout
      in
      let* () = set_fanout fanout in
      let*! retain_duration in
      let*! scores in
      let*! connections in
      let* () = Peer.Map.remove peer connections |> set_connections in
      let* () =
        Peer.Map.update
          peer
          (function
            | None ->
                (* This should not happen (global invariant). *)
                None
            | Some score -> Score.remove_peer score ~retain_duration)
          scores
        |> set_scores
      in
      Removing_peer |> return
  end

  let remove_peer : remove_peer -> [`Remove_peer] output Monad.t =
   fun {peer} -> Remove_peer.handle peer

  (* On spam protection
     ==================

     We make the assumption that the bandwidth per peer can be limited. This
     means that if the automaton allows a peer to send us an unbounded number of
     messages, this is not a case of concern by itself. There is a case of
     concern if this results in wasting space or CPU time.

     Another assumption we make is that messages are of bounded size, because
     the message encoding should ensure this.

     We detail next for each p2p message type what checks are (or are not) in
     place concerning the receipt of "spam" and what are the concerns.

     IHave messages:
     - There is a maximum number, given by [max_recv_ihave_per_heartbeat], of
       IHave messages that a peer accepts from another peer between two
       heartbeat ticks; receiving more than this limit is not punished, the
       automaton simply returns [Too_many_recv_ihave_messages].
     - The number of message ids included is not checked, even though the remote
       peer should include at most [max_sent_iwant_per_heartbeat] ids.

     IWant messages:
     - There is a maximum number, given by [max_gossip_retransmission], of times
       that a peer can request the *same* message (during the time the message is
       in the message cache).
     - There is no imposed bound on the number of IWant messages received from a peer.
     - The number of message ids included is not checked, though the remote peer
       should include at most [max_sent_iwant_per_heartbeat] ids between two
       heartbeat ticks.

     Note on IHave/IWant messages: heartbeats of different peers need not be in
     sync. Therefore we cannot use the limits supposedly used by the sender on
     the receiver side to punish the sender; we could multiply these limits by 2
     though.

     Graft messages:
     - Receiving a Graft message within the backoff period (given by
       [prune_backoff] or [unsubscribed_backoff]) is punished. Receiving such a
       Graft very soon (before [prune_backoff + graft_flood_backoff]) is punished
       further.
     - Receiving a duplicate of an accepted Graft request is not punished, the
       automaton simply returns [Peer_already_in_mesh]. In all other failure cases
       (except for unconnected peers) a backoff is set.

     Prune messages:
     - In case of a duplicate Prune the automaton simply returns
       [Peer_not_in_mesh] because the peer has already been removed from the mesh
       following the first Prune message.
     - The number of PX peers is not bounded. However, the remote peer should
       advertise at most [peers_to_px].

     Subscribe/Unsubscribe messages:
     - Duplicates are not checked for, a duplicate Subscribe/Unsubscribed (for a
       connected peer) is always successful.

     Full messages:
     - We check for duplicates with the message cache. A duplicate is not re-routed.
     - A peer sending duplicates within a small window (given by
       [mesh_message_deliveries_window] of a few ms) is rewarded because they
       count as near-first deliveries. Duplicates received outside of this window
       are not counted.
     - Potential problems:
       - receiving a large number of duplicates, as all duplicate message ids
         are stored in the cache *)

  (* "Getters" *)

  let select_px_peers state ~peer_to_prune topic ~noPX_peers =
    let do_px = do_px state in
    let peers_to_px = peers_to_px state in
    let filter peer _conn score =
      (not (Peer.equal peer_to_prune peer)) && Score.(score >= zero)
    in
    if do_px && not (Peer.Set.mem peer_to_prune noPX_peers) then
      select_connections_peers
        state.connections
        state.scores
        state.rng
        topic
        ~filter
        ~max:peers_to_px
    else []

  let select_gossip_messages state =
    let rng = state.rng in
    let select_gossip_for_peer message_ids =
      (* We shuffle the message ids so that we emit a different set for each
         peer. *)
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/5396
         Can this be optimized? *)
      message_ids |> List.shuffle ~rng
      |> List.take_n state.limits.max_sent_iwant_per_heartbeat
    in
    let select_gossip_for_topic topic excluded_peers =
      let message_ids =
        Message_cache.get_message_ids_to_gossip topic state.message_cache
      in
      if message_ids = [] then []
      else
        (* We collect the peers with a score above [gossip_threshold] that are not
           in the excluded set and are not direct peers. *)
        let filter peer {direct; _} score =
          (not direct)
          && Score.(score >= of_float state.limits.gossip_threshold)
          && not (Peer.Set.mem peer excluded_peers)
        in

        (* We first select all peers satisfying the criterion and then we see if
           we have too many. *)
        let peers =
          select_connections_peers
            state.connections
            state.scores
            rng
            topic
            ~filter
            ~max:Int.max_int
        in
        let num_peers = List.length peers in
        let target_num =
          max
            state.limits.degree_lazy
            (int_of_float
               (state.limits.gossip_factor *. float_of_int num_peers))
        in
        let selected_peers = List.take_n target_num peers in

        (* Prepare the IHave gossip to the selected peers. *)
        List.fold_left
          (fun messages peer ->
            let message_ids = select_gossip_for_peer message_ids in
            {peer; topic; message_ids} :: messages)
          []
          selected_peers
    in

    (* Prepare the IHave gossip for each topic in the mesh or fanout maps. Peers
       in the mesh/fanout are excluded from gossip (because we send full
       messages to them). *)
    let add_gossip_for_topic topic peers gossip_msgs =
      let new_msgs = select_gossip_for_topic topic peers in
      List.rev_append new_msgs gossip_msgs
    in

    Topic.Map.fold add_gossip_for_topic state.mesh []
    |> Topic.Map.fold
         (fun topic {peers; _} -> add_gossip_for_topic topic peers)
         state.fanout

  (* Helpers. *)

  let pp_add_peer fmtr ({direct; outbound; peer} : add_peer) =
    let open Format in
    fprintf
      fmtr
      "{ direct=%b; outbound=%b; peer=%a }"
      direct
      outbound
      Peer.pp
      peer

  let pp_remove_peer fmtr ({peer} : remove_peer) =
    let open Format in
    fprintf fmtr "{ peer=%a }" Peer.pp peer

  let pp_ihave fmtr ({peer; topic; message_ids} : ihave) =
    let open Format in
    fprintf
      fmtr
      "{ peer=%a; topic=%a; message_ids=[%a] }"
      Peer.pp
      peer
      Topic.pp
      topic
      (pp_print_list ~pp_sep:(fun fmtr () -> fprintf fmtr ";") Message_id.pp)
      message_ids

  let pp_iwant fmtr ({peer : Peer.t; message_ids : Message_id.t list} : iwant) =
    let open Format in
    fprintf
      fmtr
      "{ peer=%a; message_ids=[%a] }"
      Peer.pp
      peer
      (pp_print_list ~pp_sep:(fun fmtr () -> fprintf fmtr ";") Message_id.pp)
      message_ids

  let pp_graft fmtr ({peer; topic} : graft) =
    let open Format in
    fprintf fmtr "{ peer=%a; topic=%a }" Peer.pp peer Topic.pp topic

  let pp_prune fmtr ({peer; topic; px; backoff} : prune) =
    let open Format in
    fprintf
      fmtr
      "{ peer=%a; topic=%a; px=[%a]; backoff=%a }"
      Peer.pp
      peer
      Topic.pp
      topic
      (pp_print_list ~pp_sep:(fun fmtr () -> fprintf fmtr ";") Peer.pp)
      (List.of_seq px)
      Span.pp
      backoff

  let pp_receive_message fmtr
      ({sender; topic; message_id; message} : receive_message) =
    let open Format in
    fprintf
      fmtr
      "{ sender=%a; topic=%a; message_id=%a; message=%a }"
      Peer.pp
      sender
      Topic.pp
      topic
      Message_id.pp
      message_id
      Message.pp
      message

  let pp_publish_message fmtr ({topic; message_id; message} : publish_message) =
    let open Format in
    fprintf
      fmtr
      "{ topic=%a; message_id=%a; message=%a }"
      Topic.pp
      topic
      Message_id.pp
      message_id
      Message.pp
      message

  let pp_join fmtr ({topic} : join) =
    let open Format in
    fprintf fmtr "{ topic=%a }" Topic.pp topic

  let pp_leave fmtr ({topic} : leave) =
    let open Format in
    fprintf fmtr "{ topic=%a }" Topic.pp topic

  let pp_subscribe fmtr ({topic; peer} : subscribe) =
    let open Format in
    fprintf fmtr "{ topic=%a; peer=%a }" Topic.pp topic Peer.pp peer

  let pp_unsubscribe fmtr ({topic; peer} : unsubscribe) =
    let open Format in
    fprintf fmtr "{ topic=%a; peer=%a }" Topic.pp topic Peer.pp peer

  let pp_peer_map pp_elt =
    Fmt.Dump.iter_bindings Peer.Map.iter Fmt.nop Peer.pp pp_elt

  let pp_message_id_map pp_elt =
    Fmt.Dump.iter_bindings Message_id.Map.iter Fmt.nop Message_id.pp pp_elt

  let pp_peer_set = Fmt.Dump.iter Peer.Set.iter Fmt.nop Peer.pp

  let pp_topic_set = Fmt.Dump.iter Topic.Set.iter Fmt.nop Topic.pp

  let pp_topic_map pp_elt =
    Fmt.Dump.iter_bindings Topic.Map.iter Fmt.nop Topic.pp pp_elt

  let pp_output (type a) fmtr (o : a output) =
    let open Format in
    match o with
    | Ihave_from_peer_with_low_score {score; threshold} ->
        let r = (score, threshold) in
        fprintf
          fmtr
          "Negative_peer_score %a"
          Fmt.Dump.(
            record [field "score" fst Score.pp; field "threshold" snd Fmt.float])
          r
    | Too_many_recv_ihave_messages {count; max} ->
        fprintf
          fmtr
          "Too_many_recv_ihave_messages { count=%d; max=%d }"
          count
          max
    | Too_many_sent_iwant_messages {count; max} ->
        fprintf
          fmtr
          "Too_many_sent_iwant_messages { count=%d; max=%d }"
          count
          max
    | Message_topic_not_tracked -> fprintf fmtr "Message_topic_not_tracked"
    | Message_requested_message_ids ids ->
        fprintf
          fmtr
          "Message_requested_message_ids %a"
          (Fmt.Dump.list Message_id.pp)
          ids
    | Iwant_from_peer_with_low_score {score; threshold} ->
        let r = (score, threshold) in
        fprintf
          fmtr
          "Iwant_from_peer_with_low_score %a"
          Fmt.Dump.(
            record [field "score" fst Score.pp; field "threshold" snd Fmt.float])
          r
    | On_iwant_messages_to_route {routed_message_ids} ->
        let pp_elt fmtr tag =
          match tag with
          | `Ignored -> fprintf fmtr "ignored"
          | `Message m -> fprintf fmtr "message(%a)" Message.pp m
          | `Not_found -> fprintf fmtr "not_found"
          | `Too_many_requests -> fprintf fmtr "too_many_requests"
        in
        fprintf
          fmtr
          "On_iwant_messages_to_route %a"
          (pp_message_id_map pp_elt)
          routed_message_ids
    | Peer_filtered -> fprintf fmtr "Peer_filtered"
    | Unsubscribed_topic -> fprintf fmtr "Unsubscribed_topic"
    | Peer_already_in_mesh -> fprintf fmtr "Peer_already_in_mesh"
    | Grafting_direct_peer -> fprintf fmtr "Grafting_direct_peer"
    | Unexpected_grafting_peer -> fprintf fmtr "Unexpected_grafting_peer"
    | Grafting_peer_with_negative_score ->
        fprintf fmtr "Grafting_peer_with_negative_score"
    | Grafting_successfully -> fprintf fmtr "Grafting_successfully"
    | Peer_backed_off -> fprintf fmtr "Peer_backed_off"
    | Mesh_full -> fprintf fmtr "Mesh_full"
    | Prune_topic_not_tracked -> fprintf fmtr "Prune_topic_not_tracked"
    | Peer_not_in_mesh -> fprintf fmtr "Peer_not_in_mesh"
    | Ignore_PX_score_too_low score ->
        fprintf fmtr "Ignore_PX_score_too_low %a" Score.pp score
    | No_PX -> fprintf fmtr "No_PX"
    | PX peer_set -> fprintf fmtr "PX %a" pp_peer_set peer_set
    | Publish_message {to_publish} ->
        fprintf
          fmtr
          "Publish_message %a"
          Fmt.Dump.(record [field "to_publish" Fun.id pp_peer_set])
          to_publish
    | Already_published -> fprintf fmtr "Already_published"
    | Invalid_message -> fprintf fmtr "Invalid_message"
    | Unknown_validity -> fprintf fmtr "Unknown_validity"
    | Route_message {to_route} ->
        fprintf
          fmtr
          "Route_message %a"
          Fmt.Dump.(record [field "to_route" Fun.id pp_peer_set])
          to_route
    | Already_received -> fprintf fmtr "Already_received"
    | Not_subscribed -> fprintf fmtr "Not_subscribed"
    | Already_joined -> fprintf fmtr "Already_joined"
    | Joining_topic {to_graft} ->
        fprintf fmtr "Joining_topic %a" pp_peer_set to_graft
    | Not_joined -> fprintf fmtr "Not_joined"
    | Leaving_topic {to_prune; noPX_peers} ->
        let p = (to_prune, noPX_peers) in
        fprintf
          fmtr
          "Leaving_topic %a"
          Fmt.Dump.(
            record
              [
                field "to_prune" fst pp_peer_set;
                field "noPX_peers" snd pp_peer_set;
              ])
          p
    | Heartbeat {to_graft; to_prune; noPX_peers} ->
        let r = (to_graft, to_prune, noPX_peers) in
        Fmt.pf
          fmtr
          "Heartbeat %a"
          Fmt.Dump.(
            record
              [
                field
                  "to_graft"
                  (fun (to_graft, _, _) -> to_graft)
                  (pp_peer_map pp_topic_set);
                field
                  "to_prune"
                  (fun (_, to_prune, _) -> to_prune)
                  (pp_peer_map pp_topic_set);
                field
                  "noPX_peers"
                  (fun (_, _, noPX_peers) -> noPX_peers)
                  pp_peer_set;
              ])
          r
    | Peer_added -> fprintf fmtr "Peer_added"
    | Peer_already_known -> fprintf fmtr "Peer_already_known"
    | Removing_peer -> fprintf fmtr "Removing_peer"
    | Subscribed -> fprintf fmtr "Subscribed"
    | Subscribe_to_unknown_peer -> fprintf fmtr "Subscribe_to_unknown_peer"
    | Unsubscribed -> fprintf fmtr "Unsubscribed"
    | Unsubscribe_from_unknown_peer ->
        fprintf fmtr "Unsubscribe_from_unknown_peer"
    | Set_application_score -> fprintf fmtr "Set_application_score"

  module Introspection = struct
    (* This module reexport datatypes so that it can be used for
       introspection. While at the moment, this module reexport purely
       the datatype, we can decide that for abstraction purpose, we do
       not export all those fields in the future. This may have a
       small overhead cost, but for introspection it should be
       irrelevant. *)

    type nonrec connection = connection = {
      topics : Topic.Set.t;
      direct : bool;
      outbound : bool;
    }

    type nonrec fanout_peers = fanout_peers = {
      peers : Peer.Set.t;
      last_published_time : time;
    }

    module Message_cache = Message_cache

    type view = state = {
      limits : limits;
      parameters : parameters;
      connections : connection Peer.Map.t;
      scores : Score.t Peer.Map.t;
      ihave_per_heartbeat : int Peer.Map.t;
      iwant_per_heartbeat : int Peer.Map.t;
      mesh : Peer.Set.t Topic.Map.t;
      fanout : fanout_peers Topic.Map.t;
      backoff : time Peer.Map.t Topic.Map.t;
      message_cache : Message_cache.t;
      rng : Random.State.t;
      heartbeat_ticks : int64;
    }

    let view state = state

    type connected_peers_filter =
      | Direct
      | Subscribed_to of Topic.t
      | Score_above of {threshold : Score.value}
    (* Add other cases here if needed. *)

    let connected_peers_filter _peer connection score = function
      | Direct -> connection.direct
      | Subscribed_to topic -> Topic.Set.mem topic connection.topics
      | Score_above {threshold} -> Score.(score >= threshold)

    let get_connected_peers =
      let rec filter_rec peer connection score = function
        | [] -> true
        | filter :: filters ->
            connected_peers_filter peer connection score filter
            && filter_rec peer connection score filters
      in
      fun ?(filters = []) view ->
        Peer.Map.fold
          (fun peer connection acc ->
            let score = get_score_or_zero peer view in
            if filter_rec peer connection score filters then peer :: acc
            else acc)
          view.connections
          []

    let get_peers_in_topic_mesh topic state =
      match Topic.Map.find topic state.mesh with
      | None -> []
      | Some peers -> Peer.Set.elements peers

    let get_subscribed_topics peer state =
      match Peer.Map.find peer state.connections with
      | None -> []
      | Some connection -> Topic.Set.elements connection.topics

    let get_our_topics state =
      Topic.Map.fold (fun topic _peers acc -> topic :: acc) state.mesh []

    let get_fanout_peers topic state =
      match Topic.Map.find topic state.fanout with
      | None -> []
      | Some fanout_peers -> Peer.Set.elements fanout_peers.peers

    let get_peer_score peer {scores; _} =
      match Peer.Map.find peer scores with
      | None -> Score.zero
      | Some score -> Score.value score

    let limits state = state.limits

    let has_joined topic {mesh; _} = Topic.Map.mem topic mesh

    let pp_connection fmtr c =
      let fields =
        List.concat
          [
            (if Topic.Set.is_empty c.topics then []
            else [Fmt.field "topics" (fun c -> c.topics) pp_topic_set]);
            [Fmt.field "direct" (fun c -> c.direct) Fmt.bool];
            [Fmt.field "outbound" (fun c -> c.outbound) Fmt.bool];
          ]
      in
      Fmt.record fields fmtr c

    let pp_connections =
      Fmt.Dump.iter_bindings Peer.Map.iter Fmt.nop Peer.pp pp_connection

    (* re-export printers *)
    let pp_peer_map = pp_peer_map

    let pp_message_id_map = pp_message_id_map

    let pp_topic_map = pp_topic_map

    let pp_peer_set = pp_peer_set

    let pp_topic_set = pp_topic_set
  end
end

module Internal_for_tests = struct
  module Message_cache = Message_cache.Make
end
