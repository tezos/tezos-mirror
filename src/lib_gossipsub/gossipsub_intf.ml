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

module type PRINTABLE = sig
  type t

  val pp : Format.formatter -> t -> unit
end

module type ITERABLE = sig
  type t

  include Compare.S with type t := t

  include PRINTABLE with type t := t

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t
end

module type AUTOMATON_SUBCONFIG = sig
  module Peer : ITERABLE

  module Topic : ITERABLE

  module Message_id : sig
    include ITERABLE

    (** [get_topic id] returns/computes a message's topic from the given message
        id. Message ids should be defined so that this function can be
        implemented. *)
    val get_topic : t -> Topic.t
  end

  module Message : sig
    include PRINTABLE

    (** [valid] performs an application layer-level validity check on a message. *)
    val valid : t -> [`Valid | `Unknown | `Invalid]
  end
end

module type SPAN = sig
  include Compare.S

  include PRINTABLE with type t := t

  val zero : t

  val of_int_s : int -> t

  val to_int_s : t -> int

  val of_float_s : float -> t

  val to_float_s : t -> float

  (** [mul s n] returns [n * s]. *)
  val mul : t -> int -> t
end

module type TIME = sig
  include Compare.S

  include PRINTABLE with type t := t

  type span

  val now : unit -> t

  val add : t -> span -> t

  val sub : t -> span -> t

  val to_span : t -> span
end

module type AUTOMATON_CONFIG = sig
  module Subconfig : AUTOMATON_SUBCONFIG

  module Span : SPAN

  module Time : TIME with type span = Span.t
end

type 'span per_topic_score_limits = {
  time_in_mesh_weight : float;
      (** P1: The weight of the score associated to the time spent in the mesh. *)
  time_in_mesh_cap : float;
      (** P1: The maximum value considered for the score associated to the time spent
          by a peer in the mesh. *)
  time_in_mesh_quantum : float;
      (** P1: The score associated to the time spent [t] in the mesh is
          [(min t time_in_mesh_cap) / time_in_mesh_quantum]. *)
  first_message_deliveries_weight : float;
      (** P2: The weight of the score associated to the number of first message deliveries. *)
  first_message_deliveries_cap : int;
      (** P2: The maximum value considered during score computation for the number of
          first message deliveries. *)
  first_message_deliveries_decay : float;
      (** P2: The score is multiplied by this factor every [score_cleanup_ticks] heartbeat.
          This parameter must be in the unit interval. *)
  mesh_message_deliveries_weight : float;
      (** P3: The weight of the score associated to the number of first/near-first
          mesh message deliveries. *)
  mesh_message_deliveries_window : 'span;
      (** P3: The delay added to the first delivery of a message to obtain the upper bound up to which a
          duplicate message is counted as nearly-first delivered. *)
  mesh_message_deliveries_activation : 'span;
      (** P3: How long should a peer be in the mesh before we start evaluating P3. *)
  mesh_message_deliveries_cap : int;
      (** P3: The maximum value considered during score computation for the number of
          first/near-first mesh message deliveries. *)
  mesh_message_deliveries_threshold : int;
      (** P3: The number of messages received from a peer in the mesh in the
          associated topic above which the peer won't be penalized  *)
  mesh_message_deliveries_decay : float;
      (** P3: The score is multiplied by this factor every [score_cleanup_ticks] heartbeat.
          This parameter must be in the unit interval. *)
  mesh_failure_penalty_weight : float;
      (** P3b: Penalty induced when a peer gets pruned with a non-zero mesh message delivery deficit. *)
  mesh_failure_penalty_decay : float;
      (** P3b: The score is multiplied by this factor every [score_cleanup_ticks] heartbeat.
          This parameter must be in the unit interval. *)
  invalid_message_deliveries_weight : float;
      (** P4: Penalty induced when a peer sends an invalid message. *)
  invalid_message_deliveries_decay : float;
      (** P4: The score is multiplied by this factor every [score_cleanup_ticks] heartbeat.
          This parameter must be in the unit interval. *)
}

type ('topic, 'span) topic_score_limits =
  | Topic_score_limits_single of 'span per_topic_score_limits
      (** Use this constructor when the topic score parameters do not
          depend on the topic. *)
  | Topic_score_limits_family of {
      all_topics : 'topic Seq.t;
      parameters : 'topic -> 'span per_topic_score_limits;
      weights : 'topic -> float;
    }
      (** Use this constructor when the topic score parameters may depend
          on the topic. *)

(* N.B.: unlike the Go/Rust implementations, scores are not refreshed in an asynchronous
   loop but rather in the heartbeat. Hence, we do not have the [decay_interval] parameter.
   Scores are refreshed and decayed every [score_cleanup_ticks] heartbeats (see [limits]).

   TODO: https://gitlab.com/tezos/tezos/-/issues/5545
   We did not implement P6, aka IP colocation factor *)
type ('topic, 'span) score_limits = {
  topics : ('topic, 'span) topic_score_limits;
      (** Per-topic score parameters. *)
  topic_score_cap : float option;
      (** An optional cap on the total positive contribution of topics to the score of the peer.
          If not equal to [None], must be non-negative. *)
  behaviour_penalty_weight : float;
      (** P7: The weight of the score associated to the behaviour penalty. This
          parameter must be negative. *)
  behaviour_penalty_threshold : float;
      (** The threshold on the behaviour penalty
          counter above which we start penalizing the peer. *)
  behaviour_penalty_decay : float;
      (** P7: The score is multiplied by a factor of [behaviour_penalty_decay] every [score_cleanup_ticks] heartbeat.
          This parameter must be in the unit interval. *)
  app_specific_weight : float;  (** P5: Application-specific peer scoring *)
  decay_zero : float;
      (** The minimum value under which a score is considered to be equal to 0 after
          applying decay. This parameter must be non-negative. *)
}

type ('topic, 'peer, 'message_id, 'span) limits = {
  max_recv_ihave_per_heartbeat : int;
      (** The maximum number of IHave control messages we can receive from a
          peer between two heartbeats. It is called [MaxIHaveMessages] in the Go
          implementation. *)
  max_sent_iwant_per_heartbeat : int;
      (** The maximum number of IWant control message ids we can send to a peer
          between two heartbeats. It is also the maximum number of message ids
          to include in an IHave message. It is called [MaxIHaveLength] in the
          Go implementation. *)
  max_gossip_retransmission : int;
      (** The maximum number of times the local peer allows a remote peer to
          request the same message id through IWant gossip before the local peer
          starts ignoring them. This is designed to prevent peers from spamming
          with requests. *)
  degree_optimal : int;
      (** The optimal number of full connections per topic. For
          example, if it is 6, each peer will want to have about six peers in
          their mesh for each topic they're subscribed to. It should be set
          somewhere between {!degree_low} and {!degree_high}. *)
  publish_threshold : float;
      (** The threshold value (as a score) from which we can publish a
          message to our peers. *)
  gossip_threshold : float;
      (** The threshold value (as a score) for a peer to emit/accept gossip: if
          the remote peer score is below this threshold, the local peer won't
          emit or accept gossip from the remote peer.  *)
  do_px : bool;  (** The flag controls whether peer exchange (PX) is enabled. *)
  accept_px_threshold : float;
      (** The threshold value (as a score) from which we accept peer exchanges. *)
  peers_to_px : int;
      (** The number of peers to include in prune Peer eXchange. (This is called
          [PrunePeers] in the Go implementation.) *)
  unsubscribe_backoff : 'span;
      (** The duration that prevent reconnections after leaving a topic to our full connections. *)
  graft_flood_backoff : 'span;
      (** The duration added when a peer tries to graft our connection
          too soon. *)
  prune_backoff : 'span;  (** The duration added when we prune a peer. *)
  retain_duration : 'span;
      (** The duration added to remove metadata about a disconnected peer. *)
  fanout_ttl : 'span;
      (** [fanout_ttl] controls how long we keep track of a fanout topic. If
          it's been [fanout_ttl] since we've published to a topic that we're not
          subscribed to, then we don't track that topic anymore, that is, we
          delete it from the fanout map. *)
  heartbeat_interval : 'span;  (** The time between heartbeats. *)
  backoff_cleanup_ticks : int;
      (** The number of heartbeat ticks setting the frequency at which the
          backoffs are checked and potentially cleared. *)
  score_cleanup_ticks : int;
      (** [score_cleanup_ticks] is the number of heartbeat ticks setting the
          frequency at which the scores are refreshed and potentially cleared. *)
  degree_low : int;
      (** The lower bound on the number of peers we keep in a
          topic mesh. If we have fewer than [degree_low] peers, the heartbeat will attempt
          to graft some more into the mesh at the next heartbeat. *)
  degree_high : int;
      (** The upper bound on the number of peers we keep in a topic mesh. If
          there are more than [degree_high] peers, the heartbeat will select
          some to prune from the mesh at the next heartbeat. *)
  degree_score : int;
      (** [degree_score] affects how peers are selected when pruning a mesh due
          to over subscription. At least [degree_score] of the retained peers
          will be high-scoring, while the remainder are chosen randomly. *)
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/5052
     [degree_score] must not exceed [degree_optimal - degree_out]. *)
  degree_out : int;
      (** The number of outbound connections to maintain in a topic mesh.  When
          the mesh is pruned due to over subscription, we make sure that we have
          outbound connections to at least [degree_out] of the survivor
          peers. This prevents Sybil attackers from overwhelming our mesh with
          incoming connections. [degree_out] must be set below {!degree_low},
          and must not exceed [degree_optimal / 2]. *)
  degree_lazy : int;
      (** [degree_lazy] affects how many peers the local peer will emit gossip
          to at each heartbeat. The local peer will send gossip to at least
          [degree_lazy] peers outside its mesh or fanout. The actual number may
          be more, depending on [gossip_factor] and how many peers the local
          peer is connected to. *)
  gossip_factor : float;
      (** [gossip_factor] affects how many peers the local peer will emit gossip
          to at each heartbeat. The local peer will send gossip to
          [gossip_factor] * (total number of non-mesh/non-fanout peers), or
          [degree_lazy] peers, whichever is greater. *)
  history_length : int;
      (** The size of the message cache used for gossip. The message cache will
          remember messages for [history_length] heartbeats. *)
  history_gossip_length : int;
      (** [history_gossip_length] controls how many cached message ids the local
          peer will advertise in IHave gossip messages. When asked for its seen
          message ids, the local peer will return only those from the most
          recent [history_gossip_length] heartbeats. The slack between
          [history_gossip_length] and [history_length] allows the local peer to
          avoid advertising messages that will be expired by the time they're
          requested. [history_gossip_length] must be less than or equal to
          [history_length]. *)
  opportunistic_graft_ticks : int64;
      (** The number of heartbeat ticks setting the frequency at which to
          attempt to improve the mesh with opportunistic grafting. Every
          [opportunistic_graft_ticks], if the median score of the mesh peers
          falls below the {!opportunistic_graft_threshold}, then the local peer
          will select some high-scoring mesh peers to graft.  *)
  opportunistic_graft_peers : int;
      (** The number of peers to opportunistically graft. *)
  opportunistic_graft_threshold : float;
      (** The median mesh score threshold before triggering opportunistic
          grafting; this should have a small positive value. *)
  score_limits : ('topic, 'span) score_limits;
      (** score-specific parameters. *)
  seen_history_length : int;
      (** [seen_history_length] controls the size of the message cache used for
          recording seen messages. The seen messages cache will remember messages for
          [seen_history_length] heartbeats. *)
}

type ('peer, 'message_id) parameters = {
  peer_filter :
    'peer -> [`IHave of 'message_id | `IWant of 'message_id | `Graft] -> bool;
}

(** The [SCORE] module type describes primitives used to update the scores
    associated to each peer. Score computation is described in more details in
    {{: https://github.com/libp2p/specs/blob/master/pubsub/gossipsub/gossipsub-v1.1.md#peer-scoring }the specification}.

    The score associated to a peer is a weighted sum of various counters, some
    of which are indexed by topics. Each captures a certain aspect of what a
    well-behaving peer should do. A short description follows, keeping the
    naming P1, P2, ... used in the specification.

    - P1: For each topic, we measure how long a peer has spent in the mesh for that topic. The longest, the better.
    - P2: For each topic, we measure how many times the peer was the first among all peers to send us a message on that topic.
    - P3: For each topic, we measure the number of message deliveries. If the number is below a threshold,
      we penalize the associated peer.
    - P3b: Trigger P3 computation when the peer gets pruned or removed, so as to not forget yet unaccounted for bad message
      delivery counts.
    - P4: For each topic, penalize peers sending invalid messages on that topic.
    - P5: The applicative layer can assign an arbitrary application-specific score to a peer.
    - P7: When a peer is pruned from the mesh for a topic, we install a backoff that
      prevents that peer from regrafting too soon. If the peer does not respect this backoff,
      it is penalized.
*)
module type SCORE = sig
  (** The type of peer scoring statistics. *)
  type t

  type value

  type span

  type time

  type topic

  (** [newly_connected params] creates a fresh statistics record. *)
  val newly_connected : (topic, span) score_limits -> t

  (** [value ps] evaluates the score of [ps]. *)
  val value : t -> value

  (** [penalty ps penalty] increments the behavioural penalty of [ps]. *)
  val penalty : t -> int -> t

  (** [set_connected ps] marks [ps] as being associated to a connected peer. *)
  val set_connected : t -> t

  (** [remove_peer ps ~retain_duration] will either return [None] if
      the peer statistics can be cleared, or [Some ps'] with [ps']
      some statistics to be retained for at least [retain_duration]. *)
  val remove_peer : t -> retain_duration:span -> t option

  (** [expires ps] returns [None] if the score statistics has no expiration
      time or [Some t] if it expires at time [t]. *)
  val expires : t -> time option

  (** [graft ps topic] allows to measure the time spent by the peer in the mesh.
      It is to be called upon grafting a peer to [topic]. *)
  val graft : t -> topic -> t

  (** [prune ps topic] allows to measure the time spent by the peer in the mesh.
      It is to be called upon pruning a peer from [topic]. *)
  val prune : t -> topic -> t

  (** [first_message_delivered ps topic] increments the counter related to
      first message deliveries and mesh message deliveries on [topic]
      by the associated peer. *)
  val first_message_delivered : t -> topic -> t

  (** [duplicate_message_delivered ps topic validated] increments the counter related to
      near-first mesh message deliveries on [topic] by the associated peer. [validated]
      is the time at which the message was seen by the automaton for the first time. *)
  val duplicate_message_delivered : t -> topic -> time -> t

  (** [invalid_message_delivered ps topic] increments the counter related to
      invalid messages sent by the associated peer. *)
  val invalid_message_delivered : t -> topic -> t

  (** [set_application_score ps score] sets the application-specific score. This score can
      be positive or negative.  *)
  val set_application_score : t -> float -> t

  (** [refresh ps] returns [Some ps'] with [ps'] a refreshed score record or [None]
      if the score expired. Refreshing a [ps] allows to update time-dependent spects
      of the scoring statistics. *)
  val refresh : t -> t option

  (** The zero score value, corresponding to a neutral score. *)
  val zero : value

  (** Convert a float into a score value. *)
  val of_float : float -> value

  include Compare.S with type t := value

  include PRINTABLE with type t := t

  val pp_value : Format.formatter -> value -> unit

  module Internal_for_tests : sig
    val get_topic_params :
      ('topic, 'span) score_limits -> 'topic -> 'span per_topic_score_limits
  end
end

module type MESSAGE_CACHE = sig
  module Peer : ITERABLE

  module Topic : ITERABLE

  module Message_id : ITERABLE

  module Message : PRINTABLE

  module Time : TIME

  (** A sliding window cache that stores published messages and their first seen
    time. The module also keeps track of the number of accesses to a message by
    a peer, thus indirectly tracking the number of IWant requests a peer makes
    for the same message between two heartbeats.

    The module assumes that no two different messages have the same message
    id. However, the cache stores duplicates; for instance, if [add_message id
    msg topic] is called exactly twice, then [msg] will appear twice in
    [get_message_ids_to_gossip]'s result (assuming not more than [gossip_slots]
    shifts have been executed in the meanwhile). *)
  type t

  (** [create ~history_slots ~gossip_slots ~seen_message_slots] creates two
      sliding window caches, one with length [history_slots] for storing message contents
      and another with length [seen_message] for storing seen messages and their first
      seen times.

      When queried for messages to advertise, the cache only returns messages in
      the last [gossip_slots]. The [gossip_slots] must be smaller or equal to
      [history_slots].

      The slack between [gossip_slots] and [history_slots] accounts for the
      reaction time between when a message is advertised via IHave gossip, and
      when the peer pulls it via an IWant command. To see this, if say
      [gossip_slot = history_slots] then the messages inserted in cache
      [history_slots] heartbeat ticks ago and advertised now will not be
      available after the next tick, because they are removed from the
      cache. This means IWant requests from the remote peer for such messages
      would be unfulfilled and potentially penalizing.

      @raise Assert_failure when [gossip_slots <= 0 || gossip_slots > history_slots]

      TODO: https://gitlab.com/tezos/tezos/-/issues/5129
      Error handling. *)
  val create :
    history_slots:int -> gossip_slots:int -> seen_message_slots:int -> t

  (** Add message to the most recent cache slot. If the message already exists
      in the cache, the message is not overridden, instead a duplicate message
      id is stored (the message itself is only stored once). *)
  val add_message : Message_id.t -> Message.t -> Topic.t -> t -> t

  (** Get the message associated to the given message id, increase the access
      counter for the peer requesting the message, and also returns the updated
      counter. *)
  val get_message_for_peer :
    Peer.t -> Message_id.t -> t -> (t * Message.t * int) option

  (** Get the message ids for the given topic in the last [gossip_slots] slots
      of the cache. If there were duplicates added in the cache, then there will
      be duplicates in the output. There is no guarantee about the order of
      messages in the output. *)
  val get_message_ids_to_gossip : Topic.t -> t -> Message_id.t list

  (** [get_first_seen_time message_id t] returns the time the message with [message_id]
      was first seen. Returns [None] if the message was not seen during the period
      covered by the sliding window. *)
  val get_first_seen_time : Message_id.t -> t -> Time.t option

  (** [seen_message message_id t] returns [true] if the message was seen during the
      period covered by the sliding window and returns [false] if otherwise. *)
  val seen_message : Message_id.t -> t -> bool

  (** Shift the sliding window by one slot (usually corresponding to one
      heartbeat tick). *)
  val shift : t -> t

  module Internal_for_tests : sig
    val get_access_counters : t -> (Message_id.t * int Peer.Map.t) Seq.t
  end
end

module type AUTOMATON = sig
  (** Module for peer *)
  module Peer : ITERABLE

  (** Module for topic *)
  module Topic : ITERABLE

  (** Module for message_id *)
  module Message_id : sig
    include ITERABLE

    (** Computes the topic of the given message id. *)
    val get_topic : t -> Topic.t
  end

  (** Module for message *)
  module Message : PRINTABLE

  (** Module for time *)
  module Time : PRINTABLE

  (** Module for time duration *)
  module Span : SPAN

  (** Module for peers scores *)
  module Score : SCORE with type time = Time.t

  type message = Message.t

  type span = Span.t

  (** The state managed by the gossipsub automaton. The state is
      purely functional. *)
  type state

  (** Limits of the gossipsub protocol. *)
  type limits := (Topic.t, Peer.t, Message_id.t, span) limits

  (** Parameters of the gossipsub protocol. *)
  type parameters := (Peer.t, Message_id.t) parameters

  (** The types of payloads for inputs to the gossipsub automaton. *)

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

  (** Output produced by one of the actions below. *)
  type _ output =
    | Ihave_from_peer_with_low_score : {
        score : Score.t;
        threshold : float;
      }
        -> [`IHave] output
        (** The peer who sent an IHave message has a [score] below [threshold]. *)
    | Too_many_recv_ihave_messages : {count : int; max : int} -> [`IHave] output
        (** The peer sent us more than [max] IHave messages within two
            successive heartbeat calls. *)
    | Too_many_sent_iwant_messages : {count : int; max : int} -> [`IHave] output
        (** We sent more than [max] IWant messages to this peer within two
            successive heartbeat calls. *)
    | Message_topic_not_tracked : [`IHave] output
        (** We received an IHave message for a topic we don't track. *)
    | Message_requested_message_ids : Message_id.t list -> [`IHave] output
        (** The messages ids we want to request from the peer which sent us an
            IHave message. The implementation honors the
            [max_sent_iwant_per_heartbeat] limit. *)
    | Iwant_from_peer_with_low_score : {
        score : Score.t;
        threshold : float;
      }
        -> [`IWant] output
        (** The peer who sent an IWant message has a [score] below [threshold]. *)
    | On_iwant_messages_to_route : {
        routed_message_ids :
          [`Ignored | `Not_found | `Too_many_requests | `Message of message]
          Message_id.Map.t;
      }
        -> [`IWant] output
        (** As an answer for an [`IWant] message, the automaton returns a map
            associating to each requested message_id either [`Ignored] if the
            peer is filtered out by [peer_filter], [`Not_found] if the message
            is not found, or [Message m] if [m] is the message with the given
            id. *)
    | Peer_filtered : [`Graft] output
        (** The peer we attempt to graft has not been selected by
            [peer_filter]. *)
    | Unsubscribed_topic : [`Graft] output
        (** We didn't join the topic for which we are attempting to graft a
            peer. *)
    | Peer_already_in_mesh : [`Graft] output
        (** Attempting to graft a peer which has already been grafted. *)
    | Grafting_direct_peer : [`Graft] output
        (** Attempting to graft a direct peer. *)
    | Unexpected_grafting_peer : [`Graft] output
        (** The peer we attempt to graft is not known. *)
    | Grafting_peer_with_negative_score : [`Graft] output
        (** Attempting to graft a peer with a negative score. *)
    | Grafting_successfully : [`Graft] output
        (** Grafting the given peer for the provided topic succeeded. *)
    | Peer_backed_off : [`Graft] output
        (** We cannot graft the given peer because it is backed off. *)
    | Mesh_full : [`Graft] output
        (** Grafting a peer for a topic whose mesh has already sufficiently many
            peers. *)
    | Prune_topic_not_tracked : [`Prune] output
        (** Attempting to prune a peer for a non-tracked topic. *)
    | Peer_not_in_mesh : [`Prune] output
        (** Attempting to prune a peer which is not in the mesh. *)
    | Ignore_PX_score_too_low : Score.t -> [`Prune] output
        (** The given peer has been pruned for the given topic, but no
            alternative peers are returned because the peer's score is too low.
            The score of the peer is included in the return value. *)
    | No_PX : [`Prune] output
        (** The given peer has been pruned for the given topic. No
            alternatives peers was provided in {!prune}. *)
    | PX : Peer.Set.t -> [`Prune] output
        (** The given peer has been pruned for the given topic. The given set of
            peers alternatives in {!prune} for that topic is returned. *)
    | Publish_message : {to_publish : Peer.Set.t} -> [`Publish_message] output
        (** [to_publish] contains:
            - Direct peers for the message's topic;
            - The peers in the topic's mesh, if the peer is subscribed to the
              topic. Otherwise, the peers in the topic's fanout. *)
    | Already_published : [`Publish_message] output
        (** Attempting to publish a message that has already been published. *)
    | Route_message : {to_route : Peer.Set.t} -> [`Receive_message] output
        (** [to_route] contains:
            - Direct peers for the message's topic;
            - The peers in the topic's mesh minus the original sender of the message. *)
    | Already_received : [`Receive_message] output
        (** Received a message that has already been recevied before. *)
    | Not_subscribed : [`Receive_message] output
        (** Received a message from a remote peer for a topic we are not
            subscribed to (called "unknown topic" in the Go implementation). *)
    | Invalid_message : [`Receive_message] output
    | Unknown_validity : [`Receive_message] output
        (** Attempting to publish a message that is invalid. *)
    | Already_joined : [`Join] output
        (** Attempting to join a topic we already joined. *)
    | Joining_topic : {to_graft : Peer.Set.t} -> [`Join] output
        (** When successfully joining a topic, the set of grafted peers for that
            topic is returned. *)
    | Not_joined : [`Leave] output
        (** Attempting to leave a topic which we didn't join or had already
            left. *)
    | Leaving_topic : {
        to_prune : Peer.Set.t;
        noPX_peers : Peer.Set.t;
      }
        -> [`Leave] output
        (** When successfully leaving a topic, the set of pruned peers for that
            topic is returned alongside a subset of those peers for which no
            alternative PX will be proposed. *)
    | Heartbeat : {
        to_graft : Topic.Set.t Peer.Map.t;
            (** The set of topics per peer that have been grafted. *)
        to_prune : Topic.Set.t Peer.Map.t;
            (** The set of topics per peer that have been pruned. *)
        noPX_peers : Peer.Set.t;
            (** Set of peers for which peer exchange (PX) will not be
                proposed. *)
      }
        -> [`Heartbeat] output
    | Peer_added : [`Add_peer] output
        (** The output returned when successfully adding a peer. *)
    | Peer_already_known : [`Add_peer] output
        (** The output returned when attempting to add a peer which is already
            known. *)
    | Removing_peer : [`Remove_peer] output
        (** The output returned when successfully removing a peer. *)
    | Subscribed : [`Subscribe] output
        (** The output returned once we successfully processed a subscribe
            request sent from a peer. *)
    | Subscribe_to_unknown_peer : [`Subscribe] output
        (** The output returned when we receive a subscribe message from a peer
            we don't know.*)
    | Unsubscribed : [`Unsubscribe] output
        (** The output returned once we successfully processed an unsubscribe
            request sent from a peer. *)
    | Unsubscribe_from_unknown_peer : [`Unsubscribe] output
        (** The output returned when we receive an unsubscribe message from a
            peer we don't know.*)
    | Set_application_score : [`Set_application_score] output
        (** The output returned when we set the application score of a peer *)

  (** A type alias for the state monad. *)
  type 'a monad := state -> state * 'a output

  (** Initialise a state. *)
  val make : Random.State.t -> limits -> parameters -> state

  (** [add_peer { direct; outbound; peer }] is called to notify a new connection. If
      [direct] is [true], the gossipsub always forwards messages to those
      peers. [outbound] is [true] if it is an outbound connection, that is, a
      connection initiated by the local (not the remote) peer. *)
  val add_peer : add_peer -> [`Add_peer] monad

  (** [remove_peer { peer }] notifies gossipsub that we are disconnected
      from a peer. Do note that the [state] still maintain information
      for this connection for [retain_duration] seconds. *)
  val remove_peer : remove_peer -> [`Remove_peer] monad

  (** [handle_subscribe {topic; peer}] handles a request from a remote [peer]
      informing us that it is subscribed to [topic]. *)
  val handle_subscribe : subscribe -> [`Subscribe] monad

  (** [handle_unsubscribe {topic; peer}] handles a request from a remote [peer]
      informing us that it unsubscribed from [topic]. *)
  val handle_unsubscribe : unsubscribe -> [`Unsubscribe] monad

  (** [handle_ihave { peer; topic; message_ids }] handles the gossip message
      [IHave] emitted by [peer] for [topic] with the [message_ids].  *)
  val handle_ihave : ihave -> [`IHave] monad

  (** [handle_iwant { peer; message_ids }] handles the gossip message
      [IWant] emitted by [peer] for [topic] with the [message_ids]. *)
  val handle_iwant : iwant -> [`IWant] monad

  (** [handle_graft { peer; topic }] handles the gossip message [Graft]
      emitted by [peer] for [topic]. This action allows to graft a
      connection to a full connection allowing the transmission of
      full messages for the given topic. *)
  val handle_graft : graft -> [`Graft] monad

  (** [handle_prune { peer; topic; px; backoff }] handles the gossip
      message [Prune] emitted by [peer] for [topic]. This action
      allows to prune a full connection. In that case, the remote peer
      can send a list of peers to connect to as well as a backoff
      time, which is a duration for which we cannot [Graft] this peer
      on this topic. *)
  val handle_prune : prune -> [`Prune] monad

  (** [handle_receive_message { sender; topic; message_id; message }] handles
      a message received from [sender] on the gossip network. The function returns
      a set of peers to which the (full) message will be directly forwarded.  *)
  val handle_receive_message : receive_message -> [`Receive_message] monad

  (** [publish { topic; message_id; message }] allows to publish a message
      on the gossip network from the local node. The function returns a set of peers
      to which the (full) message will be directly forwarded.  *)
  val publish_message : publish_message -> [`Publish_message] monad

  (** [heartbeat] executes the heartbeat routine of the algorithm. *)
  val heartbeat : [`Heartbeat] monad

  (** [join { topic }] handles a join to a new topic. On success, the function
      returns the set of peers that have been grafted to form the mesh of the
      joined topic. *)
  val join : join -> [`Join] monad

  (** [leave { topic }] handles a leave from a topic. On success, the function
      returns the set of peers, forming the mesh, that have been pruned for that
      topic. *)
  val leave : leave -> [`Leave] monad

  (** [set_application_score {peer; score}] handles setting the application score
      of [peer]. If the peer is not known, this does nothing. *)
  val set_application_score :
    set_application_score -> [`Set_application_score] monad

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/5352
     Handle the random state explicitly for [select_px_peers] and
     [select_gossip_messages]. *)

  (** Select random peers for Peer eXchange. Note that function is
      deterministic; however, it has side effects in that it updates the
      [state]'s random state. *)
  val select_px_peers :
    state ->
    peer_to_prune:Peer.t ->
    Topic.t ->
    noPX_peers:Peer.Set.t ->
    Peer.t list

  (** Select the gossip messages to be sent. These are IHave control messages
      referring to recently seen messages (that is, sent during the last
      [history_gossip_length] heartbeat ticks), to be sent to a random selection
      of peers. The message ids for a peer and a topic are also selected at
      random among the possible ones. At most [max_sent_iwant_per_heartbeat]
      message ids are sent.

      The local peer will send gossip to at most [gossip_factor] * (total number
      of non-mesh/non-fanout peers), or [degree_lazy] random peers, whichever is
      greater.

      Note that function is deterministic; however, it has side effects in that
      it updates the [state]'s random state.  *)
  val select_gossip_messages : state -> ihave list

  val pp_add_peer : Format.formatter -> add_peer -> unit

  val pp_remove_peer : Format.formatter -> remove_peer -> unit

  val pp_ihave : Format.formatter -> ihave -> unit

  val pp_iwant : Format.formatter -> iwant -> unit

  val pp_graft : Format.formatter -> graft -> unit

  val pp_prune : Format.formatter -> prune -> unit

  val pp_receive_message : Format.formatter -> receive_message -> unit

  val pp_publish_message : Format.formatter -> publish_message -> unit

  val pp_join : Format.formatter -> join -> unit

  val pp_leave : Format.formatter -> leave -> unit

  val pp_subscribe : Format.formatter -> subscribe -> unit

  val pp_unsubscribe : Format.formatter -> unsubscribe -> unit

  val pp_output : Format.formatter -> 'a output -> unit

  module Introspection : sig
    type connection = {topics : Topic.Set.t; direct : bool; outbound : bool}

    type fanout_peers = {peers : Peer.Set.t; last_published_time : Time.t}

    module Message_cache : sig
      type t

      val get_message_for_peer :
        Peer.t -> Message_id.t -> t -> (t * Message.t * int) option

      module Internal_for_tests : sig
        val get_access_counters : t -> (Message_id.t * int Peer.Map.t) Seq.t
      end
    end

    type view = {
      limits : limits;
      parameters : parameters;
      connections : connection Peer.Map.t;
      scores : Score.t Peer.Map.t;
      ihave_per_heartbeat : int Peer.Map.t;
      iwant_per_heartbeat : int Peer.Map.t;
      mesh : Peer.Set.t Topic.Map.t;
      fanout : fanout_peers Topic.Map.t;
      backoff : Time.t Peer.Map.t Topic.Map.t;
      message_cache : Message_cache.t;
      rng : Random.State.t;
      heartbeat_ticks : int64;
    }

    (** When selecting a set of connected peers, one can specify some criteria
        to filter the result. *)
    type connected_peers_filter =
      | Direct
      | Subscribed_to of Topic.t
      | Score_above of {threshold : Score.value}

    val view : state -> view

    (** [get_peers_in_topic_mesh topic state] returns the peers in the mesh of
        [topic]. *)
    val get_peers_in_topic_mesh : Topic.t -> view -> Peer.t list

    (** [get_connected_peers ?filters view] returns the list of connected peers
        filtered by the given criteria.  *)
    val get_connected_peers :
      ?filters:connected_peers_filter list -> view -> Peer.t list

    (** [get_our_topics state] returns the set of topics the local peer is
        subscribed to. *)
    val get_our_topics : view -> Topic.t list

    (** [get_subscribed_topics peer state] returns the set of topics
        that are subscribed by [peer] *)
    val get_subscribed_topics : Peer.t -> view -> Topic.t list

    (** [get_fanout_peers topic state] returns the fanout peers of [topic]. *)
    val get_fanout_peers : Topic.t -> view -> Peer.t list

    val limits : view -> limits

    (** [has_joined topic view] returns true if and only if the automaton is
        currently tracking messages for [topic]. That is, the local peer has joined
        and hasn't left the [topic]. *)
    val has_joined : Topic.t -> view -> bool

    val pp_connection : connection Fmt.t

    val pp_connections : connection Peer.Map.t Fmt.t

    val pp_peer_map : 'a Fmt.t -> 'a Peer.Map.t Fmt.t

    val pp_message_id_map : 'a Fmt.t -> 'a Message_id.Map.t Fmt.t

    val pp_topic_map : 'a Fmt.t -> 'a Topic.Map.t Fmt.t

    val pp_peer_set : Peer.Set.t Fmt.t

    val pp_topic_set : Topic.Set.t Fmt.t
  end
end

module type WORKER_CONFIGURATION = sig
  (** The gossipsub automaton that will be used by the worker. *)
  module GS : AUTOMATON

  (** Abstraction of the IO monad used by the worker. *)
  module Monad : sig
    (** The monad type. *)
    type 'a t

    (** Equivalent to [bind m f] function, in infix notation. *)
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

    (** The monad's return function. *)
    val return : 'a -> 'a t

    (** [sleep span] will block for the amount of time specified by [span]. *)
    val sleep : GS.span -> unit t
  end

  (** A mutable (FIFO) stream of data. *)
  module Stream : sig
    (** The stream data structure. *)
    type 'a t

    (** Create a new empty stream. *)
    val empty : unit -> 'a t

    (** Push the given value into the stream. *)
    val push : 'a -> 'a t -> unit

    (** Pops the oldest value that has been pushed to the stream. In case the
        stream is empty, the function will wait until some value is pushed. *)
    val pop : 'a t -> 'a Monad.t

    (** Returns and removes all available elements of the stream l without
        blocking. *)
    val get_available : 'a t -> 'a list
  end
end

(** The interface of a gossipsub worker. *)
module type WORKER = sig
  (** The state of a gossipsub worker. *)
  type t

  (** We (re-)export the GS, Monad and Stream modules. *)
  include WORKER_CONFIGURATION

  (** A message together with a header, that is, a topic and an id.
     This corresponds to what the spec calls a "full message". *)
  type message_with_header = {
    message : GS.Message.t;
    topic : GS.Topic.t;
    message_id : GS.Message_id.t;
  }

  (** The following type defines the different kinds of messages a peer could
      receive from or sent to the P2P layer. *)
  type p2p_message =
    | Graft of {topic : GS.Topic.t}
    | Prune of {topic : GS.Topic.t; px : GS.Peer.t Seq.t; backoff : GS.Span.t}
    | IHave of {topic : GS.Topic.t; message_ids : GS.Message_id.t list}
    | IWant of {message_ids : GS.Message_id.t list}
    | Subscribe of {topic : GS.Topic.t}
    | Unsubscribe of {topic : GS.Topic.t}
    | Message_with_header of message_with_header

  (** The different kinds of input events that could be received from the P2P
      layer. *)
  type p2p_input =
    | In_message of {from_peer : GS.Peer.t; p2p_message : p2p_message}
    | New_connection of {peer : GS.Peer.t; direct : bool; outbound : bool}
    | Disconnection of {peer : GS.Peer.t}

  (** The different kinds of input events that could be received from the
      application layer. *)
  type app_input =
    | Publish_message of message_with_header
    | Join of GS.Topic.t
    | Leave of GS.Topic.t

  (** The different kinds of outputs that could be emitted by the worker for the
      P2P layer. *)
  type p2p_output =
    | Out_message of {to_peer : GS.Peer.t; p2p_message : p2p_message}
    | Disconnect of {peer : GS.Peer.t}
    | Kick of {peer : GS.Peer.t}
    | Connect of {peer : GS.Peer.t}

  (** The application layer will be advertised about full messages it's
      interested in. *)
  type app_output = message_with_header

  (** [make rng limits parameters] initializes a new Gossipsub automaton with
      the given arguments. Then, it initializes and returns a worker for it. *)
  val make :
    Random.State.t ->
    (GS.Topic.t, GS.Peer.t, GS.Message_id.t, GS.span) limits ->
    (GS.Peer.t, GS.Message_id.t) parameters ->
    t

  (** [start topics state] runs the (not already started) worker whose [state]
      is given together with the initial list of [topics] the caller is interested in. *)
  val start : GS.Topic.t list -> t -> t

  (** [shutdown state] allows stopping the worker whose [state] is given. *)
  val shutdown : t -> unit Monad.t

  (** [app_input state app_input] adds the given application input [app_input]
      to the worker's input stream. *)
  val app_input : t -> app_input -> unit

  (** [p2p_input state p2p_input] adds the given P2P input [p2p_input] to the
      worker's input stream. *)
  val p2p_input : t -> p2p_input -> unit

  (** [p2p_output_stream t] returns the output stream containing data for the
      P2P layer. *)
  val p2p_output_stream : t -> p2p_output Stream.t

  (** [app_output_stream t] returns the output stream containing data for the
      application layer. *)
  val app_output_stream : t -> app_output Stream.t

  (** Pretty-printer for values of type {!p2p_output}. *)
  val pp_p2p_output : Format.formatter -> p2p_output -> unit

  (** Pretty-printer for values of type {!app_output}. *)
  val pp_app_output : Format.formatter -> app_output -> unit
end
