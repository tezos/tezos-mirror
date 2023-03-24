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

module type AUTOMATON_CONFIG = sig
  module Peer : ITERABLE

  module Topic : ITERABLE

  module Message_id : ITERABLE

  module Message : PRINTABLE

  module Span : PRINTABLE

  module Time : sig
    include Compare.S

    include PRINTABLE with type t := t

    type span = Span.t

    val now : unit -> t

    val add : t -> span -> t

    val sub : t -> span -> t

    (** [mul_span s n] returns [n * s]. *)
    val mul_span : span -> int -> span
  end
end

type ('peer, 'message_id, 'span) limits = {
  max_recv_ihave_per_heartbeat : int;
      (** The maximum number of control message [IHave] we can receive
          from our peers between two heartbeats. *)
  max_sent_iwant_per_heartbeat : int;
      (** The maximum number of control messages [IWant] we can sent
          to our peers between two heartbeats. *)
  degree_optimal : int;
      (** The optimal number of full connections per topic. For
          example, if it is 6, each peer will want to have about six peers in
          their mesh for each topic they're subscribed to. It should be set
          somewhere between {degree_low} and {degree_high}. *)
  gossip_publish_threshold : float;
      (** The threshold value (as a score) from which we can publish a
          message to our peers. *)
  accept_px_threshold : float;
      (** The threshold value (as a score) from which we accept peer exchanges. *)
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
  heartbeat_interval : 'span;
      (** [heartbeat_interval] controls the time between heartbeats. *)
  backoff_cleanup_ticks : int;
      (** [backoff_cleanup_ticks] is the number of heartbeat ticks setting the
          frequency at which the backoffs are checked and potentially cleared. *)
  degree_low : int;
      (** [degree_low] sets the lower bound on the number of peers we keep in a
          topic mesh. If we have fewer than [degree_low] peers, the heartbeat will attempt
          to graft some more into the mesh at the next heartbeat. *)
  degree_high : int;
      (** [degree_high] sets the upper bound on the number of peers we keep in a
          topic mesh.  If there are more than [degree_high] peers, the heartbeat will select
          some to prune from the mesh at the next heartbeat. *)
  degree_score : int;
      (** [degree_score] affects how peers are selected when pruning a mesh due
          to over subscription. At least [degree_score] of the retained peers
          will be high-scoring, while the remainder are chosen randomly. *)
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/5052
     [degree_score] must not exceed [degree_optimal - degree_out]. *)
  degree_out : int;
      (** [degree_out] is the quota for the number of outbound connections to
          maintain in a topic mesh.  When the mesh is pruned due to over
          subscription, we make sure that we have outbound connections to at
          least [degree_out] of the survivor peers. This prevents Sybil
          attackers from overwhelming our mesh with incoming connections.
	        [degree_out] must be set below {degree_low}, and must not exceed
          [degree_optimal / 2]. *)
}

type ('peer, 'message_id) parameters = {
  peer_filter :
    'peer -> [`IHave of 'message_id | `IWant of 'message_id | `Graft] -> bool;
}

module type SCORE = sig
  include Compare.S

  val float : t -> float

  val zero : t

  val penalty : t -> int -> t
end

module type AUTOMATON = sig
  (** Module for peer *)
  module Peer : ITERABLE

  (** Module for topic *)
  module Topic : ITERABLE

  (** Module for message_id *)
  module Message_id : ITERABLE

  (** Module for message *)
  module Message : PRINTABLE

  (** Module for time *)
  module Time : PRINTABLE

  (** Module for time duration *)
  module Span : PRINTABLE

  (** Module for peers scores *)
  module Score : SCORE

  type message = Message.t

  type span = Span.t

  (** The state managed by the gossipsub automaton. The state is
      purely functional. *)
  type state

  (** Limits of the gossipsub protocol. *)
  type limits := (Peer.t, Message_id.t, span) limits

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

  type publish = {
    sender : Peer.t option;
    topic : Topic.t;
    message_id : Message_id.t;
    message : message;
  }

  type join = {topic : Topic.t}

  type leave = {topic : Topic.t}

  type subscribe = {topic : Topic.t; peer : Peer.t}

  type unsubscribe = {topic : Topic.t; peer : Peer.t}

  (** Output produced by one of the actions below. *)
  type _ output =
    | Negative_peer_score : Score.t -> [`IHave] output
        (** The peer who sent an [`IHave] message has a negative score. *)
    | Too_many_recv_ihave_messages : {count : int; max : int} -> [`IHave] output
        (** The peer sent us more than [max] ihave messages within two
            successive heartbeat calls. *)
    | Too_many_sent_iwant_messages : {count : int; max : int} -> [`IHave] output
        (** We sent more than [max] iwant messages to this peer within two
            successive heartbeat calls. *)
    | Message_topic_not_tracked : [`IHave] output
        (** We received an [`IHave] message for a topic we don't track. *)
    | Message_requested_message_ids : Message_id.t list -> [`IHave] output
        (** The messages IDs we want to request from the peer which sent us an
            [`IHave] message. The implementation honors the
            [max_sent_iwant_per_heartbeat] limit. *)
    | On_iwant_messages_to_route : {
        routed_message_ids :
          [`Ignored | `Not_found | `Message of message] Message_id.Map.t;
      }
        -> [`IWant] output
        (** As an answer for an [`IWant] message, the automaton returns a map
            associating to each requested message_id either [`Ignored] if the
            peer is filtered out by [peer_filter], [`Not_found] if the message
            is not found, or [Message m] if [m] is the message of the given
            ID. *)
    | Peer_filtered : [`Graft] output
        (** The peer we attempt to graft has not been selected by
            [peer_filter]. *)
    | Unknown_topic : [`Graft] output
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
    | No_peer_in_mesh : [`Prune] output
        (** Attempting to prune a peer which is not in the mesh. *)
    | Ignore_PX_score_too_low : Score.t -> [`Prune] output
        (** The given peer has been pruned for the given topic, but no
            alternative peers are returned because the peer's score is too low.
            The score of the peer is included in the return value. *)
    | No_PX : [`Prune] output
        (** The given peer has been pruned for the given topic. But the given
            set of peers alternatives in {!prune} is empty. *)
    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5251

       Some fixes might be needed for the prune case. Once done, update the
       docstring. *)
    | PX : Peer.Set.t -> [`Prune] output
        (** The given peer has been pruned for the given topic. The given set of
            peers alternatives in {!prune} for that topic is returned. *)
    | Publish_message : Peer.Set.t -> [`Publish] output
        (** If the peer is subscribed to the topic, returns the set of peers in the topic's mesh.
            Otherwise, it will return the set of peers in the topic's fanout.*)
    | Already_joined : [`Join] output
        (** Attempting to join a topic we already joined. *)
    | Joining_topic : {to_graft : Peer.Set.t} -> [`Join] output
        (** When successfully joining a topic, the set of grafted peers for that
            topic is returned. *)
    | Not_joined : [`Leave] output
        (** Attempting to leave a topic which we didn't join or had already
            left. *)
    | Leaving_topic : {to_prune : Peer.Set.t} -> [`Leave] output
        (** When successfully leaving a topic, the set of pruned peers for that
            topic is returned. *)
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

  (** [publish { sender; topic; message_id; message }] allows to route a message
      on the gossip network. If [sender=None], the message comes from the
      application layer and the local node is the sender. The function returns a
      set of peers to which the (full) message will be directly forwarded.  *)
  val publish : publish -> [`Publish] monad

  (** [heartbeat] executes the heartbeat routine of the algorithm. *)
  val heartbeat : [`Heartbeat] monad

  (** [handle_subscribe {topic; peer}] handles a request from a remote [peer]
      informing us that it is subscribed to [topic]. *)
  val handle_subscribe : subscribe -> [`Subscribe] monad

  (** [handle_unsubscribe {topic; peer}] handles a request from a remote [peer]
      informing us that it unsubscribed from [topic]. *)
  val handle_unsubscribe : unsubscribe -> [`Unsubscribe] monad

  (** [join { topic }] handles a join to a new topic. On success, the function
      returns the set of peers that have been grafted to form the mesh of the
      joined topic. *)
  val join : join -> [`Join] monad

  (** [leave { topic }] handles a leave from a topic. On success, the function
      returns the set of peers, forming the mesh, that have been pruned for that
      topic. *)
  val leave : leave -> [`Leave] monad

  val pp_add_peer : Format.formatter -> add_peer -> unit

  val pp_remove_peer : Format.formatter -> remove_peer -> unit

  val pp_ihave : Format.formatter -> ihave -> unit

  val pp_iwant : Format.formatter -> iwant -> unit

  val pp_graft : Format.formatter -> graft -> unit

  val pp_prune : Format.formatter -> prune -> unit

  val pp_publish : Format.formatter -> publish -> unit

  val pp_join : Format.formatter -> join -> unit

  val pp_leave : Format.formatter -> leave -> unit

  val pp_subscribe : Format.formatter -> subscribe -> unit

  val pp_unsubscribe : Format.formatter -> unsubscribe -> unit

  module Introspection : sig
    type connection = {
      topics : Topic.Set.t;
      direct : bool;
      outbound : bool;
      backoff : Time.t Topic.Map.t;
      score : Score.t;
      expire : Time.t option;
    }

    type connections := connection Peer.Map.t

    type fanout_peers = {peers : Peer.Set.t; last_published_time : Time.t}

    module Message_cache : sig
      type value = {message : message; access : int Peer.Map.t}

      type t = {messages : value Message_id.Map.t}

      (** [get_message_cache_value message_id state] returns the
          cached value for [message_id]. *)
      val get_value : Message_id.t -> state -> value option
    end

    type view = {
      limits : limits;
      parameters : parameters;
      connections : connections;
      ihave_per_heartbeat : int Peer.Map.t;
      iwant_per_heartbeat : int Peer.Map.t;
      mesh : Peer.Set.t Topic.Map.t;
      fanout : fanout_peers Topic.Map.t;
      seen_messages : Message_id.Set.t;
      message_cache : Message_cache.t;
      rng : Random.State.t;
      heartbeat_ticks : int64;
    }

    val view : state -> view

    (** [get_peers_in_topic_mesh topic state] returns the peers in the mesh of [topic]. *)
    val get_peers_in_topic_mesh : Topic.t -> view -> Peer.t list

    (** [get_subscribed_topics peer state] returns the set of topics
        that are subscribed by [peer] *)
    val get_subscribed_topics : Peer.t -> view -> Topic.t list

    (** [get_fanout_peers topic state] returns the fanout peers of [topic]. *)
    val get_fanout_peers : Topic.t -> view -> Peer.t list

    val connections : view -> connection Peer.Map.t

    val limits : view -> limits
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
    val empty : 'a t

    (** Push the given value into the stream. *)
    val push : 'a -> 'a t -> unit

    (** Pops the oldest value that has been pushed to the stream. In case the
        stream is empty, the function will wait until some value is pushed. *)
    val pop : 'a t -> 'a Monad.t
  end

  (** The interface for the P2P Network from the worker's point of view. It is
      made of a [Connections_handler] module. *)
  module P2P : sig
    (** Interface for [Connections_handler] module. *)
    module Connections_handler : sig
      (** A connection is defined by a [peer] and two flags [direct] and
          [outbound] to tell whether it is direct and outgoing, or not. *)
      type connection = {peer : GS.Peer.t; direct : bool; outbound : bool}

      (** A callback to run on each new connection. *)
      val on_connection : (connection -> unit) -> unit

      (** A callback to run on each disconnection. *)
      val on_diconnection : (GS.Peer.t -> unit) -> unit

      (** This function allows disconnecting the given peer. *)
      val disconnect : GS.Peer.t -> unit Monad.t
    end
  end
end

(** The interface of a gossipsub worker. *)
module type WORKER = sig
  (** The state of a gossipsub worker. *)
  type t

  (** The Gossipsub automaton of the worker. *)
  module GS : AUTOMATON

  (** [make rng limits parameters] initializes a new Gossipsub automaton with
      the given arguments. Then, it initializes and returns a worker for it. *)
  val make :
    Random.State.t ->
    (GS.Peer.t, GS.Message_id.t, GS.span) limits ->
    (GS.Peer.t, GS.Message_id.t) parameters ->
    t

  (** [start ~heartbeat_span topics state] runs the (not already started) worker
      whose [state] is given. The worker is started with the given
      [heartbeat_span] and the initial list of [topics] the caller is interested
      in. *)
  val start : heartbeat_span:GS.Span.t -> GS.Topic.t list -> t -> t

  (** [shutdown state] allows stopping the worker whose [state] is given. *)
  val shutdown : t -> unit

  (** [inject state msg_id msg topic] is used to inject a message [msg] with
      ID [msg_id] and that belongs to [topic] to the network. *)
  val inject : t -> GS.Message_id.t -> GS.Message.t -> GS.Topic.t -> unit

  (** [join t topics] joins [topics] even if the worker is running. *)
  val join : t -> GS.Topic.t list -> unit

  (** [leave t topics] leaves [topics] even if the worker is running. *)
  val leave : t -> GS.Topic.t list -> unit
end
