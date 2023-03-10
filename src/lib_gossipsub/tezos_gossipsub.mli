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

module type CONFIGURATION = sig
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
  end
end

type ('peer, 'message_id, 'span) limits = {
  max_recv_ihave_per_heartbeat : int;
      (** The maximum number of control message [IHave] we can receive
      from our peers between two heartbeats. *)
  max_sent_iwant_per_heartbeat : int;
      (** The maximum number of control messages [IWant] we can sent
          to our peers between two heartbeats. *)
  expected_peers_per_topic : int;
      (** The number of expected full connections per topic. *)
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
      (** The duration added to remove metadata
                               about a disconnected peer. *)
}

type ('peer, 'message_id) parameters = {
  peer_filter :
    'peer -> [`IHave of 'message_id | `IWant of 'message_id | `Graft] -> bool;
}

module Score : sig
  type t

  val float : t -> float

  val zero : t

  val penalty : t -> int -> t
end

module type S = sig
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

  type message = Message.t

  type span = Span.t

  (** The state managed by the gossipsub automaton. The state is
      purely functional. *)
  type state

  (** Limits of the gossipsub protocol. *)
  type limits := (Peer.t, Message_id.t, span) limits

  (** Parameters of the gossipsub protocol. *)
  type parameters := (Peer.t, Message_id.t) parameters

  (** Output produced by one of the actions below. *)
  type _ output =
    | Negative_peer_score : Score.t -> [`IHave] output
    | Too_many_recv_ihave_messages : {count : int; max : int} -> [`IHave] output
    | Too_many_sent_iwant_messages : {count : int; max : int} -> [`IHave] output
    | Message_topic_not_tracked : [`IHave] output
    | Message_requested_message_ids : Message_id.t list -> [`IHave] output
    | On_iwant_messages_to_route : {
        peer : Peer.t;
        routed_message_ids :
          [`Ignored | `Not_found | `Message of message] Message_id.Map.t;
      }
        -> [`IWant] output
    | Peer_filtered : [`Graft] output
    | Unknown_topic : [`Graft] output
    | Peer_already_in_mesh : [`Graft] output
    | Grafting_direct_peer : [`Graft] output
    | Unexpected_grafting_peer : [`Graft] output
    | Grafting_peer_with_negative_score : [`Graft] output
    | Grafting_successfully : [`Graft] output
    | Peer_backed_off : [`Graft] output
    | No_peer_in_mesh : [`Prune] output
    | Ignore_PX_score_too_low : Score.t -> [`Prune] output
    | No_PX : [`Prune] output
    | PX : Peer.Set.t -> [`Prune] output
    | Publish_message : Peer.Set.t -> [`Publish] output
    | Already_subscribed : [`Join] output
    | Joining_topic : Peer.Set.t -> [`Join] output
    | Not_subscribed : [`Leave] output
    | Leaving_topic : {to_prune : Peer.Set.t} -> [`Leave] output
    | Peer_added : [`Add_peer] output
    | Peer_already_known : [`Add_peer] output
    | Removing_peer : [`Remove_peer] output

  (** A type alias for the state monad. *)
  type 'a monad := state -> state * 'a output

  (** Initialise a state. *)
  val make : Random.State.t -> limits -> parameters -> state

  (** [add_peer ~direct ~outbound peer] is called to notify a new
      connection. If [direct] is [true], the gossipsub always
      forward messages to those peers. [outbound] is [true] if it is
      an outbound connection. *)
  val add_peer : direct:bool -> outbound:bool -> Peer.t -> [`Add_peer] monad

  (** [remove_peer peer] notifies gossipsub that we are disconnected
      from a peer. Do note that the [state] still maintain information
      for this connection for [retain_duration] seconds. *)
  val remove_peer : Peer.t -> [`Remove_peer] monad

  (** [handle_ihave peer topic message_ids] handles the gossip message
      [IHave] emitted by [peer] for [topic] with the [message_ids].  *)
  val handle_ihave : Peer.t -> Topic.t -> Message_id.t list -> [`IHave] monad

  (** [handle_iwant peer message_ids] handles the gossip message
      [IWant] emitted by [peer] for [topic] with the [message_ids]. *)
  val handle_iwant : Peer.t -> Message_id.t list -> [`IWant] monad

  (** [handle_graft peer topic] handles the gossip message [Graft]
      emitted by [peer] for [topic]. This action allows to graft a
      connection to a full connection allowing the transmission of
      full messages for the given topic. *)
  val handle_graft : Peer.t -> Topic.t -> [`Graft] monad

  (** [handle_prune peer topic ~px ~backoff] handles the gossip
      message [Prune] emitted by [peer] for [topic]. This action
      allows to prune a full connection. In that case, the remote peer
      can send a list of peers to connect to as well as a backoff
      time, which is a duration for which we cannot [Graft] this peer
      on this topic. *)
  val handle_prune :
    Peer.t -> Topic.t -> px:Peer.t Seq.t -> backoff:span -> [`Prune] monad

  (** [publish ~sender topic message_id message] allows to route a
      message on the gossip network. If [sender=None], the message
      comes from the application layer and we are the sender. *)
  val publish :
    sender:Peer.t option ->
    Topic.t ->
    Message_id.t ->
    message ->
    [`Publish] monad

  (** [heartbeat] executes the heartbeat routine of the algorithm. *)
  val heartbeat : [`Heartbeat] monad

  (** [join topic] join/subscribe to a new topic. *)
  val join : Topic.t -> [`Join] monad

  (** [leave topic] leave/unscribe a topic. *)
  val leave : Topic.t -> [`Leave] monad
end

module Make (C : CONFIGURATION) :
  S
    with type Time.t = C.Time.t
     and type Span.t = C.Time.span
     and module Peer = C.Peer
     and module Topic = C.Topic
     and module Message_id = C.Message_id
     and module Message = C.Message
