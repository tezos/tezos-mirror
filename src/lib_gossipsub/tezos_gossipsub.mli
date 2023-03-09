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

module type ITERABLE = sig
  type t

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t
end

module type CONFIGURATION = sig
  module Peer : ITERABLE

  module Topic : ITERABLE

  module Message_id : ITERABLE

  module Message : sig
    type t
  end

  module Time : sig
    include Compare.S

    type span

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

  val penality : t -> int -> t
end

module type S = sig
  (** Module for peer *)
  module Peer : ITERABLE

  (** Module for topic *)
  module Topic : ITERABLE

  (** Module for message_id *)
  module Message_id : ITERABLE

  (** Type for message *)
  type message

  (** Type for time *)
  type time

  (** Type for time duration *)
  type span

  (** The state managed by the gossipsub automaton. The state is
      purely functional. *)
  type state

  (** Limits of the gossipsub protocol. *)
  type limits := (Peer.t, Message_id.t, span) limits

  (** Parameters of the gossipsub protocol. *)
  type parameters := (Peer.t, Message_id.t) parameters

  (** Output produced by one of the actions below. *)
  type 'a output

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
    with type time = C.Time.t
     and type span = C.Time.span
     and module Peer = C.Peer
     and module Topic = C.Topic
     and module Message_id = C.Message_id
     and type message = C.Message.t
