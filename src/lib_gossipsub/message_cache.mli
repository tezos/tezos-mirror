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

(** A sliding window cache that stores published messages and their first seen
    time.  The module also keeps track of the number of accesses to a message by
    a peer, thus indirectly tracking the number of IWant requests a peer makes
    for the same message between two heartbeats.

    The module assumes that no two different messages have the same message
    id. However, the cache stores duplicates; for instance, if [add_message id
    msg topic] is called exactly twice, then [msg] will appear twice in
    [get_message_ids_to_gossip]'s result (assuming not more than [gossip_slots]
    shifts have been executed in the meanwhile).
*)
module Make
    (C : Gossipsub_intf.AUTOMATON_SUBCONFIG)
    (Time : Gossipsub_intf.TIME) : sig
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
  val add_message : C.Message_id.t -> C.Message.t -> C.Topic.t -> t -> t

  (** Get the message associated to the given message id, increase the access
      counter for the peer requesting the message, and also returns the updated
      counter. *)
  val get_message_for_peer :
    C.Peer.t -> C.Message_id.t -> t -> (t * C.Message.t * int) option

  (** Get the message ids for the given topic in the last [gossip_slots] slots
      of the cache. If there were duplicates added in the cache, then there will
      be duplicates in the output. There is no guarantee about the order of
      messages in the output. *)
  val get_message_ids_to_gossip : C.Topic.t -> t -> C.Message_id.t list

  (** [get_first_seen_time message_id t] returns the time the message with [message_id]
      was first seen. Returns [None] if the message was not seen during the period
      covered by the sliding window. *)
  val get_first_seen_time : C.Message_id.t -> t -> Time.t option

  (** [seen_message message_id t] returns [true] if the message was seen during the
      period covered by the sliding window and returns [false] if otherwise. *)
  val seen_message : C.Message_id.t -> t -> bool

  (** Shift the sliding window by one slot (usually corresponding to one
      heartbeat tick). *)
  val shift : t -> t

  module Internal_for_tests : sig
    val get_access_counters : t -> (C.Message_id.t * int C.Peer.Map.t) Seq.t
  end
end
