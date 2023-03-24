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

(** A sliding window cache of published messages. The module also keeps track of
    the number of accesses to a message by a peer, thus indirectly tracking the
    number of IWANT requests a peer makes for the same message.

    The module assumes that no two messages have the same message id. However,
    the cache stores duplicates; for instance, if [add_message id msg topic] is
    called twice, then [msg] will appear (at least) twice in
    [get_message_ids_to_gossip]'s result (assuming not more than [gossip_slots]
    shifts have been executed in the meanwhile).
*)
module Make
    (Peer : Gossipsub_intf.ITERABLE)
    (Topic : Gossipsub_intf.ITERABLE)
    (Message_id : Gossipsub_intf.ITERABLE)
    (Message : Gossipsub_intf.PRINTABLE) : sig
  type t

  (** [create ~history_slots ~gossip_slots] creates a sliding window cache of
      length [history_slots].

      When queried for messages to advertise, the cache only returns messages in
      the last [gossip_slots]. The [gossip_slots] must be smaller or equal to
      [history_slots]. The slack between [gossip_slots] and [history_slots]
      accounts for the reaction time between when a message is advertised via
      IHAVE gossip, and when the peer pulls it via an IWANT command.

      @raise Assert_failure when [gossip_slots <= 0 || gossip_slots > history_slots]

      TODO: https://gitlab.com/tezos/tezos/-/issues/5129
      Error handling. *)
  val create : history_slots:int -> gossip_slots:int -> t

  (** Add message to the most recent cache slot. If the message already exists
      in the cache, the message is not overridden, instead a duplicate is
      stored. *)
  val add_message : Message_id.t -> Message.t -> Topic.t -> t -> t

  (** Get the message associated to the given message id, increase the access
      counter for the peer requesting the message, and also return the updated
      counter. *)
  val get_message_for_peer :
    Peer.t -> Message_id.t -> t -> (t * Message.t * int) option

  (** Get the message ids for the given topic in the last [gossip_slots] slots
      of the cache. If there were duplicates added in the cache, then there will
      be duplicates in the output. There is no guarantee about the order of
      messages in the output. *)
  val get_message_ids_to_gossip : Topic.t -> t -> Message_id.t list

  (** Shift the sliding window by one slot (usually corresponding to one
      heartbeat tick). *)
  val shift : t -> t
end
