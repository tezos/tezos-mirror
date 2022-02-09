(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(** A [P2p_reader.t] is a worker that answers the application messages
   (see [Messages]) received from a remote peer on a [P2p.connection].

  The worker behavior is parameterized by callback functions passed by
  the caller (see type [callback]).

  Terminology:
  - *this peer* refers to the peer running the worker
  - the *remote peer* refers to the peer on the other side of the [connection]
  - a chain can be *active* or *inactive*. The activation status can refer to
    this peer, or to the remote peer. If not specified, active/inactive refers
    to this peer.

  A chain can only be considered active for the remote peer if it is active
  for this peer. As a general rule, received messages that refer to a
  specific [chain_id] that is not active are ignored.

  The protocol is defined as a message handling loop. We can classify a
  message in three categories. For each category, we describe the worker
  behavior when receiving the messages.

  1. Administrative message

  - [Deactivate chain_id]
    marks chain [chain_id] as inactive for the remote peer. Call
    [callback.disconnection gid] where [gid] is the remote peer id.

  2. Chain-related messages

  These messages are used for peers to get the most recent view of the chains
  they manage.

  - [Get_current_branch chain_id]
    If [chain_id] is not active for this peer, simply ignores message
    and returns.

    Only if [chain_id] is not active for the remote peer, sends a
    [Get_current_branch chain_id] message.

    Then (in any case) sends a [Current_branch (chain_id, chain_locator)]
    message.

  - [Current_branch (chain_id, locator)]
    activates [chain_id] for the remote peer if it is not active yet.
    If the locator contains any block known to be invalid, the connection is
    closed and the remote peer is greylisted.
    If the locator head timestamp is more than 15s ahead of this peer
    system time, the message is ignored.
    Otherwise calls [callback.notify_branch locator].

  - [Get_protocol_branch chain_id proto_level]
    If [chain_id] is not active for this peer, or if [proto_level] is
    unknown simply ignores message and returns. A [proto_level] is unknown
    if it is non-positive or if it is strictly higher than the one of the
    current head.

    Otherwise, sends a message [Protocol_branch (chain_id, protocol_locator)]
    where [protocol_locator] encodes the longest branch where all the blocks
    are on [proto_level]. This branch is a subbranch of the current branch
    for the requested [chain_id].

  - [Protocol_branch chain_id proto_level locator] is a no-op

  - [Get_current_head chain_id]:
    message is ignored if the chain [chain_id] is inactive for the remote peer.
    Otherwise, replies with [Current_head (chain_id, head, mempool)] where
    [head] is the current head for the requested chain. [mempool] is the current
    mempool, or an empty mempool if the remote peer's mempool is disabled.

  - [Current_head (chain_id, header, mempool)]:
    message is ignored if the chain [chain_id] is inactive for the remote peer.
    If [header] is known to be invalid, or if the [mempool] is non empty while
    this peer's mempool is disabled, the connection is closed and the remote
    peer is greylisted.
    If [header]'s timestamp is more than 15s ahead of this peer system time,
    the message is ignored.
    Otherwise, calls [callback.notify_head].

  - [Get_checkpoint chain_id]:
    message is ignored if the chain [chain_id] is inactive for the remote peer.
    Otherwise, replies with [Checkpoint (chain_id, checkpoint)] where [checkpoint]
    is the current checkpoint for the requested chain.

  - [Checkpoint chain_id header] is a no-op

  3. "Database" messages

  These messages are used for peers to exchange "static" resources such
  as block headers, operations, protocols. The worker collaborates with
  the requesters defined in [Distributed_db]. Resources are requested by the
  requester, and the requester is notified by a [P2p_reader] upon reception of
  the resource.

  - [Get_block_headers hashes]
    Sends a [Blocker_header header] for each of the requested hash known to this
    peer.

  - [Block_header block]
    notifies the requester that the value has been received. Ignores message if
    value wasn't requested.

  - [Get_predecessor_header hash n]
    Sends [Predecessor_header hash n header] where [header] is the header of the
    [n]th predecessor of the block [hash].

  - [Predecessor_header hash n header] is a no-op.

  The other database messages work similarly
  - [Get_operations hashes]/[Operation operation]
  - [Get_protocols hashes]/[Protocol]
  - [Get_operation_hashes_for_blocks]/[Operation_hashes_for_block]
  - [Get_operations_for_blocks]/[Operation_for_block]
  *)

type t

module Message = Distributed_db_message

module Block_hash_cache : Ringo.CACHE_MAP with type key = Block_hash.t

type p2p = (Message.t, Peer_metadata.t, Connection_metadata.t) P2p.net

type connection =
  (Message.t, Peer_metadata.t, Connection_metadata.t) P2p.connection

type callback = {
  notify_branch : P2p_peer.Id.t -> Block_locator.t -> unit;
      (** callback function called on reception of a [Current_branch] message *)
  notify_head :
    P2p_peer.Id.t -> Block_hash.t -> Block_header.t -> Mempool.t -> unit;
      (** callback function called on reception of a [Current_head] message *)
  disconnection : P2p_peer.Id.t -> unit;
}

type chain_db = {
  chain_store : Store.Chain.t;
  operation_db : Distributed_db_requester.Raw_operation.t;
  block_header_db : Distributed_db_requester.Raw_block_header.t;
  operations_db : Distributed_db_requester.Raw_operations.t;
  callback : callback;
  active_peers : P2p_peer.Set.t ref;
      (** Set of remote peers for which this chain is active. *)
  active_connections : connection P2p_peer.Table.t;
}

(** Lookup for block header in any active chains *)
val read_block_header :
  t -> Block_hash.t -> (Chain_id.t * Block_header.t) option Lwt.t

(** [run ~register ~unregister p2p state protocol_db active_chains peer_id conn]
    runs an answering worker on a p2p connection [connection]. [peer_id] is
    the peer id of the remote peer. [register] is called once the worker is
    created, and [unregister] when the worker stops.

    [active_chains] is the table of active chains (i.e. test chain,
     main chain...) *)
val run :
  register:(t -> unit) ->
  unregister:(unit -> unit) ->
  p2p ->
  Store.t ->
  Distributed_db_requester.Raw_protocol.t ->
  chain_db Chain_id.Table.t ->
  P2p_peer.Id.t ->
  connection ->
  unit

val shutdown : t -> unit Lwt.t
