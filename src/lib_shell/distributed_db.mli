(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

(** Tezos Shell - High-level API for the Gossip network and local storage.

    It provides functions to query *static* resources such as blocks headers,
    operations, and functions to access dynamic resources such as heads and
    chains.

    Several chains (mainchain, testchain, ...) can be managed independently.
    First a chain is activated using [activate], which provides a
    [chain_db] from which it is possible to access resources.
    Eventually the chain is deactivated using [deactivate].

    Static resources are accessible via "Requester" modules ([Block_header],
    [Operation], [Operations], [Protocol]). These modules act as read-through
    caches in front of the local storage [State] and the p2p layer. They
    centralize concurrent requests, and cache results in memory. They don't
    update [State] directly.

    For instance, from a block_header hash, one can fetch the actual block
    header using [Block_header.fetch], then the block operations with
    [Operations.fetch].
    *)

module Message = Distributed_db_message

type t

type db = t

type p2p = (Message.t, Peer_metadata.t, Connection_metadata.t) P2p.net

val create : Store.t -> p2p -> t

val store : db -> Store.t

val shutdown : t -> unit Lwt.t

(** {1 Network database} *)

(** An instance of the distributed DB for a given chain *)
type chain_db

(** The first call to [activate t chain callbacks] activates [chain],
   creates a [chain_db] and sends a [Get_current_branch chain_id]
   message to all neighbors, where [chain_id] is the identifier of
   [chain]. This informs the neighbors that this node expects
   notifications for new heads/mempools. The given [callbacks] are
   given to the [P2p_reader] for each [peer]:

    - [notify_branch peer locator] is called when the [P2p_reader]
   receives the message [Current_branch (chain, locator)] from peer
   [peer].

    - [notify_head peer head] is called when the [P2p_reader] receives
   the message [Current_head (chain, head, mempool)] from peer [peer].

    - [Disconnection peer] is called when the [P2p_reader] receives
   the message [Deactivate chain] from peer [peer] or when the
   [P2p_reader] associated to [peer] is shutdown.

    Subsequent calls simply return the existing [chain_db]. *)
val activate : t -> Store.Chain.t -> P2p_reader.callback -> chain_db

(** Look for the database of an active chain. *)
val get_chain : t -> Chain_id.t -> chain_db option

(** [deactivate chain_db] sends a [Deactivate chain_id] message to all active
    neighbors for this chain. This notifies them that this node isn't interested
    in messages for this chain *)
val deactivate : chain_db -> unit Lwt.t

(** Kick a given peer. *)
val disconnect : reason:string -> chain_db -> P2p_peer.Id.t -> unit Lwt.t

(** Greylist a given peer. *)
val greylist : chain_db -> P2p_peer.Id.t -> unit Lwt.t

(** Various accessors. *)

val chain_store : chain_db -> Store.chain_store

val db : chain_db -> db

val information :
  chain_db -> Chain_validator_worker_state.Distributed_db_state.view

(** Return the peer id of the node *)
val my_peer_id : chain_db -> P2p_peer.Id.t

val get_peer_metadata : chain_db -> P2p_peer.Id.t -> Peer_metadata.t

(** {1 Sending messages} *)

module Request : sig
  (** [current_branch chain_db peer] sends a [Get_current_branch
      chain_id] message to [peer]. [chain_id] is the identifier for
      [chain_db]. Expected answer is a [Current_branch] message. *)
  val current_branch : chain_db -> P2p_peer.Id.t -> unit

  (** [current_head_from_peer chain_db peer] sends a [Get_Current_head
      chain_id] to a given peer. Expected answer is a
      [Get_current_head] message. *)
  val current_head_from_peer : chain_db -> P2p_peer.Id.t -> unit

  (** [current_head_from_all chain_db] sends a [Get_Current_head
      chain_id] message to all known active peers for this chain.
      [chain_id] is the identifier for [chain_db]. Expected answer is
      a [Get_current_head] message *)
  val current_head_from_all : chain_db -> unit
end

module Advertise : sig
  (** [current_head chain_db ?mempool head] sends a
      [Current_head (chain_id, head_header, mempool)] message to all known
      active peers for this chain. If [mempool] isn't specified, or if
      remote peer has disabled its mempool, [mempool] is empty. [chain_id] is
      the identifier for this [chain_db]. *)
  val current_head : chain_db -> ?mempool:Mempool.t -> Store.Block.t -> unit

  (** [prechecked_head chain_db ?mempool head] sends a [Current_head
     (chain_id, head_header, mempool)] message to all known active
     peers for this chain. If [mempool] isn't specified, or if remote
     peer has disabled its mempool, [mempool] is empty. [chain_id] is
     the identifier for this [chain_db].

      The message is only sent when the peer's network version is at
     least 2. For former protocol versions, we could get kicked kicked
     for having send an invalid block that passes the precheck. *)
  val prechecked_head : chain_db -> ?mempool:Mempool.t -> Block_header.t -> unit

  (** [current_branch chain_db] sends a
      [Current_branch (chain_id, locator)] message to all known active peers
      for this chain. [locator] is constructed based on the seed
      [(remote_peer_id, this_peer_id)]. *)
  val current_branch : chain_db -> unit Lwt.t
end

(** {2 Block index} *)

(** Index of block headers. *)
module Block_header : sig
  type t = Block_header.t (* avoid shadowing. *)

  include
    Requester.REQUESTER
      with type t := chain_db
       and type key := Block_hash.t
       and type value := Block_header.t
       and type param := unit
end

(** Index of all the operations of a given block (per validation pass).

    For instance, [fetch chain_db (block_hash, validation_pass)
    operation_list_list_hash] queries the operation requester to get all the
    operations for block [block_hash] and validation pass [validation_pass].
    It returns a list of operation, guaranteed to be valid with respect to
    [operation_list_list_hash] (root of merkle tree for this block). *)
module Operations :
  Requester.REQUESTER
    with type t := chain_db
     and type key = Block_hash.t * int
     and type value = Operation.t list
     and type param := Operation_list_list_hash.t

(** Store on disk all the data associated to a valid block. *)
val commit_block :
  chain_db ->
  Block_hash.t ->
  Block_header.t ->
  Operation.t list list ->
  Block_validation.result ->
  Store.Block.t option tzresult Lwt.t

(** Store on disk all the data associated to an invalid block. *)
val commit_invalid_block :
  chain_db ->
  Block_hash.t ->
  Block_header.t ->
  Error_monad.error list ->
  unit tzresult Lwt.t

(** {2 Operations index} *)

(** Inject a new operation in the local index (memory only). *)
val inject_operation : chain_db -> Operation_hash.t -> Operation.t -> bool Lwt.t

(** Inject a prechecked block in the [precheck_blocks] memory table.
   This is to ensure the data availability of the operations once the
   block has been prechecked and advertised to our peers.

   No need to remove the data explicitely of the [precheck_blocks]
   memory table. The table is handled as an LRU cache. *)
val inject_prechecked_block :
  chain_db ->
  Block_hash.t ->
  Block_header.t ->
  Operation.t trace trace ->
  unit tzresult Lwt.t

(** Index of operations (for the mempool). *)
module Operation :
  Requester.REQUESTER
    with type t := chain_db
     and type key := Operation_hash.t
     and type value := Operation.t
     and type param := unit

(** {2 Protocol index} *)

(** Index of protocol sources. *)
module Protocol : sig
  type t = Protocol.t (* avoid shadowing. *)

  include
    Requester.REQUESTER
      with type t := db
       and type key := Protocol_hash.t
       and type value := Protocol.t
       and type param := unit
end

(** Store on disk protocol sources. *)
val commit_protocol : db -> Protocol_hash.t -> Protocol.t -> bool tzresult Lwt.t
