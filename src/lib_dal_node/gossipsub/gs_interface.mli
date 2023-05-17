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

(** This module defines the relevant data structures to instantiate the
    gossipsub worker. *)

(** A topic is defined by a public key hash of an attestor and a slot index.
    - A slot producer tracks the topic associated to a given slot index for all
    the public key-hashes;
    - The attestor tracks its own public key hash for all the slot indices;
    - A slot consumer tracks topics associated to a given slot index and enough
    public key-hashes so that the number of covered shards is enough to recover
    the slot data. *)
type topic = {slot_index : int; pkh : Signature.Public_key_hash.t}

(** A message id uniquely identifies a share whose commitment is included in an
    L1 block. It is defined by a tuple containing the commitment, the level at
    which the commitment is successfully included in an L1 block, the
    corresponding slot index, the shard index, as well as the public key hash
    [pkh] of the delegate expected to attest it.

    Note that [pkh] is used to be able to directly infer a topic from a message id. It
    could be retrieved from L1 using the level. But, we decide to provide it
    directly in this first version. *)
type message_id = {
  commitment : Cryptobox.Commitment.t;
  level : int32;
  slot_index : int;
  shard_index : int;
  pkh : Signature.Public_key_hash.t;
}

(** A message is a portion of an encoded slot's data. It's basically a shard
    without the corresponding index. The proof that the corresponding shard
    belong to the commitment (part of the message id) is also part of the
    message. *)
type message = {share : Cryptobox.share; shard_proof : Cryptobox.shard_proof}

(** From the Gossipsub point of view, a peer is given by a cryptographic node
    identity {!P2p_peer.Id.t}. It's up to the caller to associate the
    {!P2p_peer.Id.t} to a {!P2p_point.Id.t} if needed (to e.g. implement peers
    exchange, which needs addresses and ports instead of cryptographic
    identities). *)
type peer = P2p_peer.Id.t

module Span : Gossipsub_intf.SPAN

(** Encodings for various types above. *)

val topic_encoding : topic Data_encoding.t

val message_id_encoding : message_id Data_encoding.t

val message_encoding : message Data_encoding.t

val span_encoding : Span.t Data_encoding.t

module Monad : sig
  type 'a t = 'a Lwt.t

  val return : 'a -> 'a t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val sleep : Span.t -> unit t
end

module Worker_config :
  Gossipsub_intf.WORKER_CONFIGURATION
    with type GS.Topic.t = topic
     and type GS.Message_id.t = message_id
     and type GS.Message.t = message
     and type GS.Peer.t = peer
     and module GS.Span = Span
     and module Monad = Monad

module Worker_instance :
  Gossipsub_intf.WORKER
    with type GS.Topic.t = topic
     and type GS.Message_id.t = message_id
     and type GS.Message.t = message
     and type GS.Peer.t = peer
     and module GS.Span = Span
     and module Monad = Monad

module Validate_message_hook : sig
  val set : (message -> message_id -> [`Invalid | `Unknown | `Valid]) -> unit
end
