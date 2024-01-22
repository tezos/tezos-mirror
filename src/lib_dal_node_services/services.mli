(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** This module provides different services related to DAL slots. *)

(* We cannot include a raw mli file. But this will be removed once full
   migration is done. *)
include module type of Services_legacy

open Tezos_crypto_dal

type 'rpc service =
  ('meth, 'prefix, 'params, 'query, 'input, 'output) Tezos_rpc.Service.service
  constraint
    'rpc =
    < meth : 'meth
    ; prefix : 'prefix
    ; params : 'params
    ; query : 'query
    ; input : 'input
    ; output : 'output >

(** Add the given slot in the node if not already present. The corresponding
    commitment is returned. See {!val:
    Slot_manager.add_commitment} for more details. *)
val post_commitment :
  < meth : [`POST]
  ; input : Cryptobox.slot
  ; output : Cryptobox.commitment
  ; prefix : unit
  ; params : unit
  ; query : unit >
  service

(** Associate a commitment to a level and a slot index. See {!val:
    Slot_manager.associate_slot_id_with_commitment} for more details. *)
val patch_commitment :
  < meth : [`PATCH]
  ; input : Types.slot_id
  ; output : unit
  ; prefix : unit
  ; params : unit * Cryptobox.commitment
  ; query : unit >
  service

(** Retrieve the content of the slot associated with the given commitment. *)
val get_commitment_slot :
  < meth : [`GET]
  ; input : unit
  ; output : Cryptobox.slot
  ; prefix : unit
  ; params : unit * Cryptobox.commitment
  ; query : unit >
  service

(** Compute the proof associated to a commitment. *)
val get_commitment_proof :
  < meth : [`GET]
  ; input : unit
  ; output : Cryptobox.commitment_proof
  ; prefix : unit
  ; params : unit * Cryptobox.commitment
  ; query : unit >
  service

(** Compute and save the shards of the slot associated to the given
    commitment. If the input's flag is true, the proofs associated with the
    computed shards are also computed and stored in memory. *)
val put_commitment_shards :
  < meth : [`PUT]
  ; input : Types.with_proof
  ; output : unit
  ; prefix : unit
  ; params : unit * Cryptobox.commitment
  ; query : unit >
  service

(** Return the accepted commitment associated to the given slot index and
    published at the given level, if any. *)
val get_commitment_by_published_level_and_index :
  < meth : [`GET]
  ; input : unit
  ; output : Cryptobox.commitment
  ; prefix : unit
  ; params : (unit * Types.level) * Types.slot_index
  ; query : unit >
  service

(** Return the known headers for the slot whose commitment is given. *)
val get_commitment_headers :
  < meth : [`GET]
  ; input : unit
  ; output : Types.slot_header list
  ; prefix : unit
  ; params : unit * Cryptobox.commitment
  ; query : Types.level option * Types.slot_index option >
  service

(** Return the known slot headers for the given published level. *)
val get_published_level_headers :
  < meth : [`GET]
  ; input : unit
  ; output : Types.slot_header list
  ; prefix : unit
  ; params : unit * Types.level
  ; query : Types.header_status option >
  service

(** Update the list of profiles tracked by the DAL node.
    Note that it does not take the bootstrap profile as it
    is incompatible with other profiles. *)
val patch_profiles :
  < meth : [`PATCH]
  ; input : Types.operator_profiles
  ; output : unit
  ; prefix : unit
  ; params : unit
  ; query : unit >
  service

(** Return the list of current profiles tracked by the DAL node *)
val get_profiles :
  < meth : [`GET]
  ; input : unit
  ; output : Types.profiles
  ; prefix : unit
  ; params : unit
  ; query : unit >
  service

(** Return the shard indexes assigned to the given public key hash at the given level. *)
val get_assigned_shard_indices :
  < meth : [`GET]
  ; input : unit
  ; output : Types.shard_index list
  ; prefix : unit
  ; params : (unit * Tezos_crypto.Signature.public_key_hash) * Types.level
  ; query : unit >
  service

(** Return the set of currently attestable slots. A slot is attestable at level
    [l] if it is published at level [l - attestation_lag] and *all* the shards
    assigned at level [l] to the given public key hash are available in the DAL
    node's store. *)
val get_attestable_slots :
  < meth : [`GET]
  ; input : unit
  ; output : Types.attestable_slots
  ; prefix : unit
  ; params : (unit * Tezos_crypto.Signature.public_key_hash) * Types.level
  ; query : unit >
  service

(** A service for monitor_shards RPC *)
val monitor_shards :
  < meth : [`GET]
  ; input : unit
  ; output : Cryptobox.Commitment.t
  ; prefix : unit
  ; params : unit
  ; query : unit >
  service

val version :
  < meth : [`GET]
  ; input : unit
  ; output : Types.Version.t
  ; prefix : unit
  ; params : unit
  ; query : unit >
  service

module P2P : sig
  (** A service to initiate a connection with another point. *)
  val post_connect :
    < meth : [`POST]
    ; input : P2p_point.Id.t
    ; output : unit
    ; prefix : unit
    ; params : unit
    ; query : < timeout : Ptime.Span.t option > >
    service

  val delete_disconnect_point :
    < meth : [`DELETE]
    ; input : unit
    ; output : unit
    ; prefix : unit
    ; params : unit * P2p_point.Id.t
    ; query : < wait : bool > >
    service

  val delete_disconnect_peer :
    < meth : [`DELETE]
    ; input : unit
    ; output : unit
    ; prefix : unit
    ; params : unit * P2p_peer.Id.t
    ; query : < wait : bool > >
    service

  val get_points :
    < meth : [`GET]
    ; input : unit
    ; output : P2p_point.Id.t list
    ; prefix : unit
    ; params : unit
    ; query : < connected : bool > >
    service

  val get_points_info :
    < meth : [`GET]
    ; input : unit
    ; output : (P2p_point.Id.t * P2p_point.Info.t) list
    ; prefix : unit
    ; params : unit
    ; query : < connected : bool > >
    service

  module Points : sig
    val get_point_info :
      < meth : [`GET]
      ; input : unit
      ; output : P2p_point.Info.t
      ; prefix : unit
      ; params : unit * P2p_point.Id.t
      ; query : unit >
      service
  end

  val get_peers :
    < meth : [`GET]
    ; input : unit
    ; output : P2p_peer.Id.t list
    ; prefix : unit
    ; params : unit
    ; query : < connected : bool > >
    service

  val get_peers_info :
    < meth : [`GET]
    ; input : unit
    ; output : (P2p_peer.Id.t * Types.P2P.Peer.Info.t) list
    ; prefix : unit
    ; params : unit
    ; query : < connected : bool > >
    service

  module Peers : sig
    val get_peer_info :
      < meth : [`GET]
      ; input : unit
      ; output : Types.P2P.Peer.Info.t
      ; prefix : unit
      ; params : unit * P2p_peer.Id.t
      ; query : unit >
      service
  end

  module Gossipsub : sig
    val get_topics :
      < meth : [`GET]
      ; input : unit
      ; output : Types.Topic.t list
      ; prefix : unit
      ; params : unit
      ; query : unit >
      service

    val get_topics_peers :
      < meth : [`GET]
      ; input : unit
      ; output : (Types.Topic.t * Types.Peer.t list) list
      ; prefix : unit
      ; params : unit
      ; query : < subscribed : bool > >
      service

    val get_connections :
      < meth : [`GET]
      ; input : unit
      ; output : (Types.Peer.t * Types.Gossipsub.connection) list
      ; prefix : unit
      ; params : unit
      ; query : unit >
      service

    val get_scores :
      < meth : [`GET]
      ; input : unit
      ; output : (Types.Peer.t * Types.Score.t) list
      ; prefix : unit
      ; params : unit
      ; query : unit >
      service

    val get_backoffs :
      < meth : [`GET]
      ; input : unit
      ; output : (Types.Topic.t * (Types.Peer.t * Types.Time.t) list) list
      ; prefix : unit
      ; params : unit
      ; query : unit >
      service

    val get_message_cache :
      < meth : [`GET]
      ; input : unit
      ; output : (int64 * (Types.Topic.t * int) list) list
      ; prefix : unit
      ; params : unit
      ; query : unit >
      service
  end
end
