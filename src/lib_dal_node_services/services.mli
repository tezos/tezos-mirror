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

(* This RPC aims to be used by a user to check whether its DAL node
   behaves as expected. It does not aim to provide a clear diagnostic,
   this is left to another RPC (not implemented yet). *)
val health :
  < meth : [`GET]
  ; input : unit
  ; output : Types.Health.t
  ; prefix : unit
  ; params : unit
  ; query : unit >
  service

(** This RPC should be used by a slot producer. It allows to produce a
    commitment, a commitment proof and the shards from a slot. A
    padding is added if the slot is not of the expected size
    ([slot_size] from the Cryptobox). *)
val post_slot :
  < meth : [`POST]
  ; input : string
  ; output : Cryptobox.commitment * Cryptobox.commitment_proof
  ; prefix : unit
  ; params : unit
  ; query : < padding : char ; slot_index : Types.slot_index option > >
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

(** Retrieve the content of the slot associated with the given slot id. *)
val get_slot_content :
  < meth : [`GET]
  ; input : unit
  ; output : Cryptobox.slot
  ; prefix : unit
  ; params : (unit * Types.level) * Types.slot_index
  ; query : unit >
  service

(** Returns the pages of the slot identified by the given slot id. *)
val get_slot_pages :
  < meth : [`GET]
  ; input : unit
  ; output : Cryptobox.page list
  ; prefix : unit
  ; params : (unit * Types.level) * Types.slot_index
  ; query : unit >
  service

(** Compute the proof associated to the page whose index is given of the given
    slot. *)
val get_slot_page_proof :
  < meth : [`GET]
  ; input : unit
  ; output : Cryptobox.page_proof
  ; prefix : unit
  ; params : ((unit * Types.level) * Types.slot_index) * Types.page_index
  ; query : unit >
  service

(** Return the accepted commitment associated to the given slot index and
    published at the given level, if any. *)
val get_slot_commitment :
  < meth : [`GET]
  ; input : unit
  ; output : Cryptobox.commitment
  ; prefix : unit
  ; params : (unit * Types.level) * Types.slot_index
  ; query : unit >
  service

(** Return the status for the given slot. *)
val get_slot_status :
  < meth : [`GET]
  ; input : unit
  ; output : Types.header_status
  ; prefix : unit
  ; params : (unit * Types.level) * Types.slot_index
  ; query : unit >
  service

(** Update the list of profiles tracked by the DAL node.
    Note that it does not take the bootstrap profile as it
    is incompatible with other profiles. *)
val patch_profiles :
  < meth : [`PATCH]
  ; input : Operator_profile.t
  ; output : unit
  ; prefix : unit
  ; params : unit
  ; query : unit >
  service

(** Return the list of current profiles tracked by the DAL node *)
val get_profiles :
  < meth : [`GET]
  ; input : unit
  ; output : Types.profile
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
  ; params : (unit * Signature.public_key_hash) * Types.level
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
  ; params : (unit * Signature.public_key_hash) * Types.level
  ; query : unit >
  service

(** Return the shard associated to the given index. *)
val get_slot_shard :
  < meth : [`GET]
  ; input : unit
  ; output : Cryptobox.shard
  ; prefix : unit
  ; params : ((unit * Types.level) * Types.slot_index) * int
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

  module Points : sig
    val delete_disconnect_point :
      < meth : [`DELETE]
      ; input : unit
      ; output : unit
      ; prefix : unit
      ; params : unit * P2p_point.Id.t
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

    val get_point_info :
      < meth : [`GET]
      ; input : unit
      ; output : P2p_point.Info.t
      ; prefix : unit
      ; params : unit * P2p_point.Id.t
      ; query : unit >
      service
  end

  module Peers : sig
    val delete_disconnect_peer :
      < meth : [`DELETE]
      ; input : unit
      ; output : unit
      ; prefix : unit
      ; params : unit * P2p_peer.Id.t
      ; query : < wait : bool > >
      service

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

    val get_peer_info :
      < meth : [`GET]
      ; input : unit
      ; output : Types.P2P.Peer.Info.t
      ; prefix : unit
      ; params : unit * P2p_peer.Id.t
      ; query : unit >
      service

    val patch_peer :
      < meth : [`PATCH]
      ; input : [`Ban | `Trust | `Open] option
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

    val get_slot_indexes_peers :
      < meth : [`GET]
      ; input : unit
      ; output : (Types.slot_index * Types.Peer.t list) list
      ; prefix : unit
      ; params : unit
      ; query : < subscribed : bool > >
      service

    val get_pkhs_peers :
      < meth : [`GET]
      ; input : unit
      ; output : (Signature.public_key_hash * Types.Peer.t list) list
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
