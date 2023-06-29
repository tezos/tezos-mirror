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

module Types : sig
  (** A Tezos level. *)
  type level = int32

  (** An index of a DAL slot header. *)
  type slot_index = int

  (** An ID associated to a slot or to its commitment. *)
  type slot_id = {slot_level : level; slot_index : slot_index}

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4562
     Use a bitset instead, when available in the standard library. *)

  (** A set of slots, represented by a list of 0s and 1s. It is used for
      instance to record which slots are deemed available by an attestor. *)
  type slot_set = bool list

  (** The set of attestable slots of an attestor (which may not necessarily be
      in the committee for a given level). *)
  type attestable_slots = Attestable_slots of slot_set | Not_in_committee

  (** An index of a DAL shard *)
  type shard_index = int

  (** The status of a header a DAL node is aware of: *)
  type header_status =
    [ `Waiting_attestation
      (** The slot header was included and applied in an L1 block but remains to
          be attested. *)
    | `Attested
      (** The slot header was included in an L1 block and attested. *)
    | `Unattested
      (** The slot header was included in an L1 block but not timely attested. *)
    | `Not_selected
      (** The slot header was included in an L1 block but was not selected as
          the slot header for that slot index. *)
    | `Unseen ]
  (** The slot header was never seen in an L1 block. For instance, this could
      happen if the RPC `PATCH /commitments/<commitment>` was called but the
      corresponding slot header was never included into a block. This means that
      the publish operation was not sent (yet) to L1, or sent but not included
      (yet) in a block). *)

  (** DAL node can track one or many profiles that correspond to various modes
      that the DAL node would operate in *)
  type profile =
    | Attestor of Tezos_crypto.Signature.public_key_hash
    | Producer of {slot_index : int}

  (** Information associated to a slot header in the RPC services of the DAL
      node. *)
  type slot_header = {
    slot_id : slot_id;
    commitment : Cryptobox.Commitment.t;
    status : header_status;
  }

  (** The [with_proof] flag is associated to shards computation. It indicates
      whether we also compute shards' proofs or not. *)
  type with_proof = {with_proof : bool}

  val slot_id_encoding : slot_id Data_encoding.t

  val header_status_encoding : header_status Data_encoding.t

  val profile_encoding : profile Data_encoding.t

  val with_proof_encoding : with_proof Data_encoding.t

  val equal_profile : profile -> profile -> bool
end

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

(** Update the list of profiles tracked by the DAL node *)
val patch_profiles :
  < meth : [`PATCH]
  ; input : Types.profile list
  ; output : unit
  ; prefix : unit
  ; params : unit
  ; query : unit >
  service

(** Return the list of current profiles tracked by the DAL node *)
val get_profiles :
  < meth : [`GET]
  ; input : unit
  ; output : Types.profile list
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
