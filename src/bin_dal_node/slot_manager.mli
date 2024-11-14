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

(** This module provides different handlers related to DAL slots. *)

(**
   Functions to manage slots storage.

   - writing a slot means splitting it in shards and store them on disk
   - reading a slot means rebuild it from the shards
   *)

(** FIXME: https://gitlab.com/tezos/tezos/-/issues/4099
    DAL/Node: make slot_header/commitment definition consistent with
    alpha_context.mli *)

type slot = bytes

(** [get_slot_pages ~reconstruct_if_missing cryptobox node_ctxt store
    slot_id] fetches from the store the slot corresponding to the
    given slot id and split it into pages. If the slot is not found
    in the store and [reconstruct_if_missing] is true, the slot is
    reconstructed from the stored shards.

    Returns an [Error _] if:
    - the slot is not found in the store and [reconstruct_if_missing] is false,
    - the slot is not found in the store, [reconstruct_if_missing] is
      true, and too few shards are stored to reconstruct the slot,
    - the length of the slot associated to the [Cryptobox.commitment]
      is ill-formed. Specifically, when its length is not a multiple of
      the page-size specified in the [Cryptobox.parameters] argument. *)
val get_slot_pages :
  reconstruct_if_missing:bool ->
  Node_context.t ->
  Types.slot_id ->
  (bytes list, [> Errors.not_found | Errors.other]) result Lwt.t

(* Same as [Cryptobox.polynomial_from_shards] but wraps the error. *)
val polynomial_from_shards :
  Cryptobox.t ->
  Cryptobox.shard Seq.t ->
  (Cryptobox.polynomial, [> Errors.other]) result Lwt.t

type error +=
  | Invalid_slot_size of {provided : int; expected : int}
  | No_prover_SRS

(** [Cryptobox.polynomial_from_slot] but using the [Errors] module.

    The function returns an error {!ref:Invalid_slot_size} if the
    [slot]'s size doesn't match the expected slots' size given in
    [cryptobox], or the [slot]'s polynomial otherwise. *)
val polynomial_from_slot :
  Cryptobox.t ->
  Cryptobox.slot ->
  (Cryptobox.polynomial, [> Errors.other]) result

(** [commit cryptobox polynomial] computes the commitment of the given
    [polynomial].
*)
val commit :
  Cryptobox.t ->
  Cryptobox.polynomial ->
  (Cryptobox.commitment, [> Errors.other]) result

(** [get_slot_content ~reconstruct_if_missing node_ctxt slot_id] returns the
    slot content associated with the given [slot_id] in the node's store.

    If the slot is not found in the store and [reconstruct_if_missing]
    is true, the slot is reconstructed from the stored shards.

    In addition to decoding errors, the function returns [`Not_found]
    if there is no slot content for [slot_id] in the node's store or if
    [reconstruct_if_missing] is true and not enough shards are stored
    to reconstruct the slot.
*)
val get_slot_content :
  reconstruct_if_missing:bool ->
  Node_context.t ->
  Types.slot_id ->
  (slot, [> Errors.other | Errors.not_found]) result Lwt.t

(** [add_commitment_shards ~shards_proofs_precomputation node_store
    cryptobox commitment slot polynomial] registers the shards of the
    slot whose commitment is given.

    Proofs are generated for the computed shards using
    [shards_proofs_precomputation] and stored in a bounded structure
    in memory.

    In addition to storage errors, this function may return the
    following errors:
    - [Invalid_slot_size] if the given slot does not have the expected size,
    - [No_prover_SRS] if the given [shards_proofs_precomputation] is [None].
*)
val add_commitment_shards :
  shards_proofs_precomputation:Cryptobox.shards_proofs_precomputation option ->
  Store.t ->
  Cryptobox.t ->
  Cryptobox.commitment ->
  Cryptobox.slot ->
  Cryptobox.polynomial ->
  (unit, [> Errors.other]) result Lwt.t

(** This function publishes the given shards and their proofs. *)
val publish_proved_shards :
  Types.slot_id ->
  level_committee:
    (level:int32 ->
    Committee_cache.shard_indexes Signature.Public_key_hash.Map.t tzresult Lwt.t) ->
  Dal_plugin.proto_parameters ->
  Cryptobox.commitment ->
  Cryptobox.shard Seq.t ->
  Cryptobox.shard_proof array ->
  Gossipsub.Worker.t ->
  unit tzresult Lwt.t

(** This function publishes the shards of a commitment that is waiting for
    attestion on L1 if this node has those shards on disk and their proofs in
    memory. *)
val publish_slot_data :
  level_committee:
    (level:int32 ->
    Committee_cache.shard_indexes Signature.Public_key_hash.Map.t tzresult Lwt.t) ->
  Store.t ->
  slot_size:int ->
  Gossipsub.Worker.t ->
  Dal_plugin.proto_parameters ->
  Cryptobox.commitment ->
  Types.slot_id ->
  unit tzresult Lwt.t

(** [store_slot_headers ~number_of_slots ~block_level ~block_hash slot_headers
    node_store] stores [slot_headers] onto the [node_store] associated to the
    given [block_hash] baked at level [block_level]. *)
val store_slot_headers :
  number_of_slots:int ->
  block_level:int32 ->
  (Dal_plugin.slot_header * Dal_plugin.operation_application_result) list ->
  Store.t ->
  unit tzresult Lwt.t

(** [update_selected_slot_headers_statuses ~block_level ~attestation_lag
    ~number_of_slots attested_slots store] updates the statuses of the
    previously selected slots at level [block_level] - [attestation_lag] and
    that were waiting for attestation.

    Slot headers whose indexes are in [attested_slots] are now set as
    {!`Attested} in [store]. Those which are not are marked as
    {!`Unattested} in the [store] if they previously had a "waiting for
    attestation" status.
*)
val update_selected_slot_headers_statuses :
  block_level:int32 ->
  attestation_lag:int ->
  number_of_slots:int ->
  (Dal_plugin.slot_index -> bool) ->
  Store.t ->
  unit tzresult Lwt.t

(** [get_slot_status ~slot_id store] returns the status associated to the
    accepted slot of id [slot_id] or [None] if no status is currently
    stored for that slot id.
*)
val get_slot_status :
  slot_id:Types.slot_id ->
  Store.t ->
  (Types.header_status, [Errors.other | Errors.not_found]) result Lwt.t

(** [get_slot_shard store slot_id shard_index] returns the shard at
    index [shard_index] of the slot given by [slot_id].
*)
val get_slot_shard :
  Store.t ->
  Types.slot_id ->
  Types.shard_index ->
  (Cryptobox.shard, [Errors.other | Errors.not_found]) result Lwt.t
