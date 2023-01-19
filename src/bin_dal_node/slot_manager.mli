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

(* We cannot include a raw mli file. But this will be removed once full
   migration is done. *)
include module type of Slot_manager_legacy

type error += Invalid_slot_size of {provided : int; expected : int}

(** [add_commitment node_store slot cryptobox] computes the given [slot]'s
    commitment and adds the association "commitment -> slot" in the DAL's
    [node_store] if the commitment is not already bound to some data.

    In addition to decoding errors, the function returns an error
    {!ref:Invalid_slot_size} if the [slot]'s size doesn't match the expected
    slots' size given in [cryptobox], or the [slot]'s commitment otherwise.
*)
val add_commitment :
  Store.node_store ->
  Cryptobox.slot ->
  Cryptobox.t ->
  (Cryptobox.commitment, [Errors.decoding | Errors.other]) result Lwt.t

(** [associate_slot_id_with_commitment node_store cryptobox commitment slot_id]
    associates a [slot_id] to a [commitment] in [node_store].

    In addition to decoding errors, the function returns [`Not_found]
    if there is no entry for [commitment] in [node_store].
*)
val associate_slot_id_with_commitment :
  Store.node_store ->
  Cryptobox.t ->
  Cryptobox.commitment ->
  Services.Types.slot_id ->
  (unit, [Errors.decoding | Errors.not_found]) result Lwt.t

(** [get_commitment_slot node_store cryptobox commitment] returns the slot
    content associated with the given [commitment] in [node_store].

    In addition to decoding errors, the function returns [`Not_found]
    if there is no slot content for [commitment] in [node_store].
*)
val get_commitment_slot :
  Store.node_store ->
  Cryptobox.t ->
  Cryptobox.commitment ->
  (slot, [Errors.decoding | Errors.not_found]) result Lwt.t

(** [add_commitment_shards node_store cryptobox commitment ~with_proof]
    registers the shards of the slot whose commitment is given.

    In addition to decoding errors, the function returns [`Not_found]
    if there is no slot content for [commitment] in [node_store]. *)
val add_commitment_shards :
  Store.node_store ->
  Cryptobox.t ->
  Cryptobox.commitment ->
  Services.Types.with_proof ->
  (unit, [Errors.decoding | Errors.not_found | Errors.other]) result Lwt.t

(** [store_slot_headers ~block_level ~block_hash slot_headers node_store] stores
    [slot_headers] onto the [node_store] associated to the given [block_hash]
    baked at level [block_level].
*)
val store_slot_headers :
  block_level:int32 ->
  block_hash:Block_hash.t ->
  (Dal_plugin.slot_header * Dal_plugin.operation_application_result) list ->
  Store.node_store ->
  unit Lwt.t

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
  Dal_plugin.slot_index list ->
  Store.node_store ->
  unit Lwt.t

(** [get_commitment_by_published_level_and_index ~level ~slot_index node_store]
    returns the commitment associated with the accepted slot header of index
    [slot_index] published at level [level]. Returns [Error `Not_found] if no
    such commitment is found in [node_store].

    In addition to decoding errors, the function returns [`Not_found]
    if there is no commitment for the given [level] and [slot_index] in
    [node_store].
*)
val get_commitment_by_published_level_and_index :
  level:Services.Types.level ->
  slot_index:Services.Types.slot_index ->
  Store.node_store ->
  (Cryptobox.commitment, [Errors.decoding | Errors.not_found]) result Lwt.t

(** [get_commitment_headers commitment ?slot_level ?slot_index store] returns
    the list of slot headers {!Services.Types.slot_header} known by the DAL.
    The result is filtered by [slot_level] and [slot_index] if provided.

    The function may return an decoding error in case of failure.
*)
val get_commitment_headers :
  Cryptobox.commitment ->
  ?slot_level:Services.Types.level ->
  ?slot_index:Services.Types.slot_index ->
  Store.node_store ->
  (Services.Types.slot_header list, Errors.decoding) result Lwt.t

(** [get_published_level_headers ~published_level ?header_status store] returns
    the list of slot headers {!Services.Types.slot_header} that are published
    for the given [published_level]. If a header status is given in
    [?header_status], the list is filtered accordingly.

    The function may return an decoding error in case of failure.
*)
val get_published_level_headers :
  published_level:Services.Types.level ->
  ?header_status:Services.Types.header_status ->
  Store.node_store ->
  (Services.Types.slot_header list, Errors.decoding) result Lwt.t
