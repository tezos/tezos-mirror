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

(** [add_slots node_store slot cryptobox] computes the given [slot]'s commitment
    and adds the association "commitment -> slot" in the DAL's [node_store] if
    the commitment is not already bound to some data.

    The function returns an error {!ref:Invalid_slot_size} if the [slot]'s size
    doesn't match the expected slots' size given in [cryptobox], or the [slot]'s
    commitment otherwise.
*)
val add_slots :
  Store.node_store ->
  Cryptobox.slot ->
  Cryptobox.t ->
  Cryptobox.commitment tzresult Lwt.t

(** [add_slot_id node_tore cryptobox commitment slot_id] associates a [slot_id] to a
    [commitment] in [node_store]. The function returns [Error `Not_found] if
    there is no entry for [commitment] in [node_store]. Otherwise, [Ok ()] is
    returned.
*)
val add_slot_id :
  Store.node_store ->
  Cryptobox.t ->
  Cryptobox.commitment ->
  Services.Types.slot_id ->
  (unit, [> `Not_found]) result Lwt.t

(** [find_slot node_store cryptobox commitment] returns the slot
   content associated with the given [commitment] in [node_store]. The
   function returns [Error `Not_found] if there is no slot content for
   [commitment] in [node_store].  *)
val find_slot :
  Store.node_store ->
  Cryptobox.t ->
  Cryptobox.commitment ->
  (slot, [> `Not_found]) result Lwt.t

(** [store_slot_headers ~block_level ~block_hash slot_headers node_store] stores
    [slot_headers] onto the [node_store] associated to the given [block_hash]
    baked at level [block_level]. *)
val store_slot_headers :
  block_level:int32 ->
  block_hash:Tezos_crypto.Block_hash.t ->
  (Dal_plugin.slot_header * Dal_plugin.operation_application_result) list ->
  Store.node_store ->
  unit Lwt.t

(** [update_selected_slot_headers_statuses ~block_level ~attestation_lag
    attested_slots unattested_slots store] updates the statuses of the
    previously selected slots at level [block_level] - [endorsement_lag] and
    that were waiting for attestation.

    Slot headers whose indexes are in [attested_slots] are now set as
    {!`Attested} in [store]. Those in [unattested_slots] are marked as
    {!`Unattested} in the [store].
*)
val update_selected_slot_headers_statuses :
  block_level:int32 ->
  attestation_lag:int ->
  [`Attested of Dal_plugin.slot_index list] ->
  [`Unattested of Dal_plugin.slot_index list] ->
  Store.node_store ->
  unit tzresult Lwt.t
