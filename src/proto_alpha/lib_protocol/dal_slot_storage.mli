(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Storage management of slots for the data-availability layer.

    {1 Overview}

    This module is an interface for the slot storage for the layer 1.

    Depending on the current level of the context and the [attestation_lag] (a
    constant given by the context), the status of the slot may differ:

    - For every level in the interval [current_level; current_level +
    attestation_lag - 1] the slot is [Pending]. This means a slot header was
    proposed but was not declared available yet.

    - For every level above [current_level + attestation_lag], the slot may be
    [attested]. For any slot attested by the protocol (i.e. indices returned by
    [finalize_pending_slots]), subscribers of the DAL should take into account
    the corresponding slots.

    - For every level below [current_level - attestation_lag], there should not be
   any slot in the storage.  *)

(** [find_slot_headers ctxt level] returns [Some slot_headers] where
    [slot_headers] are pending slots at level [level] alongside the contracts
    addresses that published them.  [None] is returned if no [slot_header] was
    registered at this level. The function fails if the reading into the context
    fails. *)
val find_slot_headers :
  Raw_context.t ->
  Raw_level_repr.t ->
  (Dal_slot_repr.Header.t * Contract_repr.t) list option tzresult Lwt.t

(** [find_level_histories ctxt] returns the cells of the DAL skip list produced
    at the current level alongside their hashes. Returns [None] if no entry is
    present in the storage (yet). The function fails if the reading from the
    context fails. *)
val find_level_histories :
  Raw_context.t ->
  (Dal_slot_repr.History.Pointer_hash.t * Dal_slot_repr.History.t) list option
  tzresult
  Lwt.t

(** [finalize_current_slot_headers ctxt] finalizes the current slot
   headers posted on this block and marks them as pending into the
   context.  *)
val finalize_current_slot_headers : Raw_context.t -> Raw_context.t Lwt.t

(** [finalize_pending_slot_headers ctxt] finalizes pending slot
    headers which are old enough (i.e. registered at level [current_level -
    attestation_lag]). All slots marked as available are returned. All the
    pending slots at [current_level - attestation_lag] level are removed from
    the context. *)
val finalize_pending_slot_headers :
  Raw_context.t ->
  (Raw_context.t * Dal_attestations_repr.Slot_availability.t) tzresult Lwt.t

(** [get_slot_headers_history ctxt] returns the current value of slots_history stored
   in [ctxt], or Slots_history.genesis if no value is stored yet. *)
val get_slot_headers_history :
  Raw_context.t -> Dal_slot_repr.History.t tzresult Lwt.t
