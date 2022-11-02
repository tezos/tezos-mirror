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

    Depending on the current level of the context and the [lag] (a
   constant given by the context), the status of the slot may differ:

    - For every level in the interval [current_level; current_level +
   lag -1] the slot is [Pending]. This means a slot header was
   proposed but was not declared available yet.

    - For every level above [current_level + lag], the slot may be
   [confirmed]. For any slot confirmed by the protocol (i.e. indices
   returned by [finalize_pending_slots]), subscribers of the DAL
   should take into account the corresponding slots.

    - For every level below [current_level - lag], there should not be
   any slot in the storage.  *)

(** [find ctxt level] returns [Some slots] where [slots] are pending
   slots at level [level].  [None] is returned if no [slot] was
   registered at this level. The function fails if the reading into
   the context fails. *)
val find :
  Raw_context.t ->
  Raw_level_repr.t ->
  Dal_slot_repr.t list option tzresult Lwt.t

(** [finalize_current_slots ctxt] finalizes the current slots posted
   on this block and marks them as pending into the context.  *)
val finalize_current_slots : Raw_context.t -> Raw_context.t Lwt.t

(** [finalize_pending_slots ctxt] finalizes pending slots which are
   old enough (i.e. registered at level [current_level - lag]). All
   slots marked as available are returned. All the pending slots at
   [current_level - lag] level are removed from the context. *)
val finalize_pending_slots :
  Raw_context.t -> (Raw_context.t * Dal_endorsement_repr.t) tzresult Lwt.t

(** [get_slots_history ctxt] returns the current value of slots_history stored
   in [ctxt], or Slots_history.genesis if no value is stored yet. *)
val get_slots_history :
  Raw_context.t -> Dal_slot_repr.Slots_history.t tzresult Lwt.t
