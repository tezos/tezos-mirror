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

(** This modules handles all the validation/application/finalisation
   of any operation related to the DAL. *)

open Alpha_context

(** [validate_attestation ctxt level slot consensus_key attestation] checks
    whether the DAL attestation [attestation] emitted at given [level] by the
    attester with the given [consensus_key] and given [slot] is valid. If an
    [Error _] is returned, the [op] is not valid. The checks made are:
    * the attestation size does not exceed the maximum;
    * the delegate is in the DAL committee.

    These are checks done for the DAL part alone, checks on other fields of an
    attestation (like level, round) are done by the caller. *)
val validate_attestation :
  t ->
  Raw_level.t ->
  Slot.t ->
  Consensus_key.pk ->
  Dal.Attestation.t ->
  unit tzresult Lwt.t

(** [apply_attestation ctxt attestation] records in the context that the given
    [attestation] was issued and the corresponding attester has the given
    [power]. *)
val apply_attestation : t -> Dal.Attestation.t -> power:int -> t tzresult

(** [validate_publish_commitment ctxt slot] ensures that [slot_header] is
   valid and prevents an operation containing [slot_header] to be
   refused on top of [ctxt]. If an [Error _] is returned, the [slot_header]
   is not valid. *)
val validate_publish_commitment :
  t -> Dal.Operations.Publish_commitment.t -> unit tzresult

(** [apply_publish_commitment ctxt slot_header] applies the publication of
   slot header [slot_header] on top of [ctxt]. Fails if the slot contains
   already a slot header. *)
val apply_publish_commitment :
  t -> Dal.Operations.Publish_commitment.t -> (t * Dal.Slot.Header.t) tzresult

(** [finalisation ctxt] should be executed at block finalisation
   time. A set of slots attested at level [ctxt.current_level - lag]
   is returned encapsulated into the attestation data-structure.

   [lag] is a parametric constant specific to the data-availability
   layer.  *)
val finalisation : t -> (t * Dal.Attestation.t) tzresult Lwt.t
