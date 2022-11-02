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

(** [validate_data_availability ctxt endorsement] ensures the
   [endorsement] is valid and cannot prevent an operation containing
   [endorsement] to be refused on top of [ctxt]. If an [Error _] is
   returned, the [endorsement] is not valid. *)
val validate_data_availability : t -> Dal.Endorsement.t -> unit tzresult

(** [apply_data_availability ctxt endorsement ~endorser] applies
   [endorsement] into the [ctxt] assuming [endorser] issued those
   endorsements. *)
val apply_data_availability :
  t ->
  Dal.Endorsement.t ->
  endorser:Signature.Public_key_hash.t ->
  t tzresult Lwt.t

(** [validate_publish_slot_header ctxt slot] ensures that [slot] is
   valid and cannot prevent an operation containing [slot] to be
   refused on top of [ctxt]. If an [Error _] is returned, the [slot]
   is not valid. *)
val validate_publish_slot_header : t -> Dal.Slot.t -> unit tzresult

(** [apply_publish_slot_header ctxt slot] applies the publication of
   slot header [slot] on top of [ctxt]. Fails if the slot contains
   already a slot header. *)
val apply_publish_slot_header : t -> Dal.Slot.t -> t tzresult

(** [dal_finalisation ctxt] should be executed at block finalisation
   time. A set of slots available at level [ctxt.current_level - lag]
   is returned encapsulated into the endorsement data-structure.

   [lag] is a parametric constant specific to the data-availability
   layer.  *)
val dal_finalisation : t -> (t * Dal.Endorsement.t option) tzresult Lwt.t
