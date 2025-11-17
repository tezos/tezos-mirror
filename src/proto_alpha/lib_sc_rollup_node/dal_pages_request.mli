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

open Protocol
open Alpha_context

(* TODO: https://gitlab.com/tezos/tezos/-/issues/7070

   Rework the interface of dal_pages_request.mli (see the issue for details). *)

(** Access DAL slots and page contents.

    This module exposes helpers to retrieve DAL slot/page data for a rollup,
    validating import conditions against the DAL attestation lag and the
    rollup's context. It queries a DAL node to obtain the slot status and, when
    appropriate, to download page contents. *)

(** Retrieve the pages of the given slot.

    The function queries the DAL node for the slot's attestation status:
    - If the slot is [Attested lag], the function checks that importing the slot
      at [inbox_level] is valid given:
        - the DAL activation level,
        - the rollup origination level,
        - the attested slots validity lag, and
        - index bounds (number of slots).
      If valid, it downloads and returns [Some pages]; otherwise returns [None].
    - If the slot is [Unattested] or [Unpublished], returns [None].
    - If the status is [Waiting_attestation], the function returns an error
      ([Dal_attestation_status_not_final]).

    The returned pages, when present, satisfy:
    - pages are ordered by increasing page index within the slot,
    - the list length equals the expected number of pages for the slot. *)
val slot_pages :
  Octez_smart_rollup.Rollup_constants.dal_constants ->
  dal_activation_level:Raw_level.t option ->
  inbox_level:int32 ->
  _ Node_context.t ->
  Dal.slot_id ->
  dal_attested_slots_validity_lag:int ->
  Dal.Page.content list option tzresult Lwt.t

(** Retrieve the content of a single page.

   Same as {!slot_pages}, but for a single page. *)
val page_content :
  Octez_smart_rollup.Rollup_constants.dal_constants ->
  dal_activation_level:Raw_level.t option ->
  inbox_level:int32 ->
  _ Node_context.t ->
  Dal.Page.t ->
  dal_attested_slots_validity_lag:int ->
  Dal.Page.content option tzresult Lwt.t
