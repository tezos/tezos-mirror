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

(** Access DAL page contents for a rollup.

    This module exposes helpers to retrieve DAL page data for a rollup, while
    validating import conditions against the DAL attestation lag and the
    rollup's context. It queries a DAL node to obtain the status of the page's
    slot and, when appropriate, to download the page content. *)

(** Retrieve the content of a single DAL page.

    The function queries the DAL node for the attestation status of the page's
    slot:

    - If the slot is [Attested lag], the function checks that importing the slot
      at [inbox_level] is valid given:
        - the DAL activation level,
        - the rollup origination level,
        - the attested slots validity lag, and
        - index bounds (number of slots).
      If valid, it downloads and returns [Some content]; otherwise returns
      [None].

    - If the slot is [Unattested] or [Unpublished], returns [None].

    - If the status is [Waiting_attestation], the function returns an error
      ([Dal_attestation_status_not_final]).

    When present, the returned content corresponds to the page index carried by
    the given [Dal.Page.t]. *)
val page_content :
  Octez_smart_rollup.Rollup_constants.dal_constants ->
  dal_activation_level:Raw_level.t option ->
  inbox_level:int32 ->
  _ Node_context.t ->
  Dal.Page.t ->
  dal_attested_slots_validity_lag:int ->
  Dal.Page.content option tzresult Lwt.t
