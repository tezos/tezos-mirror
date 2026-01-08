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

(** Access DAL slots and pages content.

    This module is a wrapper on top of {!Store.Dal_slot_pages} module to
    access DAL slots and pages' data that have been previously fetched by
    the rollup node.
*)

(** Retrieve the pages' content of the given slot ID's from the store.

    The function returns [Dal_slot_not_found_in_store] if no entry is found in
    the store for the given ID (i.e. no page is registered with or without content).

    If the returned value is [Some pages], the slot whose ID is given is
    supposed to be confirmed and [pages] correspond to the pages of the slot.
    Otherwise [None] is returned.

    The function relies on {!Store.Dal_slot_pages}'s invariants to guarantee that:
    - the pages are returned in increasing order w.r.t. their indexes in the slot;
    - the size of the list, in case it is not empty, is equal to the expected
      number of pages in a slot.

    [dal_attestation_lag] is used to retrieve the correct entry in [store].
*)
val slot_pages :
  Octez_smart_rollup.Rollup_constants.dal_constants ->
  dal_activation_level:Raw_level.t option ->
  inbox_level:int32 ->
  _ Node_context.t ->
  Dal.slot_id ->
  dal_attested_slots_validity_lag:int ->
  Dal.Page.content list option tzresult Lwt.t

(** Retrieve the content of the page identified by the given ID from the store.

    This function checks the attestation status of the page's slot by inspecting
    the skip list cells stored on L1 at the slot's attested level. A slot is
    considered attested if either one of the following conditions is met:

    - The [is_proto_attested] field in the content of the cell is true, and
    attestation_threshold_percent is None (default DAL behavior).

    - The attestation_threshold_percent is set to a specific threshold, the slot
    meets or exceeds this threshold, and the publisher is authorized, provided a
    whitelist is specified in [restricted_commitments_publishers].

    If the slot of the page is attested, the data is retrieved from the DAL node
    and returned. Otherwise, the function returns None. It also returns None if
    the page_id or slot_id is invalid (or no longer valid). This includes cases
    where:

    - Indices are out of range.

    - The rollup was originated after the slot's publication.

    - The slot is too old (i.e., its level is beyond
    [dal_attested_slots_validity_lag]).
*)
val page_content :
  Octez_smart_rollup.Rollup_constants.dal_constants ->
  dal_activation_level:Raw_level.t option ->
  inbox_level:int32 ->
  _ Node_context.t ->
  attestation_threshold_percent:int option ->
  restricted_commitments_publishers:Contract.t list option ->
  Dal.Page.t ->
  dal_attested_slots_validity_lag:int ->
  Dal.Page.content option tzresult Lwt.t
