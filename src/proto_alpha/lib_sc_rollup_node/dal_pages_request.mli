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

(** Access DAL slots and pages content.

    This module is a wrapper on top of {!Store.Dal_slot_pages} module to
    access DAL slots and pages' data that have been previously fetched by
    the rollup node.
*)

(** This error is returned when a slot, identified by its ID, is not found in
    the store. *)
type error += Dal_slot_not_found_in_store of Dal.Slot.Header.id

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
  dal_activation_level:Raw_level.t option ->
  dal_attestation_lag:int ->
  dal_number_of_slots:int ->
  inbox_level:int32 ->
  _ Node_context.t ->
  Dal.slot_id ->
  dal_attested_slots_validity_lag:int ->
  Dal.Page.content list option tzresult Lwt.t

(** Retrieve the content of the page identified by the given ID from the store.

    The function returns [Dal_slot_not_found_in_store] if no entry is found in
    the store for the given ID. It returns [None] in case the level of the
    requested page is out of bounds (e.g. in the future). Said otherwise,
    some content is only returned for confirmed pages (slots) for
    which the content has already been downloaded and saved to the store.

    [dal_attestation_lag] is used to retrieve the correct entry in [store].
*)
val page_content :
  dal_activation_level:Raw_level.t option ->
  dal_attestation_lag:int ->
  dal_number_of_slots:int ->
  inbox_level:int32 ->
  _ Node_context.t ->
  Dal.Page.t ->
  dal_attested_slots_validity_lag:int ->
  Dal.Page.content option tzresult Lwt.t
