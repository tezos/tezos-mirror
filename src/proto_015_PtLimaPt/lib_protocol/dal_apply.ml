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

(* Every function of this file should check the feature flag. *)

open Alpha_context
open Dal_errors

let assert_dal_feature_enabled ctxt =
  let open Constants in
  let Parametric.{dal = {feature_enable; _}; _} = parametric ctxt in
  error_unless Compare.Bool.(feature_enable = true) Dal_feature_disabled

let only_if_dal_feature_enabled ctxt ~default f =
  let open Constants in
  let Parametric.{dal = {feature_enable; _}; _} = parametric ctxt in
  if feature_enable then f ctxt else default ctxt

let slot_of_int_e n =
  let open Tzresult_syntax in
  match Dal.Slot_index.of_int n with
  | None -> fail Dal_errors.Dal_slot_index_above_hard_limit
  | Some slot_index -> return slot_index

let validate_data_availability ctxt data_availability =
  assert_dal_feature_enabled ctxt >>? fun () ->
  let open Tzresult_syntax in
  let* max_index =
    slot_of_int_e @@ ((Constants.parametric ctxt).dal.number_of_slots - 1)
  in
  let maximum_size = Dal.Endorsement.expected_size_in_bits ~max_index in
  let size = Dal.Endorsement.occupied_size_in_bits data_availability in
  error_unless
    Compare.Int.(size <= maximum_size)
    (Dal_endorsement_size_limit_exceeded {maximum_size; got = size})

let apply_data_availability ctxt data_availability ~endorser =
  assert_dal_feature_enabled ctxt >>?= fun () ->
  let shards = Dal.Endorsement.shards ctxt ~endorser in
  Dal.Endorsement.record_available_shards ctxt data_availability shards
  |> return

let validate_publish_slot_header ctxt Dal.Slot.{id = {index; _}; _} =
  assert_dal_feature_enabled ctxt >>? fun () ->
  let open Tzresult_syntax in
  let open Constants in
  let Parametric.{dal = {number_of_slots; _}; _} = parametric ctxt in
  let* number_of_slots = slot_of_int_e (number_of_slots - 1) in
  error_unless
    Compare.Int.(
      Dal.Slot_index.compare index number_of_slots <= 0
      || Dal.Slot_index.compare index Dal.Slot_index.zero >= 0)
    (Dal_publish_slot_header_invalid_index
       {given = index; maximum = number_of_slots})

let apply_publish_slot_header ctxt slot =
  assert_dal_feature_enabled ctxt >>? fun () ->
  Dal.Slot.register_slot ctxt slot >>? fun (ctxt, updated) ->
  if updated then ok ctxt else error (Dal_publish_slot_header_duplicate {slot})

let dal_finalisation ctxt =
  only_if_dal_feature_enabled
    ctxt
    ~default:(fun ctxt -> return (ctxt, None))
    (fun ctxt ->
      Dal.Slot.finalize_current_slots ctxt >>= fun ctxt ->
      Dal.Slot.finalize_pending_slots ctxt >|=? fun (ctxt, slot_availability) ->
      (ctxt, Some slot_availability))
