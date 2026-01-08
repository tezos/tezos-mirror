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

let find_slot_headers ctxt level = Storage.Dal.Slot.Headers.find ctxt level

let find_level_histories ctxt = Storage.Dal.Slot.LevelHistories.find ctxt

let finalize_current_slot_headers ctxt =
  Storage.Dal.Slot.Headers.add
    ctxt
    (Raw_context.current_level ctxt).level
    (Raw_context.Dal.candidates ctxt)

let compute_slot_headers_statuses ~is_slot_attested published_slot_headers =
  let open Dal_slot_repr in
  let fold_attested_slots (rev_attested_slot_headers, attestation)
      (slot, slot_publisher) =
    let attestation_status = is_slot_attested slot in
    let rev_attested_slot_headers =
      (slot, slot_publisher, attestation_status) :: rev_attested_slot_headers
    in
    let attestation =
      if
        attestation_status.Dal_attestation_repr.Accountability.is_proto_attested
      then Dal_attestation_repr.commit attestation slot.Header.id.index
      else attestation
    in
    (rev_attested_slot_headers, attestation)
  in
  let rev_attested_slot_headers, bitset =
    List.fold_left
      fold_attested_slots
      ([], Dal_attestation_repr.empty)
      published_slot_headers
  in
  (List.rev rev_attested_slot_headers, bitset)

let get_slot_headers_history ctxt =
  let open Lwt_result_syntax in
  let+ slots_history = Storage.Dal.Slot.History.find ctxt in
  match slots_history with
  | None -> Dal_slot_repr.History.genesis
  | Some slots_history -> slots_history

let update_skip_list ctxt ~slot_headers_statuses ~published_level
    ~number_of_slots =
  let open Lwt_result_syntax in
  let open Dal_slot_repr.History in
  let* slots_history = get_slot_headers_history ctxt in
  let*? slots_history, cache =
    (* DAL/FIXME: https://gitlab.com/tezos/tezos/-/issues/7126

       Handle DAL parameters (number_of_slots) evolution.
    *)
    (* We expect to put exactly [number_of_slots] cells in the cache. *)
    let cache = History_cache.empty ~capacity:(Int64.of_int number_of_slots) in

    update_skip_list
      ~number_of_slots
      slots_history
      cache
      ~published_level
      slot_headers_statuses
  in
  let*! ctxt = Storage.Dal.Slot.History.add ctxt slots_history in
  let*! ctxt =
    History_cache.(view cache |> Map.bindings)
    |> Storage.Dal.Slot.LevelHistories.add ctxt
  in
  return ctxt

(* TODO https://gitlab.com/tezos/tezos/-/issues/7647
   Consider if it is better to store commitments only for attestation lag
   levels, by:
   - either reducing the slashing period,
   - or adding commitments to accusations and using the skip list to check if
     the commitment was published.
*)
(* Commitments need to be stored for as long as a DAL entrapment evidence can be
   successfully emitted for a given published level. *)
let remove_old_headers ctxt ~published_level =
  let open Lwt_syntax in
  let denunciation_period =
    (Constants_repr.denunciation_period + 1)
    * (Int32.to_int @@ Constants_storage.blocks_per_cycle ctxt)
  in
  match Raw_level_repr.(sub published_level denunciation_period) with
  | None -> return ctxt
  | Some level -> Storage.Dal.Slot.Headers.remove ctxt level

let finalize_pending_slot_headers ctxt ~number_of_slots =
  let open Lwt_result_syntax in
  let {Level_repr.level = raw_level; _} = Raw_context.current_level ctxt in
  let Constants_parametric_repr.{dal; _} = Raw_context.constants ctxt in
  match Raw_level_repr.(sub raw_level dal.attestation_lag) with
  | None -> return (ctxt, Dal_attestation_repr.empty)
  | Some published_level ->
      let* published_slots = find_slot_headers ctxt published_level in
      let*! ctxt = remove_old_headers ctxt ~published_level in
      let* ctxt, attestation, slot_headers_statuses =
        match published_slots with
        | None -> return (ctxt, Dal_attestation_repr.empty, [])
        | Some published_slots ->
            let slot_headers_statuses, attestation =
              let is_slot_attested slot =
                Raw_context.Dal.is_slot_index_attested
                  ctxt
                  slot.Dal_slot_repr.Header.id.index
              in
              compute_slot_headers_statuses ~is_slot_attested published_slots
            in
            return (ctxt, attestation, slot_headers_statuses)
      in
      let* ctxt =
        update_skip_list
          ctxt
          ~slot_headers_statuses
          ~published_level
          ~number_of_slots
      in
      return (ctxt, attestation)
