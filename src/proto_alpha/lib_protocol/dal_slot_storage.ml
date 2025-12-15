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
      then
        Dal_attestation_repr.Slot_availability.commit
          attestation
          slot.Header.id.index
      else attestation
    in
    (rev_attested_slot_headers, attestation)
  in
  let rev_attested_slot_headers, bitset =
    List.fold_left
      fold_attested_slots
      ([], Dal_attestation_repr.Slot_availability.empty)
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
    ~number_of_slots ~attestation_lag =
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
      ~attestation_lag:(Dynamic attestation_lag)
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

(* Finalize DAL slot headers for a given (single) published level:
   - Fetch headers published at [published_level].
   - Compute their attestation status via [is_slot_attested].
   - Update the DAL skip-list and storage, mutating [ctxt]:
      + Prune old headers in Storage.Dal.Slot.Headers per denunciation window,
      + Write the new skip-list head to Storage.Dal.Slot.History,
      + Append per-level cells to Storage.Dal.Slot.LevelHistories.
   - Return the updated [ctxt] and the attestation bitset. *)
let finalize_slot_headers_for_published_level ctxt ~number_of_slots
    ~attestation_lag ~is_slot_attested published_level =
  let open Lwt_result_syntax in
  let* published_slots = find_slot_headers ctxt published_level in
  let*! ctxt = remove_old_headers ctxt ~published_level in
  let* ctxt, attestation, slot_headers_statuses =
    match published_slots with
    | None -> return (ctxt, Dal_attestation_repr.Slot_availability.empty, [])
    | Some published_slots ->
        let slot_headers_statuses, attestation =
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
      ~attestation_lag
  in
  return (ctxt, attestation)

(* Handle the first block after an attestation_lag shrink (P1 -> P2).  We
   "backfill" the missing published levels so the DAL skip-list has no gaps:
   process the [prev_attestation_lag - curr_attestation_lag] intermediate
   published levels in order, then the "normal" [target_published_level].

   The function assumes that prev_attestation_lag > curr_attestation_lag.

   Semantics during backfill: do NOT proto-attest slots. *)
let finalize_slot_headers_at_lag_migration ctxt ~target_published_level
    ~number_of_slots ~prev_attestation_lag ~curr_attestation_lag =
  let open Lwt_result_syntax in
  (* During migration, backfilled published levels must not be proto-attested.
     We enforce a conservative attestation status (no proto attest, zero
     shards). *)
  let is_slot_attested slot =
    let status =
      Raw_context.Dal.is_slot_index_attested
        ctxt
        slot.Dal_slot_repr.Header.id.index
    in
    {status with attested_shards = 0; is_proto_attested = false}
  in
  (* Process published levels from oldest to newest:

     - Set [current_gap = prev_attestation_lag - curr_attestation_lag]

     - Start at [target_published_level - current_gap].

     This is equal to [current_level - prev_attestation_lag], since
     [target_published_level = current_level - curr_attestation_lag].

     - Incrementally finalize each published level up to
      [target_published_level] by reducing the gap

     - Make sure to use the right (intermediate) attestation_lag for each
     processed intermediate level (we'll decrease the lag from
     [prev_attestation_lag] to [curr_attestation_lag] one by one).

     Notes:
     - [LevelHistories] is overwritten at each finalize; we collect the
     per-level cells after each step and batch-write them once at the end.
     - Only the final attestation bitset (for [target_published_level]) is
     returned for inclusion in the block header. This should not be an issue, as
     backfilled levels are non-attesting by design during migration. *)
  let rec aux ctxt ~cells_of_pub_levels ~current_gap =
    match Raw_level_repr.(sub target_published_level current_gap) with
    | None ->
        (* Defensive: not expected on our networks. *)
        return
          ( ctxt,
            Dal_attestation_repr.Slot_availability.empty,
            cells_of_pub_levels )
    | Some published_level ->
        (* Finalize this published level. *)
        let* ctxt, attestation_bitset =
          finalize_slot_headers_for_published_level
            ~attestation_lag:(curr_attestation_lag + current_gap)
            ctxt
            ~number_of_slots
            ~is_slot_attested
            published_level
        in
        (* Collect skip-list cells produced at this step. *)
        let* cells_of_pub_levels =
          let+ cells_of_this_pub_level = find_level_histories ctxt in
          cells_of_this_pub_level :: cells_of_pub_levels
        in
        if Compare.Int.(current_gap = 0) then
          (* Done: [target_published_level] processed. *)
          return (ctxt, attestation_bitset, cells_of_pub_levels)
        else aux ctxt ~cells_of_pub_levels ~current_gap:(current_gap - 1)
  in
  (* Main entry for processing several published levels at migration. *)
  let current_gap = prev_attestation_lag - curr_attestation_lag in
  let* ctxt, attestation_bitset, cells_of_pub_levels =
    aux ctxt ~cells_of_pub_levels:[] ~current_gap
  in
  (* Persist all collected per-level cells in one write. *)
  let*! ctxt =
    List.(filter_map (fun e -> e) cells_of_pub_levels |> concat)
    |> List.rev
    |> Storage.Dal.Slot.LevelHistories.add ctxt
  in
  return (ctxt, attestation_bitset)

let finalize_pending_slot_headers ctxt ~number_of_slots =
  let open Lwt_result_syntax in
  let {Level_repr.level = raw_level; _} = Raw_context.current_level ctxt in
  let Constants_parametric_repr.{dal; _} = Raw_context.constants ctxt in
  let curr_attestation_lag = dal.attestation_lag in
  match Raw_level_repr.(sub raw_level curr_attestation_lag) with
  | None -> return (ctxt, Dal_attestation_repr.Slot_availability.empty)
  | Some published_level ->
      (* DAL/TODO: remove after P1->P2 migration:

         Detect whether we are at the first block after the lag shrink. We do
         this by comparing the current attestation lag read from the protocol
         parameters with the attestation lag used for the head of the DAL skip
         list.

         Normal case:

         - This is the case when curr_attestation_lag = prev_attestation_lag

         Migration case:

         - This is the case when k = prev_attestation_lag - curr_attestation_lag
         > 0.

         => We should process [k + 1] published levels at once to avoid
         introducing a gap of [k] levels without cells in the skip list. This
         requires updating the context correctly, in particular the
         [LevelHistories] entry. *)
      let* sl_history_head = get_slot_headers_history ctxt in
      let Dal_slot_repr.History.
            {header_id = _; attestation_lag = prev_attestation_lag} =
        Dal_slot_repr.History.(content sl_history_head |> content_id)
      in
      let prev_attestation_lag =
        Dal_slot_repr.History.attestation_lag_value prev_attestation_lag
      in
      let reset_dummy_genesis =
        Dal_slot_repr.History.(equal sl_history_head genesis)
      in
      if
        Compare.Int.(
          curr_attestation_lag = prev_attestation_lag || reset_dummy_genesis)
      then
        (* Normal path: process the next published level, or the first published
           level if the previous genesis cell was the dummy value. *)
        let is_slot_attested slot =
          Raw_context.Dal.is_slot_index_attested
            ctxt
            slot.Dal_slot_repr.Header.id.index
        in
        finalize_slot_headers_for_published_level
          ctxt
          published_level
          ~number_of_slots
          ~attestation_lag:curr_attestation_lag
          ~is_slot_attested
      else
        let () =
          assert (Compare.Int.(curr_attestation_lag < prev_attestation_lag))
        in
        (* Migration path: there are missing published levels between the
           skip-list head and [published_level] because attestation_lag has
           shrunk at migration from [prev_attestation_lag] to
           [curr_attestation_lag].

           We will backfill the missing [prev_attestation_lag -
           curr_attestation_lag] levels with 32 cells each. *)
        finalize_slot_headers_at_lag_migration
          ~prev_attestation_lag
          ~curr_attestation_lag
          ctxt
          ~target_published_level:published_level
          ~number_of_slots
