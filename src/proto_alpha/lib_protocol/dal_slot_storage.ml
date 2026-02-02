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

(* This module manages the persistent storage for DAL (Data Availability Layer)
   slot headers and attestation tracking.

   == Storages used ==

   Storage.Dal.Slot.Headers : Raw_level -> (Header * Contract) list
     Path: /dal/level/<level>/slot_headers
     Purpose: Stores published slot headers with their publishers for each
     level. Used to verify DAL entrapment evidence and to build skip list
     cells. Kept for [(denunciation_period + 1) * blocks_per_cycle] levels, then
     pruned by [remove_old_headers].

   Storage.Dal.Slot.History : Dal_slot_repr.History.t
     Path: /dal/slot_headers_history
     Purpose: Stores the head of the skip list tracking all slot attestation
     results. Updated at each block finalization with newly unpublished slots,
     attested slots and finalized unattested slots.

   Storage.Dal.Slot.LevelHistories : (hash * History.t) list
     Path: /dal/slot_headers_successive_histories_of_level
     Purpose: The skip list cells constructed during the current's block
     application. Used by the RPC [skip_list_cells_of_level], itself used by
     the DAL node to populate its skip-list store.

   Storage.Dal.AttestationHistory : Accountability.history
     Path: /dal/attestation_history
     Purpose: Tracks per-slot attestation progress (attested shards, attesters
     set, threshold status) for levels within the attestation window. Maps
     [published_level -> slot_index -> attestation_status]. Pruned to keep only
     levels where [published_level > current_level - attestation_lag].

   == Lifecycle ==

   At block finalization ([finalize_pending_slot_headers]):
   1. Current block's attestations are merged into [AttestationHistory]
   2. Slots crossing the attestation threshold are identified
   3. Skip list ([LevelHistory] and [History]) is updated with newly attested
      and finalized slots
   4. Old entries in [AttestationHistory] are pruned (after [attestation_lag] levels)
   5. Old [Headers] are removed (after [denunciation period + 1] cycles)
*)

let find_slot_headers ctxt level = Storage.Dal.Slot.Headers.find ctxt level

let find_level_histories ctxt = Storage.Dal.Slot.LevelHistories.find ctxt

let finalize_current_slot_headers ctxt =
  Storage.Dal.Slot.Headers.add
    ctxt
    (Raw_context.current_level ctxt).level
    (Raw_context.Dal.candidates ctxt)

let compute_slot_headers_statuses ~number_of_slots ~number_of_lags ~to_status
    published_slot_headers =
  let open Dal_slot_repr in
  let fold_attested_slots (rev_attested_slot_headers, attestation)
      (slot, slot_publisher) =
    let attestation_status = to_status slot in
    let rev_attested_slot_headers =
      (slot, slot_publisher, attestation_status) :: rev_attested_slot_headers
    in
    let attestation =
      if
        attestation_status
          .Dal_attestations_repr.Accountability.is_proto_attested
      then
        Dal_attestations_repr.Slot_availability.commit
          attestation
          ~number_of_slots
          ~number_of_lags
          slot.Header.id.index
          ~lag_index:(number_of_lags - 1)
      else attestation
    in
    (rev_attested_slot_headers, attestation)
  in
  let rev_attested_slot_headers, bitset =
    List.fold_left
      fold_attested_slots
      ([], Dal_attestations_repr.Slot_availability.empty)
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
   - Compute their attestation status via [to_status].
   - Update the DAL skip-list and storage, mutating [ctxt]:
      + Prune old headers in Storage.Dal.Slot.Headers per denunciation window,
      + Write the new skip-list head to Storage.Dal.Slot.History,
      + Append per-level cells to Storage.Dal.Slot.LevelHistories.
   - Return the updated [ctxt] and the attestation bitset. *)
let finalize_slot_headers_for_published_level ctxt ~number_of_slots
    ~number_of_lags ~attestation_lag ~to_status published_level =
  let open Lwt_result_syntax in
  let* published_slots = find_slot_headers ctxt published_level in
  let*! ctxt = remove_old_headers ctxt ~published_level in
  let* ctxt, attestation, slot_headers_statuses =
    match published_slots with
    | None -> return (ctxt, Dal_attestations_repr.Slot_availability.empty, [])
    | Some published_slots ->
        let slot_headers_statuses, attestation =
          compute_slot_headers_statuses
            ~number_of_slots
            ~number_of_lags
            ~to_status
            published_slots
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
    ~number_of_slots ~number_of_lags ~to_status ~prev_attestation_lag
    ~curr_attestation_lag =
  let open Lwt_result_syntax in
  (* During migration, backfilled published levels must not be proto-attested.
     We enforce a conservative attestation status (no proto attest, zero
     shards). *)
  let to_status slot =
    let status = to_status slot in
    Dal_attestations_repr.Accountability.
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
            Dal_attestations_repr.Slot_availability.empty,
            cells_of_pub_levels )
    | Some published_level ->
        (* Finalize this published level. *)
        let* ctxt, attestation_bitset =
          finalize_slot_headers_for_published_level
            ~attestation_lag:(curr_attestation_lag + current_gap)
            ctxt
            ~number_of_slots
            ~number_of_lags
            ~to_status
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

let finalize_pending_slot_headers ctxt =
  let open Lwt_result_syntax in
  let {Level_repr.level = raw_level; _} = Raw_context.current_level ctxt in
  let Constants_parametric_repr.{dal; _} = Raw_context.constants ctxt in
  let curr_attestation_lag = dal.attestation_lag in
  match Raw_level_repr.(sub raw_level curr_attestation_lag) with
  | None -> return (ctxt, Dal_attestations_repr.Slot_availability.empty)
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
      let* number_of_slots =
        let+ dal_parameters = Dal_storage.parameters ctxt published_level in
        dal_parameters.number_of_slots
      in
      let number_of_lags = List.length dal.attestation_lags in
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
      let published_level_to_shard_attestations =
        let accounting = Raw_context.Dal.get_accountability ctxt in
        Dal_attestations_repr.Accountability.get_shard_attestations accounting
      in
      let number_of_shards = dal.cryptobox_parameters.number_of_shards in
      let nothing_attested =
        Dal_attestations_repr.Accountability.
          {
            total_shards = number_of_shards;
            attested_shards = 0;
            attesters = Signature.Public_key_hash.Set.empty;
            is_proto_attested = false;
          }
      in
      let to_status =
        match
          Raw_level_repr.Map.find
            published_level
            published_level_to_shard_attestations
        with
        | None -> fun _slot -> nothing_attested
        | Some slot_map -> (
            fun slot ->
              match
                Dal_slot_index_repr.Map.find
                  slot.Dal_slot_repr.Header.id.index
                  slot_map
              with
              | None -> nothing_attested
              | Some
                  Dal_attestations_repr.Accountability.
                    {attesters; attested_shards_count = attested_shards} ->
                  let is_proto_attested =
                    Dal_attestations_repr.Accountability.is_threshold_reached
                      ~threshold:dal.attestation_threshold
                      ~number_of_shards
                      ~attested_shards
                  in
                  {
                    total_shards = number_of_shards;
                    attested_shards;
                    attesters;
                    is_proto_attested;
                  })
      in
      if
        Compare.Int.(
          curr_attestation_lag = prev_attestation_lag || reset_dummy_genesis)
      then
        (* Normal path: process the next published level, or the first published
           level if the previous genesis cell was the dummy value. *)
        finalize_slot_headers_for_published_level
          ctxt
          published_level
          ~number_of_slots
          ~number_of_lags
          ~attestation_lag:curr_attestation_lag
          ~to_status
      else
        let () =
          assert (Compare.Int.(curr_attestation_lag < prev_attestation_lag))
        in
        let* previous_number_of_slots =
          match Raw_level_repr.pred published_level with
          | None -> return number_of_slots
          | Some pred_level ->
              let+ past_dal_parameters =
                Dal_storage.parameters ctxt pred_level
              in
              past_dal_parameters.number_of_slots
        in
        (* Migration path: there are missing published levels between the
           skip-list head and [published_level] because attestation_lag has
           shrunk at migration from [prev_attestation_lag] to
           [curr_attestation_lag].

           We will backfill the missing [prev_attestation_lag -
           curr_attestation_lag] levels with 32 cells each. *)
        finalize_slot_headers_at_lag_migration
          ~to_status
          ~prev_attestation_lag
          ~curr_attestation_lag
          ctxt
          ~target_published_level:published_level
          ~number_of_slots:previous_number_of_slots
          ~number_of_lags
