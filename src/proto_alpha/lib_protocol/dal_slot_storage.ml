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

let get_slot_headers_history ctxt =
  let open Lwt_result_syntax in
  let+ slots_history = Storage.Dal.Slot.History.find ctxt in
  match slots_history with
  | None -> Dal_slot_repr.History.genesis
  | Some slots_history -> slots_history

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

let get_number_of_shards ctxt ~shard_assignment_level delegate =
  let delegate_to_shard_count =
    Raw_context.Consensus.delegate_to_shard_count ctxt
  in
  match
    Raw_level_repr.Map.find shard_assignment_level delegate_to_shard_count
  with
  | None -> 0
  | Some map -> (
      match Signature.Public_key_hash.Map.find delegate map with
      | None -> 0
      | Some n -> n)

(** [merge_attestation_history ~attestation_lag ~threshold
    ~number_of_shards ~get_attester_shards
    current_block_accountability stored_history] merges the current block's
    accountability data into the stored history.

    Data sources:
    - [current_block_accountability]: Data accumulated during THIS block only.
      For each (level, slot), contains the set of attesters who attested this
      block.
    - [stored_history]: Persistent data from PREVIOUS blocks. For each (level,
      slot), contains the attested shard counts (and their attesters) accumulated
      over all previous blocks in the current attestation window.
    - [get_attester_shards]: Lookup function to get the number of shards
      assigned to a given delegate.

    Returns [(updated_history, newly_attested_by_level)] where:
    - [updated_history] is the merged history with the newly finalized level
      dropped
    - [newly_attested_by_level] is a map from published_level to list of
      slot_indices for slots that crossed the attestation threshold during
      this merge *)
let merge_attestation_history ~attestation_lag ~threshold ~number_of_shards
    ~get_number_of_shards current_block_accountability stored_history =
  let open Dal_attestations_repr.Accountability in
  (* Merge a single slot's attestation data from the current block into the
     stored history for that slot.

     - [current_attesters]: delegates who attested this slot in THIS block
     - [stored_status]: accumulated attestation status from PREVIOUS blocks

     We only add shards from attesters who haven't attested before (to avoid
     double-counting). For each new attester, we look up their shard count
     using [get_attester_shards]. *)
  let merge_slot ~shard_assignment_level slot_index
      {attesters = current_attesters; attested_shards_count = _}
      (level_history, newly_attested_slots) =
    let stored_status =
      match Dal_slot_index_repr.Map.find slot_index level_history with
      | Some status -> status
      | None ->
          {
            total_shards = number_of_shards;
            attested_shards = 0;
            attesters = Signature.Public_key_hash.Set.empty;
            is_proto_attested = false;
          }
    in
    (* Find attesters who are new (attested this block but not in previous blocks) *)
    let new_attesters =
      Signature.Public_key_hash.Set.diff
        current_attesters
        stored_status.attesters
    in
    if Signature.Public_key_hash.Set.is_empty new_attesters then
      (* All current attesters already attested before; nothing to add *)
      (level_history, newly_attested_slots)
    else
      (* Sum up shards from new attesters using the lookup function. *)
      let shards_to_add =
        Signature.Public_key_hash.Set.fold
          (fun attester acc ->
            acc + get_number_of_shards ~shard_assignment_level attester)
          new_attesters
          0
      in
      let merged_shards = stored_status.attested_shards + shards_to_add in
      let is_attested_now =
        is_threshold_reached
          ~threshold
          ~number_of_shards
          ~attested_shards:merged_shards
      in
      let merged_status =
        {
          total_shards = number_of_shards;
          attested_shards = merged_shards;
          attesters =
            Signature.Public_key_hash.Set.union
              stored_status.attesters
              new_attesters;
          is_proto_attested = is_attested_now;
        }
      in
      let level_history =
        Dal_slot_index_repr.Map.add slot_index merged_status level_history
      in
      let was_attested_before = stored_status.is_proto_attested in
      let newly_attested_slots =
        if (not was_attested_before) && is_attested_now then
          slot_index :: newly_attested_slots
        else newly_attested_slots
      in
      (level_history, newly_attested_slots)
  in
  (* Merge all slots for a single published level.
     - [current_slot_map]: attestation data from THIS block for this level
     - Returns updated history and newly attested slots map *)
  let merge_level published_level current_slot_map (history, newly_attested) =
    (* shard_assignment_level = published_level + attestation_lag - 1 *)
    let shard_assignment_level =
      Raw_level_repr.add published_level (attestation_lag - 1)
    in
    let stored_level_history =
      match Raw_level_repr.Map.find published_level history with
      | Some lh -> lh
      | None -> Dal_slot_index_repr.Map.empty
    in
    let merged_level_history, level_newly_attested_slots =
      Dal_slot_index_repr.Map.fold
        (merge_slot ~shard_assignment_level)
        current_slot_map
        (stored_level_history, [])
    in
    let history =
      Raw_level_repr.Map.add published_level merged_level_history history
    in
    let newly_attested =
      if List.is_empty level_newly_attested_slots then newly_attested
      else
        Raw_level_repr.Map.add
          published_level
          level_newly_attested_slots
          newly_attested
    in
    (history, newly_attested)
  in
  (* Merge current block's accountability into stored history *)
  Raw_level_repr.Map.fold
    merge_level
    (get_shard_attestations current_block_accountability)
    (stored_history, Raw_level_repr.Map.empty)

(** [newly_attested_to_bitset ~number_of_slots ~attestation_lags ~current_level
    newly_attested_by_level] converts a map from published_level to slot_indices
    to a [Dal_attestations_repr.t] bitset. *)
let newly_attested_to_bitset ~number_of_slots ~attestation_lags ~current_level
    newly_attested_by_level =
  let number_of_lags = List.length attestation_lags in
  Raw_level_repr.Map.fold
    (fun published_level slot_indices bitset ->
      let lag = Raw_level_repr.diff current_level published_level in
      let lag_index =
        match List.find_index (Compare.Int32.equal lag) attestation_lags with
        | None ->
            (* By construction, [newly_attested_by_level] contains only valid
               level of the form [current_level + lag]), with [lag] in
               [attestation_lags]. *)
            assert false
        | Some i -> i
      in
      List.fold_left
        (fun bitset slot_index ->
          Dal_attestations_repr.Slot_availability.commit
            bitset
            ~number_of_slots
            ~number_of_lags
            ~lag_index
            slot_index)
        bitset
        slot_indices)
    newly_attested_by_level
    Dal_attestations_repr.Slot_availability.empty

let update_skip_list ctxt ~stored_history ~updated_history newly_attested
    ~current_level ~attestation_lag ~number_of_slots ~number_of_shards =
  let open Lwt_result_syntax in
  let* slots_history = get_slot_headers_history ctxt in
  (* We expect to put at most [attestation_lag * number_of_slots] cells in the
     cache. *)
  let cache =
    Dal_slot_repr.History.History_cache.empty
      ~capacity:(Int64.of_int (attestation_lag * number_of_slots))
  in
  let find_slot ~published_level ~slot_index history =
    match Raw_level_repr.Map.find published_level history with
    | None -> None
    | Some level_map -> Dal_slot_index_repr.Map.find slot_index level_map
  in
  let finalized_level = Raw_level_repr.sub current_level attestation_lag in
  let newly_attested =
    (* Make sure there is a key in [newly_attested] for [finalized_level] *)
    match finalized_level with
    | None -> newly_attested
    | Some level -> (
        match Raw_level_repr.Map.find level newly_attested with
        | None -> Raw_level_repr.Map.add level [] newly_attested
        | Some _ -> newly_attested)
  in
  let* slots_history, cache =
    Raw_level_repr.Map.fold_es
      (fun published_level newly_attested_slot_indices (slots_history, cache) ->
        (* Get slot headers for this level *)
        let* published_slots_opt = find_slot_headers ctxt published_level in
        match published_slots_opt with
        | None ->
            (* No slots published at this level, nothing to update *)
            return (slots_history, cache)
        | Some published_slots ->
            let is_finalized_level =
              match finalized_level with
              | None -> false
              | Some level ->
                  assert (Raw_level_repr.(level <= published_level)) ;
                  Raw_level_repr.(level = published_level)
            in
            let slots =
              List.filter_map
                (fun (slot, slot_publisher) ->
                  let slot_index = slot.Dal_slot_repr.Header.id.index in
                  let is_newly_attested =
                    List.exists
                      (fun idx -> Dal_slot_index_repr.equal idx slot_index)
                      newly_attested_slot_indices
                  in
                  (* Get/compute the new status if a cell is to be inserted, or
                     return [None] otherwise *)
                  let status_opt =
                    if is_newly_attested then
                      match
                        find_slot ~published_level ~slot_index updated_history
                      with
                      | None -> (* it was just added there... *) assert false
                      | v -> v
                    else if is_finalized_level then
                      (* We add a cell only if we didn't do it before. *)
                      match
                        find_slot ~published_level ~slot_index stored_history
                      with
                      | None ->
                          Some
                            Dal_attestations_repr.Accountability.
                              {
                                is_proto_attested = false;
                                attested_shards = 0;
                                total_shards = number_of_shards;
                                attesters = Signature.Public_key_hash.Set.empty;
                              }
                      | Some status -> (
                          if
                            status
                              .Dal_attestations_repr.Accountability
                               .is_proto_attested
                          then
                            (* It was previously attested, then it was already
                               inserted. *)
                            None
                          else
                            (* Fetch the updated status *)
                            match
                              find_slot
                                ~published_level
                                ~slot_index
                                updated_history
                            with
                            | None ->
                                (* If it was in the previous history, it should
                                   be in the current history. *)
                                assert false
                            | v -> v)
                    else None
                  in
                  match status_opt with
                  | None -> None
                  | Some status -> Some (slot, slot_publisher, Some status))
                published_slots
            in
            let actual_lag =
              Raw_level_repr.diff current_level published_level |> Int32.to_int
            in
            let*? slots_history, cache =
              Dal_slot_repr.History.update_skip_list
                slots_history
                cache
                ~published_level
                ~number_of_slots
                ~attestation_lag:(Dynamic actual_lag)
                ~slots
                ~fill_unpublished_gaps:false
            in
            return (slots_history, cache))
      newly_attested
      (slots_history, cache)
  in
  let* published_slots_opt = find_slot_headers ctxt current_level in
  let published_headers =
    match published_slots_opt with None -> [] | Some v -> v
  in
  (* For newly published slots at [current_level], pass [status = None] so they
     are skipped. Unpublished slots at [current_level] will have cells added with
     [attestation_lag = 0], making their status immediately queryable. Published
     slots will have cells added later when their attestation status is known. *)
  let slots =
    List.map
      (fun (header, publisher) -> (header, publisher, None))
      published_headers
  in
  let*? slots_history, cache =
    Dal_slot_repr.History.update_skip_list
      slots_history
      cache
      ~published_level:current_level
      ~number_of_slots
      ~attestation_lag:(Dynamic 0)
      ~slots
      ~fill_unpublished_gaps:true
  in
  let*! ctxt = Storage.Dal.Slot.History.add ctxt slots_history in
  let*! ctxt =
    Dal_slot_repr.History.History_cache.(view cache |> Map.bindings)
    |> Storage.Dal.Slot.LevelHistories.add ctxt
  in
  return ctxt

(** [finalize_attestation_history ctxt] merges the current block's
    accountability with stored history, updates storage, and returns the newly
    attested slots as a bitset. *)
let finalize_attestation_history ctxt =
  let open Lwt_result_syntax in
  let {Level_repr.level = current_level; _} = Raw_context.current_level ctxt in
  let Constants_parametric_repr.{dal; _} = Raw_context.constants ctxt in
  let attestation_lag = dal.attestation_lag in
  let threshold = dal.attestation_threshold in
  let number_of_shards = dal.cryptobox_parameters.number_of_shards in
  let current_block_accountability = Raw_context.Dal.get_accountability ctxt in
  let* stored_history_opt = Storage.Dal.AttestationHistory.find ctxt in
  let stored_history =
    Option.value
      ~default:Dal_attestations_repr.Accountability.empty_history
      stored_history_opt
  in
  let get_number_of_shards = get_number_of_shards ctxt in
  let updated_history, newly_attested =
    merge_attestation_history
      ~attestation_lag
      ~threshold
      ~number_of_shards
      ~get_number_of_shards
      current_block_accountability
      stored_history
  in
  let number_of_slots = dal.number_of_slots in
  let* ctxt =
    update_skip_list
      ctxt
      ~stored_history
      ~updated_history
      newly_attested
      ~current_level
      ~attestation_lag
      ~number_of_slots
      ~number_of_shards
  in
  (* Drop finalized levels: those with [published_level <= current_level -
     attestation_lag] *)
  let finalized_threshold = Raw_level_repr.sub current_level attestation_lag in
  let pruned_history =
    match finalized_threshold with
    | None ->
        updated_history (* [current_level < attestation_lag], keep everything *)
    | Some threshold_level ->
        Raw_level_repr.Map.filter
          (fun published_level _ ->
            Raw_level_repr.(published_level > threshold_level))
          updated_history
  in
  let*! ctxt = Storage.Dal.AttestationHistory.add ctxt pruned_history in
  let*! ctxt =
    match Raw_level_repr.sub current_level attestation_lag with
    | Some published_level -> remove_old_headers ctxt ~published_level
    | None -> Lwt.return ctxt
  in
  let attestation_bitset =
    newly_attested_to_bitset
      ~number_of_slots
      ~attestation_lags:(List.map Int32.of_int dal.attestation_lags)
      ~current_level
      newly_attested
  in
  return (ctxt, attestation_bitset)

let finalize_pending_slot_headers ctxt =
  let open Lwt_result_syntax in
  let* sl_history_head = get_slot_headers_history ctxt in
  let Dal_slot_repr.History.
        {header_id = _; attestation_lag = last_attestation_lag} =
    Dal_slot_repr.History.(content sl_history_head |> content_id)
  in
  let dynamic_lag =
    match last_attestation_lag with
    | Dal_slot_repr.History.Legacy -> false
    | Dynamic _ -> true
  in
  let reset_dummy_genesis =
    Dal_slot_repr.History.(equal sl_history_head genesis)
  in
  if dynamic_lag || reset_dummy_genesis then
    (* Normal path: use new multi-lag attestation history. *)
    finalize_attestation_history ctxt
  else (* DEALT WITH IN THE NEXT COMMITS *) assert false
