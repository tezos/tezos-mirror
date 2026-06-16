(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** SWRR (Stake-Weighted Round Robin) Baker Selection

    This module implements deterministic baker selection for block proposals.
    At the end of each cycle, it precomputes an ordered list of [blocks_per_cycle] bakers
    by running a weighted round-robin lottery where delegates with higher
    stakes are selected proportionally more often.

    Key design decisions:
    - FallbackArray storage: O(1) indexed access for get_baker (called per block)
    - Z.t credits: arbitrary precision prevents overflow, ensures exact fairness
    - Credit persistence: carries fractional stake weights across cycles
    - Cache index 4: in-memory array for block production performance

    Note that we use {!FallbackArray}s since it's the only allowed implementation of
    arrays in the protocol, whose constant time access property is critical
    for the complexity of the [get_baker] function.

    The algorithm ensures fairness: a delegate with stake s out of total T
    receives approximately (s/T) * blocks_per_cycle selections.

    Invariants:
    - Selected bakers array is never empty when it exists (Some _).
      This is guaranteed by [select_bakers_at_cycle_end] which always creates
      an array with [blocks_per_cycle] elements.
      This invariant ensures that [ReadonlyArray.get array (idx mod length)]
      in [get_baker] never fails, since modulo a non-zero length always produces
      a valid index. *)

type credit_info = {
  consensus_pk : Raw_context.consensus_pk;
  credit : Z.t;
  stake : Z.t;
}

(* Cache infrastructure for selected baker arrays.

   We use cache index 4 (fifth slot in the protocol's cache layout)
   with namespace "swrr_selected_distribution" to store the precomputed
   baker arrays in memory during block production.

   The cache is keyed by cycle number and stores FallbackArray structures.
   Symbolic size=1 means one entry per cycle, but actual memory usage
   depends on blocks_per_cycle.

   Cache lifetime is controlled by the protocol constant
   [cache_swrr_selected_distribution_cycles]. *)
module Swrr_selected_distribution = struct
  module Cache_client = struct
    type cached_value = Raw_context.consensus_pk FallbackArray.t

    let namespace = Cache_repr.create_namespace "swrr_selected_distribution"

    let cache_index = 4

    let value_of_identifier ctxt identifier =
      let cycle = Cycle_repr.of_string_exn identifier in
      Storage.Stake.Selected_bakers.get ctxt cycle
  end

  module Cache = (val Cache_repr.register_exn (module Cache_client))

  let identifier_of_cycle cycle = Format.asprintf "%a" Cycle_repr.pp cycle

  (* that's symbolic: 1 cycle = 1 entry *)
  let size = 1

  let init ctxt cycle delegates =
    let open Lwt_result_syntax in
    let id = identifier_of_cycle cycle in
    let* ctxt = Storage.Stake.Selected_bakers.init ctxt cycle delegates in
    let*? ctxt = Cache.update ctxt id (Some (delegates, size)) in
    return ctxt

  let find ctxt cycle =
    let open Lwt_result_syntax in
    let id = identifier_of_cycle cycle in
    let* ctxt, cached_opt = Cache.find ctxt id in
    match cached_opt with
    | Some _ as some_cache -> return (ctxt, some_cache)
    | None ->
        let* delegates = Storage.Stake.Selected_bakers.find ctxt cycle in
        let*? ctxt =
          Cache.update ctxt id (Option.map (fun v -> (v, size)) delegates)
        in
        return (ctxt, delegates)

  (* Note: uses [remove] not [remove_existing] because SWRR data might not exist.

     Unlike the alias sampler (which always creates sampler state for each cycle),
     SWRR data only exists when [swrr_new_baker_lottery_enable] is true.

     [Storage.Stake.Selected_bakers.remove] silently succeeds if data doesn't exist,
     allowing safe cleanup regardless of whether SWRR was active for that cycle. *)
  let remove ctxt cycle =
    let open Lwt_result_syntax in
    let id = identifier_of_cycle cycle in
    let*? ctxt = Cache.update ctxt id None in
    let*! ctxt = Storage.Stake.Selected_bakers.remove ctxt cycle in
    return ctxt
end

(** [select_bakers_at_cycle_end ctxt ~target_cycle ~stakes_pk ~total_stake_weight]
    precomputes the ordered list of bakers for [target_cycle] using the SWRR
    weighted round-robin lottery algorithm.

    Algorithm:
    1. Initialize credits: load each delegate's credit from storage (persisted
       from previous cycle) and their stake for [target_cycle]
    2. For each of [blocks_per_cycle] levels:
       - Increase all delegates' credits by their stake
       - Select delegate with maximum credit. In case of a tie, the
         first delegate in the credits list is chosen, but the order of the list
         itself is unspecified (it changes for every iteration, see implementation for details)
       - Subtract total active stake from selected delegate's credit
    3. Store results: selected bakers as FallbackArray, updated credits

    Complexity: O(blocks_per_cycle × num_delegates)
    - Acceptable as this runs once per cycle
    - NOT on critical path (block production uses cached result)

    Fairness property: delegate with stake s receives ~(s/total_stake) * nb_slots
    selections. Credit subtraction creates a "debt" mechanism ensuring
    fair distribution even with fractional stake weights. *)
let select_bakers_at_cycle_end ctxt ~target_cycle ~stakes_pk ~total_stake_weight
    =
  let open Lwt_result_syntax in
  let total_stake_weight = Z.of_int64 total_stake_weight in
  let blocks_per_cycle = Constants_storage.blocks_per_cycle ctxt in
  let nb_slots = Int32.to_int blocks_per_cycle in
  let* init_credit_list =
    List.fold_left_es
      (fun acc (consensus_pk, stake_weight) ->
        let* credit_opt =
          Storage.Contract.SWRR_credit.find
            ctxt
            (Contract_repr.Implicit consensus_pk.Raw_context.delegate)
        in
        let old_credit = Option.value ~default:Z.zero credit_opt in
        let acc =
          {consensus_pk; credit = old_credit; stake = Z.of_int64 stake_weight}
          :: acc
        in
        return acc)
      []
      stakes_pk
  in
  let fallback_baker =
    match init_credit_list with
    | x :: _ -> x.consensus_pk
    | _ ->
        assert
          false (* Can only happen if stakes_pk = [], which should not happen *)
  in
  (* Use a FallbackArray with fallback=fallback_baker for the selected bakers

     FallbackArray is used instead of list for O(1) indexed access in get_baker,
     which is called every 6 seconds during block production. Lists would require
     O(n) traversal via List.nth.

     The fallback value [fallback_baker] should never be accessed
     in normal operation since indices are always mod array_length. If accessed,
     it indicates a bug in the index calculation or when populating the array
     in [loop]. *)
  let init_selected_bakers_array = FallbackArray.make nb_slots fallback_baker in
  (* 0 <= idx < nb_slots *)
  let rec loop idx (credit_list : credit_info list)
      (acc : Raw_context.consensus_pk FallbackArray.t) :
      credit_info list * Raw_context.consensus_pk FallbackArray.t =
    if Compare.Int.(idx >= nb_slots) then (credit_list, acc)
    else
      (* Increase everyone's credit by their stake, and find the max *)
      let best_credit_info_opt, updated_credit_list =
        List.fold_left
          (fun (current_best_credit_opt, updated_credit_list)
               {consensus_pk; credit; stake}
             ->
            let new_credit = Z.add credit stake in
            let new_credit_info = {consensus_pk; credit = new_credit; stake} in
            match current_best_credit_opt with
            (* First iteration, so the first credit is the best *)
            | None -> (Some new_credit_info, updated_credit_list)
            (* The new staker has better credit: we add the previous one to the list, and keep going. *)
            | Some previous_best_credit
              when Compare.Z.(new_credit > previous_best_credit.credit) ->
                ( Some new_credit_info,
                  previous_best_credit :: updated_credit_list )
            (* The new credit is still less than the best. *)
            | Some _ as x -> (x, new_credit_info :: updated_credit_list))
          (None, [])
          credit_list
      in
      (* At this point `updated_credit_list` doesn't contain `best_credit_info_opt`,
         so we can simply update it and add it to the list. *)
      let updated_credit_list, acc =
        match best_credit_info_opt with
        | None -> assert false
        (* Can only happen if credit_list is empty, which is implies stakes_pk
           to be empty, which means no baker is active, which is,
           if not impossible, problematic for many other reasons. *)
        | Some ({credit; consensus_pk; _} as best_credit_info) ->
            (* Subtract total_stake (not just proportional amount) to create "debt".
               This ensures fairness: high-stake delegates are selected more frequently
               but accumulate negative credit proportionally, preventing monopolization.
               Over many iterations, this guarantees selection proportional to stake. *)
            let updated_credit_list =
              {best_credit_info with credit = Z.sub credit total_stake_weight}
              :: updated_credit_list
            in
            FallbackArray.set acc idx consensus_pk ;
            (updated_credit_list, acc)
      in
      loop (idx + 1) updated_credit_list acc
  in
  let new_credits, selected_bakers =
    loop 0 init_credit_list init_selected_bakers_array
  in
  (* Update context: store selected bakers *)
  let* ctxt =
    Swrr_selected_distribution.init ctxt target_cycle selected_bakers
  in
  (* Update credits for all delegates.

     Use [add] instead of [update] to handle both cases:
     - New delegates: first time participating in SWRR, no credit entry exists yet
     - Existing delegates: already have credit from previous cycles

     [add] creates new entries or updates existing ones, making it robust to
     delegate set changes across cycles (e.g., new delegates meeting minimal stake). *)
  let*! ctxt =
    List.fold_left_s
      (fun ctxt {consensus_pk; credit; _} ->
        Storage.Contract.SWRR_credit.add
          ctxt
          (Contract_repr.Implicit consensus_pk.delegate)
          credit)
      ctxt
      new_credits
  in
  return ctxt

(** [get_baker ctxt level round] retrieves the consensus key for the baker
    assigned to propose a block at [level] and [round].

    Index formula: idx = (cycle_position + 3 * round) mod array_length
    Complexity: O(1)
    Returns None if SWRR is disabled or precomputed data not found for this cycle
    (e.g., during protocol migration). Caller falls back to alias sampler. *)
let get_baker ctxt level round =
  let open Lwt_result_syntax in
  let cycle = level.Level_repr.cycle in
  let pos = level.Level_repr.cycle_position in
  let*? round_int = Round_repr.to_int round in
  let* ctxt, selected_bakers = Swrr_selected_distribution.find ctxt cycle in
  match selected_bakers with
  | None -> return (ctxt, None)
  | Some selected_bakers ->
      let len = FallbackArray.length selected_bakers in
      let idx = (Int32.to_int pos + (3 * round_int)) mod len in
      (* Safe: [selected_bakers] is never empty (invariant documented at file level).
         For any non-empty array [l] and index [n], accessing [l.(n mod length l)]
         always succeeds since [n mod length l] ∈ [0, length l - 1]. *)
      let consensus_pk = FallbackArray.get selected_bakers idx in
      return (ctxt, Some consensus_pk)

(** [reset_credit_for_deactivated_delegates ctxt deactivated_delegates] sets
    SWRR credits to zero for delegates that have been deactivated.

    This prevents negative credits from accumulating during deactivation periods.
    Without reset, a delegate could accumulate large negative credit while inactive,
    making them unfairly disadvantaged when (if) they reactivate.

    Called from delegate_cycles.ml during cycle finalization, before computing
    the new stake distribution, ensuring deactivated delegates don't influence
    next cycle's selection.

    Note: credits can be negative at deactivation time if the delegate was
    recently selected multiple times. This is expected and handled correctly. *)
let reset_credit_for_deactivated_delegates ctxt deactivated_delegates =
  let open Lwt_result_syntax in
  let*! ctxt =
    List.fold_left_s
      (fun ctxt pkh ->
        Storage.Contract.SWRR_credit.add
          ctxt
          (Contract_repr.Implicit pkh)
          Z.zero)
      ctxt
      deactivated_delegates
  in
  return ctxt

let remove_outdated_cycle = Swrr_selected_distribution.remove
