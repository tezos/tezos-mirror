(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 G.B. Fefe, <gb.fefe@protonmail.com>                    *)
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

let update_activity ctxt last_cycle =
  let open Lwt_result_syntax in
  let rights_delay = Constants_storage.consensus_rights_delay ctxt in
  match Cycle_repr.sub last_cycle rights_delay with
  | None ->
      (* do not update activity in the first cycles of a network.*)
      return (ctxt, [])
  | Some _unfrozen_cycle ->
      Stake_storage.fold_on_active_delegates_with_minimal_stake_s
        ctxt
        ~order:`Sorted
        ~init:(Ok (ctxt, []))
        ~f:(fun delegate acc ->
          let*? ctxt, deactivated = acc in
          let* cycle =
            Delegate_activation_storage.last_cycle_before_deactivation
              ctxt
              delegate
          in
          if Cycle_repr.(cycle <= last_cycle) then
            let*! ctxt = Stake_storage.set_inactive ctxt delegate in
            return (ctxt, delegate :: deactivated)
          else return (ctxt, deactivated))

let delegate_has_revealed_nonces delegate unrevelead_nonces_set =
  not (Signature.Public_key_hash.Set.mem delegate unrevelead_nonces_set)

let distribute_attesting_rewards ctxt last_cycle unrevealed_nonces =
  let open Lwt_result_syntax in
  let*? attesting_reward_per_slot =
    Delegate_rewards.attesting_reward_per_slot ctxt
  in
  let unrevealed_nonces_set =
    List.fold_left
      (fun set {Storage.Seed.nonce_hash = _; delegate} ->
        Signature.Public_key_hash.Set.add delegate set)
      Signature.Public_key_hash.Set.empty
      unrevealed_nonces
  in
  let* total_active_stake =
    Stake_storage.get_total_active_stake ctxt last_cycle
  in
  let total_active_stake_weight =
    Stake_repr.staking_weight total_active_stake
  in
  let* delegates = Stake_storage.get_selected_distribution ctxt last_cycle in
  List.fold_left_es
    (fun (ctxt, balance_updates) (delegate, active_stake) ->
      let* ctxt, sufficient_participation =
        Delegate_missed_attestations_storage
        .check_and_reset_delegate_participation
          ctxt
          delegate
      in
      let has_revealed_nonces =
        delegate_has_revealed_nonces delegate unrevealed_nonces_set
      in
      let active_stake_weight = Stake_repr.staking_weight active_stake in
      let expected_slots =
        Delegate_missed_attestations_storage
        .expected_slots_for_given_active_stake
          ctxt
          ~total_active_stake_weight
          ~active_stake_weight
      in
      let rewards = Tez_repr.mul_exn attesting_reward_per_slot expected_slots in
      if sufficient_participation && has_revealed_nonces then
        (* Sufficient participation: we pay the rewards *)
        let+ ctxt, payed_rewards_receipts =
          Shared_stake.pay_rewards
            ctxt
            ~active_stake
            ~source:`Attesting_rewards
            ~delegate
            rewards
        in
        (ctxt, payed_rewards_receipts @ balance_updates)
      else
        (* Insufficient participation or unrevealed nonce: no rewards *)
        let+ ctxt, payed_rewards_receipts =
          Token.transfer
            ctxt
            `Attesting_rewards
            (`Lost_attesting_rewards
              (delegate, not sufficient_participation, not has_revealed_nonces))
            rewards
        in
        (ctxt, payed_rewards_receipts @ balance_updates))
    (ctxt, [])
    delegates

let adjust_frozen_stakes ctxt ~deactivated_delegates :
    (Raw_context.t * Receipt_repr.balance_updates) tzresult Lwt.t =
  let open Lwt_result_syntax in
  (* Note: deactivated_delegates have just been removed from the set of
     active delegates with minimal stake by [update_activity] so the two
     following iterations are on disjoint sets of delegates. *)
  let* ctxt, balance_updates =
    Stake_storage.fold_on_active_delegates_with_minimal_stake_es
      ctxt
      ~order:`Undefined
      ~init:(ctxt, [])
      ~f:(fun delegate (ctxt, balance_updates) ->
        let* full_staking_balance =
          Stake_storage.get_full_staking_balance ctxt delegate
        in
        let own_frozen =
          Full_staking_balance_repr.own_frozen full_staking_balance
        in
        let*? optimal_frozen =
          Stake_context.optimal_frozen_wrt_delegated_without_ai
            ctxt
            full_staking_balance
        in
        let* deposit_limit =
          Delegate_storage.frozen_deposits_limit ctxt delegate
        in
        let optimal_frozen =
          match deposit_limit with
          | None -> optimal_frozen
          | Some deposit_limit -> Tez_repr.min optimal_frozen deposit_limit
        in
        let* ctxt, new_balance_updates =
          if Tez_repr.(optimal_frozen > own_frozen) then
            let*? optimal_to_stake = Tez_repr.(optimal_frozen -? own_frozen) in
            Staking.stake
              ctxt
              ~for_next_cycle_use_only_after_slashing:true
              ~amount:(`At_most optimal_to_stake)
              ~sender:delegate
              ~delegate
          else if Tez_repr.(optimal_frozen < own_frozen) then
            let*? to_unstake = Tez_repr.(own_frozen -? optimal_frozen) in
            Staking.request_unstake
              ctxt
              ~for_next_cycle_use_only_after_slashing:true
              ~sender_contract:Contract_repr.(Implicit delegate)
              ~delegate
              to_unstake
          else
            Staking.finalize_unstake
              ctxt
              ~for_next_cycle_use_only_after_slashing:true
              Contract_repr.(Implicit delegate)
        in
        return (ctxt, new_balance_updates @ balance_updates))
  in
  List.fold_left_es
    (fun (ctxt, balance_updates) delegate ->
      let+ ctxt, new_balance_updates =
        Staking.request_unstake
          ctxt
          ~for_next_cycle_use_only_after_slashing:true
          ~sender_contract:(Implicit delegate)
          ~delegate
          Tez_repr.max_mutez
      in
      (ctxt, new_balance_updates @ balance_updates))
    (ctxt, balance_updates)
    deactivated_delegates

let cycle_end ctxt last_cycle =
  let open Lwt_result_syntax in
  let* ctxt, unrevealed_nonces = Seed_storage.cycle_end ctxt last_cycle in
  let* ctxt, attesting_balance_updates =
    distribute_attesting_rewards ctxt last_cycle unrevealed_nonces
  in
  let* ctxt, slashing_balance_updates =
    Delegate_slashed_deposits_storage.apply_and_clear_denunciations ctxt
  in
  let new_cycle = Cycle_repr.add last_cycle 1 in
  let* ctxt =
    Delegate_sampler.select_new_distribution_at_cycle_end ctxt ~new_cycle
  in
  let*! ctxt = Delegate_consensus_key.activate ctxt ~new_cycle in
  let*! ctxt =
    Delegate_slashed_deposits_storage.clear_outdated_already_denounced
      ctxt
      ~new_cycle
  in
  let* ctxt, deactivated_delegates = update_activity ctxt last_cycle in
  let* ctxt, autostake_balance_updates =
    match Staking.staking_automation ctxt with
    | Manual_staking -> return (ctxt, [])
    | Auto_staking -> adjust_frozen_stakes ctxt ~deactivated_delegates
  in
  let* ctxt =
    Forbidden_delegates_storage.update_at_cycle_end_after_slashing
      ctxt
      ~new_cycle
  in
  let* ctxt = Stake_storage.clear_at_cycle_end ctxt ~new_cycle in
  let* ctxt = Delegate_sampler.clear_outdated_sampling_data ctxt ~new_cycle in
  let*! ctxt = Delegate_staking_parameters.activate ctxt ~new_cycle in
  let* ctxt =
    Adaptive_issuance_storage.update_stored_rewards_at_cycle_end ctxt ~new_cycle
  in
  let balance_updates =
    slashing_balance_updates @ attesting_balance_updates
    @ autostake_balance_updates
  in
  return (ctxt, balance_updates, deactivated_delegates)

let init_first_cycles ctxt =
  let consensus_rights_delay = Constants_storage.consensus_rights_delay ctxt in
  List.fold_left_es
    (fun ctxt c ->
      let cycle = Cycle_repr.of_int32_exn (Int32.of_int c) in
      Delegate_sampler.select_distribution_for_cycle ctxt cycle)
    ctxt
    Misc.(0 --> consensus_rights_delay)
