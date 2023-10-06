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
  let preserved = Constants_storage.preserved_cycles ctxt in
  match Cycle_repr.sub last_cycle preserved with
  | None -> return (ctxt, [])
  | Some _unfrozen_cycle ->
      Stake_storage.fold_on_active_delegates_with_minimal_stake
        ctxt
        ~order:`Sorted
        ~init:(Ok (ctxt, []))
        ~f:(fun delegate () acc ->
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

let update_initial_frozen_deposits ctxt ~new_cycle =
  let open Lwt_result_syntax in
  let*! ctxt = Delegate_storage.reset_forbidden_delegates ctxt in
  let* last_cycle_delegates =
    match Cycle_repr.pred new_cycle with
    | None -> return Signature.Public_key_hash.Set.empty
    | Some last_cycle -> (
        let+ last_cycle_distribution =
          Stake_storage.find_selected_distribution ctxt last_cycle
        in
        match last_cycle_distribution with
        | None -> Signature.Public_key_hash.Set.empty
        | Some distribution ->
            List.fold_left
              (fun delegates (d, _) ->
                Signature.Public_key_hash.Set.add d delegates)
              Signature.Public_key_hash.Set.empty
              distribution)
  in
  let* selection_for_new_cycle =
    Stake_storage.get_selected_distribution ctxt new_cycle
  in
  let* ctxt, delegates_to_remove =
    List.fold_left_es
      (fun (ctxt, delegates_to_remove)
           (delegate, Stake_repr.{frozen; weighted_delegated = _}) ->
        let delegates_to_remove =
          Signature.Public_key_hash.Set.remove delegate delegates_to_remove
        in
        let delegate_contract = Contract_repr.Implicit delegate in
        let* ctxt =
          Frozen_deposits_storage.update_initial_amount
            ctxt
            delegate_contract
            frozen
        in
        let* deposits = Frozen_deposits_storage.get ctxt delegate_contract in
        let current_amount = deposits.current_amount in
        if Tez_repr.(current_amount = zero) then
          (* If the delegate's current deposit remains at zero then we add it to
             the forbidden set. *)
          let*! ctxt = Delegate_storage.forbid_delegate ctxt delegate in
          return (ctxt, delegates_to_remove)
        else return (ctxt, delegates_to_remove))
      (ctxt, last_cycle_delegates)
      selection_for_new_cycle
  in
  Signature.Public_key_hash.Set.fold_es
    (fun delegate ctxt ->
      let delegate_contract = Contract_repr.Implicit delegate in
      Frozen_deposits_storage.update_initial_amount
        ctxt
        delegate_contract
        Tez_repr.zero)
    delegates_to_remove
    ctxt

let delegate_has_revealed_nonces delegate unrevelead_nonces_set =
  not (Signature.Public_key_hash.Set.mem delegate unrevelead_nonces_set)

let distribute_attesting_rewards ctxt last_cycle unrevealed_nonces =
  let open Lwt_result_syntax in
  let attesting_reward_per_slot =
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
          Delegate_staking_parameters.pay_rewards
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

let cycle_end ctxt last_cycle =
  let open Lwt_result_syntax in
  let* ctxt, unrevealed_nonces = Seed_storage.cycle_end ctxt last_cycle in
  let* ctxt, balance_updates =
    distribute_attesting_rewards ctxt last_cycle unrevealed_nonces
  in
  let new_cycle = Cycle_repr.add last_cycle 1 in
  let* ctxt =
    Delegate_sampler.select_new_distribution_at_cycle_end ctxt ~new_cycle
  in
  let*! ctxt = Delegate_consensus_key.activate ctxt ~new_cycle in
  let*! ctxt =
    Delegate_slashed_deposits_storage.clear_outdated_slashed_deposits
      ctxt
      ~new_cycle
  in
  let* ctxt = update_initial_frozen_deposits ctxt ~new_cycle in
  let* ctxt = Stake_storage.clear_at_cycle_end ctxt ~new_cycle in
  let* ctxt = Delegate_sampler.clear_outdated_sampling_data ctxt ~new_cycle in
  let*! ctxt = Delegate_staking_parameters.activate ctxt ~new_cycle in
  let* ctxt, deactivated_delegates = update_activity ctxt last_cycle in
  let* ctxt =
    Adaptive_issuance_storage.update_stored_rewards_at_cycle_end ctxt ~new_cycle
  in
  return (ctxt, balance_updates, deactivated_delegates)

let init_first_cycles ctxt =
  let open Lwt_result_syntax in
  let preserved = Constants_storage.preserved_cycles ctxt in
  let* ctxt =
    List.fold_left_es
      (fun ctxt c ->
        let cycle = Cycle_repr.of_int32_exn (Int32.of_int c) in
        let* ctxt = Stake_storage.snapshot ctxt in
        (* NB: we need to take several snapshots because
           select_distribution_for_cycle deletes the snapshots *)
        Delegate_sampler.select_distribution_for_cycle ctxt cycle)
      ctxt
      Misc.(0 --> preserved)
  in
  let cycle = (Raw_context.current_level ctxt).cycle in
  update_initial_frozen_deposits ~new_cycle:cycle ctxt
