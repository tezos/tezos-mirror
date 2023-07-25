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
  let preserved = Constants_storage.preserved_cycles ctxt in
  match Cycle_repr.sub last_cycle preserved with
  | None -> return (ctxt, [])
  | Some _unfrozen_cycle ->
      Stake_storage.fold_on_active_delegates_with_minimal_stake
        ctxt
        ~order:`Sorted
        ~init:(Ok (ctxt, []))
        ~f:(fun delegate () acc ->
          acc >>?= fun (ctxt, deactivated) ->
          Delegate_activation_storage.last_cycle_before_deactivation
            ctxt
            delegate
          >>=? fun cycle ->
          if Cycle_repr.(cycle <= last_cycle) then
            Stake_storage.set_inactive ctxt delegate >>= fun ctxt ->
            return (ctxt, delegate :: deactivated)
          else return (ctxt, deactivated))
      >|=? fun (ctxt, deactivated) -> (ctxt, deactivated)

let update_initial_frozen_deposits ctxt ~new_cycle =
  Delegate_storage.reset_forbidden_delegates ctxt >>= fun ctxt ->
  (match Cycle_repr.pred new_cycle with
  | None -> return Signature.Public_key_hash.Set.empty
  | Some last_cycle -> (
      Stake_storage.find_selected_distribution ctxt last_cycle
      >|=? fun last_cycle_distribution ->
      match last_cycle_distribution with
      | None -> Signature.Public_key_hash.Set.empty
      | Some distribution ->
          List.fold_left
            (fun delegates (d, _) ->
              Signature.Public_key_hash.Set.add d delegates)
            Signature.Public_key_hash.Set.empty
            distribution))
  >>=? fun last_cycle_delegates ->
  Stake_storage.get_selected_distribution ctxt new_cycle
  >>=? fun selection_for_new_cycle ->
  List.fold_left_es
    (fun (ctxt, delegates_to_remove)
         (delegate, Stake_repr.{frozen; delegated = _}) ->
      let delegates_to_remove =
        Signature.Public_key_hash.Set.remove delegate delegates_to_remove
      in
      let delegate_contract = Contract_repr.Implicit delegate in
      Frozen_deposits_storage.update_initial_amount
        ctxt
        delegate_contract
        frozen
      >>=? fun ctxt ->
      Frozen_deposits_storage.get ctxt delegate_contract >>=? fun deposits ->
      let current_amount = deposits.current_amount in
      if Tez_repr.(current_amount = zero) then
        (* If the delegate's current deposit remains at zero then we add it to
           the forbidden set. *)
        Delegate_storage.forbid_delegate ctxt delegate >>= fun ctxt ->
        return (ctxt, delegates_to_remove)
      else return (ctxt, delegates_to_remove))
    (ctxt, last_cycle_delegates)
    selection_for_new_cycle
  >>=? fun (ctxt, delegates_to_remove) ->
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
  Stake_storage.get_total_active_stake ctxt last_cycle
  >>=? fun total_active_stake ->
  let total_active_stake_weight =
    Stake_context.staking_weight ctxt total_active_stake
  in
  Stake_storage.get_selected_distribution ctxt last_cycle >>=? fun delegates ->
  List.fold_left_es
    (fun (ctxt, balance_updates) (delegate, active_stake) ->
      Delegate_missed_attestations_storage
      .check_and_reset_delegate_participation
        ctxt
        delegate
      >>=? fun (ctxt, sufficient_participation) ->
      let has_revealed_nonces =
        delegate_has_revealed_nonces delegate unrevealed_nonces_set
      in
      let active_stake_weight =
        Stake_context.staking_weight ctxt active_stake
      in
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
        Delegate_staking_parameters.pay_rewards
          ctxt
          ~active_stake
          ~source:`Attesting_rewards
          ~delegate
          rewards
        >|=? fun (ctxt, payed_rewards_receipts) ->
        (ctxt, payed_rewards_receipts @ balance_updates)
      else
        (* Insufficient participation or unrevealed nonce: no rewards *)
        Token.transfer
          ctxt
          `Attesting_rewards
          (`Lost_attesting_rewards
            (delegate, not sufficient_participation, not has_revealed_nonces))
          rewards
        >|=? fun (ctxt, payed_rewards_receipts) ->
        (ctxt, payed_rewards_receipts @ balance_updates))
    (ctxt, [])
    delegates

let cycle_end ctxt last_cycle =
  Seed_storage.cycle_end ctxt last_cycle >>=? fun (ctxt, unrevealed_nonces) ->
  let new_cycle = Cycle_repr.add last_cycle 1 in
  Delegate_sampler.select_new_distribution_at_cycle_end ctxt ~new_cycle
  >>=? fun ctxt ->
  Delegate_consensus_key.activate ctxt ~new_cycle >>= fun ctxt ->
  Delegate_slashed_deposits_storage.clear_outdated_slashed_deposits
    ctxt
    ~new_cycle
  >>= fun ctxt ->
  distribute_attesting_rewards ctxt last_cycle unrevealed_nonces
  >>=? fun (ctxt, balance_updates) ->
  update_initial_frozen_deposits ctxt ~new_cycle >>=? fun ctxt ->
  Stake_storage.clear_at_cycle_end ctxt ~new_cycle >>=? fun ctxt ->
  Delegate_sampler.clear_outdated_sampling_data ctxt ~new_cycle >>=? fun ctxt ->
  Delegate_staking_parameters.activate ctxt ~new_cycle >>= fun ctxt ->
  update_activity ctxt last_cycle >>=? fun (ctxt, deactivated_delegates) ->
  Adaptive_issuance_storage.update_stored_rewards_at_cycle_end ctxt ~new_cycle
  >>=? fun ctxt -> return (ctxt, balance_updates, deactivated_delegates)

let init_first_cycles ctxt =
  let preserved = Constants_storage.preserved_cycles ctxt in
  List.fold_left_es
    (fun ctxt c ->
      let cycle = Cycle_repr.of_int32_exn (Int32.of_int c) in
      Stake_storage.snapshot ctxt >>=? fun ctxt ->
      (* NB: we need to take several snapshots because
         select_distribution_for_cycle deletes the snapshots *)
      Delegate_sampler.select_distribution_for_cycle ctxt cycle)
    ctxt
    Misc.(0 --> preserved)
  >>=? fun ctxt ->
  let cycle = (Raw_context.current_level ctxt).cycle in
  update_initial_frozen_deposits ~new_cycle:cycle ctxt
