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

let distribute_dal_attesting_rewards ctxt delegate ~gets_consensus_rewards
    ~dal_attesting_reward_per_shard ~total_active_stake_weight
    ~active_stake_weight active_stake =
  let open Lwt_result_syntax in
  let*! denounced_in_cycle =
    Dal_already_denounced_storage.is_denounced ctxt delegate
  in
  let* ctxt, dal_participation =
    Delegate_missed_attestations_storage
    .get_and_reset_delegate_dal_participation
      ctxt
      delegate
  in
  let sufficient_dal_participation =
    Delegate_missed_attestations_storage.is_dal_participation_sufficient
      ctxt
      dal_participation
  in
  let expected_dal_shards =
    Delegate_missed_attestations_storage
    .expected_dal_shards_per_slot_for_given_active_stake
      ctxt
      ~total_active_stake_weight
      ~active_stake_weight
  in
  let dal_rewards =
    Tez_repr.mul_exn dal_attesting_reward_per_shard expected_dal_shards
  in
  if
    gets_consensus_rewards && sufficient_dal_participation
    && not denounced_in_cycle
  then
    (* Mostly for UX reasons, if the baker lost its attestation rewards, then it
       does not receive DAL rewards. Indeed, the corner case that we want to
       avoid is when a baker does not TB attest at all, but would get the DAL
       rewards because no DAL slot was protocol-attested. *)
    Shared_stake.pay_rewards
      ctxt
      ~active_stake
      ~source:`Dal_attesting_rewards
      ~delegate
      dal_rewards
  else
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/7606
       Handle the case when the delegate had no DAL slot during the
       cycle. Currently the probability for this to happen is very low:
       (1-f)^(number_of_shards * blocks_per_cycle), where f if the delegate's
       stake fraction. For the smallest baker, f is around 6000 / 7 * 10^8. With
       the Q parameters, that's 2.6e-21. *)
    Token.transfer
      ctxt
      `Dal_attesting_rewards
      (`Lost_dal_attesting_rewards delegate)
      dal_rewards

let maybe_distribute_dal_attesting_rewards ctxt delegate ~gets_consensus_rewards
    ~dal_attesting_reward_per_shard ~total_active_stake_weight
    ~active_stake_weight active_stake =
  let open Lwt_result_syntax in
  Raw_context.Dal.only_if_incentives_enabled
    ctxt
    ~default:(fun ctxt -> return (ctxt, []))
    (fun ctxt ->
      let dal_attesting_reward_per_shard =
        match dal_attesting_reward_per_shard with
        | Some v -> v
        | _ -> (* unreachable *) Tez_repr.zero
      in
      distribute_dal_attesting_rewards
        ctxt
        delegate
        ~gets_consensus_rewards
        ~dal_attesting_reward_per_shard
        ~total_active_stake_weight
        ~active_stake_weight
        active_stake)

(* This includes DAL rewards. *)
let distribute_attesting_rewards ctxt last_cycle unrevealed_nonces =
  let open Lwt_result_syntax in
  let all_bakers_attest_enabled =
    Consensus_parameters_storage.is_all_bakers_attest_enabled_for_cycle
      ctxt
      last_cycle
  in
  let*? attesting_reward_per_block =
    Delegate_rewards.attesting_reward_per_block ctxt
  in
  (* Attesting power is staking power *)
  let* ctxt, {total_stake_weight = total_active_stake_weight; _} =
    Delegate_sampler.stake_info_for_cycle ctxt last_cycle
  in
  let unrevealed_nonces_set =
    List.fold_left
      (fun set {Storage.Seed.nonce_hash = _; delegate} ->
        Signature.Public_key_hash.Set.add delegate set)
      Signature.Public_key_hash.Set.empty
      unrevealed_nonces
  in
  let* dal_attesting_reward_per_shard =
    Raw_context.Dal.only_if_incentives_enabled
      ctxt
      ~default:(fun _ctxt -> return None)
      (fun ctxt ->
        let*? dal_attesting_reward_per_shard =
          Delegate_rewards.dal_attesting_reward_per_shard ctxt
        in
        return @@ Some dal_attesting_reward_per_shard)
  in
  (* We cannot use the cached stake info: the detailed stake is needed for reward
     distribution, but it is not cached. *)
  let* ctxt, delegates =
    Stake_storage.get_selected_distribution ctxt last_cycle
  in
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
      let*? rewards =
        Delegate_missed_attestations_storage.attestation_rewards_per_cycle
          ctxt
          ~all_bakers_attest_enabled
          ~total_active_stake_weight
          ~active_stake_weight
          ~rewards_per_block:attesting_reward_per_block
      in
      let gets_consensus_rewards =
        sufficient_participation && has_revealed_nonces
      in
      let* ctxt, payed_rewards_receipts =
        if gets_consensus_rewards then
          Shared_stake.pay_rewards
            ctxt
            ~active_stake
            ~source:`Attesting_rewards
            ~delegate
            rewards
        else
          Token.transfer
            ctxt
            `Attesting_rewards
            (`Lost_attesting_rewards
               (delegate, not sufficient_participation, not has_revealed_nonces))
            rewards
      in
      let* ctxt, payed_dal_rewards_receipts =
        maybe_distribute_dal_attesting_rewards
          ctxt
          delegate
          ~gets_consensus_rewards
          ~dal_attesting_reward_per_shard
          ~total_active_stake_weight
          ~active_stake_weight
          active_stake
      in
      return
        ( ctxt,
          payed_dal_rewards_receipts @ payed_rewards_receipts @ balance_updates
        ))
    (ctxt, [])
    delegates

let cycle_end ctxt last_cycle =
  let open Lwt_result_syntax in
  (* attributing attesting rewards   *)
  let* ctxt, unrevealed_nonces = Seed_storage.cycle_end ctxt last_cycle in
  let* ctxt, attesting_balance_updates =
    distribute_attesting_rewards ctxt last_cycle unrevealed_nonces
  in
  (* Applying slashing related to expiring denunciations *)
  let* ctxt, slashing_balance_updates =
    Delegate_slashed_deposits_storage.apply_and_clear_denunciations ctxt
  in
  let new_cycle = Cycle_repr.add last_cycle 1 in
  let*! ctxt = Already_denounced_storage.clear_outdated_cycle ctxt ~new_cycle in
  let*! ctxt =
    Dal_already_denounced_storage.clear_outdated_cycle ctxt ~new_cycle
  in
  (* Deactivating delegates which didn't participate to consensus for too long *)
  let* ctxt, deactivated_delegates = update_activity ctxt last_cycle in
  (* Computing future staking rights *)
  let* ctxt =
    Delegate_sampler.select_new_distribution_at_cycle_end ctxt ~new_cycle
  in
  (* Activating consensus key for the cycle to come *)
  let*! ctxt = Delegate_consensus_key.activate ctxt ~new_cycle in
  (* trying to unforbid delegates for the cycle to come. *)
  let* ctxt =
    Forbidden_delegates_storage.update_at_cycle_end_after_slashing
      ctxt
      ~new_cycle
  in
  (* clear deprecated cycles data.  *)
  let* ctxt = Stake_storage.clear_at_cycle_end ctxt ~new_cycle in
  let* ctxt = Delegate_sampler.clear_outdated_sampling_data ctxt ~new_cycle in
  (* activate delegate parameters for the cycle to come.  *)
  let*! ctxt = Delegate_staking_parameters.activate ctxt ~new_cycle in
  (* updating AI coefficient. It should remain after all balance changes of the
     cycle-end operations *)
  let* ctxt =
    Adaptive_issuance_storage.update_stored_rewards_at_cycle_end ctxt ~new_cycle
  in
  let balance_updates = slashing_balance_updates @ attesting_balance_updates in
  return (ctxt, balance_updates, deactivated_delegates)

let init_first_cycles ctxt =
  let consensus_rights_delay = Constants_storage.consensus_rights_delay ctxt in
  List.fold_left_es
    (fun ctxt c ->
      let cycle = Cycle_repr.of_int32_exn (Int32.of_int c) in
      Delegate_sampler.select_distribution_for_cycle ctxt cycle)
    ctxt
    Misc.(0 --> consensus_rights_delay)
