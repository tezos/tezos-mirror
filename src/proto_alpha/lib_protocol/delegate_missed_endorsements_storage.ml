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

let expected_slots_for_given_active_stake ctxt ~total_active_stake_weight
    ~active_stake_weight =
  let blocks_per_cycle =
    Int32.to_int (Constants_storage.blocks_per_cycle ctxt)
  in
  let consensus_committee_size =
    Constants_storage.consensus_committee_size ctxt
  in
  let number_of_endorsements_per_cycle =
    blocks_per_cycle * consensus_committee_size
  in
  Z.to_int
    (Z.div
       (Z.mul
          (Z.of_int64 active_stake_weight)
          (Z.of_int number_of_endorsements_per_cycle))
       (Z.of_int64 total_active_stake_weight))

type level_participation = Participated | Didn't_participate

(* Note that the participation for the last block of a cycle is
   recorded in the next cycle. *)
let record_endorsing_participation ctxt ~delegate ~participation
    ~endorsing_power =
  match participation with
  | Participated -> Stake_storage.set_active ctxt delegate
  | Didn't_participate -> (
      let contract = Contract_repr.Implicit delegate in
      Storage.Contract.Missed_endorsements.find ctxt contract >>=? function
      | Some {remaining_slots; missed_levels} ->
          let remaining_slots = remaining_slots - endorsing_power in
          Storage.Contract.Missed_endorsements.update
            ctxt
            contract
            {remaining_slots; missed_levels = missed_levels + 1}
      | None -> (
          let level = Level_storage.current ctxt in
          Raw_context.stake_distribution_for_current_cycle ctxt
          >>?= fun stake_distribution ->
          match
            Signature.Public_key_hash.Map.find delegate stake_distribution
          with
          | None ->
              (* This happens when the block is the first one in a
                 cycle, and therefore the endorsements are for the last
                 block of the previous cycle, and when the delegate does
                 not have an active stake at the current cycle; in this
                 case its participation is simply ignored. *)
              assert (Compare.Int32.(level.cycle_position = 0l)) ;
              return ctxt
          | Some active_stake ->
              Stake_storage.get_total_active_stake ctxt level.cycle
              >>=? fun total_active_stake ->
              let expected_slots =
                let active_stake_weight =
                  Stake_repr.staking_weight active_stake
                in
                let total_active_stake_weight =
                  Stake_repr.staking_weight total_active_stake
                in
                expected_slots_for_given_active_stake
                  ctxt
                  ~total_active_stake_weight
                  ~active_stake_weight
              in
              let Ratio_repr.{numerator; denominator} =
                Constants_storage.minimal_participation_ratio ctxt
              in
              let minimal_activity = expected_slots * numerator / denominator in
              let maximal_inactivity = expected_slots - minimal_activity in
              let remaining_slots = maximal_inactivity - endorsing_power in
              Storage.Contract.Missed_endorsements.init
                ctxt
                contract
                {remaining_slots; missed_levels = 1}))

let record_baking_activity_and_pay_rewards_and_fees ctxt ~payload_producer
    ~block_producer ~baking_reward ~reward_bonus =
  Stake_storage.set_active ctxt payload_producer >>=? fun ctxt ->
  (if not (Signature.Public_key_hash.equal payload_producer block_producer) then
   Stake_storage.set_active ctxt block_producer
  else return ctxt)
  >>=? fun ctxt ->
  let freeze_rewards = Constants_storage.freeze_rewards ctxt in
  let pay_payload_producer ctxt delegate =
    let contract = Contract_repr.Implicit delegate in
    Token.balance ctxt `Block_fees >>=? fun (ctxt, block_fees) ->
    Token.transfer ctxt `Block_fees (`Contract contract) block_fees
    >>=? fun (ctxt, balance_updates_block_fees) ->
    let receiver =
      if freeze_rewards then `Frozen_deposits delegate else `Contract contract
    in
    Token.transfer ctxt `Baking_rewards receiver baking_reward
    >|=? fun (ctxt, balance_updates_baking_rewards) ->
    (ctxt, balance_updates_block_fees @ balance_updates_baking_rewards)
  in
  let pay_block_producer ctxt delegate bonus =
    let receiver =
      if freeze_rewards then `Frozen_deposits delegate
      else `Contract (Contract_repr.Implicit delegate)
    in
    Token.transfer ctxt `Baking_bonuses receiver bonus
  in
  pay_payload_producer ctxt payload_producer
  >>=? fun (ctxt, balance_updates_payload_producer) ->
  (match reward_bonus with
  | Some bonus -> pay_block_producer ctxt block_producer bonus
  | None -> return (ctxt, []))
  >>=? fun (ctxt, balance_updates_block_producer) ->
  return
    (ctxt, balance_updates_payload_producer @ balance_updates_block_producer)

let check_and_reset_delegate_participation ctxt delegate =
  let contract = Contract_repr.Implicit delegate in
  Storage.Contract.Missed_endorsements.find ctxt contract >>=? fun missed ->
  match missed with
  | None -> return (ctxt, true)
  | Some missed_endorsements ->
      Storage.Contract.Missed_endorsements.remove ctxt contract >>= fun ctxt ->
      return (ctxt, Compare.Int.(missed_endorsements.remaining_slots >= 0))

type participation_info = {
  expected_cycle_activity : int;
  minimal_cycle_activity : int;
  missed_slots : int;
  missed_levels : int;
  remaining_allowed_missed_slots : int;
  expected_endorsing_rewards : Tez_repr.t;
}

(* Inefficient, only for RPC *)
let participation_info ctxt delegate =
  let level = Level_storage.current ctxt in
  Stake_storage.get_selected_distribution ctxt level.cycle
  >>=? fun stake_distribution ->
  match
    List.assoc_opt
      ~equal:Signature.Public_key_hash.equal
      delegate
      stake_distribution
  with
  | None ->
      (* delegate does not have an active stake at the current cycle *)
      return
        {
          expected_cycle_activity = 0;
          minimal_cycle_activity = 0;
          missed_slots = 0;
          missed_levels = 0;
          remaining_allowed_missed_slots = 0;
          expected_endorsing_rewards = Tez_repr.zero;
        }
  | Some active_stake ->
      Stake_storage.get_total_active_stake ctxt level.cycle
      >>=? fun total_active_stake ->
      let expected_cycle_activity =
        let active_stake_weight = Stake_repr.staking_weight active_stake in
        let total_active_stake_weight =
          Stake_repr.staking_weight total_active_stake
        in
        expected_slots_for_given_active_stake
          ctxt
          ~total_active_stake_weight
          ~active_stake_weight
      in
      let Ratio_repr.{numerator; denominator} =
        Constants_storage.minimal_participation_ratio ctxt
      in
      let endorsing_reward_per_slot =
        Delegate_rewards.endorsing_reward_per_slot ctxt
      in
      let minimal_cycle_activity =
        expected_cycle_activity * numerator / denominator
      in
      let maximal_cycle_inactivity =
        expected_cycle_activity - minimal_cycle_activity
      in
      let expected_endorsing_rewards =
        Tez_repr.mul_exn endorsing_reward_per_slot expected_cycle_activity
      in
      let contract = Contract_repr.Implicit delegate in
      Storage.Contract.Missed_endorsements.find ctxt contract
      >>=? fun missed_endorsements ->
      let missed_slots, missed_levels, remaining_allowed_missed_slots =
        match missed_endorsements with
        | None -> (0, 0, maximal_cycle_inactivity)
        | Some {remaining_slots; missed_levels} ->
            ( maximal_cycle_inactivity - remaining_slots,
              missed_levels,
              Compare.Int.max 0 remaining_slots )
      in
      let expected_endorsing_rewards =
        match missed_endorsements with
        | Some r when Compare.Int.(r.remaining_slots < 0) -> Tez_repr.zero
        | _ -> expected_endorsing_rewards
      in
      return
        {
          expected_cycle_activity;
          minimal_cycle_activity;
          missed_slots;
          missed_levels;
          remaining_allowed_missed_slots;
          expected_endorsing_rewards;
        }
