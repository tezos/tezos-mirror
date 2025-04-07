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
  let number_of_attestations_per_cycle =
    blocks_per_cycle * consensus_committee_size
  in
  Z.to_int
    (Z.div
       (Z.mul
          (Z.of_int64 active_stake_weight)
          (Z.of_int number_of_attestations_per_cycle))
       (Z.of_int64 total_active_stake_weight))

let expected_dal_shards_per_slot_for_given_active_stake ctxt
    ~total_active_stake_weight ~active_stake_weight =
  let blocks_per_cycle =
    Int32.to_int (Constants_storage.blocks_per_cycle ctxt)
  in
  let number_of_shards = Constants_storage.dal_number_of_shards ctxt in
  let number_of_shards_per_cycle = number_of_shards * blocks_per_cycle in
  Z.to_int
    (Z.div
       (Z.mul
          (Z.of_int64 active_stake_weight)
          (Z.of_int number_of_shards_per_cycle))
       (Z.of_int64 total_active_stake_weight))

type level_participation = Participated | Didn't_participate

(* Note that the participation for the last block of a cycle is
   recorded in the next cycle. *)
let record_attesting_participation ctxt ~delegate ~participation
    ~attesting_power =
  let open Lwt_result_syntax in
  match participation with
  | Participated -> Stake_storage.set_active ctxt delegate
  | Didn't_participate -> (
      let contract = Contract_repr.Implicit delegate in
      let* result = Storage.Contract.Missed_attestations.find ctxt contract in
      match result with
      | Some {remaining_slots; missed_levels} ->
          let remaining_slots = remaining_slots - attesting_power in
          Storage.Contract.Missed_attestations.update
            ctxt
            contract
            {remaining_slots; missed_levels = missed_levels + 1}
      | None -> (
          let level = Level_storage.current ctxt in
          let*? stake_distribution =
            Raw_context.stake_distribution_for_current_cycle ctxt
          in
          match
            Signature.Public_key_hash.Map.find delegate stake_distribution
          with
          | None ->
              (* This happens when the block is the first one in a
                 cycle, and therefore the attestations are for the last
                 block of the previous cycle, and when the delegate does
                 not have an active stake at the current cycle; in this
                 case its participation is simply ignored. *)
              assert (Compare.Int32.(level.cycle_position = 0l)) ;
              return ctxt
          | Some active_stake ->
              let* total_active_stake =
                Stake_storage.get_total_active_stake ctxt level.cycle
              in
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
              let remaining_slots = maximal_inactivity - attesting_power in
              Storage.Contract.Missed_attestations.init
                ctxt
                contract
                {remaining_slots; missed_levels = 1}))

let record_dal_participation ctxt ~delegate
    ~number_of_slots_attested_by_delegate ~number_of_protocol_attested_slots =
  let open Lwt_result_syntax in
  if Compare.Int.(number_of_protocol_attested_slots = 0) then
    (* then the number of slots attested by the delegate is also 0 *)
    return ctxt
  else
    let contract = Contract_repr.Implicit delegate in
    let* result = Storage.Contract.Dal_participation.find ctxt contract in
    match result with
    | Some past_participation ->
        let participation =
          Storage.
            {
              attested_slots =
                past_participation.attested_slots
                + number_of_slots_attested_by_delegate;
              attestable_slots =
                past_participation.attestable_slots
                + number_of_protocol_attested_slots;
            }
        in
        Storage.Contract.Dal_participation.update ctxt contract participation
    | None ->
        Storage.Contract.Dal_participation.init
          ctxt
          contract
          {
            attested_slots = number_of_slots_attested_by_delegate;
            attestable_slots = number_of_protocol_attested_slots;
          }

let is_dal_participation_sufficient ctxt participation =
  let open Q in
  let minimal_dal_participation_ratio =
    (Raw_context.constants ctxt).dal.minimal_participation_ratio
  in
  Compare.Int.(participation.Storage.attestable_slots = 0)
  || leq
       minimal_dal_participation_ratio
       (div
          (of_int participation.attested_slots)
          (of_int participation.attestable_slots))

let record_baking_activity_and_pay_rewards_and_fees ctxt ~payload_producer
    ~block_producer ~baking_reward ~reward_bonus =
  let open Lwt_result_syntax in
  let* ctxt = Stake_storage.set_active ctxt payload_producer in
  let* ctxt =
    if not (Signature.Public_key_hash.equal payload_producer block_producer)
    then Stake_storage.set_active ctxt block_producer
    else return ctxt
  in
  let pay_payload_producer ctxt delegate =
    let contract = Contract_repr.Implicit delegate in
    let* ctxt, block_fees = Token.balance ctxt `Block_fees in
    let* ctxt, balance_updates_block_fees =
      Token.transfer ctxt `Block_fees (`Contract contract) block_fees
    in
    let+ ctxt, balance_updates_baking_rewards =
      Shared_stake.pay_rewards
        ctxt
        ~source:`Baking_rewards
        ~delegate
        baking_reward
    in
    (ctxt, balance_updates_block_fees @ balance_updates_baking_rewards)
  in
  let pay_block_producer ctxt delegate bonus =
    Shared_stake.pay_rewards ctxt ~source:`Baking_bonuses ~delegate bonus
  in
  let* ctxt, balance_updates_payload_producer =
    pay_payload_producer ctxt payload_producer
  in
  let* ctxt, balance_updates_block_producer =
    match reward_bonus with
    | Some bonus -> pay_block_producer ctxt block_producer bonus
    | None -> return (ctxt, [])
  in
  return
    (ctxt, balance_updates_payload_producer @ balance_updates_block_producer)

let check_and_reset_delegate_participation ctxt delegate =
  let open Lwt_result_syntax in
  let contract = Contract_repr.Implicit delegate in
  let* missed = Storage.Contract.Missed_attestations.find ctxt contract in
  match missed with
  | None -> return (ctxt, true)
  | Some missed_attestations ->
      let*! ctxt = Storage.Contract.Missed_attestations.remove ctxt contract in
      return (ctxt, Compare.Int.(missed_attestations.remaining_slots >= 0))

let get_and_maybe_reset_delegate_dal_participation ~reset ctxt delegate =
  let open Lwt_result_syntax in
  let contract = Contract_repr.Implicit delegate in
  let* result = Storage.Contract.Dal_participation.find ctxt contract in
  match result with
  | None -> return (ctxt, Storage.{attested_slots = 0; attestable_slots = 0})
  | Some participation ->
      let*! ctxt =
        if reset then Storage.Contract.Dal_participation.remove ctxt contract
        else Lwt.return ctxt
      in
      return (ctxt, participation)

let get_and_reset_delegate_dal_participation =
  get_and_maybe_reset_delegate_dal_participation ~reset:true

let get_delegate_dal_participation ctxt delegate =
  let open Lwt_result_syntax in
  let* _ctxt, n =
    get_and_maybe_reset_delegate_dal_participation ~reset:false ctxt delegate
  in
  return n

module For_RPC = struct
  type participation_info = {
    expected_cycle_activity : int;
    minimal_cycle_activity : int;
    missed_slots : int;
    missed_levels : int;
    remaining_allowed_missed_slots : int;
    expected_attesting_rewards : Tez_repr.t;
  }

  (* Inefficient, only for RPC *)
  let participation_info ctxt delegate =
    let open Lwt_result_syntax in
    let level = Level_storage.current ctxt in
    let* stake_distribution =
      Stake_storage.get_selected_distribution ctxt level.cycle
    in
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
            expected_attesting_rewards = Tez_repr.zero;
          }
    | Some active_stake ->
        let* total_active_stake =
          Stake_storage.get_total_active_stake ctxt level.cycle
        in
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
        let*? attesting_reward_per_slot =
          Delegate_rewards.attesting_reward_per_slot ctxt
        in
        let minimal_cycle_activity =
          expected_cycle_activity * numerator / denominator
        in
        let maximal_cycle_inactivity =
          expected_cycle_activity - minimal_cycle_activity
        in
        let expected_attesting_rewards =
          Tez_repr.mul_exn attesting_reward_per_slot expected_cycle_activity
        in
        let contract = Contract_repr.Implicit delegate in
        let* missed_attestations =
          Storage.Contract.Missed_attestations.find ctxt contract
        in
        let missed_slots, missed_levels, remaining_allowed_missed_slots =
          match missed_attestations with
          | None -> (0, 0, maximal_cycle_inactivity)
          | Some {remaining_slots; missed_levels} ->
              ( maximal_cycle_inactivity - remaining_slots,
                missed_levels,
                Compare.Int.max 0 remaining_slots )
        in
        let expected_attesting_rewards =
          match missed_attestations with
          | Some r when Compare.Int.(r.remaining_slots < 0) -> Tez_repr.zero
          | _ -> expected_attesting_rewards
        in
        return
          {
            expected_cycle_activity;
            minimal_cycle_activity;
            missed_slots;
            missed_levels;
            remaining_allowed_missed_slots;
            expected_attesting_rewards;
          }

  type dal_participation_info = {
    expected_assigned_shards_per_slot : int;
    delegate_attested_dal_slots : int;
    delegate_attestable_dal_slots : int;
    expected_dal_rewards : Tez_repr.t;
    sufficient_dal_participation : bool;
    denounced : bool;
  }

  (* Inefficient, only for RPC *)
  let dal_participation_info_enabled ctxt delegate =
    let open Lwt_result_syntax in
    let level = Level_storage.current ctxt in
    let* stake_distribution =
      Stake_storage.get_selected_distribution ctxt level.cycle
    in
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
            expected_assigned_shards_per_slot = 0;
            delegate_attested_dal_slots = 0;
            delegate_attestable_dal_slots = 0;
            expected_dal_rewards = Tez_repr.zero;
            sufficient_dal_participation = false;
            denounced = false;
          }
    | Some active_stake ->
        let* total_active_stake =
          Stake_storage.get_total_active_stake ctxt level.cycle
        in
        let expected_assigned_shards_per_slot =
          let active_stake_weight = Stake_repr.staking_weight active_stake in
          let total_active_stake_weight =
            Stake_repr.staking_weight total_active_stake
          in
          expected_dal_shards_per_slot_for_given_active_stake
            ctxt
            ~total_active_stake_weight
            ~active_stake_weight
        in
        let* participation = get_delegate_dal_participation ctxt delegate in
        let sufficient_dal_participation =
          is_dal_participation_sufficient ctxt participation
        in
        let*! denounced =
          Dal_already_denounced_storage.is_denounced ctxt delegate
        in
        let*? dal_attesting_reward_per_shard =
          Delegate_rewards.dal_attesting_reward_per_shard ctxt
        in
        let expected_dal_rewards =
          if denounced then Tez_repr.zero
          else
            Tez_repr.mul_exn
              dal_attesting_reward_per_shard
              expected_assigned_shards_per_slot
        in
        return
          {
            expected_assigned_shards_per_slot;
            delegate_attested_dal_slots = participation.Storage.attested_slots;
            delegate_attestable_dal_slots = participation.attestable_slots;
            expected_dal_rewards;
            sufficient_dal_participation;
            denounced;
          }

  (* Inefficient, only for RPC *)
  let dal_participation_info ctxt delegate =
    Raw_context.Dal.only_if_incentives_enabled
      ctxt
      ~default:(fun _ctxt -> tzfail Dal_errors_repr.Dal_incentives_disabled)
      (fun ctxt -> dal_participation_info_enabled ctxt delegate)
end
