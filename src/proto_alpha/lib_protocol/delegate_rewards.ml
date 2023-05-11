(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* Sum weights for normalizing *)
let sum_weights (rewards : Constants_parametric_repr.reward_weights) =
  let r = rewards.baking_reward_fixed_portion_weight in
  let r = rewards.baking_reward_bonus_weight + r in
  let r = rewards.endorsing_reward_weight + r in
  let r = rewards.liquidity_baking_subsidy_weight + r in
  let r = rewards.seed_nonce_revelation_tip_weight + r in
  let r = rewards.vdf_revelation_tip_weight + r in
  assert (Compare.Int.(r > 0)) ;
  r

(* [tez_from_weights] returns an amount of rewards in [Tez.t],
   given a couple of parameters:
   [rewards] describes all the possible rewards, as a record of weights
   for each of them. It also gives the (maximum) amount of rewards per minute
   expected on the chain
   [weight] is one of those reward weights as described in [rewards]
   [minimal_block_delay] is the minimum amouht of time between two blocks. *)
let tez_from_weights
    ~(reward_weights : Constants_parametric_repr.reward_weights) ~(weight : int)
    ~(minimal_block_delay : Period_repr.t) =
  let sum_weights = sum_weights reward_weights in
  let block_delay =
    minimal_block_delay |> Period_repr.to_seconds |> Int64.to_int
  in
  let weighted_rewards_per_minute =
    Tez_repr.mul_exn reward_weights.base_total_rewards_per_minute weight
  in
  let weighted_rewards_per_block =
    Tez_repr.(div_exn (mul_exn weighted_rewards_per_minute block_delay) 60)
  in
  let normalized_rewards_per_block =
    Tez_repr.div_exn weighted_rewards_per_block sum_weights
  in
  normalized_rewards_per_block

module Internal_for_tests = struct
  type reward_kind =
    | Baking_reward_fixed_portion
    | Baking_reward_bonus_per_slot
    | Endorsing_reward_per_slot
    | Liquidity_baking_subsidy
    | Seed_nonce_revelation_tip
    | Vdf_revelation_tip

  let reward_from_constants ~(csts : Constants_parametric_repr.t) ~reward_kind =
    let reward_weights = csts.reward_weights in
    let weight =
      match reward_kind with
      | Baking_reward_fixed_portion ->
          reward_weights.baking_reward_fixed_portion_weight
      | Baking_reward_bonus_per_slot ->
          reward_weights.baking_reward_bonus_weight
      | Endorsing_reward_per_slot -> reward_weights.endorsing_reward_weight
      | Liquidity_baking_subsidy ->
          reward_weights.liquidity_baking_subsidy_weight
      | Seed_nonce_revelation_tip ->
          (* Seed nonce revelation rewards are given every [blocks_per_commitment](=128)th block *)
          let blocks_per_commitment = Int32.to_int csts.blocks_per_commitment in
          reward_weights.seed_nonce_revelation_tip_weight
          * blocks_per_commitment
      | Vdf_revelation_tip ->
          (* Vdf revelation rewards are given every [blocks_per_commitment](=128)th block *)
          let blocks_per_commitment = Int32.to_int csts.blocks_per_commitment in
          reward_weights.vdf_revelation_tip_weight * blocks_per_commitment
    in
    let minimal_block_delay = csts.minimal_block_delay in
    let rewards =
      tez_from_weights ~reward_weights ~weight ~minimal_block_delay
    in
    match reward_kind with
    | Baking_reward_bonus_per_slot ->
        let bonus_committee_size =
          csts.consensus_committee_size - csts.consensus_threshold
        in
        if Compare.Int.(bonus_committee_size <= 0) then Tez_repr.zero
        else Tez_repr.div_exn rewards bonus_committee_size
    | Endorsing_reward_per_slot ->
        Tez_repr.div_exn rewards csts.consensus_committee_size
    | _ -> rewards
end

open Internal_for_tests

let reward_from_context ctxt reward_kind =
  let csts = Raw_context.constants ctxt in
  reward_from_constants ~csts ~reward_kind

let baking_reward_fixed_portion c =
  reward_from_context c Baking_reward_fixed_portion

let baking_reward_bonus_per_slot c =
  reward_from_context c Baking_reward_bonus_per_slot

let endorsing_reward_per_slot c =
  reward_from_context c Endorsing_reward_per_slot

let liquidity_baking_subsidy c = reward_from_context c Liquidity_baking_subsidy

let seed_nonce_revelation_tip c =
  reward_from_context c Seed_nonce_revelation_tip

let vdf_revelation_tip c = reward_from_context c Vdf_revelation_tip
