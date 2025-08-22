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
let sum_weights
    ({
       base_total_issued_per_minute = _;
       baking_reward_fixed_portion_weight;
       baking_reward_bonus_weight;
       attesting_reward_weight;
       seed_nonce_revelation_tip_weight;
       vdf_revelation_tip_weight;
       dal_rewards_weight;
     } :
      Constants_parametric_repr.issuance_weights) ~dal_incentives_enabled =
  let r = baking_reward_fixed_portion_weight in
  let r = baking_reward_bonus_weight + r in
  let r = attesting_reward_weight + r in
  let r = seed_nonce_revelation_tip_weight + r in
  let r = vdf_revelation_tip_weight + r in
  let r = if dal_incentives_enabled then dal_rewards_weight + r else r in
  assert (Compare.Int.(r > 0)) ;
  r

(* [tez_from_weights] returns an amount of rewards in [Tez.t],
   given a couple of parameters:
   [rewards] describes all the possible rewards, as a record of weights
   for each of them. It also gives the (maximum) amount of rewards per minute
   expected on the chain
   [weight] is one of those reward weights as described in [rewards]
   [minimal_block_delay] is the minimum amount of time between two blocks. *)
let tez_from_weights
    ~(issuance_weights : Constants_parametric_repr.issuance_weights)
    ~(weight : int) ~(minimal_block_delay : Period_repr.t)
    ~dal_incentives_enabled =
  let sum_weights = sum_weights issuance_weights ~dal_incentives_enabled in
  let block_delay = minimal_block_delay |> Period_repr.to_seconds in
  (* base_tez = issuance_weights.base_total_issued_per_minute
     relative_weight = reward_weight / sum_weights
     minute_per_block = block_delay (in seconds) / 60
     rewarded_tez = base_tez * relative_weight * blocks_per_minute *)
  let num = Int64.(mul (of_int weight) block_delay) in
  let den = Int64.of_int (sum_weights * 60) in
  Tez_repr.mul_ratio
    ~rounding:`Down
    issuance_weights.base_total_issued_per_minute
    ~num
    ~den

(* Bundling some functions inside a module so they can be exported as part
   of `Internal_for_tests` further down. *)
module M = struct
  type reward_kind =
    | Baking_reward_fixed_portion
    | Baking_reward_bonus_per_block
    | Attesting_reward_per_block
    | Dal_attesting_reward_per_shard
    | Seed_nonce_revelation_tip
    | Vdf_revelation_tip

  let reward_from_constants ~(csts : Constants_parametric_repr.t) ~reward_kind
      ~(coeff : Q.t) =
    let open Result_syntax in
    let issuance_weights = csts.issuance_weights in
    let dal_incentives_enabled = csts.dal.incentives_enable in
    let weight =
      match reward_kind with
      | Baking_reward_fixed_portion ->
          issuance_weights.baking_reward_fixed_portion_weight
      | Baking_reward_bonus_per_block ->
          issuance_weights.baking_reward_bonus_weight
      | Attesting_reward_per_block -> issuance_weights.attesting_reward_weight
      | Dal_attesting_reward_per_shard ->
          if dal_incentives_enabled then issuance_weights.dal_rewards_weight
          else 0
      | Seed_nonce_revelation_tip ->
          (* Seed nonce revelation rewards are given every [blocks_per_commitment](=240)th block *)
          let blocks_per_commitment = Int32.to_int csts.blocks_per_commitment in
          issuance_weights.seed_nonce_revelation_tip_weight
          * blocks_per_commitment
      | Vdf_revelation_tip ->
          (* Vdf revelation rewards are given every [blocks_per_commitment](=240)th block *)
          let blocks_per_commitment = Int32.to_int csts.blocks_per_commitment in
          issuance_weights.vdf_revelation_tip_weight * blocks_per_commitment
    in
    let minimal_block_delay = csts.minimal_block_delay in
    let* rewards =
      tez_from_weights
        ~issuance_weights
        ~weight
        ~minimal_block_delay
        ~dal_incentives_enabled
    in
    let base_rewards =
      match reward_kind with
      | Dal_attesting_reward_per_shard ->
          Tez_repr.div_exn
            rewards
            csts.dal.cryptobox_parameters.number_of_shards
      | Baking_reward_fixed_portion | Baking_reward_bonus_per_block
      | Attesting_reward_per_block | Seed_nonce_revelation_tip
      | Vdf_revelation_tip ->
          rewards
    in
    Tez_repr.mul_q ~rounding:`Down base_rewards coeff

  let liquidity_baking_subsidy_from_constants
      (constants : Constants_parametric_repr.t) =
    let liquidity_baking_subsidy = constants.liquidity_baking_subsidy in
    let minimal_block_delay =
      constants.minimal_block_delay |> Period_repr.to_seconds |> Int64.to_int
    in
    Tez_repr.mul_q
      ~rounding:`Down
      liquidity_baking_subsidy
      Q.(minimal_block_delay // 60)
end

open M

let reward_from_context ~ctxt ~reward_kind =
  let csts = Raw_context.constants ctxt in
  let coeff = Raw_context.reward_coeff_for_current_cycle ctxt in
  reward_from_constants ~csts ~reward_kind ~coeff

let baking_reward_fixed_portion ctxt =
  reward_from_context ~ctxt ~reward_kind:Baking_reward_fixed_portion

let baking_reward_bonus_per_block ctxt =
  reward_from_context ~ctxt ~reward_kind:Baking_reward_bonus_per_block

let attesting_reward_per_block ctxt =
  reward_from_context ~ctxt ~reward_kind:Attesting_reward_per_block

let dal_attesting_reward_per_shard ctxt =
  reward_from_context ~ctxt ~reward_kind:Dal_attesting_reward_per_shard

let liquidity_baking_subsidy ctxt =
  let constants = Raw_context.constants ctxt in
  liquidity_baking_subsidy_from_constants constants

let seed_nonce_revelation_tip ctxt =
  reward_from_context ~ctxt ~reward_kind:Seed_nonce_revelation_tip

let vdf_revelation_tip ctxt =
  reward_from_context ~ctxt ~reward_kind:Vdf_revelation_tip

module For_RPC = struct
  include M

  let reward_from_constants ?(coeff = Q.one)
      (csts : Constants_parametric_repr.t) ~reward_kind =
    reward_from_constants ~csts ~reward_kind ~coeff
end
