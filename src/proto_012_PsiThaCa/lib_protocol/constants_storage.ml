(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

let preserved_cycles c =
  let constants = Raw_context.constants c in
  constants.preserved_cycles

let blocks_per_cycle c =
  let constants = Raw_context.constants c in
  constants.blocks_per_cycle

let blocks_per_commitment c =
  let constants = Raw_context.constants c in
  constants.blocks_per_commitment

let blocks_per_stake_snapshot c =
  let constants = Raw_context.constants c in
  constants.blocks_per_stake_snapshot

let blocks_per_voting_period c =
  let constants = Raw_context.constants c in
  constants.blocks_per_voting_period

let hard_gas_limit_per_operation c =
  let constants = Raw_context.constants c in
  constants.hard_gas_limit_per_operation

let hard_gas_limit_per_block c =
  let constants = Raw_context.constants c in
  constants.hard_gas_limit_per_block

let cost_per_byte c =
  let constants = Raw_context.constants c in
  constants.cost_per_byte

let hard_storage_limit_per_operation c =
  let constants = Raw_context.constants c in
  constants.hard_storage_limit_per_operation

let proof_of_work_threshold c =
  let constants = Raw_context.constants c in
  constants.proof_of_work_threshold

let tokens_per_roll c =
  let constants = Raw_context.constants c in
  constants.tokens_per_roll

let seed_nonce_revelation_tip c =
  let constants = Raw_context.constants c in
  constants.seed_nonce_revelation_tip

let origination_size c =
  let constants = Raw_context.constants c in
  constants.origination_size

let baking_reward_fixed_portion c =
  let constants = Raw_context.constants c in
  constants.baking_reward_fixed_portion

let baking_reward_bonus_per_slot c =
  let constants = Raw_context.constants c in
  constants.baking_reward_bonus_per_slot

let endorsing_reward_per_slot c =
  let constants = Raw_context.constants c in
  constants.endorsing_reward_per_slot

let quorum_min c =
  let constants = Raw_context.constants c in
  constants.quorum_min

let quorum_max c =
  let constants = Raw_context.constants c in
  constants.quorum_max

let min_proposal_quorum c =
  let constants = Raw_context.constants c in
  constants.min_proposal_quorum

let liquidity_baking_subsidy c =
  let constants = Raw_context.constants c in
  constants.liquidity_baking_subsidy

let liquidity_baking_sunset_level c =
  let constants = Raw_context.constants c in
  constants.liquidity_baking_sunset_level

let liquidity_baking_escape_ema_threshold c =
  let constants = Raw_context.constants c in
  constants.liquidity_baking_escape_ema_threshold

let parametric c = Raw_context.constants c

let minimal_block_delay c =
  let constants = Raw_context.constants c in
  constants.minimal_block_delay

let delay_increment_per_round c =
  let constants = Raw_context.constants c in
  constants.delay_increment_per_round

let consensus_committee_size c =
  let constants = Raw_context.constants c in
  constants.consensus_committee_size

let consensus_threshold c =
  let constants = Raw_context.constants c in
  constants.consensus_threshold

let minimal_participation_ratio c =
  let constants = Raw_context.constants c in
  constants.minimal_participation_ratio

let max_slashing_period c =
  let constants = Raw_context.constants c in
  constants.max_slashing_period

let frozen_deposits_percentage c =
  let constants = Raw_context.constants c in
  constants.frozen_deposits_percentage

let double_baking_punishment c =
  let constants = Raw_context.constants c in
  constants.double_baking_punishment

let ratio_of_frozen_deposits_slashed_per_double_endorsement c =
  let constants = Raw_context.constants c in
  constants.ratio_of_frozen_deposits_slashed_per_double_endorsement
