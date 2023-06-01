(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021-2022 Trili Tech, <contact@trili.tech>                  *)
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

let nonce_revelation_threshold c =
  let constants = Raw_context.constants c in
  constants.nonce_revelation_threshold

let blocks_per_stake_snapshot c =
  let constants = Raw_context.constants c in
  constants.blocks_per_stake_snapshot

let cycles_per_voting_period c =
  let constants = Raw_context.constants c in
  constants.cycles_per_voting_period

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

let minimal_stake c =
  let constants = Raw_context.constants c in
  constants.minimal_stake

let vdf_difficulty c =
  let constants = Raw_context.constants c in
  constants.vdf_difficulty

let origination_size c =
  let constants = Raw_context.constants c in
  constants.origination_size

let reward_weights c =
  let constants = Raw_context.constants c in
  constants.reward_weights

let quorum_min c =
  let constants = Raw_context.constants c in
  constants.quorum_min

let quorum_max c =
  let constants = Raw_context.constants c in
  constants.quorum_max

let min_proposal_quorum c =
  let constants = Raw_context.constants c in
  constants.min_proposal_quorum

let liquidity_baking_toggle_ema_threshold c =
  let constants = Raw_context.constants c in
  constants.liquidity_baking_toggle_ema_threshold

let parametric c = Raw_context.constants c

let tx_rollup c = (Raw_context.constants c).tx_rollup

let sc_rollup c = (Raw_context.constants c).sc_rollup

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

let delegation_over_baking_limit c =
  let constants = Raw_context.constants c in
  constants.delegation_over_baking_limit

let percentage_of_frozen_deposits_slashed_per_double_baking c =
  let constants = Raw_context.constants c in
  constants.percentage_of_frozen_deposits_slashed_per_double_baking

let tx_rollup_enable c =
  let tx_rollup = Raw_context.tx_rollup c in
  tx_rollup.enable

let tx_rollup_sunset_level c =
  let tx_rollup = Raw_context.tx_rollup c in
  tx_rollup.sunset_level

let tx_rollup_origination_size c =
  let tx_rollup = Raw_context.tx_rollup c in
  tx_rollup.origination_size

let tx_rollup_hard_size_limit_per_inbox c =
  let tx_rollup = Raw_context.tx_rollup c in
  tx_rollup.hard_size_limit_per_inbox

let tx_rollup_hard_size_limit_per_message c =
  let tx_rollup = Raw_context.tx_rollup c in
  tx_rollup.hard_size_limit_per_message

let tx_rollup_max_withdrawals_per_batch c =
  let tx_rollup = Raw_context.tx_rollup c in
  tx_rollup.max_withdrawals_per_batch

let tx_rollup_commitment_bond c =
  let tx_rollup = Raw_context.tx_rollup c in
  tx_rollup.commitment_bond

let tx_rollup_finality_period c =
  let tx_rollup = Raw_context.tx_rollup c in
  tx_rollup.finality_period

let tx_rollup_withdraw_period c =
  let tx_rollup = Raw_context.tx_rollup c in
  tx_rollup.withdraw_period

let tx_rollup_max_inboxes_count c =
  let tx_rollup = Raw_context.tx_rollup c in
  tx_rollup.max_inboxes_count

let tx_rollup_max_messages_per_inbox c =
  let tx_rollup = Raw_context.tx_rollup c in
  tx_rollup.max_messages_per_inbox

let tx_rollup_max_commitments_count c =
  let tx_rollup = Raw_context.tx_rollup c in
  tx_rollup.max_commitments_count

let tx_rollup_cost_per_byte_ema_factor c =
  let tx_rollup = Raw_context.tx_rollup c in
  tx_rollup.cost_per_byte_ema_factor

let tx_rollup_max_ticket_payload_size c =
  let tx_rollup = Raw_context.tx_rollup c in
  tx_rollup.max_ticket_payload_size

let tx_rollup_rejection_max_proof_size c =
  let tx_rollup = Raw_context.tx_rollup c in
  tx_rollup.rejection_max_proof_size

let percentage_of_frozen_deposits_slashed_per_double_endorsement c =
  let constants = Raw_context.constants c in
  constants.percentage_of_frozen_deposits_slashed_per_double_endorsement

let testnet_dictator c =
  let constants = Raw_context.constants c in
  constants.testnet_dictator

let sc_rollup_enable c =
  let sc_rollup = Raw_context.sc_rollup c in
  sc_rollup.enable

let sc_rollup_arith_pvm_enable c =
  let sc_rollup = Raw_context.sc_rollup c in
  sc_rollup.arith_pvm_enable

let sc_rollup_origination_size c =
  let sc_rollup = Raw_context.sc_rollup c in
  sc_rollup.origination_size

let sc_rollup_challenge_window_in_blocks c =
  let sc_rollup = Raw_context.sc_rollup c in
  sc_rollup.challenge_window_in_blocks

let sc_rollup_stake_amount c =
  let sc_rollup = Raw_context.sc_rollup c in
  sc_rollup.stake_amount

let sc_rollup_commitment_period_in_blocks c =
  let sc_rollup = Raw_context.sc_rollup c in
  sc_rollup.commitment_period_in_blocks

let sc_rollup_max_lookahead_in_blocks c =
  let sc_rollup = Raw_context.sc_rollup c in
  sc_rollup.max_lookahead_in_blocks

let sc_rollup_max_active_outbox_levels c =
  let sc_rollup = Raw_context.sc_rollup c in
  sc_rollup.max_active_outbox_levels

let sc_rollup_max_outbox_messages_per_level c =
  let sc_rollup = Raw_context.sc_rollup c in
  sc_rollup.max_outbox_messages_per_level

let sc_rollup_number_of_sections_in_dissection c =
  let sc_rollup = Raw_context.sc_rollup c in
  sc_rollup.number_of_sections_in_dissection

let sc_rollup_timeout_period_in_blocks c =
  let sc_rollup = Raw_context.sc_rollup c in
  sc_rollup.timeout_period_in_blocks

let sc_rollup_max_number_of_parallel_games c =
  let sc_rollup = Raw_context.sc_rollup c in
  sc_rollup.max_number_of_parallel_games

let max_number_of_stored_cemented_commitments c =
  let sc_rollup = Raw_context.sc_rollup c in
  sc_rollup.max_number_of_stored_cemented_commitments

let dal_number_of_slots c =
  let constants = Raw_context.constants c in
  constants.dal.number_of_slots

let dal_enable c =
  let constants = Raw_context.constants c in
  constants.dal.feature_enable

let zk_rollup_enable c =
  let zk_rollup = Raw_context.zk_rollup c in
  zk_rollup.enable

let zk_rollup_min_pending_to_process c =
  let zk_rollup = Raw_context.zk_rollup c in
  zk_rollup.min_pending_to_process

let zk_rollup_origination_size c =
  let zk_rollup = Raw_context.zk_rollup c in
  zk_rollup.origination_size

let adaptive_inflation c = (Raw_context.constants c).adaptive_inflation

let adaptive_inflation_enable c = (adaptive_inflation c).enable

let adaptive_inflation_staking_over_baking_limit c =
  (adaptive_inflation c).staking_over_baking_limit

let freeze_rewards = adaptive_inflation_enable
