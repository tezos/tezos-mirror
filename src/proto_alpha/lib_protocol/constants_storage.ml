(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021-2022 Trili Tech, <contact@trili.tech>                  *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
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

let consensus_rights_delay c =
  let constants = Raw_context.constants c in
  constants.consensus_rights_delay

let blocks_preservation_cycles c =
  let constants = Raw_context.constants c in
  constants.blocks_preservation_cycles

let delegate_parameters_activation_delay c =
  let constants = Raw_context.constants c in
  constants.delegate_parameters_activation_delay

(** Adaptive Issuance activation delay:
    After the e.m.a. of AI votes reaches the threshold, we wait for this delay
    before effectively activating AI.
*)
let adaptive_issuance_activation_delay c =
  let constants = Raw_context.constants c in
  1 + constants.consensus_rights_delay + Constants_repr.slashing_delay + 1

(** Tolerated inactivity period for delegates before being deactivated. *)
let tolerated_inactivity_period c =
  let constants = Raw_context.constants c in
  constants.tolerated_inactivity_period

(** Number of cycles during which a misbehavior of the delegate will induce a
    slashing of the funds that are currently in its frozen deposits. *)
let slashable_deposits_period c =
  let constants = Raw_context.constants c in
  constants.consensus_rights_delay

let blocks_per_cycle c =
  let constants = Raw_context.constants c in
  constants.blocks_per_cycle

let blocks_per_commitment c =
  let constants = Raw_context.constants c in
  constants.blocks_per_commitment

let nonce_revelation_threshold c =
  let constants = Raw_context.constants c in
  constants.nonce_revelation_threshold

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

let minimal_frozen_stake c =
  let constants = Raw_context.constants c in
  constants.minimal_frozen_stake

let vdf_difficulty c =
  let constants = Raw_context.constants c in
  constants.vdf_difficulty

let origination_size c =
  let constants = Raw_context.constants c in
  constants.origination_size

let issuance_weights c =
  let constants = Raw_context.constants c in
  constants.issuance_weights

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

let consensus_threshold_size c =
  let constants = Raw_context.constants c in
  constants.consensus_threshold_size

let minimal_participation_ratio c =
  let constants = Raw_context.constants c in
  constants.minimal_participation_ratio

let limit_of_delegation_over_baking c =
  let constants = Raw_context.constants c in
  constants.limit_of_delegation_over_baking

let percentage_of_frozen_deposits_slashed_per_double_baking c =
  let constants = Raw_context.constants c in
  constants.percentage_of_frozen_deposits_slashed_per_double_baking

let testnet_dictator c =
  let constants = Raw_context.constants c in
  constants.testnet_dictator

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

let sc_rollup_reveal_activation_level c =
  let sc_rollup = Raw_context.sc_rollup c in
  sc_rollup.reveal_activation_level

let sc_rollup_private_enable c =
  let sc_rollup = Raw_context.sc_rollup c in
  sc_rollup.private_enable

let sc_rollup_riscv_pvm_enable c =
  let sc_rollup = Raw_context.sc_rollup c in
  sc_rollup.riscv_pvm_enable

let dal_number_of_slots c =
  let constants = Raw_context.constants c in
  constants.dal.number_of_slots

let dal_attestation_lag c =
  let constants = Raw_context.constants c in
  constants.dal.attestation_lag

let dal_number_of_shards c =
  let constants = Raw_context.constants c in
  constants.dal.cryptobox_parameters.number_of_shards

let dal_attestation_lags c =
  let constants = Raw_context.constants c in
  constants.dal.attestation_lags

let dal_number_of_lags c =
  let constants = Raw_context.constants c in
  List.length constants.dal.attestation_lags

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

let zk_rollup_max_ticket_payload_size c =
  let zk_rollup = Raw_context.zk_rollup c in
  zk_rollup.max_ticket_payload_size

let adaptive_issuance c = (Raw_context.constants c).adaptive_issuance

let adaptive_issuance_global_limit_of_staking_over_baking c =
  (adaptive_issuance c).global_limit_of_staking_over_baking

let adaptive_issuance_edge_of_staking_over_delegation c =
  (adaptive_issuance c).edge_of_staking_over_delegation

let adaptive_issuance_rewards_params c =
  (adaptive_issuance c).adaptive_rewards_params

let max_slashing_threshold c = (Raw_context.constants c).max_slashing_threshold

let max_slashing_per_block c = (Raw_context.constants c).max_slashing_per_block

let direct_ticket_spending_enable c =
  (Raw_context.constants c).direct_ticket_spending_enable

let aggregate_attestation c = (Raw_context.constants c).aggregate_attestation

let allow_tz4_delegate_enable c =
  (Raw_context.constants c).allow_tz4_delegate_enable

let tz5_account_enable c = (Raw_context.constants c).tz5_account_enable

let all_bakers_attest_activation_threshold c =
  (Raw_context.constants c).all_bakers_attest_activation_threshold

let all_bakers_attest_first_level c =
  Raw_context.all_bakers_attest_first_level c

let round_durations ctxt = Raw_context.round_durations ctxt

let native_contracts_enable c =
  (Raw_context.constants c).native_contracts_enable

let all ctxt = Constants_repr.all_of_parametric (parametric ctxt)

(* Derived pseudo-constants *)

let issuance_modification_delay c =
  Constants_repr.Derived.issuance_modification_delay ~parametric:(parametric c)

let consensus_key_activation_delay c =
  Constants_repr.Derived.consensus_key_activation_delay
    ~parametric:(parametric c)

let unstake_finalization_delay c =
  Constants_repr.Derived.unstake_finalization_delay ~parametric:(parametric c)
