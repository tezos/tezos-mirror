(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type t = Protocol.Alpha_context.Constants.Parametric.t

(* Warning: not a Set *)
module Set = struct
  let consensus_rights_delay (c : t) consensus_rights_delay =
    {c with consensus_rights_delay}

  let blocks_preservation_cycles (c : t) blocks_preservation_cycles =
    {c with blocks_preservation_cycles}

  let delegate_parameters_activation_delay (c : t)
      delegate_parameters_activation_delay =
    {c with delegate_parameters_activation_delay}

  let blocks_per_cycle (c : t) blocks_per_cycle = {c with blocks_per_cycle}

  let blocks_per_commitment (c : t) blocks_per_commitment =
    {c with blocks_per_commitment}

  let nonce_revelation_threshold (c : t) nonce_revelation_threshold =
    {c with nonce_revelation_threshold}

  let cycles_per_voting_period (c : t) cycles_per_voting_period =
    {c with cycles_per_voting_period}

  let hard_gas_limit_per_operation (c : t) hard_gas_limit_per_operation =
    {c with hard_gas_limit_per_operation}

  let hard_gas_limit_per_block (c : t) hard_gas_limit_per_block =
    {c with hard_gas_limit_per_block}

  let proof_of_work_threshold (c : t) proof_of_work_threshold =
    {c with proof_of_work_threshold}

  let minimal_stake (c : t) minimal_stake = {c with minimal_stake}

  let minimal_frozen_stake (c : t) minimal_frozen_stake =
    {c with minimal_frozen_stake}

  let vdf_difficulty (c : t) vdf_difficulty = {c with vdf_difficulty}

  let origination_size (c : t) origination_size = {c with origination_size}

  let cost_per_byte (c : t) cost_per_byte = {c with cost_per_byte}

  let hard_storage_limit_per_operation (c : t) hard_storage_limit_per_operation
      =
    {c with hard_storage_limit_per_operation}

  let quorum_min (c : t) quorum_min = {c with quorum_min}

  let quorum_max (c : t) quorum_max = {c with quorum_max}

  let min_proposal_quorum (c : t) min_proposal_quorum =
    {c with min_proposal_quorum}

  let liquidity_baking_subsidy (c : t) liquidity_baking_subsidy =
    {c with liquidity_baking_subsidy}

  let liquidity_baking_toggle_ema_threshold (c : t)
      liquidity_baking_toggle_ema_threshold =
    {c with liquidity_baking_toggle_ema_threshold}

  let max_operations_time_to_live (c : t) max_operations_time_to_live =
    {c with max_operations_time_to_live}

  let minimal_block_delay (c : t) minimal_block_delay =
    {c with minimal_block_delay}

  let delay_increment_per_round (c : t) delay_increment_per_round =
    {c with delay_increment_per_round}

  let minimal_participation_ratio (c : t) minimal_participation_ratio =
    {c with minimal_participation_ratio}

  let consensus_committee_size (c : t) consensus_committee_size =
    {c with consensus_committee_size}

  let consensus_threshold (c : t) consensus_threshold =
    {c with consensus_threshold}

  let limit_of_delegation_over_baking (c : t) limit_of_delegation_over_baking =
    {c with limit_of_delegation_over_baking}

  let percentage_of_frozen_deposits_slashed_per_double_baking (c : t)
      percentage_of_frozen_deposits_slashed_per_double_baking =
    {c with percentage_of_frozen_deposits_slashed_per_double_baking}

  let percentage_of_frozen_deposits_slashed_per_double_attestation (c : t)
      percentage_of_frozen_deposits_slashed_per_double_attestation =
    {c with percentage_of_frozen_deposits_slashed_per_double_attestation}

  let max_slashing_per_block (c : t) max_slashing_per_block =
    {c with max_slashing_per_block}

  let max_slashing_threshold (c : t) max_slashing_threshold =
    {c with max_slashing_threshold}

  let testnet_dictator (c : t) testnet_dictator = {c with testnet_dictator}

  let initial_seed (c : t) initial_seed = {c with initial_seed}

  let cache_script_size (c : t) cache_script_size = {c with cache_script_size}

  let cache_stake_distribution_cycles (c : t) cache_stake_distribution_cycles =
    {c with cache_stake_distribution_cycles}

  let cache_sampler_state_cycles (c : t) cache_sampler_state_cycles =
    {c with cache_sampler_state_cycles}

  let dal (c : t) dal = {c with dal}

  let sc_rollup (c : t) sc_rollup = {c with sc_rollup}

  let zk_rollup (c : t) zk_rollup = {c with zk_rollup}

  let direct_ticket_spending_enable (c : t) direct_ticket_spending_enable =
    {c with direct_ticket_spending_enable}

  let issuance_weights (c : t) issuance_weights = {c with issuance_weights}

  module Issuance_weights = struct
    let base_total_issued_per_minute (c : t) base_total_issued_per_minute =
      issuance_weights c {c.issuance_weights with base_total_issued_per_minute}
  end

  let adaptive_issuance (c : t) adaptive_issuance = {c with adaptive_issuance}

  module Adaptive_issuance = struct
    let activation_vote_enable (c : t) activation_vote_enable =
      adaptive_issuance c {c.adaptive_issuance with activation_vote_enable}

    let autostaking_enable (c : t) autostaking_enable =
      adaptive_issuance c {c.adaptive_issuance with autostaking_enable}

    let force_activation (c : t) force_activation =
      adaptive_issuance c {c.adaptive_issuance with force_activation}

    let ns_enable (c : t) ns_enable =
      adaptive_issuance c {c.adaptive_issuance with ns_enable}

    let launch_ema_threshold (c : t) launch_ema_threshold =
      adaptive_issuance c {c.adaptive_issuance with launch_ema_threshold}

    let adaptive_rewards_params (c : t) adaptive_rewards_params =
      adaptive_issuance c {c.adaptive_issuance with adaptive_rewards_params}

    module Adaptive_rewards_params = struct
      let max_bonus (c : t) max_bonus =
        adaptive_rewards_params
          c
          {c.adaptive_issuance.adaptive_rewards_params with max_bonus}
    end
  end
end
