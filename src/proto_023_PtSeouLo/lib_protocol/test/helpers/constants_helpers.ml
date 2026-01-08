(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type t = Protocol.Alpha_context.Constants.Parametric.t

(* Warning: not a Set *)
module Set = struct
  let consensus_rights_delay consensus_rights_delay (c : t) =
    {c with consensus_rights_delay}

  let blocks_preservation_cycles blocks_preservation_cycles (c : t) =
    {c with blocks_preservation_cycles}

  let delegate_parameters_activation_delay delegate_parameters_activation_delay
      (c : t) =
    {c with delegate_parameters_activation_delay}

  let blocks_per_cycle blocks_per_cycle (c : t) = {c with blocks_per_cycle}

  let blocks_per_commitment blocks_per_commitment (c : t) =
    {c with blocks_per_commitment}

  let nonce_revelation_threshold nonce_revelation_threshold (c : t) =
    {c with nonce_revelation_threshold}

  let cycles_per_voting_period cycles_per_voting_period (c : t) =
    {c with cycles_per_voting_period}

  let hard_gas_limit_per_operation hard_gas_limit_per_operation (c : t) =
    {c with hard_gas_limit_per_operation}

  let hard_gas_limit_per_block hard_gas_limit_per_block (c : t) =
    {c with hard_gas_limit_per_block}

  let proof_of_work_threshold proof_of_work_threshold (c : t) =
    {c with proof_of_work_threshold}

  let minimal_stake minimal_stake (c : t) = {c with minimal_stake}

  let minimal_frozen_stake minimal_frozen_stake (c : t) =
    {c with minimal_frozen_stake}

  let vdf_difficulty vdf_difficulty (c : t) = {c with vdf_difficulty}

  let origination_size origination_size (c : t) = {c with origination_size}

  let cost_per_byte cost_per_byte (c : t) = {c with cost_per_byte}

  let hard_storage_limit_per_operation hard_storage_limit_per_operation (c : t)
      =
    {c with hard_storage_limit_per_operation}

  let quorum_min quorum_min (c : t) = {c with quorum_min}

  let quorum_max quorum_max (c : t) = {c with quorum_max}

  let min_proposal_quorum min_proposal_quorum (c : t) =
    {c with min_proposal_quorum}

  let liquidity_baking_subsidy liquidity_baking_subsidy (c : t) =
    {c with liquidity_baking_subsidy}

  let liquidity_baking_toggle_ema_threshold
      liquidity_baking_toggle_ema_threshold (c : t) =
    {c with liquidity_baking_toggle_ema_threshold}

  let max_operations_time_to_live max_operations_time_to_live (c : t) =
    {c with max_operations_time_to_live}

  let minimal_block_delay minimal_block_delay (c : t) =
    {c with minimal_block_delay}

  let delay_increment_per_round delay_increment_per_round (c : t) =
    {c with delay_increment_per_round}

  let minimal_participation_ratio minimal_participation_ratio (c : t) =
    {c with minimal_participation_ratio}

  let consensus_committee_size consensus_committee_size (c : t) =
    {c with consensus_committee_size}

  let consensus_threshold_size consensus_threshold_size (c : t) =
    {c with consensus_threshold_size}

  let limit_of_delegation_over_baking limit_of_delegation_over_baking (c : t) =
    {c with limit_of_delegation_over_baking}

  let percentage_of_frozen_deposits_slashed_per_double_baking
      percentage_of_frozen_deposits_slashed_per_double_baking (c : t) =
    {c with percentage_of_frozen_deposits_slashed_per_double_baking}

  let max_slashing_per_block max_slashing_per_block (c : t) =
    {c with max_slashing_per_block}

  let max_slashing_threshold max_slashing_threshold (c : t) =
    {c with max_slashing_threshold}

  let testnet_dictator testnet_dictator (c : t) = {c with testnet_dictator}

  let initial_seed initial_seed (c : t) = {c with initial_seed}

  let cache_script_size cache_script_size (c : t) = {c with cache_script_size}

  let cache_stake_distribution_cycles cache_stake_distribution_cycles (c : t) =
    {c with cache_stake_distribution_cycles}

  let cache_sampler_state_cycles cache_sampler_state_cycles (c : t) =
    {c with cache_sampler_state_cycles}

  let dal dal (c : t) = {c with dal}

  module Dal = struct
    let number_of_slots number_of_slots (c : t) =
      dal {c.dal with number_of_slots} c
  end

  let sc_rollup sc_rollup (c : t) = {c with sc_rollup}

  let zk_rollup zk_rollup (c : t) = {c with zk_rollup}

  let direct_ticket_spending_enable direct_ticket_spending_enable (c : t) =
    {c with direct_ticket_spending_enable}

  let allow_tz4_delegate_enable allow_tz4_delegate_enable (c : t) =
    {c with allow_tz4_delegate_enable}

  let issuance_weights issuance_weights (c : t) = {c with issuance_weights}

  module Issuance_weights = struct
    let base_total_issued_per_minute base_total_issued_per_minute (c : t) =
      issuance_weights {c.issuance_weights with base_total_issued_per_minute} c
  end

  let adaptive_issuance adaptive_issuance (c : t) = {c with adaptive_issuance}

  module Adaptive_issuance = struct
    let adaptive_rewards_params adaptive_rewards_params (c : t) =
      adaptive_issuance {c.adaptive_issuance with adaptive_rewards_params} c

    module Adaptive_rewards_params = struct
      let max_bonus max_bonus (c : t) =
        adaptive_rewards_params
          {c.adaptive_issuance.adaptive_rewards_params with max_bonus}
          c

      let issuance_ratio_final_min issuance_ratio_final_min (c : t) =
        adaptive_rewards_params
          {
            c.adaptive_issuance.adaptive_rewards_params with
            issuance_ratio_final_min;
          }
          c

      let issuance_ratio_final_max issuance_ratio_final_max (c : t) =
        adaptive_rewards_params
          {
            c.adaptive_issuance.adaptive_rewards_params with
            issuance_ratio_final_max;
          }
          c

      let issuance_ratio_initial_min issuance_ratio_initial_min (c : t) =
        adaptive_rewards_params
          {
            c.adaptive_issuance.adaptive_rewards_params with
            issuance_ratio_initial_min;
          }
          c

      let issuance_ratio_initial_max issuance_ratio_initial_max (c : t) =
        adaptive_rewards_params
          {
            c.adaptive_issuance.adaptive_rewards_params with
            issuance_ratio_initial_max;
          }
          c
    end
  end
end
