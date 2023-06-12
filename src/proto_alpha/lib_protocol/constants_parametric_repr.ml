(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
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

type dal = {
  feature_enable : bool;
  number_of_slots : int;
  attestation_lag : int;
  attestation_threshold : int;
  blocks_per_epoch : int32;
  cryptobox_parameters : Dal.parameters;
}

let dal_encoding =
  let open Data_encoding in
  conv
    (fun {
           feature_enable;
           number_of_slots;
           attestation_lag;
           attestation_threshold;
           cryptobox_parameters;
           blocks_per_epoch;
         } ->
      ( ( feature_enable,
          number_of_slots,
          attestation_lag,
          attestation_threshold,
          blocks_per_epoch ),
        cryptobox_parameters ))
    (fun ( ( feature_enable,
             number_of_slots,
             attestation_lag,
             attestation_threshold,
             blocks_per_epoch ),
           cryptobox_parameters ) ->
      {
        feature_enable;
        number_of_slots;
        attestation_lag;
        attestation_threshold;
        blocks_per_epoch;
        cryptobox_parameters;
      })
    (merge_objs
       (obj5
          (req "feature_enable" bool)
          (req "number_of_slots" int16)
          (req "attestation_lag" int16)
          (req "attestation_threshold" int16)
          (req "blocks_per_epoch" int32))
       Dal.parameters_encoding)

(* The encoded representation of this type is stored in the context as
   bytes. Changing the encoding, or the value of these constants from
   the previous protocol may break the context migration, or (even
   worse) yield an incorrect context after migration.

   If you change this encoding compared to `Constants_parametric_previous_repr.t`,
   you should ensure that there is a proper migration of the constants
   during context migration. See: `Raw_context.prepare_first_block` *)

type tx_rollup = {
  enable : bool;
  origination_size : int;
  hard_size_limit_per_inbox : int;
  hard_size_limit_per_message : int;
  commitment_bond : Tez_repr.t;
  finality_period : int;
  withdraw_period : int;
  max_inboxes_count : int;
  max_messages_per_inbox : int;
  max_commitments_count : int;
  cost_per_byte_ema_factor : int;
  max_ticket_payload_size : int;
  max_withdrawals_per_batch : int;
  rejection_max_proof_size : int;
  sunset_level : int32;
}

type sc_rollup = {
  enable : bool;
  arith_pvm_enable : bool;
  origination_size : int;
  challenge_window_in_blocks : int;
  stake_amount : Tez_repr.t;
  commitment_period_in_blocks : int;
  max_lookahead_in_blocks : int32;
  max_active_outbox_levels : int32;
  max_outbox_messages_per_level : int;
  number_of_sections_in_dissection : int;
  timeout_period_in_blocks : int;
  max_number_of_stored_cemented_commitments : int;
  max_number_of_parallel_games : int;
}

type zk_rollup = {
  enable : bool;
  origination_size : int;
  min_pending_to_process : int;
}

type adaptive_inflation = {
  enable : bool;
  staking_over_baking_limit : int;
  max_costaking_baker_count : int;
  staking_over_delegation_edge : int;
  launch_ema_threshold : int32;
}

type reward_weights = {
  base_total_rewards_per_minute : Tez_repr.t;
  baking_reward_fixed_portion_weight : int;
  baking_reward_bonus_weight : int;
  endorsing_reward_weight : int;
  liquidity_baking_subsidy_weight : int;
  seed_nonce_revelation_tip_weight : int;
  vdf_revelation_tip_weight : int;
}

type t = {
  preserved_cycles : int;
  blocks_per_cycle : int32;
  blocks_per_commitment : int32;
  nonce_revelation_threshold : int32;
  blocks_per_stake_snapshot : int32;
  cycles_per_voting_period : int32;
  hard_gas_limit_per_operation : Gas_limit_repr.Arith.integral;
  hard_gas_limit_per_block : Gas_limit_repr.Arith.integral;
  proof_of_work_threshold : int64;
  minimal_stake : Tez_repr.t;
  vdf_difficulty : int64;
  origination_size : int;
  reward_weights : reward_weights;
  cost_per_byte : Tez_repr.t;
  hard_storage_limit_per_operation : Z.t;
  quorum_min : int32;
  quorum_max : int32;
  min_proposal_quorum : int32;
  liquidity_baking_toggle_ema_threshold : int32;
  max_operations_time_to_live : int;
  minimal_block_delay : Period_repr.t;
  delay_increment_per_round : Period_repr.t;
  minimal_participation_ratio : Ratio_repr.t;
  consensus_committee_size : int;
  consensus_threshold : int;
  max_slashing_period : int;
  delegation_over_baking_limit : int;
  percentage_of_frozen_deposits_slashed_per_double_baking : int;
  percentage_of_frozen_deposits_slashed_per_double_endorsement : int;
  testnet_dictator : Signature.Public_key_hash.t option;
  initial_seed : State_hash.t option;
  (* If a new cache is added, please also modify the
     [cache_layout_size] value. *)
  cache_script_size : int;
  cache_stake_distribution_cycles : int;
  cache_sampler_state_cycles : int;
  tx_rollup : tx_rollup;
  dal : dal;
  sc_rollup : sc_rollup;
  zk_rollup : zk_rollup;
  adaptive_inflation : adaptive_inflation;
}

let tx_rollup_encoding =
  let open Data_encoding in
  conv
    (fun (c : tx_rollup) ->
      ( ( c.enable,
          c.origination_size,
          c.hard_size_limit_per_inbox,
          c.hard_size_limit_per_message,
          c.max_withdrawals_per_batch,
          c.commitment_bond,
          c.finality_period,
          c.withdraw_period,
          c.max_inboxes_count,
          c.max_messages_per_inbox ),
        ( c.max_commitments_count,
          c.cost_per_byte_ema_factor,
          c.max_ticket_payload_size,
          c.rejection_max_proof_size,
          c.sunset_level ) ))
    (fun ( ( tx_rollup_enable,
             tx_rollup_origination_size,
             tx_rollup_hard_size_limit_per_inbox,
             tx_rollup_hard_size_limit_per_message,
             tx_rollup_max_withdrawals_per_batch,
             tx_rollup_commitment_bond,
             tx_rollup_finality_period,
             tx_rollup_withdraw_period,
             tx_rollup_max_inboxes_count,
             tx_rollup_max_messages_per_inbox ),
           ( tx_rollup_max_commitments_count,
             tx_rollup_cost_per_byte_ema_factor,
             tx_rollup_max_ticket_payload_size,
             tx_rollup_rejection_max_proof_size,
             tx_rollup_sunset_level ) ) ->
      {
        enable = tx_rollup_enable;
        origination_size = tx_rollup_origination_size;
        hard_size_limit_per_inbox = tx_rollup_hard_size_limit_per_inbox;
        hard_size_limit_per_message = tx_rollup_hard_size_limit_per_message;
        max_withdrawals_per_batch = tx_rollup_max_withdrawals_per_batch;
        commitment_bond = tx_rollup_commitment_bond;
        finality_period = tx_rollup_finality_period;
        withdraw_period = tx_rollup_withdraw_period;
        max_inboxes_count = tx_rollup_max_inboxes_count;
        max_messages_per_inbox = tx_rollup_max_messages_per_inbox;
        max_commitments_count = tx_rollup_max_commitments_count;
        cost_per_byte_ema_factor = tx_rollup_cost_per_byte_ema_factor;
        max_ticket_payload_size = tx_rollup_max_ticket_payload_size;
        rejection_max_proof_size = tx_rollup_rejection_max_proof_size;
        sunset_level = tx_rollup_sunset_level;
      })
    (merge_objs
       (obj10
          (req "tx_rollup_enable" bool)
          (req "tx_rollup_origination_size" int31)
          (req "tx_rollup_hard_size_limit_per_inbox" int31)
          (req "tx_rollup_hard_size_limit_per_message" int31)
          (req "tx_rollup_max_withdrawals_per_batch" int31)
          (req "tx_rollup_commitment_bond" Tez_repr.encoding)
          (req "tx_rollup_finality_period" int31)
          (req "tx_rollup_withdraw_period" int31)
          (req "tx_rollup_max_inboxes_count" int31)
          (req "tx_rollup_max_messages_per_inbox" int31))
       (obj5
          (req "tx_rollup_max_commitments_count" int31)
          (req "tx_rollup_cost_per_byte_ema_factor" int31)
          (req "tx_rollup_max_ticket_payload_size" int31)
          (req "tx_rollup_rejection_max_proof_size" int31)
          (req "tx_rollup_sunset_level" int32)))

let sc_rollup_encoding =
  let open Data_encoding in
  conv
    (fun (c : sc_rollup) ->
      ( ( c.enable,
          c.arith_pvm_enable,
          c.origination_size,
          c.challenge_window_in_blocks,
          c.stake_amount,
          c.commitment_period_in_blocks,
          c.max_lookahead_in_blocks,
          c.max_active_outbox_levels,
          c.max_outbox_messages_per_level ),
        ( c.number_of_sections_in_dissection,
          c.timeout_period_in_blocks,
          c.max_number_of_stored_cemented_commitments,
          c.max_number_of_parallel_games ) ))
    (fun ( ( sc_rollup_enable,
             sc_rollup_arith_pvm_enable,
             sc_rollup_origination_size,
             sc_rollup_challenge_window_in_blocks,
             sc_rollup_stake_amount,
             sc_rollup_commitment_period_in_blocks,
             sc_rollup_max_lookahead_in_blocks,
             sc_rollup_max_active_outbox_levels,
             sc_rollup_max_outbox_messages_per_level ),
           ( sc_rollup_number_of_sections_in_dissection,
             sc_rollup_timeout_period_in_blocks,
             sc_rollup_max_number_of_cemented_commitments,
             sc_rollup_max_number_of_parallel_games ) ) ->
      {
        enable = sc_rollup_enable;
        arith_pvm_enable = sc_rollup_arith_pvm_enable;
        origination_size = sc_rollup_origination_size;
        challenge_window_in_blocks = sc_rollup_challenge_window_in_blocks;
        stake_amount = sc_rollup_stake_amount;
        commitment_period_in_blocks = sc_rollup_commitment_period_in_blocks;
        max_lookahead_in_blocks = sc_rollup_max_lookahead_in_blocks;
        max_active_outbox_levels = sc_rollup_max_active_outbox_levels;
        max_outbox_messages_per_level = sc_rollup_max_outbox_messages_per_level;
        number_of_sections_in_dissection =
          sc_rollup_number_of_sections_in_dissection;
        timeout_period_in_blocks = sc_rollup_timeout_period_in_blocks;
        max_number_of_stored_cemented_commitments =
          sc_rollup_max_number_of_cemented_commitments;
        max_number_of_parallel_games = sc_rollup_max_number_of_parallel_games;
      })
    (merge_objs
       (obj9
          (req "smart_rollup_enable" bool)
          (req "smart_rollup_arith_pvm_enable" bool)
          (req "smart_rollup_origination_size" int31)
          (req "smart_rollup_challenge_window_in_blocks" int31)
          (req "smart_rollup_stake_amount" Tez_repr.encoding)
          (req "smart_rollup_commitment_period_in_blocks" int31)
          (req "smart_rollup_max_lookahead_in_blocks" int32)
          (req "smart_rollup_max_active_outbox_levels" int32)
          (req "smart_rollup_max_outbox_messages_per_level" int31))
       (obj4
          (req "smart_rollup_number_of_sections_in_dissection" uint8)
          (req "smart_rollup_timeout_period_in_blocks" int31)
          (req "smart_rollup_max_number_of_cemented_commitments" int31)
          (req "smart_rollup_max_number_of_parallel_games" int31)))

let zk_rollup_encoding =
  let open Data_encoding in
  conv
    (fun ({enable; origination_size; min_pending_to_process} : zk_rollup) ->
      (enable, origination_size, min_pending_to_process))
    (fun ( zk_rollup_enable,
           zk_rollup_origination_size,
           zk_rollup_min_pending_to_process ) ->
      {
        enable = zk_rollup_enable;
        origination_size = zk_rollup_origination_size;
        min_pending_to_process = zk_rollup_min_pending_to_process;
      })
    (obj3
       (req "zk_rollup_enable" bool)
       (req "zk_rollup_origination_size" int31)
       (req "zk_rollup_min_pending_to_process" int31))

let adaptive_inflation_encoding =
  let open Data_encoding in
  conv
    (fun {
           enable;
           staking_over_baking_limit;
           max_costaking_baker_count;
           staking_over_delegation_edge;
           launch_ema_threshold;
         } ->
      ( enable,
        staking_over_baking_limit,
        max_costaking_baker_count,
        staking_over_delegation_edge,
        launch_ema_threshold ))
    (fun ( adaptive_inflation_enable,
           staking_over_baking_limit,
           max_costaking_baker_count,
           staking_over_delegation_edge,
           launch_ema_threshold ) ->
      {
        enable = adaptive_inflation_enable;
        staking_over_baking_limit;
        max_costaking_baker_count;
        staking_over_delegation_edge;
        launch_ema_threshold;
      })
    (obj5
       (req "adaptive_inflation_enable" bool)
       (req "staking_over_baking_limit" uint8)
       (req "max_costaking_baker_count" uint16)
       (req "staking_over_delegation_edge" uint8)
       (req "adaptive_inflation_launch_ema_threshold" int32))

let reward_weights_encoding =
  let open Data_encoding in
  conv
    (fun ({
            base_total_rewards_per_minute;
            baking_reward_fixed_portion_weight;
            baking_reward_bonus_weight;
            endorsing_reward_weight;
            liquidity_baking_subsidy_weight;
            seed_nonce_revelation_tip_weight;
            vdf_revelation_tip_weight;
          } :
           reward_weights) ->
      ( base_total_rewards_per_minute,
        baking_reward_fixed_portion_weight,
        baking_reward_bonus_weight,
        endorsing_reward_weight,
        liquidity_baking_subsidy_weight,
        seed_nonce_revelation_tip_weight,
        vdf_revelation_tip_weight ))
    (fun ( base_total_rewards_per_minute,
           baking_reward_fixed_portion_weight,
           baking_reward_bonus_weight,
           endorsing_reward_weight,
           liquidity_baking_subsidy_weight,
           seed_nonce_revelation_tip_weight,
           vdf_revelation_tip_weight ) ->
      {
        base_total_rewards_per_minute;
        baking_reward_fixed_portion_weight;
        baking_reward_bonus_weight;
        endorsing_reward_weight;
        liquidity_baking_subsidy_weight;
        seed_nonce_revelation_tip_weight;
        vdf_revelation_tip_weight;
      })
    (obj7
       (req "base_total_rewards_per_minute" Tez_repr.encoding)
       (req "baking_reward_fixed_portion_weight" int31)
       (req "baking_reward_bonus_weight" int31)
       (req "endorsing_reward_weight" int31)
       (req "liquidity_baking_subsidy_weight" int31)
       (req "seed_nonce_revelation_tip_weight" int31)
       (req "vdf_revelation_tip_weight" int31))

let encoding =
  let open Data_encoding in
  conv
    (fun c ->
      ( ( c.preserved_cycles,
          c.blocks_per_cycle,
          c.blocks_per_commitment,
          c.nonce_revelation_threshold,
          c.blocks_per_stake_snapshot,
          c.cycles_per_voting_period,
          c.hard_gas_limit_per_operation,
          c.hard_gas_limit_per_block,
          c.proof_of_work_threshold,
          c.minimal_stake ),
        ( ( c.vdf_difficulty,
            c.origination_size,
            c.reward_weights,
            c.cost_per_byte,
            c.hard_storage_limit_per_operation,
            c.quorum_min ),
          ( ( c.quorum_max,
              c.min_proposal_quorum,
              c.liquidity_baking_toggle_ema_threshold,
              c.max_operations_time_to_live,
              c.minimal_block_delay,
              c.delay_increment_per_round,
              c.consensus_committee_size,
              c.consensus_threshold ),
            ( ( c.minimal_participation_ratio,
                c.max_slashing_period,
                c.delegation_over_baking_limit,
                c.percentage_of_frozen_deposits_slashed_per_double_baking,
                c.percentage_of_frozen_deposits_slashed_per_double_endorsement,
                c.testnet_dictator,
                c.initial_seed ),
              ( ( c.cache_script_size,
                  c.cache_stake_distribution_cycles,
                  c.cache_sampler_state_cycles ),
                ( c.tx_rollup,
                  (c.dal, ((c.sc_rollup, c.zk_rollup), c.adaptive_inflation)) )
              ) ) ) ) ))
    (fun ( ( preserved_cycles,
             blocks_per_cycle,
             blocks_per_commitment,
             nonce_revelation_threshold,
             blocks_per_stake_snapshot,
             cycles_per_voting_period,
             hard_gas_limit_per_operation,
             hard_gas_limit_per_block,
             proof_of_work_threshold,
             minimal_stake ),
           ( ( vdf_difficulty,
               origination_size,
               reward_weights,
               cost_per_byte,
               hard_storage_limit_per_operation,
               quorum_min ),
             ( ( quorum_max,
                 min_proposal_quorum,
                 liquidity_baking_toggle_ema_threshold,
                 max_operations_time_to_live,
                 minimal_block_delay,
                 delay_increment_per_round,
                 consensus_committee_size,
                 consensus_threshold ),
               ( ( minimal_participation_ratio,
                   max_slashing_period,
                   delegation_over_baking_limit,
                   percentage_of_frozen_deposits_slashed_per_double_baking,
                   percentage_of_frozen_deposits_slashed_per_double_endorsement,
                   testnet_dictator,
                   initial_seed ),
                 ( ( cache_script_size,
                     cache_stake_distribution_cycles,
                     cache_sampler_state_cycles ),
                   ( tx_rollup,
                     (dal, ((sc_rollup, zk_rollup), adaptive_inflation)) ) ) )
             ) ) ) ->
      {
        preserved_cycles;
        blocks_per_cycle;
        blocks_per_commitment;
        nonce_revelation_threshold;
        blocks_per_stake_snapshot;
        cycles_per_voting_period;
        hard_gas_limit_per_operation;
        hard_gas_limit_per_block;
        proof_of_work_threshold;
        minimal_stake;
        vdf_difficulty;
        origination_size;
        reward_weights;
        cost_per_byte;
        hard_storage_limit_per_operation;
        quorum_min;
        quorum_max;
        min_proposal_quorum;
        liquidity_baking_toggle_ema_threshold;
        max_operations_time_to_live;
        minimal_block_delay;
        delay_increment_per_round;
        minimal_participation_ratio;
        max_slashing_period;
        consensus_committee_size;
        consensus_threshold;
        delegation_over_baking_limit;
        percentage_of_frozen_deposits_slashed_per_double_baking;
        percentage_of_frozen_deposits_slashed_per_double_endorsement;
        testnet_dictator;
        initial_seed;
        cache_script_size;
        cache_stake_distribution_cycles;
        cache_sampler_state_cycles;
        tx_rollup;
        dal;
        sc_rollup;
        zk_rollup;
        adaptive_inflation;
      })
    (merge_objs
       (obj10
          (req "preserved_cycles" uint8)
          (req "blocks_per_cycle" int32)
          (req "blocks_per_commitment" int32)
          (req "nonce_revelation_threshold" int32)
          (req "blocks_per_stake_snapshot" int32)
          (req "cycles_per_voting_period" int32)
          (req
             "hard_gas_limit_per_operation"
             Gas_limit_repr.Arith.z_integral_encoding)
          (req
             "hard_gas_limit_per_block"
             Gas_limit_repr.Arith.z_integral_encoding)
          (req "proof_of_work_threshold" int64)
          (req "minimal_stake" Tez_repr.encoding))
       (merge_objs
          (obj6
             (req "vdf_difficulty" int64)
             (req "origination_size" int31)
             (req "reward_weights" reward_weights_encoding)
             (req "cost_per_byte" Tez_repr.encoding)
             (req "hard_storage_limit_per_operation" z)
             (req "quorum_min" int32))
          (merge_objs
             (obj8
                (req "quorum_max" int32)
                (req "min_proposal_quorum" int32)
                (req "liquidity_baking_toggle_ema_threshold" int32)
                (req "max_operations_time_to_live" int16)
                (req "minimal_block_delay" Period_repr.encoding)
                (req "delay_increment_per_round" Period_repr.encoding)
                (req "consensus_committee_size" int31)
                (req "consensus_threshold" int31))
             (merge_objs
                (obj7
                   (req "minimal_participation_ratio" Ratio_repr.encoding)
                   (req "max_slashing_period" int31)
                   (req "delegation_over_baking_limit" uint8)
                   (req
                      "percentage_of_frozen_deposits_slashed_per_double_baking"
                      uint8)
                   (req
                      "percentage_of_frozen_deposits_slashed_per_double_endorsement"
                      uint8)
                   (opt "testnet_dictator" Signature.Public_key_hash.encoding)
                   (opt "initial_seed" State_hash.encoding))
                (merge_objs
                   (obj3
                      (req "cache_script_size" int31)
                      (req "cache_stake_distribution_cycles" int8)
                      (req "cache_sampler_state_cycles" int8))
                   (merge_objs
                      tx_rollup_encoding
                      (merge_objs
                         (obj1 (req "dal_parametric" dal_encoding))
                         (merge_objs
                            (merge_objs sc_rollup_encoding zk_rollup_encoding)
                            adaptive_inflation_encoding))))))))
