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
  incentives_enable : bool;
  number_of_slots : int;
  attestation_lag : int;
  attestation_threshold : int;
  cryptobox_parameters : Dal.parameters;
}

let dal_encoding =
  let open Data_encoding in
  conv
    (fun {
           feature_enable;
           incentives_enable;
           number_of_slots;
           attestation_lag;
           attestation_threshold;
           cryptobox_parameters;
         } ->
      ( ( feature_enable,
          incentives_enable,
          number_of_slots,
          attestation_lag,
          attestation_threshold ),
        cryptobox_parameters ))
    (fun ( ( feature_enable,
             incentives_enable,
             number_of_slots,
             attestation_lag,
             attestation_threshold ),
           cryptobox_parameters ) ->
      {
        feature_enable;
        incentives_enable;
        number_of_slots;
        attestation_lag;
        attestation_threshold;
        cryptobox_parameters;
      })
    (merge_objs
       (obj5
          (req "feature_enable" bool)
          (req "incentives_enable" bool)
          (req "number_of_slots" uint16)
          (req "attestation_lag" uint8)
          (req "attestation_threshold" uint8))
       Dal.parameters_encoding)

(* The encoded representation of this type is stored in the context as
   bytes. Changing the encoding, or the value of these constants from
   the previous protocol may break the context migration, or (even
   worse) yield an incorrect context after migration.

   If you change this encoding compared to `Constants_parametric_previous_repr.t`,
   you should ensure that there is a proper migration of the constants
   during context migration. See: `Raw_context.prepare_first_block` *)

type sc_rollup_reveal_hashing_schemes = {blake2B : Raw_level_repr.t}

type sc_rollup_reveal_activation_level = {
  raw_data : sc_rollup_reveal_hashing_schemes;
  metadata : Raw_level_repr.t;
  dal_page : Raw_level_repr.t;
  dal_parameters : Raw_level_repr.t;
  (* Once a DAL slot is attested, a rollup can only import it within the range
     of levels [attested_level; attested_level +
     dal_attested_slots_validity_lag]. *)
  (* Warning: the semantics of valid slots needs to be adapted if the
     value of this parameter is changed in the future.
     - If it is increased, some attested slots that were outdated with
       the old value will become valid again.
     - If it is decreased, some attested slots that were valid with
       the old value will become outdated.

     In both cases, the status of slots before and after the value change is
     different. Said otherwise, the validity of the slot may differ depending on
     the time of the check, in particular it may be different in the following
     two cases: (a) the slot is imported before the value upgrade, (2) a
     refutation game targeting a page of that slot is started after the
     upgrade. *)
  dal_attested_slots_validity_lag : int;
}

let sc_rollup_reveal_hashing_schemes_encoding =
  let open Data_encoding in
  conv
    (fun t -> t.blake2B)
    (fun blake2B -> {blake2B})
    (obj1 (req "Blake2B" Raw_level_repr.encoding))

let sc_rollup_reveal_activation_level_encoding :
    sc_rollup_reveal_activation_level Data_encoding.t =
  let open Data_encoding in
  conv
    (fun t ->
      ( t.raw_data,
        t.metadata,
        t.dal_page,
        t.dal_parameters,
        t.dal_attested_slots_validity_lag ))
    (fun ( raw_data,
           metadata,
           dal_page,
           dal_parameters,
           dal_attested_slots_validity_lag ) ->
      {
        raw_data;
        metadata;
        dal_page;
        dal_parameters;
        dal_attested_slots_validity_lag;
      })
    (obj5
       (req "raw_data" sc_rollup_reveal_hashing_schemes_encoding)
       (req "metadata" Raw_level_repr.encoding)
       (req "dal_page" Raw_level_repr.encoding)
       (req "dal_parameters" Raw_level_repr.encoding)
       (req "dal_attested_slots_validity_lag" Data_encoding.int31))

type sc_rollup = {
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
  reveal_activation_level : sc_rollup_reveal_activation_level;
  private_enable : bool;
  riscv_pvm_enable : bool;
}

type zk_rollup = {
  enable : bool;
  origination_size : int;
  min_pending_to_process : int;
  max_ticket_payload_size : int;
}

type adaptive_rewards_params = {
  issuance_ratio_final_min : Q.t;
  issuance_ratio_final_max : Q.t;
  issuance_ratio_initial_min : Q.t;
  issuance_ratio_initial_max : Q.t;
  initial_period : int;
  transition_period : int;
  max_bonus : Issuance_bonus_repr.max_bonus;
  growth_rate : Q.t;
  center_dz : Q.t;
  radius_dz : Q.t;
}

type adaptive_issuance = {
  global_limit_of_staking_over_baking : int;
  edge_of_staking_over_delegation : int;
  launch_ema_threshold : int32;
  adaptive_rewards_params : adaptive_rewards_params;
  activation_vote_enable : bool;
  autostaking_enable : bool;
  force_activation : bool;
  ns_enable : bool;
}

type issuance_weights = {
  base_total_issued_per_minute : Tez_repr.t;
  baking_reward_fixed_portion_weight : int;
  baking_reward_bonus_weight : int;
  attesting_reward_weight : int;
  seed_nonce_revelation_tip_weight : int;
  vdf_revelation_tip_weight : int;
}

type t = {
  consensus_rights_delay : int;
  blocks_preservation_cycles : int;
  delegate_parameters_activation_delay : int;
  blocks_per_cycle : int32;
  blocks_per_commitment : int32;
  nonce_revelation_threshold : int32;
  cycles_per_voting_period : int32;
  hard_gas_limit_per_operation : Gas_limit_repr.Arith.integral;
  hard_gas_limit_per_block : Gas_limit_repr.Arith.integral;
  proof_of_work_threshold : int64;
  minimal_stake : Tez_repr.t;
  minimal_frozen_stake : Tez_repr.t;
  vdf_difficulty : int64;
  origination_size : int;
  issuance_weights : issuance_weights;
  cost_per_byte : Tez_repr.t;
  hard_storage_limit_per_operation : Z.t;
  quorum_min : int32;
  quorum_max : int32;
  min_proposal_quorum : int32;
  liquidity_baking_subsidy : Tez_repr.t;
  liquidity_baking_toggle_ema_threshold : int32;
  max_operations_time_to_live : int;
  minimal_block_delay : Period_repr.t;
  delay_increment_per_round : Period_repr.t;
  minimal_participation_ratio : Ratio_repr.t;
  consensus_committee_size : int;
  consensus_threshold : int;
  limit_of_delegation_over_baking : int;
  percentage_of_frozen_deposits_slashed_per_double_baking : Percentage.t;
  percentage_of_frozen_deposits_slashed_per_double_attestation : Percentage.t;
  max_slashing_per_block : Percentage.t;
  max_slashing_threshold : int;
  testnet_dictator : Signature.Public_key_hash.t option;
  initial_seed : State_hash.t option;
  (* If a new cache is added, please also modify the
     [cache_layout_size] value. *)
  cache_script_size : int;
  cache_stake_distribution_cycles : int;
  cache_sampler_state_cycles : int;
  dal : dal;
  sc_rollup : sc_rollup;
  zk_rollup : zk_rollup;
  adaptive_issuance : adaptive_issuance;
  direct_ticket_spending_enable : bool;
}

let sc_rollup_encoding =
  let open Data_encoding in
  conv
    (fun (c : sc_rollup) ->
      ( ( c.arith_pvm_enable,
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
          c.max_number_of_parallel_games,
          c.reveal_activation_level,
          c.private_enable,
          c.riscv_pvm_enable ) ))
    (fun ( ( sc_rollup_arith_pvm_enable,
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
             sc_rollup_max_number_of_parallel_games,
             sc_rollup_reveal_activation_level,
             sc_rollup_private_enable,
             sc_rollup_riscv_pvm_enable ) ) ->
      {
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
        reveal_activation_level = sc_rollup_reveal_activation_level;
        private_enable = sc_rollup_private_enable;
        riscv_pvm_enable = sc_rollup_riscv_pvm_enable;
      })
    (merge_objs
       (obj8
          (req "smart_rollup_arith_pvm_enable" bool)
          (req "smart_rollup_origination_size" int31)
          (req "smart_rollup_challenge_window_in_blocks" int31)
          (req "smart_rollup_stake_amount" Tez_repr.encoding)
          (req "smart_rollup_commitment_period_in_blocks" int31)
          (req "smart_rollup_max_lookahead_in_blocks" int32)
          (req "smart_rollup_max_active_outbox_levels" int32)
          (req "smart_rollup_max_outbox_messages_per_level" int31))
       (obj7
          (req "smart_rollup_number_of_sections_in_dissection" uint8)
          (req "smart_rollup_timeout_period_in_blocks" int31)
          (req "smart_rollup_max_number_of_cemented_commitments" int31)
          (req "smart_rollup_max_number_of_parallel_games" int31)
          (req
             "smart_rollup_reveal_activation_level"
             sc_rollup_reveal_activation_level_encoding)
          (req "smart_rollup_private_enable" bool)
          (req "smart_rollup_riscv_pvm_enable" bool)))

let zk_rollup_encoding =
  let open Data_encoding in
  conv
    (fun ({
            enable;
            origination_size;
            min_pending_to_process;
            max_ticket_payload_size;
          } :
           zk_rollup) ->
      (enable, origination_size, min_pending_to_process, max_ticket_payload_size))
    (fun ( zk_rollup_enable,
           zk_rollup_origination_size,
           zk_rollup_min_pending_to_process,
           zk_rollup_max_ticket_payload_size ) ->
      {
        enable = zk_rollup_enable;
        origination_size = zk_rollup_origination_size;
        min_pending_to_process = zk_rollup_min_pending_to_process;
        max_ticket_payload_size = zk_rollup_max_ticket_payload_size;
      })
    (obj4
       (req "zk_rollup_enable" bool)
       (req "zk_rollup_origination_size" int31)
       (req "zk_rollup_min_pending_to_process" int31)
       (req "zk_rollup_max_ticket_payload_size" int31))

let extremum_encoding =
  Data_encoding.(
    conv_with_guard
      (fun Q.{num; den} -> (num, den))
      (fun (num, den) ->
        if Compare.Z.(num > Z.zero && den > Z.zero) then Ok (Q.make num den)
        else
          Error
            "Invalid Reward Extremum Parameter: only positive values allowed")
      (obj2 (req "numerator" z) (req "denominator" z)))

let center_encoding =
  Data_encoding.(
    conv_with_guard
      (fun Q.{num; den} -> (num, den))
      (fun (num, den) ->
        if Compare.Z.(num >= Z.zero && den > Z.zero && num <= den) then
          Ok (Q.make num den)
        else
          Error
            "Invalid Reward Parameter: dead zone center can only be between 0 \
             and 1")
      (obj2 (req "numerator" z) (req "denominator" z)))

let radius_encoding =
  Data_encoding.(
    conv_with_guard
      (fun Q.{num; den} -> (num, den))
      (fun (num, den) ->
        if Compare.Z.(num >= Z.zero && den > Z.zero) then Ok (Q.make num den)
        else
          Error
            "Invalid Reward Parameter: dead zone radius must be non-negative")
      (obj2 (req "numerator" z) (req "denominator" z)))

let growth_rate_encoding =
  Data_encoding.(
    conv_with_guard
      (fun Q.{num; den} -> (num, den))
      (fun (num, den) ->
        if Compare.Z.(num >= Z.zero && den > Z.zero) then Ok (Q.make num den)
        else Error "Invalid Reward Parameter: growth rate must be non-negative")
      (obj2 (req "numerator" z) (req "denominator" z)))

let adaptive_rewards_params_encoding =
  let open Data_encoding in
  conv
    (fun {
           issuance_ratio_final_min;
           issuance_ratio_final_max;
           issuance_ratio_initial_min;
           issuance_ratio_initial_max;
           initial_period;
           transition_period;
           max_bonus;
           growth_rate;
           center_dz;
           radius_dz;
         } ->
      ( issuance_ratio_final_min,
        issuance_ratio_final_max,
        issuance_ratio_initial_min,
        issuance_ratio_initial_max,
        initial_period,
        transition_period,
        max_bonus,
        growth_rate,
        center_dz,
        radius_dz ))
    (fun ( issuance_ratio_final_min,
           issuance_ratio_final_max,
           issuance_ratio_initial_min,
           issuance_ratio_initial_max,
           initial_period,
           transition_period,
           max_bonus,
           growth_rate,
           center_dz,
           radius_dz ) ->
      {
        issuance_ratio_final_min;
        issuance_ratio_final_max;
        issuance_ratio_initial_min;
        issuance_ratio_initial_max;
        initial_period;
        transition_period;
        max_bonus;
        growth_rate;
        center_dz;
        radius_dz;
      })
    (obj10
       (req "issuance_ratio_final_min" extremum_encoding)
       (req "issuance_ratio_final_max" extremum_encoding)
       (req "issuance_ratio_initial_min" extremum_encoding)
       (req "issuance_ratio_initial_max" extremum_encoding)
       (req "initial_period" uint8)
       (req "transition_period" uint8)
       (req "max_bonus" Issuance_bonus_repr.max_bonus_encoding)
       (req "growth_rate" growth_rate_encoding)
       (req "center_dz" center_encoding)
       (req "radius_dz" radius_encoding))

let adaptive_issuance_encoding =
  let open Data_encoding in
  conv
    (fun {
           global_limit_of_staking_over_baking;
           edge_of_staking_over_delegation;
           launch_ema_threshold;
           adaptive_rewards_params;
           activation_vote_enable;
           autostaking_enable;
           force_activation;
           ns_enable;
         } ->
      ( global_limit_of_staking_over_baking,
        edge_of_staking_over_delegation,
        launch_ema_threshold,
        adaptive_rewards_params,
        activation_vote_enable,
        autostaking_enable,
        force_activation,
        ns_enable ))
    (fun ( global_limit_of_staking_over_baking,
           edge_of_staking_over_delegation,
           launch_ema_threshold,
           adaptive_rewards_params,
           activation_vote_enable,
           autostaking_enable,
           force_activation,
           ns_enable ) ->
      {
        global_limit_of_staking_over_baking;
        edge_of_staking_over_delegation;
        launch_ema_threshold;
        adaptive_rewards_params;
        activation_vote_enable;
        autostaking_enable;
        force_activation;
        ns_enable;
      })
    (obj8
       (req "global_limit_of_staking_over_baking" uint8)
       (req "edge_of_staking_over_delegation" uint8)
       (req "adaptive_issuance_launch_ema_threshold" int32)
       (req "adaptive_rewards_params" adaptive_rewards_params_encoding)
       (req "adaptive_issuance_activation_vote_enable" bool)
       (req "autostaking_enable" bool)
       (req "adaptive_issuance_force_activation" bool)
       (req "ns_enable" bool))

let issuance_weights_encoding =
  let open Data_encoding in
  conv
    (fun ({
            base_total_issued_per_minute;
            baking_reward_fixed_portion_weight;
            baking_reward_bonus_weight;
            attesting_reward_weight;
            seed_nonce_revelation_tip_weight;
            vdf_revelation_tip_weight;
          } :
           issuance_weights) ->
      ( base_total_issued_per_minute,
        baking_reward_fixed_portion_weight,
        baking_reward_bonus_weight,
        attesting_reward_weight,
        seed_nonce_revelation_tip_weight,
        vdf_revelation_tip_weight ))
    (fun ( base_total_issued_per_minute,
           baking_reward_fixed_portion_weight,
           baking_reward_bonus_weight,
           attesting_reward_weight,
           seed_nonce_revelation_tip_weight,
           vdf_revelation_tip_weight ) ->
      {
        base_total_issued_per_minute;
        baking_reward_fixed_portion_weight;
        baking_reward_bonus_weight;
        attesting_reward_weight;
        seed_nonce_revelation_tip_weight;
        vdf_revelation_tip_weight;
      })
    (obj6
       (req "base_total_issued_per_minute" Tez_repr.encoding)
       (req "baking_reward_fixed_portion_weight" int31)
       (req "baking_reward_bonus_weight" int31)
       (req "attesting_reward_weight" int31)
       (req "seed_nonce_revelation_tip_weight" int31)
       (req "vdf_revelation_tip_weight" int31))

let encoding =
  let open Data_encoding in
  conv
    (fun c ->
      ( ( ( c.consensus_rights_delay,
            c.blocks_preservation_cycles,
            c.delegate_parameters_activation_delay ),
          ( c.blocks_per_cycle,
            c.blocks_per_commitment,
            c.nonce_revelation_threshold,
            c.cycles_per_voting_period,
            c.hard_gas_limit_per_operation,
            c.hard_gas_limit_per_block,
            c.proof_of_work_threshold,
            c.minimal_stake ) ),
        ( ( c.minimal_frozen_stake,
            c.vdf_difficulty,
            c.origination_size,
            c.issuance_weights,
            c.cost_per_byte,
            c.hard_storage_limit_per_operation,
            c.quorum_min ),
          ( ( c.quorum_max,
              c.min_proposal_quorum,
              c.liquidity_baking_subsidy,
              c.liquidity_baking_toggle_ema_threshold,
              c.max_operations_time_to_live,
              c.minimal_block_delay,
              c.delay_increment_per_round,
              c.consensus_committee_size,
              c.consensus_threshold ),
            ( ( c.minimal_participation_ratio,
                c.limit_of_delegation_over_baking,
                c.percentage_of_frozen_deposits_slashed_per_double_baking,
                c.percentage_of_frozen_deposits_slashed_per_double_attestation,
                c.max_slashing_per_block,
                c.max_slashing_threshold,
                c.testnet_dictator,
                c.initial_seed ),
              ( ( c.cache_script_size,
                  c.cache_stake_distribution_cycles,
                  c.cache_sampler_state_cycles ),
                ( c.dal,
                  ( (c.sc_rollup, c.zk_rollup),
                    (c.adaptive_issuance, c.direct_ticket_spending_enable) ) )
              ) ) ) ) ))
    (fun ( ( ( consensus_rights_delay,
               blocks_preservation_cycles,
               delegate_parameters_activation_delay ),
             ( blocks_per_cycle,
               blocks_per_commitment,
               nonce_revelation_threshold,
               cycles_per_voting_period,
               hard_gas_limit_per_operation,
               hard_gas_limit_per_block,
               proof_of_work_threshold,
               minimal_stake ) ),
           ( ( minimal_frozen_stake,
               vdf_difficulty,
               origination_size,
               issuance_weights,
               cost_per_byte,
               hard_storage_limit_per_operation,
               quorum_min ),
             ( ( quorum_max,
                 min_proposal_quorum,
                 liquidity_baking_subsidy,
                 liquidity_baking_toggle_ema_threshold,
                 max_operations_time_to_live,
                 minimal_block_delay,
                 delay_increment_per_round,
                 consensus_committee_size,
                 consensus_threshold ),
               ( ( minimal_participation_ratio,
                   limit_of_delegation_over_baking,
                   percentage_of_frozen_deposits_slashed_per_double_baking,
                   percentage_of_frozen_deposits_slashed_per_double_attestation,
                   max_slashing_per_block,
                   max_slashing_threshold,
                   testnet_dictator,
                   initial_seed ),
                 ( ( cache_script_size,
                     cache_stake_distribution_cycles,
                     cache_sampler_state_cycles ),
                   ( dal,
                     ( (sc_rollup, zk_rollup),
                       (adaptive_issuance, direct_ticket_spending_enable) ) ) )
               ) ) ) ) ->
      {
        consensus_rights_delay;
        blocks_preservation_cycles;
        delegate_parameters_activation_delay;
        blocks_per_cycle;
        blocks_per_commitment;
        nonce_revelation_threshold;
        cycles_per_voting_period;
        hard_gas_limit_per_operation;
        hard_gas_limit_per_block;
        proof_of_work_threshold;
        minimal_stake;
        minimal_frozen_stake;
        vdf_difficulty;
        origination_size;
        issuance_weights;
        cost_per_byte;
        hard_storage_limit_per_operation;
        quorum_min;
        quorum_max;
        min_proposal_quorum;
        liquidity_baking_subsidy;
        liquidity_baking_toggle_ema_threshold;
        max_operations_time_to_live;
        minimal_block_delay;
        delay_increment_per_round;
        minimal_participation_ratio;
        consensus_committee_size;
        consensus_threshold;
        limit_of_delegation_over_baking;
        percentage_of_frozen_deposits_slashed_per_double_baking;
        percentage_of_frozen_deposits_slashed_per_double_attestation;
        max_slashing_per_block;
        max_slashing_threshold;
        testnet_dictator;
        initial_seed;
        cache_script_size;
        cache_stake_distribution_cycles;
        cache_sampler_state_cycles;
        dal;
        sc_rollup;
        zk_rollup;
        adaptive_issuance;
        direct_ticket_spending_enable;
      })
    (merge_objs
       (merge_objs
          (obj3
             (req "consensus_rights_delay" uint8)
             (req "blocks_preservation_cycles" uint8)
             (req "delegate_parameters_activation_delay" uint8))
          (obj8
             (req "blocks_per_cycle" int32)
             (req "blocks_per_commitment" int32)
             (req "nonce_revelation_threshold" int32)
             (req "cycles_per_voting_period" int32)
             (req
                "hard_gas_limit_per_operation"
                Gas_limit_repr.Arith.z_integral_encoding)
             (req
                "hard_gas_limit_per_block"
                Gas_limit_repr.Arith.z_integral_encoding)
             (req "proof_of_work_threshold" int64)
             (req "minimal_stake" Tez_repr.encoding)))
       (merge_objs
          (obj7
             (req "minimal_frozen_stake" Tez_repr.encoding)
             (req "vdf_difficulty" int64)
             (req "origination_size" int31)
             (req "issuance_weights" issuance_weights_encoding)
             (req "cost_per_byte" Tez_repr.encoding)
             (req "hard_storage_limit_per_operation" z)
             (req "quorum_min" int32))
          (merge_objs
             (obj9
                (req "quorum_max" int32)
                (req "min_proposal_quorum" int32)
                (req "liquidity_baking_subsidy" Tez_repr.encoding)
                (req "liquidity_baking_toggle_ema_threshold" int32)
                (req "max_operations_time_to_live" int16)
                (req "minimal_block_delay" Period_repr.encoding)
                (req "delay_increment_per_round" Period_repr.encoding)
                (req "consensus_committee_size" int31)
                (req "consensus_threshold" int31))
             (merge_objs
                (obj8
                   (req "minimal_participation_ratio" Ratio_repr.encoding)
                   (req "limit_of_delegation_over_baking" uint8)
                   (req
                      "percentage_of_frozen_deposits_slashed_per_double_baking"
                      Percentage.encoding)
                   (req
                      "percentage_of_frozen_deposits_slashed_per_double_attestation"
                      Percentage.encoding)
                   (req "max_slashing_per_block" Percentage.encoding)
                   (req "max_slashing_threshold" int31)
                   (opt "testnet_dictator" Signature.Public_key_hash.encoding)
                   (opt "initial_seed" State_hash.encoding))
                (merge_objs
                   (obj3
                      (req "cache_script_size" int31)
                      (req "cache_stake_distribution_cycles" int8)
                      (req "cache_sampler_state_cycles" int8))
                   (merge_objs
                      (obj1 (req "dal_parametric" dal_encoding))
                      (merge_objs
                         (merge_objs sc_rollup_encoding zk_rollup_encoding)
                         (merge_objs
                            adaptive_issuance_encoding
                            (obj1 (req "direct_ticket_spending_enable" bool))))))))))

let update_sc_rollup_parameter ~block_time c =
  let seconds_in_a_day = 60 * 60 * 24 in
  let seconds_in_a_week = seconds_in_a_day * 7 in
  {
    c with
    challenge_window_in_blocks = seconds_in_a_week * 2 / block_time;
    (* Same as challenge_window_in_blocks *)
    max_active_outbox_levels = Int32.of_int (seconds_in_a_week * 2 / block_time);
    commitment_period_in_blocks = 60 * 15 / block_time;
    max_lookahead_in_blocks = Int32.of_int (seconds_in_a_day * 30 / block_time);
    timeout_period_in_blocks = seconds_in_a_week / block_time;
  }

module Internal_for_tests = struct
  let sc_rollup_encoding = sc_rollup_encoding
end
