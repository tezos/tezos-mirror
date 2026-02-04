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

let q_encoding cond error_msg =
  Data_encoding.(
    conv_with_guard
      (fun Q.{num; den} -> (num, den))
      (fun (num, den) ->
        if Compare.Z.(den > Z.zero) && cond num den then Ok (Q.make num den)
        else Error error_msg)
      (obj2 (req "numerator" z) (req "denominator" z)))

let positive_q_encoding = q_encoding (fun num _den -> Compare.Z.(num > Z.zero))

let non_negative_q_encoding =
  q_encoding (fun num _den -> Compare.Z.(num >= Z.zero))

let between_zero_and_one_q_encoding =
  q_encoding (fun num den -> Compare.Z.(num >= Z.zero && num <= den))

let between_zero_and_excluding_one_q_encoding =
  q_encoding (fun num den -> Compare.Z.(num >= Z.zero && num < den))

type dal = {
  feature_enable : bool;
  incentives_enable : bool;
  dynamic_lag_enable : bool;
  number_of_slots : int;
  attestation_lag : int;
  attestation_lags : int list;
  attestation_threshold : int;
  cryptobox_parameters : Dal.parameters;
  minimal_participation_ratio : Q.t;
  rewards_ratio : Q.t;
  traps_fraction : Q.t;
}

type past_dal_parameters = {
  dal_parameters : dal;
  next_protocol_activation : Raw_level_repr.t;
      (** This is the migration level up to which the parameters were used. *)
}

let minimal_participation_ratio_encoding =
  between_zero_and_one_q_encoding
    "dal.minimal_participation_ratio must be a value between zero and one"

let rewards_ratio_encoding =
  between_zero_and_excluding_one_q_encoding
    "dal.rewards_ratio must be a value between zero (inclusive) and one \
     (exclusive)"

let traps_fraction_encoding =
  between_zero_and_one_q_encoding
    "traps_fraction must be a value between zero and one"

let dal_encoding =
  let open Data_encoding in
  conv
    (fun {
           feature_enable;
           incentives_enable;
           dynamic_lag_enable;
           number_of_slots;
           attestation_lag;
           attestation_lags;
           attestation_threshold;
           cryptobox_parameters;
           minimal_participation_ratio;
           rewards_ratio;
           traps_fraction;
         }
       ->
      ( ( feature_enable,
          incentives_enable,
          dynamic_lag_enable,
          number_of_slots,
          attestation_lag,
          attestation_lags,
          attestation_threshold,
          minimal_participation_ratio,
          rewards_ratio,
          traps_fraction ),
        cryptobox_parameters ))
    (fun ( ( feature_enable,
             incentives_enable,
             dynamic_lag_enable,
             number_of_slots,
             attestation_lag,
             attestation_lags,
             attestation_threshold,
             minimal_participation_ratio,
             rewards_ratio,
             traps_fraction ),
           cryptobox_parameters )
       ->
      {
        feature_enable;
        incentives_enable;
        dynamic_lag_enable;
        number_of_slots;
        attestation_lag;
        attestation_lags;
        attestation_threshold;
        cryptobox_parameters;
        minimal_participation_ratio;
        rewards_ratio;
        traps_fraction;
      })
    (merge_objs
       (obj10
          (req "feature_enable" bool)
          (req "incentives_enable" bool)
          (req "dynamic_lag_enable" bool)
          (req "number_of_slots" uint16)
          (req "attestation_lag" uint8)
          (req "attestation_lags" (list uint8))
          (req "attestation_threshold" uint8)
          (req
             "minimal_participation_ratio"
             minimal_participation_ratio_encoding)
          (req "rewards_ratio" rewards_ratio_encoding)
          (req "traps_fraction" traps_fraction_encoding))
       Dal.parameters_encoding)

let past_dal_parameters_encoding =
  let open Data_encoding in
  conv
    (fun {dal_parameters; next_protocol_activation} ->
      (dal_parameters, next_protocol_activation))
    (fun (dal_parameters, next_protocol_activation) ->
      {dal_parameters; next_protocol_activation})
    (obj2
       (req "dal_parameters" dal_encoding)
       (req "next_protocol_activation" Raw_level_repr.encoding))

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
           dal_attested_slots_validity_lag )
       ->
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
  adaptive_rewards_params : adaptive_rewards_params;
}

type issuance_weights = {
  base_total_issued_per_minute : Tez_repr.t;
  baking_reward_fixed_portion_weight : int;
  baking_reward_bonus_weight : int;
  attesting_reward_weight : int;
  seed_nonce_revelation_tip_weight : int;
  vdf_revelation_tip_weight : int;
  dal_rewards_weight : int;
}

type t = {
  consensus_rights_delay : int;
  blocks_preservation_cycles : int;
  delegate_parameters_activation_delay : int;
  tolerated_inactivity_period : int;
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
  consensus_threshold_size : int;
  limit_of_delegation_over_baking : int;
  percentage_of_frozen_deposits_slashed_per_double_baking : Percentage.t;
  max_slashing_per_block : Percentage.t;
  max_slashing_threshold : Ratio_repr.t;
  testnet_dictator : Signature.Public_key_hash.t option;
  initial_seed : State_hash.t option;
  (* If a new cache is added, please also modify the
     [cache_layout_size] value. *)
  cache_script_size : int;
  cache_stake_distribution_cycles : int;
  cache_sampler_state_cycles : int;
  cache_stake_info_cycles : int;
  dal : dal;
  sc_rollup : sc_rollup;
  zk_rollup : zk_rollup;
  adaptive_issuance : adaptive_issuance;
  direct_ticket_spending_enable : bool;
  aggregate_attestation : bool;
  allow_tz4_delegate_enable : bool;
  all_bakers_attest_activation_threshold : Ratio_repr.t;
  native_contracts_enable : bool;
  swrr_new_baker_lottery_enable : bool;
  tz5_account_enable : bool;
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
             sc_rollup_riscv_pvm_enable ) )
       ->
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
           zk_rollup)
       ->
      (enable, origination_size, min_pending_to_process, max_ticket_payload_size))
    (fun ( zk_rollup_enable,
           zk_rollup_origination_size,
           zk_rollup_min_pending_to_process,
           zk_rollup_max_ticket_payload_size )
       ->
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
  positive_q_encoding
    "Invalid Reward Extremum Parameter: only positive values allowed"

let center_encoding =
  between_zero_and_one_q_encoding
    "Invalid Reward Parameter: dead zone center can only be between 0 and 1"

let radius_encoding =
  non_negative_q_encoding
    "Invalid Reward Parameter: dead zone radius must be non-negative"

let growth_rate_encoding =
  non_negative_q_encoding
    "Invalid Reward Parameter: growth rate must be non-negative"

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
         }
       ->
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
           radius_dz )
       ->
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
           adaptive_rewards_params;
         }
       ->
      ( global_limit_of_staking_over_baking,
        edge_of_staking_over_delegation,
        adaptive_rewards_params ))
    (fun ( global_limit_of_staking_over_baking,
           edge_of_staking_over_delegation,
           adaptive_rewards_params )
       ->
      {
        global_limit_of_staking_over_baking;
        edge_of_staking_over_delegation;
        adaptive_rewards_params;
      })
    (obj3
       (req "global_limit_of_staking_over_baking" uint8)
       (req "edge_of_staking_over_delegation" uint8)
       (req "adaptive_rewards_params" adaptive_rewards_params_encoding))

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
            dal_rewards_weight;
          } :
           issuance_weights)
       ->
      ( base_total_issued_per_minute,
        baking_reward_fixed_portion_weight,
        baking_reward_bonus_weight,
        attesting_reward_weight,
        seed_nonce_revelation_tip_weight,
        vdf_revelation_tip_weight,
        dal_rewards_weight ))
    (fun ( base_total_issued_per_minute,
           baking_reward_fixed_portion_weight,
           baking_reward_bonus_weight,
           attesting_reward_weight,
           seed_nonce_revelation_tip_weight,
           vdf_revelation_tip_weight,
           dal_rewards_weight )
       ->
      {
        base_total_issued_per_minute;
        baking_reward_fixed_portion_weight;
        baking_reward_bonus_weight;
        attesting_reward_weight;
        seed_nonce_revelation_tip_weight;
        vdf_revelation_tip_weight;
        dal_rewards_weight;
      })
    (obj7
       (req "base_total_issued_per_minute" Tez_repr.encoding)
       (req "baking_reward_fixed_portion_weight" int31)
       (req "baking_reward_bonus_weight" int31)
       (req "attesting_reward_weight" int31)
       (req "seed_nonce_revelation_tip_weight" int31)
       (req "vdf_revelation_tip_weight" int31)
       (req "dal_rewards_weight" int31))

let encoding =
  let open Data_encoding in
  conv
    (fun c ->
      ( ( ( c.consensus_rights_delay,
            c.blocks_preservation_cycles,
            c.delegate_parameters_activation_delay,
            c.tolerated_inactivity_period ),
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
              c.consensus_threshold_size ),
            ( ( c.minimal_participation_ratio,
                c.limit_of_delegation_over_baking,
                c.percentage_of_frozen_deposits_slashed_per_double_baking,
                c.max_slashing_per_block,
                c.max_slashing_threshold,
                c.testnet_dictator,
                c.initial_seed ),
              ( ( c.cache_script_size,
                  c.cache_stake_distribution_cycles,
                  c.cache_sampler_state_cycles,
                  c.cache_stake_info_cycles ),
                ( c.dal,
                  ( (c.sc_rollup, c.zk_rollup),
                    ( c.adaptive_issuance,
                      ( c.direct_ticket_spending_enable,
                        c.aggregate_attestation,
                        c.allow_tz4_delegate_enable,
                        c.all_bakers_attest_activation_threshold,
                        c.native_contracts_enable,
                        c.swrr_new_baker_lottery_enable,
                        c.tz5_account_enable ) ) ) ) ) ) ) ) ))
    (fun ( ( ( consensus_rights_delay,
               blocks_preservation_cycles,
               delegate_parameters_activation_delay,
               tolerated_inactivity_period ),
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
                 consensus_threshold_size ),
               ( ( minimal_participation_ratio,
                   limit_of_delegation_over_baking,
                   percentage_of_frozen_deposits_slashed_per_double_baking,
                   max_slashing_per_block,
                   max_slashing_threshold,
                   testnet_dictator,
                   initial_seed ),
                 ( ( cache_script_size,
                     cache_stake_distribution_cycles,
                     cache_sampler_state_cycles,
                     cache_stake_info_cycles ),
                   ( dal,
                     ( (sc_rollup, zk_rollup),
                       ( adaptive_issuance,
                         ( direct_ticket_spending_enable,
                           aggregate_attestation,
                           allow_tz4_delegate_enable,
                           all_bakers_attest_activation_threshold,
                           native_contracts_enable,
                           swrr_new_baker_lottery_enable,
                           tz5_account_enable ) ) ) ) ) ) ) ) )
       ->
      {
        consensus_rights_delay;
        blocks_preservation_cycles;
        delegate_parameters_activation_delay;
        tolerated_inactivity_period;
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
        consensus_threshold_size;
        limit_of_delegation_over_baking;
        percentage_of_frozen_deposits_slashed_per_double_baking;
        max_slashing_per_block;
        max_slashing_threshold;
        testnet_dictator;
        initial_seed;
        cache_script_size;
        cache_stake_distribution_cycles;
        cache_sampler_state_cycles;
        cache_stake_info_cycles;
        dal;
        sc_rollup;
        zk_rollup;
        adaptive_issuance;
        direct_ticket_spending_enable;
        aggregate_attestation;
        allow_tz4_delegate_enable;
        all_bakers_attest_activation_threshold;
        native_contracts_enable;
        swrr_new_baker_lottery_enable;
        tz5_account_enable;
      })
    (merge_objs
       (merge_objs
          (obj4
             (req "consensus_rights_delay" uint8)
             (req "blocks_preservation_cycles" uint8)
             (req "delegate_parameters_activation_delay" uint8)
             (req "tolerated_inactivity_period" uint8))
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
                (req "consensus_threshold_size" int31))
             (merge_objs
                (obj7
                   (req "minimal_participation_ratio" Ratio_repr.encoding)
                   (req "limit_of_delegation_over_baking" uint8)
                   (req
                      "percentage_of_frozen_deposits_slashed_per_double_baking"
                      Percentage.encoding)
                   (req "max_slashing_per_block" Percentage.encoding)
                   (req "max_slashing_threshold" Ratio_repr.encoding)
                   (opt "testnet_dictator" Signature.Public_key_hash.encoding)
                   (opt "initial_seed" State_hash.encoding))
                (merge_objs
                   (obj4
                      (req "cache_script_size" int31)
                      (req "cache_stake_distribution_cycles" int8)
                      (req "cache_sampler_state_cycles" int8)
                      (req "cache_stake_info_cycles" int8))
                   (merge_objs
                      (obj1 (req "dal_parametric" dal_encoding))
                      (merge_objs
                         (merge_objs sc_rollup_encoding zk_rollup_encoding)
                         (merge_objs
                            adaptive_issuance_encoding
                            (obj7
                               (req "direct_ticket_spending_enable" bool)
                               (req "aggregate_attestation" bool)
                               (req "allow_tz4_delegate_enable" bool)
                               (req
                                  "all_bakers_attest_activation_threshold"
                                  Ratio_repr.encoding)
                               (req "native_contracts_enable" bool)
                               (req "swrr_new_baker_lottery_enable" bool)
                               (req "tz5_account_enable" bool))))))))))

let update_sc_rollup_parameter ratio_i32 c =
  (* Constants remain small enough to fit in [int32] after update (as a
     reminder, a Tezos level is encoded in a signed 32-byte integer). *)
  let ratio_int x = Int32.of_int x |> ratio_i32 |> Int32.to_int in
  {
    (* Constants expressed in number of blocks *)
    challenge_window_in_blocks = ratio_int c.challenge_window_in_blocks;
    max_active_outbox_levels = c.max_active_outbox_levels;
    commitment_period_in_blocks = ratio_int c.commitment_period_in_blocks;
    max_lookahead_in_blocks = ratio_i32 c.max_lookahead_in_blocks;
    timeout_period_in_blocks = ratio_int c.timeout_period_in_blocks;
    (* Other constants *)
    max_outbox_messages_per_level = c.max_outbox_messages_per_level;
    arith_pvm_enable = c.arith_pvm_enable;
    origination_size = c.origination_size;
    stake_amount = c.stake_amount;
    number_of_sections_in_dissection = c.number_of_sections_in_dissection;
    max_number_of_stored_cemented_commitments =
      c.max_number_of_stored_cemented_commitments;
    max_number_of_parallel_games = c.max_number_of_parallel_games;
    reveal_activation_level = c.reveal_activation_level;
    private_enable = c.private_enable;
    riscv_pvm_enable = c.riscv_pvm_enable;
  }

let update_sc_rollup_parameter_with_block_time block_time c =
  (* For comments, see [make_sc_rollup_parameter] from
     [lib_parameters/default_parameters.ml] *)
  let seconds_in_a_day = 60 * 60 * 24 in
  let seconds_in_a_week = seconds_in_a_day * 7 in
  let commitment_period_in_blocks = 60 * 15 / block_time in
  let challenge_window_in_blocks = seconds_in_a_week * 2 / block_time in

  (* Here we don't update the value so there is no need for such
     migration, but it reduces the time a user has to execute a outbox
     message.

     For a new network it should be ~ 2 weeks, so `seconds_in_a_week *
     2 / block_time`. *)
  let max_active_outbox_levels = c.max_active_outbox_levels in
  let timeout_period_in_blocks = seconds_in_a_week / block_time in
  let max_lookahead_in_blocks =
    let seconds_in_a_month = Int32.of_int (seconds_in_a_day * 30) in
    let block_time = Int32.of_int block_time in
    Int32.div seconds_in_a_month block_time
  in
  {
    (* Constants expressed in number of blocks *)
    challenge_window_in_blocks;
    max_active_outbox_levels;
    commitment_period_in_blocks;
    max_lookahead_in_blocks;
    timeout_period_in_blocks;
    (* Other constants *)
    max_outbox_messages_per_level = c.max_outbox_messages_per_level;
    arith_pvm_enable = c.arith_pvm_enable;
    origination_size = c.origination_size;
    stake_amount = c.stake_amount;
    number_of_sections_in_dissection = c.number_of_sections_in_dissection;
    max_number_of_stored_cemented_commitments =
      c.max_number_of_stored_cemented_commitments;
    max_number_of_parallel_games = c.max_number_of_parallel_games;
    reveal_activation_level = c.reveal_activation_level;
    private_enable = c.private_enable;
    riscv_pvm_enable = c.riscv_pvm_enable;
  }

module Internal_for_tests = struct
  let sc_rollup_encoding = sc_rollup_encoding
end
