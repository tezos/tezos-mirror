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

val dal_encoding : dal Data_encoding.t

type sc_rollup_reveal_hashing_schemes = {blake2B : Raw_level_repr.t}

(** Associates reveal kinds to their activation level. *)
type sc_rollup_reveal_activation_level = {
  raw_data : sc_rollup_reveal_hashing_schemes;
  metadata : Raw_level_repr.t;
  dal_page : Raw_level_repr.t;
}

type sc_rollup = {
  enable : bool;
  arith_pvm_enable : bool;
  origination_size : int;
  challenge_window_in_blocks : int;
  stake_amount : Tez_repr.t;
  (* The period with which commitments are made. *)
  commitment_period_in_blocks : int;
  (* The maximum depth of a staker's position - chosen alongside
     [commitment_period_in_blocks] to prevent the cost
     of a staker's commitments' storage being greater than their deposit. *)
  max_lookahead_in_blocks : int32;
  (* Maximum number of active outbox levels allowed. An outbox level is active
     if it has an associated record of applied messages. *)
  max_active_outbox_levels : int32;
  max_outbox_messages_per_level : int;
  (* The default number of required sections in a dissection *)
  number_of_sections_in_dissection : int;
  (* The timeout period for a player in a refutation game.

     Timeout logic is similar to a chess clock. Each player starts with the same
     timeout = [timeout_period_in_blocks]. Each game move updates the timeout of
     the current player by decreasing it by the amount of time she took to play,
     i.e. number of blocks since the opponent last move. See
     {!Sc_rollup_game_repr.timeout} and
     {!Sc_rollup_refutation_storage.game_move} to see the implementation.

     Because of that [timeout_period_in_blocks] must be at least half the upper
     bound number of blocks needed for a game to finish. This bound is
     correlated to the maximum distance allowed between the first and last tick
     of a dissection. For example, when the maximum distance allowed is half the
     total distance [(last_tick - last_tick) / 2] then bound is [Log^2
     (Int64.max_int) + 2 = 65]. See {!Sc_rollup_game_repr.check_dissection} for
     more information on the dissection logic. *)
  timeout_period_in_blocks : int;
  (* The maximum number of cemented commitments stored for a sc rollup. *)
  max_number_of_stored_cemented_commitments : int;
  (* The maximum number of parallel games played by a given staker. *)
  max_number_of_parallel_games : int;
  (* Activation's block level of reveal kinds. *)
  reveal_activation_level : sc_rollup_reveal_activation_level;
}

type zk_rollup = {
  enable : bool;
  origination_size : int;
  (* Minimum number of pending operations that can be processed by a ZKRU
     update, if available.
     If the length of the pending list is less than [min_pending_to_process],
     then an update needs to process all pending operations to be valid.
     That is, every update must process at least
     [min(length pending_list, min_pending_to_process)] pending operations. *)
  min_pending_to_process : int;
  max_ticket_payload_size : int;
}

type adaptive_rewards_params = {
  reward_ratio_min : (* Maximum yearly inflation rate *) Q.t;
  reward_ratio_max : (* Minimum yearly inflation rate *) Q.t;
  max_bonus : (* Maximum reward bonus value *) int64;
  growth_rate : (* Bonus value's groth rate *) int64;
  center_dz : (* Center for bonus *) Q.t;
  radius_dz :
    (* Minimum distance from center required for non-zero growth *) Q.t;
}

type adaptive_inflation = {
  staking_over_baking_global_limit
    (* Global maximum costake tokens taken into account per baking token. Each baker can set their own lower limit. *) :
    int;
  staking_over_delegation_edge : (* Weight of staking over delegation. *) int;
  launch_ema_threshold : (* Threshold of the activation vote *) int32;
  adaptive_rewards_params :
    (* Parameters for the reward mechanism *) adaptive_rewards_params;
}

type reward_weights = {
  (* [base_total_rewards_per_minute] is the total amount of rewards expected to
     be distributed every minute *)
  base_total_rewards_per_minute : Tez_repr.t;
  (* The following fields represent the "weights" of the respective reward kinds.
     The actual reward values are computed proportionally from the other weights
     as a portion of the [base_total_rewards_per_minute]. See the module
     {!Delegate_rewards} for more details *)
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
  (* in centile of a percentage *)
  quorum_max : int32;
  min_proposal_quorum : int32;
  liquidity_baking_toggle_ema_threshold : int32;
  max_operations_time_to_live : int;
  minimal_block_delay : Period_repr.t;
  delay_increment_per_round : Period_repr.t;
  minimal_participation_ratio : Ratio_repr.t;
  consensus_committee_size : int;
  (* in slots *)
  consensus_threshold : int;
  (* in slots *)
  max_slashing_period : int;
  (* in cycles *)
  delegation_over_baking_limit : int;
  (* upper bound on the (delegated tz / own frozen tz) ratio *)
  percentage_of_frozen_deposits_slashed_per_double_baking : int;
  percentage_of_frozen_deposits_slashed_per_double_endorsement : int;
  testnet_dictator : Signature.Public_key_hash.t option;
  initial_seed : State_hash.t option;
  cache_script_size : int;
  (* in bytes *)
  cache_stake_distribution_cycles : int;
  (* in cycles *)
  cache_sampler_state_cycles : int;
  (* in cycles *)
  dal : dal;
  sc_rollup : sc_rollup;
  zk_rollup : zk_rollup;
  adaptive_inflation : adaptive_inflation;
}

val encoding : t Data_encoding.encoding
