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

val dal_encoding : dal Data_encoding.t

type sc_rollup_reveal_hashing_schemes = {blake2B : Raw_level_repr.t}

(** Associates reveal kinds to their activation level. *)
type sc_rollup_reveal_activation_level = {
  raw_data : sc_rollup_reveal_hashing_schemes;
  metadata : Raw_level_repr.t;
  dal_page : Raw_level_repr.t;
  dal_parameters : Raw_level_repr.t;
}

type sc_rollup = {
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
  (* Activates an updatable whitelist of stakers. Only keys in the whitelist are
     allowed to stake and publish a commitment. *)
  private_enable : bool;
  (* Activates the RISC-V pvm. *)
  riscv_pvm_enable : bool;
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
  issuance_ratio_final_min : (* Minimum yearly issuance rate *) Q.t;
  issuance_ratio_final_max : (* Maximum yearly issuance rate *) Q.t;
  issuance_ratio_initial_min :
    (* Minimum yearly issuance rate at adaptive issuance activation *) Q.t;
  issuance_ratio_initial_max :
    (* Maximum yearly issuance rate at adaptive issuance activation *) Q.t;
  initial_period :
    (* Period in cycles during which the minimum and maximum yearly
       issuance rate values stay at their initial values *)
    int;
  transition_period :
    (* Period in cycles during which the minimum and maximum yearly
       issuance rate values decrease/increase until they reach their global values *)
    int;
  max_bonus : (* Maximum issuance bonus value *) Issuance_bonus_repr.max_bonus;
  growth_rate : (* Bonus value's growth rate *) Q.t;
  center_dz : (* Center for bonus *) Q.t;
  radius_dz :
    (* Minimum distance from center required for non-zero growth *) Q.t;
}

type adaptive_issuance = {
  global_limit_of_staking_over_baking
    (* Global maximum stake tokens taken into account per baking token. Each baker can set their own lower limit. *) :
    int;
  edge_of_staking_over_delegation :
    (* Weight of staking over delegation. *) int;
  launch_ema_threshold : (* Threshold of the activation vote *) int32;
  adaptive_rewards_params :
    (* Parameters for the reward mechanism *) adaptive_rewards_params;
  activation_vote_enable :
    (* If set to true, reaching the launch_ema_threshold in the adaptive
       issuance activation vote triggers the activation of the adaptive
       inflation feature; otherwise the activation vote has no effect. *)
    bool;
  autostaking_enable :
    (* If set to true, a stake/unstake/finalize operation will be triggered for
       all delegate at end of cycle. *)
    bool;
  force_activation :
    (* For testing purposes. If set to true, the adaptive issuance feature is
       enabled without waiting to reach the launch_ema_threshold.*)
    bool;
  ns_enable : (* If set to true, enables the NS feature *)
              bool;
}

type issuance_weights = {
  (* [base_total_issued_per_minute] is the total amount of rewards expected to
     be distributed every minute *)
  base_total_issued_per_minute : Tez_repr.t;
  (* The following fields represent the "weights" of the respective reward kinds.
     The actual reward values are computed proportionally from the other weights
     as a portion of the [base_total_issued_per_minute]. See the module
     {!Delegate_rewards} for more details *)
  baking_reward_fixed_portion_weight : int;
  baking_reward_bonus_weight : int;
  attesting_reward_weight : int;
  liquidity_baking_subsidy_weight : int;
  seed_nonce_revelation_tip_weight : int;
  vdf_revelation_tip_weight : int;
}

type t = {
  (* Number of cycles after which computed consensus rights are used to actually
     participate in the consensus *)
  consensus_rights_delay : int;
  (* Number of past cycles about which the protocol hints the shell that it should
     keep them in its history. *)
  blocks_preservation_cycles : int;
  (* Number of cycles after which submitted delegate parameters are being
     used. *)
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
  limit_of_delegation_over_baking : int;
  (* upper bound on the (delegated tz / own frozen tz) ratio *)
  percentage_of_frozen_deposits_slashed_per_double_baking : Percentage.t;
  percentage_of_frozen_deposits_slashed_per_double_attestation : Percentage.t;
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
  adaptive_issuance : adaptive_issuance;
  direct_ticket_spending_enable : bool;
}

val encoding : t Data_encoding.encoding

val update_sc_rollup_parameter : block_time:int -> sc_rollup -> sc_rollup

module Internal_for_tests : sig
  val sc_rollup_encoding : sc_rollup Data_encoding.t
end
