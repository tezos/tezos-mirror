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

type t = {
  preserved_cycles : int;
  blocks_per_cycle : int32;
  blocks_per_commitment : int32;
  blocks_per_stake_snapshot : int32;
  cycles_per_voting_period : int32;
  hard_gas_limit_per_operation : Gas_limit_repr.Arith.integral;
  hard_gas_limit_per_block : Gas_limit_repr.Arith.integral;
  proof_of_work_threshold : int64;
  tokens_per_roll : Tez_repr.t;
  seed_nonce_revelation_tip : Tez_repr.t;
  origination_size : int;
  baking_reward_fixed_portion : Tez_repr.t;
  baking_reward_bonus_per_slot : Tez_repr.t;
  endorsing_reward_per_slot : Tez_repr.t;
  cost_per_byte : Tez_repr.t;
  hard_storage_limit_per_operation : Z.t;
  quorum_min : int32;
  quorum_max : int32;
  min_proposal_quorum : int32;
  liquidity_baking_subsidy : Tez_repr.t;
  liquidity_baking_sunset_level : int32;
  liquidity_baking_toggle_ema_threshold : int32;
  max_operations_time_to_live : int;
  minimal_block_delay : Period_repr.t;
  delay_increment_per_round : Period_repr.t;
  minimal_participation_ratio : Ratio_repr.t;
  consensus_committee_size : int;
  consensus_threshold : int;
  max_slashing_period : int;
  frozen_deposits_percentage : int;
  double_baking_punishment : Tez_repr.t;
  ratio_of_frozen_deposits_slashed_per_double_endorsement : Ratio_repr.t;
  initial_seed : State_hash.t option;
  cache_script_size : int;
  cache_stake_distribution_cycles : int;
  cache_sampler_state_cycles : int;
  tx_rollup_enable : bool;
  tx_rollup_origination_size : int;
  tx_rollup_hard_size_limit_per_inbox : int;
  tx_rollup_hard_size_limit_per_message : int;
  tx_rollup_commitment_bond : Tez_repr.t;
  tx_rollup_finality_period : int;
  tx_rollup_withdraw_period : int;
  tx_rollup_max_inboxes_count : int;
  tx_rollup_max_messages_per_inbox : int;
  tx_rollup_max_commitments_count : int;
  tx_rollup_cost_per_byte_ema_factor : int;
  tx_rollup_max_ticket_payload_size : int;
  tx_rollup_max_withdrawals_per_batch : int;
  tx_rollup_rejection_max_proof_size : int;
  tx_rollup_sunset_level : int32;
  sc_rollup_enable : bool;
  sc_rollup_origination_size : int;
  sc_rollup_challenge_window_in_blocks : int;
  sc_rollup_max_available_messages : int;
}

val encoding : t Data_encoding.encoding
