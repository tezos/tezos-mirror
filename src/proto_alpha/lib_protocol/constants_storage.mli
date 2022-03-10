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

val preserved_cycles : Raw_context.t -> int

val blocks_per_cycle : Raw_context.t -> int32

val blocks_per_commitment : Raw_context.t -> int32

val blocks_per_stake_snapshot : Raw_context.t -> int32

val cycles_per_voting_period : Raw_context.t -> int32

val hard_gas_limit_per_operation :
  Raw_context.t -> Gas_limit_repr.Arith.integral

val hard_gas_limit_per_block : Raw_context.t -> Gas_limit_repr.Arith.integral

val cost_per_byte : Raw_context.t -> Tez_repr.t

val hard_storage_limit_per_operation : Raw_context.t -> Z.t

val proof_of_work_threshold : Raw_context.t -> int64

val tokens_per_roll : Raw_context.t -> Tez_repr.t

val seed_nonce_revelation_tip : Raw_context.t -> Tez_repr.t

val origination_size : Raw_context.t -> int

val baking_reward_fixed_portion : Raw_context.t -> Tez_repr.t

val baking_reward_bonus_per_slot : Raw_context.t -> Tez_repr.t

val endorsing_reward_per_slot : Raw_context.t -> Tez_repr.t

val quorum_min : Raw_context.t -> int32

val quorum_max : Raw_context.t -> int32

val min_proposal_quorum : Raw_context.t -> int32

val liquidity_baking_subsidy : Raw_context.t -> Tez_repr.t

val liquidity_baking_sunset_level : Raw_context.t -> int32

val liquidity_baking_toggle_ema_threshold : Raw_context.t -> int32

val parametric : Raw_context.t -> Constants_repr.parametric

val consensus_committee_size : Raw_context.t -> int

val consensus_threshold : Raw_context.t -> int

val minimal_participation_ratio : Raw_context.t -> Constants_repr.ratio

val max_slashing_period : Raw_context.t -> int

val frozen_deposits_percentage : Raw_context.t -> int

val double_baking_punishment : Raw_context.t -> Tez_repr.t

val tx_rollup_enable : Raw_context.t -> bool

val tx_rollup_origination_size : Raw_context.t -> int

val tx_rollup_hard_size_limit_per_inbox : Raw_context.t -> int

val tx_rollup_hard_size_limit_per_message : Raw_context.t -> int

val tx_rollup_commitment_bond : Raw_context.t -> Tez_repr.t

val tx_rollup_finality_period : Raw_context.t -> int

val tx_rollup_withdraw_period : Raw_context.t -> int

val tx_rollup_max_unfinalized_levels : Raw_context.t -> int

val tx_rollup_max_messages_per_inbox : Raw_context.t -> int

val tx_rollup_max_finalized_levels : Raw_context.t -> int

val ratio_of_frozen_deposits_slashed_per_double_endorsement :
  Raw_context.t -> Constants_repr.ratio

val minimal_block_delay : Raw_context.t -> Period_repr.t

val delay_increment_per_round : Raw_context.t -> Period_repr.t

val sc_rollup_enable : Raw_context.t -> bool

val sc_rollup_origination_size : Raw_context.t -> int

val sc_rollup_challenge_window_in_blocks : Raw_context.t -> int
