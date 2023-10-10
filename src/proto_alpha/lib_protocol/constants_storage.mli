(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(** This module provides functions to extract the value of protocol parameters
    from the context.
    See {!Constant_repr.parametric} for more details about these values. *)

val preserved_cycles : Raw_context.t -> int

val blocks_per_cycle : Raw_context.t -> int32

val blocks_per_commitment : Raw_context.t -> int32

val nonce_revelation_threshold : Raw_context.t -> int32

val blocks_per_stake_snapshot : Raw_context.t -> int32

val cycles_per_voting_period : Raw_context.t -> int32

val hard_gas_limit_per_operation :
  Raw_context.t -> Gas_limit_repr.Arith.integral

val hard_gas_limit_per_block : Raw_context.t -> Gas_limit_repr.Arith.integral

val cost_per_byte : Raw_context.t -> Tez_repr.t

val hard_storage_limit_per_operation : Raw_context.t -> Z.t

val proof_of_work_threshold : Raw_context.t -> int64

val minimal_stake : Raw_context.t -> Tez_repr.t

val minimal_frozen_stake : Raw_context.t -> Tez_repr.t

val vdf_difficulty : Raw_context.t -> int64

val origination_size : Raw_context.t -> int

val issuance_weights :
  Raw_context.t -> Constants_parametric_repr.issuance_weights

val quorum_min : Raw_context.t -> int32

val quorum_max : Raw_context.t -> int32

val min_proposal_quorum : Raw_context.t -> int32

val liquidity_baking_toggle_ema_threshold : Raw_context.t -> int32

val parametric : Raw_context.t -> Constants_parametric_repr.t

val sc_rollup : Raw_context.t -> Constants_parametric_repr.sc_rollup

val consensus_committee_size : Raw_context.t -> int

val consensus_threshold : Raw_context.t -> int

val minimal_participation_ratio : Raw_context.t -> Ratio_repr.t

val limit_of_delegation_over_baking : Raw_context.t -> int

val percentage_of_frozen_deposits_slashed_per_double_baking :
  Raw_context.t -> Int_percentage.t

val percentage_of_frozen_deposits_slashed_per_double_attestation :
  Raw_context.t -> Int_percentage.t

val testnet_dictator : Raw_context.t -> Signature.Public_key_hash.t option

val minimal_block_delay : Raw_context.t -> Period_repr.t

val delay_increment_per_round : Raw_context.t -> Period_repr.t

val sc_rollup_arith_pvm_enable : Raw_context.t -> bool

val sc_rollup_origination_size : Raw_context.t -> int

val sc_rollup_challenge_window_in_blocks : Raw_context.t -> int

val sc_rollup_stake_amount : Raw_context.t -> Tez_repr.t

val sc_rollup_commitment_period_in_blocks : Raw_context.t -> int

val sc_rollup_max_lookahead_in_blocks : Raw_context.t -> int32

val sc_rollup_max_active_outbox_levels : Raw_context.t -> int32

val sc_rollup_max_outbox_messages_per_level : Raw_context.t -> int

val sc_rollup_number_of_sections_in_dissection : Raw_context.t -> int

val sc_rollup_max_number_of_parallel_games : Raw_context.t -> int

val max_number_of_stored_cemented_commitments : Raw_context.t -> int

val sc_rollup_timeout_period_in_blocks : Raw_context.t -> int

val sc_rollup_reveal_activation_level :
  Raw_context.t -> Constants_parametric_repr.sc_rollup_reveal_activation_level

val sc_rollup_private_enable : Raw_context.t -> bool

val dal_number_of_slots : Raw_context.t -> int

val dal_enable : Raw_context.t -> bool

val zk_rollup_enable : Raw_context.t -> bool

val zk_rollup_min_pending_to_process : Raw_context.t -> int

val zk_rollup_origination_size : Raw_context.t -> int

val zk_rollup_max_ticket_payload_size : Raw_context.t -> int

val adaptive_issuance_enable : Raw_context.t -> bool

val adaptive_issuance_global_limit_of_staking_over_baking : Raw_context.t -> int

val adaptive_issuance_edge_of_staking_over_delegation : Raw_context.t -> int

val adaptive_issuance_launch_ema_threshold : Raw_context.t -> int32

val adaptive_issuance_activation_vote_enable : Raw_context.t -> bool

val adaptive_issuance_rewards_params :
  Raw_context.t -> Constants_parametric_repr.adaptive_rewards_params

val adaptive_issuance_autostaking_enable : Raw_context.t -> bool

val direct_ticket_spending_enable : Raw_context.t -> bool
