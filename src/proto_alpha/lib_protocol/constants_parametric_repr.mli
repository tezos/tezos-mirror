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

type tx_rollup = {
  enable : bool;
  origination_size : int;
  (* the maximum amount of bytes messages can allocate in an inbox *)
  hard_size_limit_per_inbox : int;
  (* the maximum amount of bytes one batch can allocate in an inbox *)
  hard_size_limit_per_message : int;
  (* the amount of tez to bond a tx rollup commitment *)
  commitment_bond : Tez_repr.t;
  (* the number of blocks before a tx rollup block is final *)
  finality_period : int;
  (* the maximum number of levels that can be left unfinalized
     before we stop accepting new inboxes for a tx rollup *)
  (* the minimum number of blocks to wait before removing a finalised
     commitment from the context. *)
  withdraw_period : int;
  max_inboxes_count : int;
  (* the maximum number of messages in an inbox.  This bounds the
     size of a commitment. *)
  max_messages_per_inbox : int;
  (* the maximum number of finalized commitments, to ensure that
     remove_commitment is ever called *)
  max_commitments_count : int;
  (* The number of blocks used to compute the ema factor determining
     the cost per byte for new messages in the inbox. *)
  cost_per_byte_ema_factor : int;
  (* Tickets are transmitted in batches in the
     [Tx_rollup_dispatch_tickets] operation.

     The semantics is that this operation is used to
     concretize the withdraw orders emitted by the layer-2,
     one layer-1 operation per messages of an
     inbox. Therefore, it is of significant importance that
     a valid batch does not produce a list of withdraw
     orders which could not fit in a layer-1 operation.

     With these values, at least 2048 bytes remain available
     to store the rest of the operands of
     [Tx_rollup_dispatch_tickets] (in practice, even more,
     because we overapproximate the size of tickets). So we
     are safe. *)
  max_ticket_payload_size : int;
  max_withdrawals_per_batch : int;
  (* The maximum size, in bytes, of a Merkle proof.  Operations which would
     require proofs larger than this should be no-ops. *)
  rejection_max_proof_size : int;
  sunset_level : int32;
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
}

type adaptive_inflation = {enable : bool}

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
  seed_nonce_revelation_tip : Tez_repr.t;
  origination_size : int;
  baking_reward_fixed_portion : Tez_repr.t;
  baking_reward_bonus_per_slot : Tez_repr.t;
  endorsing_reward_per_slot : Tez_repr.t;
  cost_per_byte : Tez_repr.t;
  hard_storage_limit_per_operation : Z.t;
  quorum_min : int32;
  (* in centile of a percentage *)
  quorum_max : int32;
  min_proposal_quorum : int32;
  liquidity_baking_subsidy : Tez_repr.t;
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
  frozen_deposits_percentage : int;
  (* that is, (100 * delegated tz / own tz) *)
  double_baking_punishment : Tez_repr.t;
  ratio_of_frozen_deposits_slashed_per_double_endorsement : Ratio_repr.t;
  testnet_dictator : Signature.Public_key_hash.t option;
  initial_seed : State_hash.t option;
  cache_script_size : int;
  (* in bytes *)
  cache_stake_distribution_cycles : int;
  (* in cycles *)
  cache_sampler_state_cycles : int;
  (* in cycles *)
  tx_rollup : tx_rollup;
  dal : dal;
  sc_rollup : sc_rollup;
  zk_rollup : zk_rollup;
  adaptive_inflation : adaptive_inflation;
}

val encoding : t Data_encoding.encoding
