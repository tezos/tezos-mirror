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

(* The fitness version number was:
   - "\000" until and including proto 004
   - "\001" until and including proto 010
*)
let fitness_version_number = "\002"

let proof_of_work_nonce_size = 8

let nonce_length = 32

let max_anon_ops_per_block = 132

let max_proposals_per_delegate = 20

let max_operation_data_length = 32 * 1024 (* 32kB *)

let max_micheline_node_count = 50_000

let max_micheline_bytes_limit = 50_000

let max_allowed_global_constant_depth = 10_000

(* In previous versions of the protocol, this
   [michelson_maximum_type_size] limit was set to 1000 but
   the contract input types (pair <parameter_type> <storage_type>)
   were not checked. Both components, <parameter_type> and
   <storage_type> where however checked hence it was possible to build
   types as big as 2001. *)
let michelson_maximum_type_size = 2001

(* This constant declares the number of subcaches used by the cache
   mechanism (see {Context.Cache}). *)
let cache_layout_size = 3

type fixed = unit

let fixed_encoding =
  let open Data_encoding in
  conv
    (fun () ->
      ( proof_of_work_nonce_size,
        nonce_length,
        max_anon_ops_per_block,
        max_operation_data_length,
        max_proposals_per_delegate,
        max_micheline_node_count,
        max_micheline_bytes_limit,
        max_allowed_global_constant_depth,
        cache_layout_size,
        michelson_maximum_type_size ))
    (fun ( _proof_of_work_nonce_size,
           _nonce_length,
           _max_anon_ops_per_block,
           _max_operation_data_length,
           _max_proposals_per_delegate,
           _max_micheline_node_count,
           _max_micheline_bytes_limit,
           _max_allowed_global_constant_depth,
           _cache_layout_size,
           _michelson_maximum_type_size ) -> ())
    (obj10
       (req "proof_of_work_nonce_size" uint8)
       (req "nonce_length" uint8)
       (req "max_anon_ops_per_block" uint8)
       (req "max_operation_data_length" int31)
       (req "max_proposals_per_delegate" uint8)
       (req "max_micheline_node_count" int31)
       (req "max_micheline_bytes_limit" int31)
       (req "max_allowed_global_constants_depth" int31)
       (req "cache_layout_size" uint8)
       (req "michelson_maximum_type_size" uint16))

let fixed = ()

(* The encoded representation of this type is stored in the context as
   bytes. Changing the encoding, or the value of these constants from
   the previous protocol may break the context migration, or (even
   worse) yield an incorrect context after migration.

   If you change this encoding, you should ensure that there is a
   proper migration of the constants during context migration.  *)
type parametric = {
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
  (* If a new cache is added, please also modify the
     [cache_layout_size] value. *)
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
  sc_rollup_stake_amount_in_mutez : int;
  sc_rollup_commitment_frequency_in_blocks : int;
  sc_rollup_commitment_storage_size_in_bytes : int;
  sc_rollup_max_lookahead_in_blocks : int32;
}

let parametric_encoding =
  let open Data_encoding in
  conv
    (fun c ->
      ( ( c.preserved_cycles,
          c.blocks_per_cycle,
          c.blocks_per_commitment,
          c.blocks_per_stake_snapshot,
          c.cycles_per_voting_period,
          c.hard_gas_limit_per_operation,
          c.hard_gas_limit_per_block,
          c.proof_of_work_threshold,
          c.tokens_per_roll ),
        ( ( c.seed_nonce_revelation_tip,
            c.origination_size,
            c.baking_reward_fixed_portion,
            c.baking_reward_bonus_per_slot,
            c.endorsing_reward_per_slot,
            c.cost_per_byte,
            c.hard_storage_limit_per_operation,
            c.quorum_min ),
          ( ( c.quorum_max,
              c.min_proposal_quorum,
              c.liquidity_baking_subsidy,
              c.liquidity_baking_sunset_level,
              c.liquidity_baking_toggle_ema_threshold,
              c.max_operations_time_to_live,
              c.minimal_block_delay,
              c.delay_increment_per_round,
              c.consensus_committee_size,
              c.consensus_threshold ),
            ( ( c.minimal_participation_ratio,
                c.max_slashing_period,
                c.frozen_deposits_percentage,
                c.double_baking_punishment,
                c.ratio_of_frozen_deposits_slashed_per_double_endorsement,
                c.initial_seed ),
              ( ( c.cache_script_size,
                  c.cache_stake_distribution_cycles,
                  c.cache_sampler_state_cycles ),
                ( ( ( c.tx_rollup_enable,
                      c.tx_rollup_origination_size,
                      c.tx_rollup_hard_size_limit_per_inbox,
                      c.tx_rollup_hard_size_limit_per_message,
                      c.tx_rollup_max_withdrawals_per_batch,
                      c.tx_rollup_commitment_bond,
                      c.tx_rollup_finality_period,
                      c.tx_rollup_withdraw_period,
                      c.tx_rollup_max_inboxes_count,
                      c.tx_rollup_max_messages_per_inbox ),
                    ( c.tx_rollup_max_commitments_count,
                      c.tx_rollup_cost_per_byte_ema_factor,
                      c.tx_rollup_max_ticket_payload_size,
                      c.tx_rollup_rejection_max_proof_size,
                      c.tx_rollup_sunset_level ) ),
                  ( c.sc_rollup_enable,
                    c.sc_rollup_origination_size,
                    c.sc_rollup_challenge_window_in_blocks,
                    c.sc_rollup_max_available_messages,
                    c.sc_rollup_stake_amount_in_mutez,
                    c.sc_rollup_commitment_frequency_in_blocks,
                    c.sc_rollup_commitment_storage_size_in_bytes,
                    c.sc_rollup_max_lookahead_in_blocks ) ) ) ) ) ) ))
    (fun ( ( preserved_cycles,
             blocks_per_cycle,
             blocks_per_commitment,
             blocks_per_stake_snapshot,
             cycles_per_voting_period,
             hard_gas_limit_per_operation,
             hard_gas_limit_per_block,
             proof_of_work_threshold,
             tokens_per_roll ),
           ( ( seed_nonce_revelation_tip,
               origination_size,
               baking_reward_fixed_portion,
               baking_reward_bonus_per_slot,
               endorsing_reward_per_slot,
               cost_per_byte,
               hard_storage_limit_per_operation,
               quorum_min ),
             ( ( quorum_max,
                 min_proposal_quorum,
                 liquidity_baking_subsidy,
                 liquidity_baking_sunset_level,
                 liquidity_baking_toggle_ema_threshold,
                 max_operations_time_to_live,
                 minimal_block_delay,
                 delay_increment_per_round,
                 consensus_committee_size,
                 consensus_threshold ),
               ( ( minimal_participation_ratio,
                   max_slashing_period,
                   frozen_deposits_percentage,
                   double_baking_punishment,
                   ratio_of_frozen_deposits_slashed_per_double_endorsement,
                   initial_seed ),
                 ( ( cache_script_size,
                     cache_stake_distribution_cycles,
                     cache_sampler_state_cycles ),
                   ( ( ( tx_rollup_enable,
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
                         tx_rollup_sunset_level ) ),
                     ( sc_rollup_enable,
                       sc_rollup_origination_size,
                       sc_rollup_challenge_window_in_blocks,
                       sc_rollup_max_available_messages,
                       sc_rollup_stake_amount_in_mutez,
                       sc_rollup_commitment_frequency_in_blocks,
                       sc_rollup_commitment_storage_size_in_bytes,
                       sc_rollup_max_lookahead_in_blocks ) ) ) ) ) ) ) ->
      {
        preserved_cycles;
        blocks_per_cycle;
        blocks_per_commitment;
        blocks_per_stake_snapshot;
        cycles_per_voting_period;
        hard_gas_limit_per_operation;
        hard_gas_limit_per_block;
        proof_of_work_threshold;
        tokens_per_roll;
        seed_nonce_revelation_tip;
        origination_size;
        baking_reward_fixed_portion;
        baking_reward_bonus_per_slot;
        endorsing_reward_per_slot;
        cost_per_byte;
        hard_storage_limit_per_operation;
        quorum_min;
        quorum_max;
        min_proposal_quorum;
        liquidity_baking_subsidy;
        liquidity_baking_sunset_level;
        liquidity_baking_toggle_ema_threshold;
        max_operations_time_to_live;
        minimal_block_delay;
        delay_increment_per_round;
        minimal_participation_ratio;
        max_slashing_period;
        consensus_committee_size;
        consensus_threshold;
        frozen_deposits_percentage;
        double_baking_punishment;
        ratio_of_frozen_deposits_slashed_per_double_endorsement;
        initial_seed;
        cache_script_size;
        cache_stake_distribution_cycles;
        cache_sampler_state_cycles;
        tx_rollup_enable;
        tx_rollup_origination_size;
        tx_rollup_hard_size_limit_per_inbox;
        tx_rollup_hard_size_limit_per_message;
        tx_rollup_max_withdrawals_per_batch;
        tx_rollup_commitment_bond;
        tx_rollup_finality_period;
        tx_rollup_withdraw_period;
        tx_rollup_max_inboxes_count;
        tx_rollup_max_messages_per_inbox;
        tx_rollup_max_commitments_count;
        tx_rollup_cost_per_byte_ema_factor;
        tx_rollup_max_ticket_payload_size;
        tx_rollup_rejection_max_proof_size;
        tx_rollup_sunset_level;
        sc_rollup_enable;
        sc_rollup_origination_size;
        sc_rollup_challenge_window_in_blocks;
        sc_rollup_max_available_messages;
        sc_rollup_stake_amount_in_mutez;
        sc_rollup_commitment_frequency_in_blocks;
        sc_rollup_commitment_storage_size_in_bytes;
        sc_rollup_max_lookahead_in_blocks;
      })
    (merge_objs
       (obj9
          (req "preserved_cycles" uint8)
          (req "blocks_per_cycle" int32)
          (req "blocks_per_commitment" int32)
          (req "blocks_per_stake_snapshot" int32)
          (req "cycles_per_voting_period" int32)
          (req
             "hard_gas_limit_per_operation"
             Gas_limit_repr.Arith.z_integral_encoding)
          (req
             "hard_gas_limit_per_block"
             Gas_limit_repr.Arith.z_integral_encoding)
          (req "proof_of_work_threshold" int64)
          (req "tokens_per_roll" Tez_repr.encoding))
       (merge_objs
          (obj8
             (req "seed_nonce_revelation_tip" Tez_repr.encoding)
             (req "origination_size" int31)
             (req "baking_reward_fixed_portion" Tez_repr.encoding)
             (req "baking_reward_bonus_per_slot" Tez_repr.encoding)
             (req "endorsing_reward_per_slot" Tez_repr.encoding)
             (req "cost_per_byte" Tez_repr.encoding)
             (req "hard_storage_limit_per_operation" z)
             (req "quorum_min" int32))
          (merge_objs
             (obj10
                (req "quorum_max" int32)
                (req "min_proposal_quorum" int32)
                (req "liquidity_baking_subsidy" Tez_repr.encoding)
                (req "liquidity_baking_sunset_level" int32)
                (req "liquidity_baking_toggle_ema_threshold" int32)
                (req "max_operations_time_to_live" int16)
                (req "minimal_block_delay" Period_repr.encoding)
                (req "delay_increment_per_round" Period_repr.encoding)
                (req "consensus_committee_size" int31)
                (req "consensus_threshold" int31))
             (merge_objs
                (obj6
                   (req "minimal_participation_ratio" Ratio_repr.encoding)
                   (req "max_slashing_period" int31)
                   (req "frozen_deposits_percentage" int31)
                   (req "double_baking_punishment" Tez_repr.encoding)
                   (req
                      "ratio_of_frozen_deposits_slashed_per_double_endorsement"
                      Ratio_repr.encoding)
                   (opt "initial_seed" State_hash.encoding))
                (merge_objs
                   (obj3
                      (req "cache_script_size" int31)
                      (req "cache_stake_distribution_cycles" int8)
                      (req "cache_sampler_state_cycles" int8))
                   (merge_objs
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
                      (obj8
                         (req "sc_rollup_enable" bool)
                         (req "sc_rollup_origination_size" int31)
                         (req "sc_rollup_challenge_window_in_blocks" int31)
                         (req "sc_rollup_max_available_messages" int31)
                         (req "sc_rollup_stake_amount_in_mutez" int31)
                         (req "sc_rollup_commitment_frequency_in_blocks" int31)
                         (req
                            "sc_rollup_commitment_storage_size_in_bytes"
                            int31)
                         (req "sc_rollup_max_lookahead_in_blocks" int32))))))))

type t = {fixed : fixed; parametric : parametric}

let all_of_parametric parametric = {fixed; parametric}

let encoding =
  let open Data_encoding in
  conv
    (fun {fixed; parametric} -> (fixed, parametric))
    (fun (fixed, parametric) -> {fixed; parametric})
    (merge_objs fixed_encoding parametric_encoding)

type error += Invalid_protocol_constants of string (* `Permanent *)

let () =
  register_error_kind
    `Permanent
    ~id:"constants.invalid_protocol_constants"
    ~title:"Invalid protocol constants"
    ~description:"The provided protocol constants are not coherent."
    ~pp:(fun ppf reason ->
      Format.fprintf ppf "Invalid protocol constants: %s" reason)
    Data_encoding.(obj1 (req "reason" string))
    (function Invalid_protocol_constants reason -> Some reason | _ -> None)
    (fun reason -> Invalid_protocol_constants reason)

let check_constants constants =
  error_unless
    Period_repr.(constants.minimal_block_delay > zero)
    (Invalid_protocol_constants
       "The minimal block delay must be greater than zero")
  >>? fun () ->
  error_unless
    Period_repr.(constants.delay_increment_per_round > zero)
    (Invalid_protocol_constants
       "The delay increment per round must be greater than zero")
  >>? fun () ->
  error_unless
    Compare.Int.(constants.consensus_committee_size > 0)
    (Invalid_protocol_constants
       "The consensus committee size must be strictly greater than 0.")
  >>? fun () ->
  error_unless
    Compare.Int.(
      constants.consensus_threshold >= 0
      && constants.consensus_threshold <= constants.consensus_committee_size)
    (Invalid_protocol_constants
       "The consensus threshold must be greater than or equal to 0 and less \
        than or equal to the consensus commitee size.")
  >>? fun () ->
  error_unless
    (let Ratio_repr.{numerator; denominator} =
       constants.minimal_participation_ratio
     in
     Compare.Int.(numerator >= 0 && denominator > 0))
    (Invalid_protocol_constants
       "The minimal participation ratio must be a non-negative valid ratio.")
  >>? fun () ->
  error_unless
    Compare.Int.(
      constants.minimal_participation_ratio.numerator
      <= constants.minimal_participation_ratio.denominator)
    (Invalid_protocol_constants
       "The minimal participation ratio must be less than or equal to 100%.")
  >>? fun () ->
  error_unless
    Compare.Int.(constants.max_slashing_period > 0)
    (Invalid_protocol_constants
       "The unfreeze delay must be strictly greater than 0.")
  >>? fun () ->
  (* The [frozen_deposits_percentage] should be a percentage *)
  error_unless
    Compare.Int.(
      constants.frozen_deposits_percentage > 0
      && constants.frozen_deposits_percentage <= 100)
    (Invalid_protocol_constants
       "The frozen percentage ratio must be strictly greater than 0 and less \
        or equal than 100.")
  >>? fun () ->
  error_unless
    Tez_repr.(constants.double_baking_punishment >= zero)
    (Invalid_protocol_constants
       "The double baking punishment must be non-negative.")
  >>? fun () ->
  error_unless
    (let Ratio_repr.{numerator; denominator} =
       constants.ratio_of_frozen_deposits_slashed_per_double_endorsement
     in
     Compare.Int.(numerator >= 0 && denominator > 0))
    (Invalid_protocol_constants
       "The ratio of frozen deposits ratio slashed per double endorsement must \
        be a non-negative valid ratio.")
  >>? fun () ->
  error_unless
    (let snapshot_frequence =
       Int32.div constants.blocks_per_cycle constants.blocks_per_stake_snapshot
     in
     Compare.Int32.(
       snapshot_frequence > Int32.zero
       && snapshot_frequence < Int32.of_int (1 lsl 16)))
    (Invalid_protocol_constants
       "The ratio blocks_per_cycle per blocks_per_stake_snapshot should be \
        between 1 and 65535")
  >>? fun () -> Result.return_unit

module Generated = struct
  type t = {
    consensus_threshold : int;
    baking_reward_fixed_portion : Tez_repr.t;
    baking_reward_bonus_per_slot : Tez_repr.t;
    endorsing_reward_per_slot : Tez_repr.t;
  }

  let generate ~consensus_committee_size ~blocks_per_minute =
    let consensus_threshold = (consensus_committee_size * 2 / 3) + 1 in
    (* As in previous protocols, we set the maximum total rewards per minute to
       be 80 tez. *)
    let rewards_per_minute = Tez_repr.(mul_exn one 80) in
    let rewards_per_block =
      Ratio_repr.(
        Tez_repr.(
          div_exn
            (mul_exn rewards_per_minute blocks_per_minute.denominator)
            blocks_per_minute.numerator))
    in
    let rewards_half = Tez_repr.(div_exn rewards_per_block 2) in
    let rewards_quarter = Tez_repr.(div_exn rewards_per_block 4) in
    let bonus_committee_size = consensus_committee_size - consensus_threshold in
    {
      consensus_threshold;
      baking_reward_fixed_portion =
        (if Compare.Int.(bonus_committee_size <= 0) then
         (* a fortiori, consensus_committee_size < 4 *)
         rewards_half
        else rewards_quarter);
      baking_reward_bonus_per_slot =
        (if Compare.Int.(bonus_committee_size <= 0) then Tez_repr.zero
        else Tez_repr.div_exn rewards_quarter bonus_committee_size);
      endorsing_reward_per_slot =
        Tez_repr.div_exn rewards_half consensus_committee_size;
    }
end

module Proto_previous = struct
  type delegate_selection =
    | Random
    | Round_robin_over of Signature.Public_key.t list list

  let delegate_selection_encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"Random_delegate_selection"
          (constant "random")
          (function Random -> Some () | _ -> None)
          (fun () -> Random);
        case
          (Tag 1)
          ~title:"Round_robin_over_delegates"
          (list (list Signature.Public_key.encoding))
          (function Round_robin_over l -> Some l | _ -> None)
          (fun l -> Round_robin_over l);
      ]

  type parametric = {
    preserved_cycles : int;
    blocks_per_cycle : int32;
    blocks_per_commitment : int32;
    blocks_per_stake_snapshot : int32;
    blocks_per_voting_period : int32;
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
    delegate_selection : delegate_selection;
  }

  let parametric_encoding =
    let open Data_encoding in
    conv
      (fun c ->
        ( ( c.preserved_cycles,
            c.blocks_per_cycle,
            c.blocks_per_commitment,
            c.blocks_per_stake_snapshot,
            c.blocks_per_voting_period,
            c.hard_gas_limit_per_operation,
            c.hard_gas_limit_per_block,
            c.proof_of_work_threshold,
            c.tokens_per_roll ),
          ( ( c.seed_nonce_revelation_tip,
              c.origination_size,
              c.baking_reward_fixed_portion,
              c.baking_reward_bonus_per_slot,
              c.endorsing_reward_per_slot,
              c.cost_per_byte,
              c.hard_storage_limit_per_operation,
              c.quorum_min ),
            ( ( c.quorum_max,
                c.min_proposal_quorum,
                c.liquidity_baking_subsidy,
                c.liquidity_baking_sunset_level,
                c.liquidity_baking_toggle_ema_threshold,
                c.max_operations_time_to_live,
                c.minimal_block_delay,
                c.delay_increment_per_round,
                c.consensus_committee_size,
                c.consensus_threshold ),
              ( c.minimal_participation_ratio,
                c.max_slashing_period,
                c.frozen_deposits_percentage,
                c.double_baking_punishment,
                c.ratio_of_frozen_deposits_slashed_per_double_endorsement,
                c.delegate_selection ) ) ) ))
      (fun ( ( preserved_cycles,
               blocks_per_cycle,
               blocks_per_commitment,
               blocks_per_stake_snapshot,
               blocks_per_voting_period,
               hard_gas_limit_per_operation,
               hard_gas_limit_per_block,
               proof_of_work_threshold,
               tokens_per_roll ),
             ( ( seed_nonce_revelation_tip,
                 origination_size,
                 baking_reward_fixed_portion,
                 baking_reward_bonus_per_slot,
                 endorsing_reward_per_slot,
                 cost_per_byte,
                 hard_storage_limit_per_operation,
                 quorum_min ),
               ( ( quorum_max,
                   min_proposal_quorum,
                   liquidity_baking_subsidy,
                   liquidity_baking_sunset_level,
                   liquidity_baking_toggle_ema_threshold,
                   max_operations_time_to_live,
                   minimal_block_delay,
                   delay_increment_per_round,
                   consensus_committee_size,
                   consensus_threshold ),
                 ( minimal_participation_ratio,
                   max_slashing_period,
                   frozen_deposits_percentage,
                   double_baking_punishment,
                   ratio_of_frozen_deposits_slashed_per_double_endorsement,
                   delegate_selection ) ) ) ) ->
        {
          preserved_cycles;
          blocks_per_cycle;
          blocks_per_commitment;
          blocks_per_stake_snapshot;
          blocks_per_voting_period;
          hard_gas_limit_per_operation;
          hard_gas_limit_per_block;
          proof_of_work_threshold;
          tokens_per_roll;
          seed_nonce_revelation_tip;
          origination_size;
          baking_reward_fixed_portion;
          baking_reward_bonus_per_slot;
          endorsing_reward_per_slot;
          cost_per_byte;
          hard_storage_limit_per_operation;
          quorum_min;
          quorum_max;
          min_proposal_quorum;
          liquidity_baking_subsidy;
          liquidity_baking_sunset_level;
          liquidity_baking_toggle_ema_threshold;
          max_operations_time_to_live;
          minimal_block_delay;
          delay_increment_per_round;
          minimal_participation_ratio;
          max_slashing_period;
          consensus_committee_size;
          consensus_threshold;
          frozen_deposits_percentage;
          double_baking_punishment;
          ratio_of_frozen_deposits_slashed_per_double_endorsement;
          delegate_selection;
        })
      (merge_objs
         (obj9
            (req "preserved_cycles" uint8)
            (req "blocks_per_cycle" int32)
            (req "blocks_per_commitment" int32)
            (req "blocks_per_stake_snapshot" int32)
            (req "blocks_per_voting_period" int32)
            (req
               "hard_gas_limit_per_operation"
               Gas_limit_repr.Arith.z_integral_encoding)
            (req
               "hard_gas_limit_per_block"
               Gas_limit_repr.Arith.z_integral_encoding)
            (req "proof_of_work_threshold" int64)
            (req "tokens_per_roll" Tez_repr.encoding))
         (merge_objs
            (obj8
               (req "seed_nonce_revelation_tip" Tez_repr.encoding)
               (req "origination_size" int31)
               (req "baking_reward_fixed_portion" Tez_repr.encoding)
               (req "baking_reward_bonus_per_slot" Tez_repr.encoding)
               (req "endorsing_reward_per_slot" Tez_repr.encoding)
               (req "cost_per_byte" Tez_repr.encoding)
               (req "hard_storage_limit_per_operation" z)
               (req "quorum_min" int32))
            (merge_objs
               (obj10
                  (req "quorum_max" int32)
                  (req "min_proposal_quorum" int32)
                  (req "liquidity_baking_subsidy" Tez_repr.encoding)
                  (req "liquidity_baking_sunset_level" int32)
                  (req "liquidity_baking_toggle_ema_threshold" int32)
                  (req "max_operations_time_to_live" int16)
                  (req "minimal_block_delay" Period_repr.encoding)
                  (req "delay_increment_per_round" Period_repr.encoding)
                  (req "consensus_committee_size" int31)
                  (req "consensus_threshold" int31))
               (obj6
                  (req "minimal_participation_ratio" Ratio_repr.encoding)
                  (req "max_slashing_period" int31)
                  (req "frozen_deposits_percentage" int31)
                  (req "double_baking_punishment" Tez_repr.encoding)
                  (req
                     "ratio_of_frozen_deposits_slashed_per_double_endorsement"
                     Ratio_repr.encoding)
                  (dft "delegate_selection" delegate_selection_encoding Random)))))
end

let cache_layout p =
  [
    p.cache_script_size;
    p.cache_stake_distribution_cycles;
    p.cache_sampler_state_cycles;
  ]
