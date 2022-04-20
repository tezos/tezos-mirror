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

type t = {
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
  liquidity_baking_escape_ema_threshold : int32;
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

let encoding =
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
              c.liquidity_baking_escape_ema_threshold,
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
                 liquidity_baking_escape_ema_threshold,
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
        liquidity_baking_escape_ema_threshold;
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
                (req "liquidity_baking_escape_ema_threshold" int32)
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
