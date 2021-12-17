(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(* In this version of the protocol, there are the following subcaches:

   * One for contract source code and storage. Its size has been
   chosen not too exceed 100 000 000 bytes.

   * One for the stake distribution for all cycles stored at any
   moment (* preserved_cycles + max_slashing_period + 1 = 8 currently. *)

   * One for the sampler state for all cycles stored at any moment (as above). *)

let cache_layout = [100_000_000; 8 (* cycles *); 8 (* cycles *)]

(* In previous versions of the protocol, this
   [michelson_maximum_type_size] limit was set to 1000 but
   the contract input types (pair <parameter_type> <storage_type>)
   were not checked. Both components, <parameter_type> and
   <storage_type> where however checked hence it was possible to build
   types as big as 2001. *)
let michelson_maximum_type_size = 2001

type fixed = unit

type ratio = {numerator : int; denominator : int}

let ratio_encoding =
  let open Data_encoding in
  conv_with_guard
    (fun r -> (r.numerator, r.denominator))
    (fun (numerator, denominator) ->
      if Compare.Int.(denominator > 0) then ok {numerator; denominator}
      else Error "The denominator must be greater than 0.")
    (obj2 (req "numerator" uint16) (req "denominator" uint16))

let pp_ratio fmt {numerator; denominator} =
  Format.fprintf fmt "%d/%d" numerator denominator

let fixed_encoding =
  let open Data_encoding in
  let uint62 =
    let max_int_int64 = Int64.of_int max_int in
    conv_with_guard
      (fun int -> Int64.of_int int)
      (fun int64 ->
        if Compare.Int64.(int64 < 0L) then Error "Negative integer"
        else if Compare.Int64.(int64 > max_int_int64) then
          Error "Integer does not fit in 62 bits"
        else ok @@ Int64.to_int int64)
      int64
  in
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
        cache_layout,
        michelson_maximum_type_size ))
    (fun ( _proof_of_work_nonce_size,
           _nonce_length,
           _max_anon_ops_per_block,
           _max_operation_data_length,
           _max_proposals_per_delegate,
           _max_micheline_node_count,
           _max_micheline_bytes_limit,
           _max_allowed_global_constant_depth,
           _cache_layout,
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
       (req "cache_layout" (list uint62))
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
  minimal_participation_ratio : ratio;
  consensus_committee_size : int;
  consensus_threshold : int;
  max_slashing_period : int;
  frozen_deposits_percentage : int;
  double_baking_punishment : Tez_repr.t;
  ratio_of_frozen_deposits_slashed_per_double_endorsement : ratio;
  initial_seed : State_hash.t option;
  tx_rollup_enable : bool;
  tx_rollup_origination_size : int;
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
              c.liquidity_baking_escape_ema_threshold,
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
              (c.tx_rollup_enable, c.tx_rollup_origination_size) ) ) ) ))
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
               ( ( minimal_participation_ratio,
                   max_slashing_period,
                   frozen_deposits_percentage,
                   double_baking_punishment,
                   ratio_of_frozen_deposits_slashed_per_double_endorsement,
                   initial_seed ),
                 (tx_rollup_enable, tx_rollup_origination_size) ) ) ) ) ->
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
        initial_seed;
        tx_rollup_enable;
        tx_rollup_origination_size;
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
             (merge_objs
                (obj6
                   (req "minimal_participation_ratio" ratio_encoding)
                   (req "max_slashing_period" int31)
                   (req "frozen_deposits_percentage" int31)
                   (req "double_baking_punishment" Tez_repr.encoding)
                   (req
                      "ratio_of_frozen_deposits_slashed_per_double_endorsement"
                      ratio_encoding)
                   (opt "initial_seed" State_hash.encoding))
                (obj2
                   (req "tx_rollup_enable" bool)
                   (req "tx_rollup_origination_size" int31))))))

type t = {fixed : fixed; parametric : parametric}

let all parametric = {fixed; parametric}

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
    Compare.Int.(constants.consensus_committee_size > 3)
    (Invalid_protocol_constants
       "The consensus committee size must be strictly greater than 3.")
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
    (let {numerator; denominator} = constants.minimal_participation_ratio in
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
    (let {numerator; denominator} =
       constants.ratio_of_frozen_deposits_slashed_per_double_endorsement
     in
     Compare.Int.(numerator >= 0 && denominator > 0))
    (Invalid_protocol_constants
       "The ratio of frozen deposits ratio slashed per double endorsement must \
        be a non-negative valid ratio.")
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
      Tez_repr.(
        div_exn
          (mul_exn rewards_per_minute blocks_per_minute.denominator)
          blocks_per_minute.numerator)
    in
    let rewards_half = Tez_repr.(div_exn rewards_per_block 2) in
    let rewards_quarter = Tez_repr.(div_exn rewards_per_block 4) in
    {
      consensus_threshold;
      baking_reward_fixed_portion = rewards_quarter;
      baking_reward_bonus_per_slot =
        Tez_repr.div_exn
          rewards_quarter
          (consensus_committee_size - consensus_threshold);
      endorsing_reward_per_slot =
        Tez_repr.div_exn rewards_half consensus_committee_size;
    }
end

module Proto_previous = struct
  type parametric = {
    preserved_cycles : int;
    blocks_per_cycle : int32;
    blocks_per_commitment : int32;
    blocks_per_roll_snapshot : int32;
    blocks_per_voting_period : int32;
    time_between_blocks : Period_repr.t list;
    minimal_block_delay : Period_repr.t;
    endorsers_per_block : int;
    hard_gas_limit_per_operation : Gas_limit_repr.Arith.integral;
    hard_gas_limit_per_block : Gas_limit_repr.Arith.integral;
    proof_of_work_threshold : int64;
    tokens_per_roll : Tez_repr.t;
    seed_nonce_revelation_tip : Tez_repr.t;
    origination_size : int;
    block_security_deposit : Tez_repr.t;
    endorsement_security_deposit : Tez_repr.t;
    baking_reward_per_endorsement : Tez_repr.t list;
    endorsement_reward : Tez_repr.t list;
    cost_per_byte : Tez_repr.t;
    hard_storage_limit_per_operation : Z.t;
    quorum_min : int32;
    quorum_max : int32;
    min_proposal_quorum : int32;
    initial_endorsers : int;
    delay_per_missing_endorsement : Period_repr.t;
    liquidity_baking_subsidy : Tez_repr.t;
    liquidity_baking_sunset_level : int32;
    liquidity_baking_escape_ema_threshold : int32;
  }

  let parametric_encoding =
    let open Data_encoding in
    conv
      (fun c ->
        ( ( c.preserved_cycles,
            c.blocks_per_cycle,
            c.blocks_per_commitment,
            c.blocks_per_roll_snapshot,
            c.blocks_per_voting_period,
            c.time_between_blocks,
            c.endorsers_per_block,
            c.hard_gas_limit_per_operation,
            c.hard_gas_limit_per_block,
            c.proof_of_work_threshold ),
          ( ( c.tokens_per_roll,
              c.seed_nonce_revelation_tip,
              c.origination_size,
              c.block_security_deposit,
              c.endorsement_security_deposit,
              c.baking_reward_per_endorsement,
              c.endorsement_reward,
              c.cost_per_byte,
              c.hard_storage_limit_per_operation ),
            ( c.quorum_min,
              c.quorum_max,
              c.min_proposal_quorum,
              c.initial_endorsers,
              c.delay_per_missing_endorsement,
              c.minimal_block_delay,
              c.liquidity_baking_subsidy,
              c.liquidity_baking_sunset_level,
              c.liquidity_baking_escape_ema_threshold ) ) ))
      (fun ( ( preserved_cycles,
               blocks_per_cycle,
               blocks_per_commitment,
               blocks_per_roll_snapshot,
               blocks_per_voting_period,
               time_between_blocks,
               endorsers_per_block,
               hard_gas_limit_per_operation,
               hard_gas_limit_per_block,
               proof_of_work_threshold ),
             ( ( tokens_per_roll,
                 seed_nonce_revelation_tip,
                 origination_size,
                 block_security_deposit,
                 endorsement_security_deposit,
                 baking_reward_per_endorsement,
                 endorsement_reward,
                 cost_per_byte,
                 hard_storage_limit_per_operation ),
               ( quorum_min,
                 quorum_max,
                 min_proposal_quorum,
                 initial_endorsers,
                 delay_per_missing_endorsement,
                 minimal_block_delay,
                 liquidity_baking_subsidy,
                 liquidity_baking_sunset_level,
                 liquidity_baking_escape_ema_threshold ) ) ) ->
        {
          preserved_cycles;
          blocks_per_cycle;
          blocks_per_commitment;
          blocks_per_roll_snapshot;
          blocks_per_voting_period;
          time_between_blocks;
          endorsers_per_block;
          hard_gas_limit_per_operation;
          hard_gas_limit_per_block;
          proof_of_work_threshold;
          tokens_per_roll;
          seed_nonce_revelation_tip;
          origination_size;
          block_security_deposit;
          endorsement_security_deposit;
          baking_reward_per_endorsement;
          endorsement_reward;
          cost_per_byte;
          hard_storage_limit_per_operation;
          quorum_min;
          quorum_max;
          min_proposal_quorum;
          initial_endorsers;
          delay_per_missing_endorsement;
          minimal_block_delay;
          liquidity_baking_subsidy;
          liquidity_baking_sunset_level;
          liquidity_baking_escape_ema_threshold;
        })
      (merge_objs
         (obj10
            (req "preserved_cycles" uint8)
            (req "blocks_per_cycle" int32)
            (req "blocks_per_commitment" int32)
            (req "blocks_per_roll_snapshot" int32)
            (req "blocks_per_voting_period" int32)
            (req "time_between_blocks" (list Period_repr.encoding))
            (req "endorsers_per_block" uint16)
            (req
               "hard_gas_limit_per_operation"
               Gas_limit_repr.Arith.z_integral_encoding)
            (req
               "hard_gas_limit_per_block"
               Gas_limit_repr.Arith.z_integral_encoding)
            (req "proof_of_work_threshold" int64))
         (merge_objs
            (obj9
               (req "tokens_per_roll" Tez_repr.encoding)
               (req "seed_nonce_revelation_tip" Tez_repr.encoding)
               (req "origination_size" int31)
               (req "block_security_deposit" Tez_repr.encoding)
               (req "endorsement_security_deposit" Tez_repr.encoding)
               (req "baking_reward_per_endorsement" (list Tez_repr.encoding))
               (req "endorsement_reward" (list Tez_repr.encoding))
               (req "cost_per_byte" Tez_repr.encoding)
               (req "hard_storage_limit_per_operation" z))
            (obj9
               (req "quorum_min" int32)
               (req "quorum_max" int32)
               (req "min_proposal_quorum" int32)
               (req "initial_endorsers" uint16)
               (req "delay_per_missing_endorsement" Period_repr.encoding)
               (req "minimal_block_delay" Period_repr.encoding)
               (req "liquidity_baking_subsidy" Tez_repr.encoding)
               (req "liquidity_baking_sunset_level" int32)
               (req "liquidity_baking_escape_ema_threshold" int32))))
end
