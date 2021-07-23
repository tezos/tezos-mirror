(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

let version_number_004 = "\000"

let version_number = "\001"

let proof_of_work_nonce_size = 8

let nonce_length = 32

let max_anon_ops_per_block = 132

let max_proposals_per_delegate = 20

let max_operation_data_length = 32 * 1024 (* 32kB *)

let max_micheline_node_count = 50_000

let max_micheline_bytes_limit = 50_000

let max_allowed_global_constant_depth = 10_000

(* In this version of the protocol, there is a single cache for
   contract source code and storage. Its size has been chosen
   not too exceed 100 000 000 bytes. *)
let cache_layout = [100_000_000]

let michelson_maximum_type_size = 1000

type fixed = {
  proof_of_work_nonce_size : int;
  nonce_length : int;
  max_anon_ops_per_block : int;
  max_operation_data_length : int;
  max_proposals_per_delegate : int;
  max_micheline_node_count : int;
  max_micheline_bytes_limit : int;
  max_allowed_global_constant_depth : int;
}

let fixed_encoding =
  let open Data_encoding in
  conv
    (fun c ->
      ( c.proof_of_work_nonce_size,
        c.nonce_length,
        c.max_anon_ops_per_block,
        c.max_operation_data_length,
        c.max_proposals_per_delegate,
        c.max_micheline_node_count,
        c.max_micheline_bytes_limit,
        max_allowed_global_constant_depth ))
    (fun ( proof_of_work_nonce_size,
           nonce_length,
           max_anon_ops_per_block,
           max_operation_data_length,
           max_proposals_per_delegate,
           max_micheline_node_count,
           max_micheline_bytes_limit,
           max_allowed_global_constant_depth ) ->
      {
        proof_of_work_nonce_size;
        nonce_length;
        max_anon_ops_per_block;
        max_operation_data_length;
        max_proposals_per_delegate;
        max_micheline_node_count;
        max_micheline_bytes_limit;
        max_allowed_global_constant_depth;
      })
    (obj8
       (req "proof_of_work_nonce_size" uint8)
       (req "nonce_length" uint8)
       (req "max_anon_ops_per_block" uint8)
       (req "max_operation_data_length" int31)
       (req "max_proposals_per_delegate" uint8)
       (req "max_micheline_node_count" int31)
       (req "max_micheline_bytes_limit" int31)
       (req "max_allowed_global_constants_depth" int31))

let fixed =
  {
    proof_of_work_nonce_size;
    nonce_length;
    max_anon_ops_per_block;
    max_operation_data_length;
    max_proposals_per_delegate;
    max_micheline_node_count;
    max_micheline_bytes_limit;
    max_allowed_global_constant_depth;
  }

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

type t = {fixed : fixed; parametric : parametric}

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
  let min_time_between_blocks =
    match constants.time_between_blocks with
    | first_time_between_blocks :: _ -> first_time_between_blocks
    | [] ->
        (* this constant is used in the Baking module *)
        Period_repr.one_minute
  in
  error_unless
    Compare.Int64.(
      Period_repr.to_seconds min_time_between_blocks
      >= Period_repr.to_seconds constants.minimal_block_delay)
    (Invalid_protocol_constants
       (Format.asprintf
          "minimal_block_delay value (%Ld) should be smaller than \
           time_between_blocks[0] value (%Ld)"
          (Period_repr.to_seconds constants.minimal_block_delay)
          (Period_repr.to_seconds min_time_between_blocks)))
  >>? fun () ->
  error_unless
    Compare.Int.(constants.endorsers_per_block >= constants.initial_endorsers)
    (Invalid_protocol_constants
       "initial_endorsers should be smaller than endorsers_per_block")

module Proto_previous = struct
  type parametric = {
    preserved_cycles : int;
    blocks_per_cycle : int32;
    blocks_per_commitment : int32;
    blocks_per_roll_snapshot : int32;
    blocks_per_voting_period : int32;
    time_between_blocks : Period_repr.t list;
    endorsers_per_block : int;
    hard_gas_limit_per_operation : Gas_limit_repr.Arith.integral;
    hard_gas_limit_per_block : Gas_limit_repr.Arith.integral;
    proof_of_work_threshold : int64;
    tokens_per_roll : Tez_repr.t;
    michelson_maximum_type_size : int;
    seed_nonce_revelation_tip : Tez_repr.t;
    origination_size : int;
    block_security_deposit : Tez_repr.t;
    endorsement_security_deposit : Tez_repr.t;
    baking_reward_per_endorsement : Tez_repr.t list;
    endorsement_reward : Tez_repr.t list;
    cost_per_byte : Tez_repr.t;
    hard_storage_limit_per_operation : Z.t;
    test_chain_duration : int64;
    (* in seconds *)
    quorum_min : int32;
    quorum_max : int32;
    min_proposal_quorum : int32;
    initial_endorsers : int;
    delay_per_missing_endorsement : Period_repr.t;
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
            c.hard_gas_limit_per_block ),
          ( ( c.proof_of_work_threshold,
              c.tokens_per_roll,
              c.michelson_maximum_type_size,
              c.seed_nonce_revelation_tip,
              c.origination_size,
              c.block_security_deposit,
              c.endorsement_security_deposit,
              c.baking_reward_per_endorsement ),
            ( c.endorsement_reward,
              c.cost_per_byte,
              c.hard_storage_limit_per_operation,
              c.test_chain_duration,
              c.quorum_min,
              c.quorum_max,
              c.min_proposal_quorum,
              c.initial_endorsers,
              c.delay_per_missing_endorsement ) ) ))
      (fun ( ( preserved_cycles,
               blocks_per_cycle,
               blocks_per_commitment,
               blocks_per_roll_snapshot,
               blocks_per_voting_period,
               time_between_blocks,
               endorsers_per_block,
               hard_gas_limit_per_operation,
               hard_gas_limit_per_block ),
             ( ( proof_of_work_threshold,
                 tokens_per_roll,
                 michelson_maximum_type_size,
                 seed_nonce_revelation_tip,
                 origination_size,
                 block_security_deposit,
                 endorsement_security_deposit,
                 baking_reward_per_endorsement ),
               ( endorsement_reward,
                 cost_per_byte,
                 hard_storage_limit_per_operation,
                 test_chain_duration,
                 quorum_min,
                 quorum_max,
                 min_proposal_quorum,
                 initial_endorsers,
                 delay_per_missing_endorsement ) ) ) ->
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
          michelson_maximum_type_size;
          seed_nonce_revelation_tip;
          origination_size;
          block_security_deposit;
          endorsement_security_deposit;
          baking_reward_per_endorsement;
          endorsement_reward;
          cost_per_byte;
          hard_storage_limit_per_operation;
          test_chain_duration;
          quorum_min;
          quorum_max;
          min_proposal_quorum;
          initial_endorsers;
          delay_per_missing_endorsement;
        })
      (merge_objs
         (obj9
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
               Gas_limit_repr.Arith.z_integral_encoding))
         (merge_objs
            (obj8
               (req "proof_of_work_threshold" int64)
               (req "tokens_per_roll" Tez_repr.encoding)
               (req "michelson_maximum_type_size" uint16)
               (req "seed_nonce_revelation_tip" Tez_repr.encoding)
               (req "origination_size" int31)
               (req "block_security_deposit" Tez_repr.encoding)
               (req "endorsement_security_deposit" Tez_repr.encoding)
               (req "baking_reward_per_endorsement" (list Tez_repr.encoding)))
            (obj9
               (req "endorsement_reward" (list Tez_repr.encoding))
               (req "cost_per_byte" Tez_repr.encoding)
               (req "hard_storage_limit_per_operation" z)
               (req "test_chain_duration" int64)
               (req "quorum_min" int32)
               (req "quorum_max" int32)
               (req "min_proposal_quorum" int32)
               (req "initial_endorsers" uint16)
               (req "delay_per_missing_endorsement" Period_repr.encoding))))
end
