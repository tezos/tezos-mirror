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
let version_value = "t024_024"

let version = "v1"

let mainnet_id = Chain_id.of_b58check_exn "NetXdQprcVkpaWU"

let shadownet_id = Chain_id.of_b58check_exn "NetXsqzbfFenSTS"

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

(* /!\ Several parts of the codebase may assume that
   [denunciation_period = 1] and [slashing_delay = 1] **without being
   parametrized using these constants**. So if they are ever modified,
   the codebase needs to be examined extensively; searching for places
   that use these constants is not enough. *)
let denunciation_period = 1

let slashing_delay = denunciation_period

(* The {!Sc_rollups.wrapped_proof_encoding} uses unbounded sub-encodings.
   To avoid attacks through too large proofs and long decoding times on public
   nodes, we put another layer of security by restricting the maximum_size
   to [30Kb].

   Even if the operation size limit is currently [32Kb] (see
   {!Constants_repr.max_operation_data_length}) the node's mempool can still
   be spammed with larger proofs before detecting that the operations are
   indeed larger than the limit.

   By design, the proofs should be created and verified for a single tick
   which should limit the number of read/writes in the Merkle tree, and thefore,
   limit the total size of a proof. Thus, [30Kb] can be lowered once we
   have empirically observed that a valid proof can not be that large.

   Note that an encoded proof that is [30Kb] might still be not included
   in a valid L1 operation. The refutation operation also contains other
   information such as an inbox proof. We only put here an upper bound
   for the size.
*)
let sc_max_wrapped_proof_binary_size = 30_000

(* A limit on the size of the binary encoding of sc rollup messages. This limit
   depends on the assumed overhead of the proof and metadata in a manager
   operation justifying the existence of some chunk of data in the rollup state.
   The value of this constant reflects the global constant of 4KB in the WASM
   PVM specification chosen for the limit of chunks that are embedded in proofs.
*)
let sc_rollup_message_size_limit = 4_096

(** A limit on the number of messages per inbox level.

    Benchmarks have shown that proving the inclusion of the element at
    index 0 in a skip list of [1_000_000] elements is ~=6Kb large.
*)
let sc_rollup_max_number_of_messages_per_level = Z.of_int 1_000_000

type fixed = unit

let fixed_encoding =
  let open Data_encoding in
  conv
    (fun () ->
      ( ( proof_of_work_nonce_size,
          nonce_length,
          max_anon_ops_per_block,
          max_operation_data_length,
          max_proposals_per_delegate,
          max_micheline_node_count,
          max_micheline_bytes_limit,
          max_allowed_global_constant_depth,
          cache_layout_size,
          michelson_maximum_type_size ),
        ( denunciation_period,
          slashing_delay,
          sc_max_wrapped_proof_binary_size,
          sc_rollup_message_size_limit,
          sc_rollup_max_number_of_messages_per_level ) ))
    (fun ( ( _proof_of_work_nonce_size,
             _nonce_length,
             _max_anon_ops_per_block,
             _max_operation_data_length,
             _max_proposals_per_delegate,
             _max_micheline_node_count,
             _max_micheline_bytes_limit,
             _max_allowed_global_constant_depth,
             _cache_layout_size,
             _michelson_maximum_type_size ),
           ( _denunciation_period,
             _slashing_delay,
             _sc_max_wrapped_proof_binary_size,
             _sc_rollup_message_size_limit,
             _sc_rollup_number_of_messages_per_level ) )
       -> ())
    (merge_objs
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
       (obj5
          (req "denunciation_period" uint8)
          (req "slashing_delay" uint8)
          (req "smart_rollup_max_wrapped_proof_binary_size" int31)
          (req "smart_rollup_message_size_limit" int31)
          (req "smart_rollup_max_number_of_messages_per_level" n)))

type error += Invalid_protocol_constants of string (* `Permanent *)

let () =
  register_error_kind
    `Permanent
    ~id:"constants.invalid_protocol_constants"
    ~title:"Invalid protocol constants"
    ~description:"The provided protocol constants are not coherent."
    ~pp:(fun ppf reason ->
      Format.fprintf ppf "Invalid protocol constants: %s" reason)
    Data_encoding.(obj1 (req "reason" @@ string Plain))
    (function Invalid_protocol_constants reason -> Some reason | _ -> None)
    (fun reason -> Invalid_protocol_constants reason)

let check_constants constants =
  let open Result_syntax in
  let open Constants_parametric_repr in
  let* () =
    error_unless
      Period_repr.(constants.minimal_block_delay > zero)
      (Invalid_protocol_constants
         "The minimal block delay must be greater than zero")
  in
  let* () =
    (* Allowing [delay_increment_per_round] = 0 is intended only for test
       environments. This value MUST be strictly positive on any actual
       network to ensure strictly increasing round durations, as required by
       Tenderbake. *)
    error_unless
      Period_repr.(constants.delay_increment_per_round >= zero)
      (Invalid_protocol_constants
         "The delay increment per round must be non-negative")
  in
  let* () =
    error_unless
      Compare.Int.(constants.consensus_committee_size > 0)
      (Invalid_protocol_constants
         "The consensus committee size must be strictly greater than 0.")
  in
  let* () =
    error_unless
      Compare.Int.(
        constants.consensus_threshold_size >= 0
        && constants.consensus_threshold_size
           <= constants.consensus_committee_size)
      (Invalid_protocol_constants
         "The consensus threshold must be greater than or equal to 0 and less \
          than or equal to the consensus commitee size.")
  in
  let* () =
    error_unless
      (let Ratio_repr.{numerator; denominator} =
         constants.minimal_participation_ratio
       in
       Compare.Int.(numerator >= 0 && denominator > 0))
      (Invalid_protocol_constants
         "The minimal participation ratio must be a non-negative valid ratio.")
  in
  let* () =
    error_unless
      Compare.Int.(
        constants.minimal_participation_ratio.numerator
        <= constants.minimal_participation_ratio.denominator)
      (Invalid_protocol_constants
         "The minimal participation ratio must be less than or equal to 100%.")
  in
  (* The [limit_of_delegation_over_baking] should be non-negative. *)
  let* () =
    error_unless
      Compare.Int.(constants.limit_of_delegation_over_baking >= 0)
      (Invalid_protocol_constants
         "The delegation over baking limit must be greater than or equal to 0.")
  in
  let* () =
    error_unless
      Compare.Int32.(
        constants.nonce_revelation_threshold > Int32.zero
        && constants.nonce_revelation_threshold < constants.blocks_per_cycle)
      (Invalid_protocol_constants
         "The nonce revelation threshold must be strictly smaller than \
          blocks_per_cycle and strictly positive.")
  in
  let* () =
    let benchmark_ips = Int64.of_int 200_000 in
    let difficulty period_in_blocks ~ips ~factor =
      let block_time = Period_repr.to_seconds constants.minimal_block_delay in
      let period = Int64.mul period_in_blocks block_time in
      Int64.(mul (mul ips (of_int factor)) period)
    in
    let* () =
      error_unless
        Compare.Int64.(
          let ips =
            (* We reduce the ips for short blocks_per_commitment so that we have
               low difficulty during tests *)
            if Compare.Int32.(constants.blocks_per_commitment > 32l) then
              benchmark_ips
            else Int64.one
          in
          let difficulty =
            difficulty
              (Int64.of_int32 constants.nonce_revelation_threshold)
              ~ips
              ~factor:5
          in
          constants.vdf_difficulty >= difficulty)
        (Invalid_protocol_constants
           "The VDF difficulty must be greater than the number of modulo \
            squarings in class groups that can be done on the faster CPU \
            during the nonce revelation period.")
    in
    error_unless
      Compare.Int64.(
        let difficulty =
          difficulty
            (Int64.of_int32 constants.blocks_per_cycle)
            ~ips:benchmark_ips
            ~factor:1
        in
        constants.vdf_difficulty < difficulty)
      (Invalid_protocol_constants
         "The VDF difficulty must be strictly smaller than the number of \
          modulo squarings in class groups that can be done on the benchmark \
          CPU during one cycle.")
  in
  let* () =
    error_unless
      Compare.Int.(constants.sc_rollup.origination_size >= 0)
      (Invalid_protocol_constants
         "The smart rollup origination size must be non-negative.")
  in
  let* () =
    error_unless
      Compare.Int.(constants.sc_rollup.challenge_window_in_blocks >= 0)
      (Invalid_protocol_constants
         "The smart rollup challenge window in blocks must be non-negative.")
  in
  let* () =
    error_unless
      Tez_repr.(constants.sc_rollup.stake_amount >= zero)
      (Invalid_protocol_constants
         "The smart rollup max stake amount must be non-negative.")
  in
  let* () =
    error_unless
      Compare.Int.(constants.sc_rollup.commitment_period_in_blocks > 0)
      (Invalid_protocol_constants
         "The smart rollup commitment period in blocks must be strictly \
          greater than 0.")
  in
  let* () =
    error_unless
      (let sc_rollup_max_lookahead_in_blocks =
         constants.sc_rollup.max_lookahead_in_blocks
       in
       Compare.Int32.(
         sc_rollup_max_lookahead_in_blocks
         > Int32.of_int constants.sc_rollup.commitment_period_in_blocks
         &&
         (* Check that [smart_rollup_challenge_window_in_blocks <
               smart_rollup_max_lookahead_in_blocks]. Otherwise committers would be
               forced to commit at an artificially slow rate, affecting the
               throughput of the rollup. *)
         sc_rollup_max_lookahead_in_blocks
         > Int32.of_int constants.sc_rollup.challenge_window_in_blocks))
      (Invalid_protocol_constants
         "The smart rollup max lookahead in blocks must be greater than \
          [smart_rollup_commitment_period_in_blocks] and \
          [smart_rollup_challenge_window_in_blocks].")
  in
  let* () =
    error_unless
      Compare.Int.(
        constants.dal.number_of_slots > 0
        && constants.dal.number_of_slots <= 256)
      (Invalid_protocol_constants
         "The number of data availability slot must be between 1 and 256")
  in
  let* () =
    error_unless
      Compare.Int.(constants.dal.attestation_lag > 1)
      (Invalid_protocol_constants
         "The attestation_lag must be strictly greater than 1, because only \
          slot headers in finalized blocks are attested.")
  in
  let* () =
    error_unless
      Compare.Int.(
        constants.dal.cryptobox_parameters.number_of_shards
        <= constants.consensus_committee_size)
      (Invalid_protocol_constants
         "The DAL committee must be a subset of the Tenderbake committee.")
  in
  let* () =
    error_unless
      Q.(constants.dal.rewards_ratio < one)
      (Invalid_protocol_constants
         "The DAL rewards_ratio should be strictly less than one.")
  in
  let* () =
    error_unless
      Compare.Int.(
        constants.sc_rollup.max_number_of_stored_cemented_commitments > 0)
      (Invalid_protocol_constants
         "The number of maximum stored cemented commitments must be strictly \
          positive")
  in
  let* () =
    error_unless
      Compare.Int.(
        constants.cache_stake_distribution_cycles
        = constants.consensus_rights_delay + slashing_delay + 2)
      (Invalid_protocol_constants
         (Format.sprintf
            "We should have cache_stake_distribution_cycles (%d) = \
             consensus_rights_delay (%d) + slashing_delay (%d) + 2."
            constants.cache_stake_distribution_cycles
            constants.consensus_rights_delay
            slashing_delay))
  in
  let* () =
    error_unless
      Compare.Int.(
        constants.cache_sampler_state_cycles
        = constants.cache_stake_distribution_cycles)
      (Invalid_protocol_constants
         (Format.sprintf
            "The number cached cycles for the sampler state (%d) and for the \
             stake distribution (%d) should currently be the same."
            constants.cache_sampler_state_cycles
            constants.cache_stake_distribution_cycles))
  in
  Result.return_unit

module Generated = struct
  type t = {
    consensus_threshold_size : int;
    issuance_weights : Constants_parametric_repr.issuance_weights;
  }

  let generate ~consensus_committee_size ~dal_rewards_ratio =
    (* The weights are expressed in [(256 * 80)]th of the total
       reward, because it is the smallest proportion used so far *)
    (* let f = consensus_committee_size / 3 in *)
    let consensus_threshold_size = (consensus_committee_size * 2 / 3) + 1 in
    let bonus_committee_size =
      consensus_committee_size - consensus_threshold_size
    in
    let base_total_issued_per_minute = Tez_repr.of_mutez_exn 80_007_812L in
    let reward_parts_whole =
      20480
      (* = 256 * 80 *)
    in
    let reward_parts_half =
      10240
      (* = reward_parts_whole / 2 *)
    in
    let reward_parts_quarter =
      5120
      (* = reward_parts_whole / 4 *)
    in
    let baking_reward_fixed_portion_weight =
      (* 1/4 or 1/2 *)
      if Compare.Int.(bonus_committee_size <= 0) then
        (* a fortiori, consensus_committee_size < 4 *)
        reward_parts_half
      else reward_parts_quarter
    in
    let baking_reward_bonus_weight =
      (* 1/4 or 0 *)
      if Compare.Int.(bonus_committee_size <= 0) then 0
      else reward_parts_quarter
    in
    let attesting_reward_weight =
      (* 1/2 *)
      reward_parts_half
    in
    (* All block (baking + attesting) rewards sum to 1 ( *256*80 ) *)
    let seed_nonce_revelation_tip_weight = (* 1/20480 *) 1 in
    let vdf_revelation_tip_weight = (* 1/20480 *) 1 in
    let sum_non_dal_weights =
      reward_parts_whole + seed_nonce_revelation_tip_weight
      + vdf_revelation_tip_weight
    in
    (* Compute the weight of DAL rewards such that these represent
       [dal_rewards_ratio] of the total rewards. *)
    let dal_rewards_weight =
      let open Q in
      div
        (mul (of_int sum_non_dal_weights) dal_rewards_ratio)
        (sub one dal_rewards_ratio)
      |> to_int
    in
    {
      consensus_threshold_size;
      issuance_weights =
        {
          base_total_issued_per_minute;
          (* 80.007812 tez/minute *)
          baking_reward_fixed_portion_weight;
          baking_reward_bonus_weight;
          attesting_reward_weight;
          dal_rewards_weight;
          seed_nonce_revelation_tip_weight;
          vdf_revelation_tip_weight;
        };
    }
end

module Derived = struct
  (** Number of full cycles after which the issuance rate -- computed
      from current stake over total supply -- will be used.

      That is, the issuance rate corresponding to the staking ratio at
      the end of cycle n takes effect at the beginning of cycle n +
      issuance_modification_delay + 1.

      We use consensus_rights_delay so that the issuance rate in one
      cycle corresponds to the "active" baking power. *)
  let issuance_modification_delay ~(parametric : Constants_parametric_repr.t) =
    parametric.consensus_rights_delay

  (** Number of full cycles between the declaration of a new consensus
      key by a delegate and when it has to be used to sign on behalf of
      the delegate.

      That is, a key registered at any time during cycle n has to be
      used from cycle n + consensus_key_activation_delay + 1 on. *)
  let consensus_key_activation_delay ~(parametric : Constants_parametric_repr.t)
      =
    parametric.consensus_rights_delay

  (** Number of full cycles to wait for an unstake request to become
      finalizable.

      That is, if the unstake is requested in cycle n, then it
      becomes finalizable at the beginning of cycle n +
      unstake_finalization_delay + 1.

      The exact waiting time depends on when the unstake operation
      happened inside cycle n, but it is always at least n +
      unstake_finalization_delay cycles and less than n +
      unstake_finalization_delay + 1 cycles. *)
  let unstake_finalization_delay ~(parametric : Constants_parametric_repr.t) =
    parametric.consensus_rights_delay + slashing_delay

  type t = {
    issuance_modification_delay : int;
    consensus_key_activation_delay : int;
    unstake_finalization_delay : int;
  }

  let encoding : t Data_encoding.encoding =
    let open Data_encoding in
    conv
      (fun {
             issuance_modification_delay;
             consensus_key_activation_delay;
             unstake_finalization_delay;
           }
         ->
        ( issuance_modification_delay,
          consensus_key_activation_delay,
          unstake_finalization_delay ))
      (fun ( issuance_modification_delay,
             consensus_key_activation_delay,
             unstake_finalization_delay )
         ->
        {
          issuance_modification_delay;
          consensus_key_activation_delay;
          unstake_finalization_delay;
        })
      (obj3
         (req "issuance_modification_delay" uint8)
         (req "consensus_key_activation_delay" uint8)
         (req "unstake_finalization_delay" uint8))

  let of_parametric parametric =
    {
      issuance_modification_delay = issuance_modification_delay ~parametric;
      consensus_key_activation_delay =
        consensus_key_activation_delay ~parametric;
      unstake_finalization_delay = unstake_finalization_delay ~parametric;
    }
end

let fixed = ()

type t = {
  fixed : fixed;
  parametric : Constants_parametric_repr.t;
  derived : Derived.t;
}

let all_of_parametric parametric =
  {fixed; parametric; derived = Derived.of_parametric parametric}

let encoding =
  let open Data_encoding in
  conv
    (fun {fixed; parametric; derived} -> ((fixed, parametric), derived))
    (fun ((fixed, parametric), derived) -> {fixed; parametric; derived})
    (merge_objs
       (merge_objs fixed_encoding Constants_parametric_repr.encoding)
       Derived.encoding)

let cache_layout p =
  Constants_parametric_repr.
    [
      p.cache_script_size;
      p.cache_stake_distribution_cycles;
      p.cache_sampler_state_cycles;
    ]
