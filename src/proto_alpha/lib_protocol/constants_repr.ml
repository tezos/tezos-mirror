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

type t = {fixed : fixed; parametric : Constants_parametric_repr.t}

let all_of_parametric parametric = {fixed; parametric}

let encoding =
  let open Data_encoding in
  conv
    (fun {fixed; parametric} -> (fixed, parametric))
    (fun (fixed, parametric) -> {fixed; parametric})
    (merge_objs fixed_encoding Constants_parametric_repr.encoding)

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
  let open Constants_parametric_repr in
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

let cache_layout p =
  Constants_parametric_repr.
    [
      p.cache_script_size;
      p.cache_stake_distribution_cycles;
      p.cache_sampler_state_cycles;
    ]
