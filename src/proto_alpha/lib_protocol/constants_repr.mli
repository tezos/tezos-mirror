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

val version_number_004 : string

val version_number : string

val proof_of_work_nonce_size : int

val nonce_length : int

val max_anon_ops_per_block : int

val max_proposals_per_delegate : int

val max_operation_data_length : int

type fixed = {
  proof_of_work_nonce_size : int;
  nonce_length : int;
  max_anon_ops_per_block : int;
  max_operation_data_length : int;
  max_proposals_per_delegate : int;
}

val fixed_encoding : fixed Data_encoding.encoding

val fixed : fixed

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
  michelson_maximum_type_size : int;
  seed_nonce_revelation_tip : Tez_repr.t;
  origination_size : int;
  block_security_deposit : Tez_repr.t;
  endorsement_security_deposit : Tez_repr.t;
  baking_reward_per_endorsement : Tez_repr.t list;
  endorsement_reward : Tez_repr.t list;
  cost_per_byte : Tez_repr.t;
  hard_storage_limit_per_operation : Z.t;
  (* in seconds *)
  quorum_min : int32;
  quorum_max : int32;
  min_proposal_quorum : int32;
  initial_endorsers : int;
  delay_per_missing_endorsement : Period_repr.t;
}

val parametric_encoding : parametric Data_encoding.encoding

type t = {fixed : fixed; parametric : parametric}

val encoding : t Data_encoding.encoding
