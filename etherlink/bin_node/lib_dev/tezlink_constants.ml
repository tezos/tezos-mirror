(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let mainnet =
  Tezos_services.Imported_protocol_parameters.Default_parameters
  .constants_mainnet

let ( proof_of_work_nonce_size,
      nonce_length,
      max_anon_ops_per_block,
      max_operation_data_length,
      max_proposals_per_delegate,
      max_micheline_node_count,
      max_micheline_bytes_limit,
      max_allowed_global_constant_depth,
      cache_layout_size,
      michelson_maximum_type_size,
      max_slashing_period,
      sc_max_wrapped_proof_binary_size,
      sc_rollup_message_size_limit,
      sc_rollup_max_number_of_messages_per_level ) =
  Tezos_services.Imported_protocol.Constants_repr.
    ( proof_of_work_nonce_size,
      nonce_length,
      max_anon_ops_per_block,
      max_operation_data_length,
      max_proposals_per_delegate,
      max_micheline_node_count,
      max_micheline_bytes_limit,
      max_allowed_global_constant_depth,
      cache_layout_size,
      michelson_maximum_type_size,
      max_slashing_period,
      sc_max_wrapped_proof_binary_size,
      sc_rollup_message_size_limit,
      sc_rollup_max_number_of_messages_per_level )
