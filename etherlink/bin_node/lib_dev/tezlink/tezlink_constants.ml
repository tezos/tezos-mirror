(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
open Tezlink_imports

type fixed = Alpha_context.Constants.fixed

type parametric = Alpha_context.Constants.Parametric.t

type t = {fixed : fixed; parametric : parametric}

let fixed_values_encoding =
  let open Data_encoding in
  merge_objs
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
    (obj4
       (req "max_slashing_period" uint8)
       (req "smart_rollup_max_wrapped_proof_binary_size" int31)
       (req "smart_rollup_message_size_limit" int31)
       (req "smart_rollup_max_number_of_messages_per_level" n))

let values_to_fixed =
  Tezos_types.convert_using_serialization
    ~name:"values_to_fixed"
    ~dst:Alpha_context.Constants.fixed_encoding
    ~src:fixed_values_encoding

let constants_encoding =
  let open Data_encoding in
  conv
    (fun {fixed; parametric} -> (fixed, parametric))
    (fun (fixed, parametric) -> {fixed; parametric})
    (obj2
       (req "fixed" Alpha_context.Constants.fixed_encoding)
       (req "parametric" Alpha_context.Constants.Parametric.encoding))

let convert : t -> Alpha_context.Constants.t tzresult =
  Tezos_types.convert_using_serialization
    ~name:"constants"
    ~dst:Alpha_context.Constants.encoding
    ~src:constants_encoding

let mainnet = Imported_protocol_parameters.Default_parameters.constants_mainnet

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
  Imported_protocol.Constants_repr.
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
