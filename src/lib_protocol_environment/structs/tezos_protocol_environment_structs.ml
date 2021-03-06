module V0 = struct
  module S = V0_s
  module Option = V0_option
  module MBytes = V0_mBytes
  module Blake2B = V0_blake2B
  module Ed25519 = V0_ed25519
  module Secp256k1 = V0_secp256k1
  module P256 = V0_p256
  module Signature = V0_signature
  module Chain_id = V0_chain_id
  module Block_hash = V0_block_hash
  module Operation_hash = V0_operation_hash
  module Operation_list_hash = V0_operation_list_hash
  module Operation_list_list_hash = V0_operation_list_list_hash
  module Protocol_hash = V0_protocol_hash
  module Context_hash = V0_context_hash
  module Error_monad_traversors = V0_error_monad_traversors
  module Data_encoding = V0_data_encoding
  module Error_monad_infix_globals = V0_error_monad_infix_globals
  module Error_monad_trace_eval = V0_error_monad_trace_eval
end

module V1 = struct
  module S = V0_s
  module Blake2B = V0_blake2B
  module Ed25519 = V0_ed25519
  module Secp256k1 = V0_secp256k1
  module P256 = V0_p256
  module Signature = V0_signature
  module Chain_id = V0_chain_id
  module Block_hash = V0_block_hash
  module Operation_hash = V0_operation_hash
  module Operation_list_hash = V0_operation_list_hash
  module Operation_list_list_hash = V0_operation_list_list_hash
  module Protocol_hash = V0_protocol_hash
  module Context_hash = V0_context_hash
  module Error_monad_traversors = V0_error_monad_traversors
  module Data_encoding = V0_data_encoding
  module Option = V1_option
  module Bls12_381 = V1_bls12_381
  module Error_monad_preallocated_values = V1_error_monad_preallocated_values
  module Hex = V1_hex
  module Error_monad_infix_globals = V0_error_monad_infix_globals
  module Error_monad_trace_eval = V0_error_monad_trace_eval
end

module V2 = struct
  module S = V0_s
  module Blake2B = V0_blake2B
  module Ed25519 = V0_ed25519
  module Secp256k1 = V0_secp256k1
  module P256 = V0_p256
  module Signature = V0_signature
  module Chain_id = V0_chain_id
  module Block_hash = V0_block_hash
  module Operation_hash = V0_operation_hash
  module Operation_list_hash = V0_operation_list_hash
  module Operation_list_list_hash = V0_operation_list_list_hash
  module Protocol_hash = V0_protocol_hash
  module Context_hash = V0_context_hash
  module Error_monad_traversors = V0_error_monad_traversors
  module Data_encoding = V0_data_encoding
  module Bls12_381 = V1_bls12_381
  module Error_monad_preallocated_values = V1_error_monad_preallocated_values
  module Hex = V1_hex
  module Error_monad_infix_globals = V0_error_monad_infix_globals
  module Error_monad_trace_eval = V0_error_monad_trace_eval
end

module V3 = struct
  module Data_encoding = V3_data_encoding
  module Replicated_signatures = V3_replicated_signatures
  module Lwtreslib_list_combine = V3_lwtreslib_list_combine
  module Bls12_381 = V1_bls12_381
  module Error_monad_preallocated_values = V1_error_monad_preallocated_values
  module Hex = V1_hex
  module Error_monad_infix_globals = V0_error_monad_infix_globals
  module Error_monad_trace_eval = V0_error_monad_trace_eval
end

module V4 = struct
  module Data_encoding = V3_data_encoding
  module Replicated_signatures = V3_replicated_signatures
  module Lwtreslib_list_combine = V3_lwtreslib_list_combine
  module Error_monad_infix_globals = V0_error_monad_infix_globals
end

module V5 = struct
  module Error_monad_infix_globals = V0_error_monad_infix_globals
end

module V6 = struct
  module Error_monad_infix_globals = V0_error_monad_infix_globals
  module Plonk = V6_plonk
end

module V7 = struct
  module Error_monad_infix_globals = V0_error_monad_infix_globals
  module Plonk = V7_plonk
end
