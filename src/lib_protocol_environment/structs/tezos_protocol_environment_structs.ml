module V0 = struct
  open Tezos_protocol_environment_structs_v0
  module S = S
  module Option = Option
  module MBytes = MBytes
  module Blake2B = Blake2B
  module Ed25519 = Ed25519
  module Secp256k1 = Secp256k1
  module P256 = P256
  module Signature = Signature
  module Chain_id = Chain_id
  module Block_hash = Block_hash
  module Operation_hash = Operation_hash
  module Operation_list_hash = Operation_list_hash
  module Operation_list_list_hash = Operation_list_list_hash
  module Protocol_hash = Protocol_hash
  module Context_hash = Context_hash
  module Error_monad_traversors = Error_monad_traversors
  module Data_encoding = Struct_data_encoding
  module Error_monad_infix_globals = Error_monad_infix_globals
  module Error_monad_trace_eval = Error_monad_trace_eval
  module Error_monad_classification = Error_monad_classification
end

module V1 = struct
  open Tezos_protocol_environment_structs_v1
  module S = V0.S
  module Blake2B = V0.Blake2B
  module Ed25519 = V0.Ed25519
  module Secp256k1 = V0.Secp256k1
  module P256 = V0.P256
  module Signature = V0.Signature
  module Chain_id = V0.Chain_id
  module Block_hash = V0.Block_hash
  module Operation_hash = V0.Operation_hash
  module Operation_list_hash = V0.Operation_list_hash
  module Operation_list_list_hash = V0.Operation_list_list_hash
  module Protocol_hash = V0.Protocol_hash
  module Context_hash = V0.Context_hash
  module Error_monad_traversors = V0.Error_monad_traversors
  module Data_encoding = V0.Data_encoding
  module Option = Option
  module Bls12_381 = Struct_bls12_381
  module Error_monad_preallocated_values = Error_monad_preallocated_values
  module Hex = Struct_hex
  module Error_monad_infix_globals = V0.Error_monad_infix_globals
  module Error_monad_trace_eval = V0.Error_monad_trace_eval
  module Error_monad_classification = V0.Error_monad_classification
end

module V2 = struct
  module S = V0.S
  module Blake2B = V0.Blake2B
  module Ed25519 = V0.Ed25519
  module Secp256k1 = V0.Secp256k1
  module P256 = V0.P256
  module Signature = V0.Signature
  module Chain_id = V0.Chain_id
  module Block_hash = V0.Block_hash
  module Operation_hash = V0.Operation_hash
  module Operation_list_hash = V0.Operation_list_hash
  module Operation_list_list_hash = V0.Operation_list_list_hash
  module Protocol_hash = V0.Protocol_hash
  module Context_hash = V0.Context_hash
  module Error_monad_traversors = V0.Error_monad_traversors
  module Data_encoding = V0.Data_encoding
  module Bls12_381 = V1.Bls12_381
  module Error_monad_preallocated_values = V1.Error_monad_preallocated_values
  module Hex = V1.Hex
  module Error_monad_infix_globals = V0.Error_monad_infix_globals
  module Error_monad_trace_eval = V0.Error_monad_trace_eval
  module Error_monad_classification = V0.Error_monad_classification
end

module V3 = struct
  open Tezos_protocol_environment_structs_v3
  module Data_encoding = Struct_data_encoding
  module Replicated_signatures = Replicated_signatures
  module Lwtreslib_list_combine = Lwtreslib_list_combine
  module Bls12_381 = V1.Bls12_381
  module Error_monad_preallocated_values = V1.Error_monad_preallocated_values
  module Hex = V1.Hex
  module Error_monad_infix_globals = V0.Error_monad_infix_globals
  module Error_monad_trace_eval = V0.Error_monad_trace_eval
  module Error_monad_classification = V0.Error_monad_classification
end

module V4 = struct
  module Data_encoding = V3.Data_encoding
  module Replicated_signatures = V3.Replicated_signatures
  module Lwtreslib_list_combine = V3.Lwtreslib_list_combine
  module Error_monad_infix_globals = V0.Error_monad_infix_globals
end

module V5 = struct
  module Error_monad_infix_globals = V0.Error_monad_infix_globals
end

module V6 = struct
  module Error_monad_infix_globals = V0.Error_monad_infix_globals
end
