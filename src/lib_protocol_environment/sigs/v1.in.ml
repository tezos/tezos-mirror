open Tezos_protocol_environment_sigs_stdlib_compat.V_all

module type T = sig
  module Pervasives : [%sig "v1/pervasives.mli"] [@@coq_plain_module]

  open Pervasives

  module List : [%sig "v1/list.mli"] [@@coq_plain_module]

  module String : [%sig "v1/string.mli"] [@@coq_plain_module]

  module Char : [%sig "v1/char.mli"] [@@coq_plain_module]

  module Bytes : [%sig "v1/bytes.mli"] [@@coq_plain_module]

  module Int32 : [%sig "v1/int32.mli"] [@@coq_plain_module]

  module Int64 : [%sig "v1/int64.mli"] [@@coq_plain_module]

  module Format : [%sig "v1/format.mli"] [@@coq_plain_module]

  module Hex : [%sig "v1/hex.mli"] [@@coq_plain_module]

  module Z : [%sig "v1/z.mli"] [@@coq_plain_module]

  module Lwt : [%sig "v1/lwt.mli"] [@@coq_plain_module]

  module Lwt_list : [%sig "v1/lwt_list.mli"] [@@coq_plain_module]

  module Data_encoding : [%sig "v1/data_encoding.mli"] [@@coq_plain_module]

  module Raw_hashes : [%sig "v1/raw_hashes.mli"] [@@coq_plain_module]

  module Compare : [%sig "v1/compare.mli"] [@@coq_plain_module]

  module Error_monad : [%sig "v1/error_monad.mli"] [@@coq_plain_module]

  open Error_monad

  module Logging : [%sig "v1/logging.mli"] [@@coq_plain_module]

  module Time : [%sig "v1/time.mli"] [@@coq_plain_module]

  module Option : [%sig "v1/option.mli"] [@@coq_plain_module]

  module TzEndian : [%sig "v1/tzEndian.mli"] [@@coq_plain_module]

  module RPC_arg : [%sig "v1/RPC_arg.mli"] [@@coq_plain_module]

  module RPC_path : [%sig "v1/RPC_path.mli"] [@@coq_plain_module]

  module RPC_query : [%sig "v1/RPC_query.mli"] [@@coq_plain_module]

  module RPC_service : [%sig "v1/RPC_service.mli"] [@@coq_plain_module]

  module RPC_answer : [%sig "v1/RPC_answer.mli"] [@@coq_plain_module]

  module RPC_directory : [%sig "v1/RPC_directory.mli"] [@@coq_plain_module]

  module Base58 : [%sig "v1/base58.mli"] [@@coq_plain_module]

  module S : [%sig "v1/s.mli"] [@@coq_plain_module]

  module Set : [%sig "v1/set.mli"] [@@coq_plain_module]

  module Map : [%sig "v1/map.mli"] [@@coq_plain_module]

  module Blake2B : [%sig "v1/blake2B.mli"] [@@coq_plain_module]

  module Bls12_381 : [%sig "v1/bls12_381.mli"] [@@coq_plain_module]

  module Ed25519 : [%sig "v1/ed25519.mli"] [@@coq_plain_module]

  module Secp256k1 : [%sig "v1/secp256k1.mli"] [@@coq_plain_module]

  module P256 : [%sig "v1/p256.mli"] [@@coq_plain_module]

  module Chain_id : [%sig "v1/chain_id.mli"] [@@coq_plain_module]

  module Signature : [%sig "v1/signature.mli"] [@@coq_plain_module]

  module Block_hash : [%sig "v1/block_hash.mli"] [@@coq_plain_module]

  module Operation_hash : [%sig "v1/operation_hash.mli"] [@@coq_plain_module]

  module Operation_list_hash : [%sig "v1/operation_list_hash.mli"]
  [@@coq_plain_module]

  module Operation_list_list_hash : [%sig "v1/operation_list_list_hash.mli"]
  [@@coq_plain_module]

  module Protocol_hash : [%sig "v1/protocol_hash.mli"] [@@coq_plain_module]

  module Context_hash : [%sig "v1/context_hash.mli"] [@@coq_plain_module]

  module Pvss_secp256k1 : [%sig "v1/pvss_secp256k1.mli"] [@@coq_plain_module]

  module Sapling : [%sig "v1/sapling.mli"] [@@coq_plain_module]

  module Micheline : [%sig "v1/micheline.mli"] [@@coq_plain_module]

  module Block_header : [%sig "v1/block_header.mli"] [@@coq_plain_module]

  module Fitness : [%sig "v1/fitness.mli"] [@@coq_plain_module]

  module Operation : [%sig "v1/operation.mli"] [@@coq_plain_module]

  module Context : [%sig "v1/context.mli"] [@@coq_plain_module]

  module Updater : [%sig "v1/updater.mli"] [@@coq_plain_module]

  module RPC_context : [%sig "v1/RPC_context.mli"] [@@coq_plain_module]
end
