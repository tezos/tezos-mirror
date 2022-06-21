open Tezos_protocol_environment_sigs_stdlib_compat.V_all

module type T = sig
  module Pervasives : [%sig "v2/pervasives.mli"] [@@coq_plain_module]

  open Pervasives

  module List : [%sig "v2/list.mli"] [@@coq_plain_module]

  module String : [%sig "v2/string.mli"] [@@coq_plain_module]

  module Char : [%sig "v2/char.mli"] [@@coq_plain_module]

  module Bytes : [%sig "v2/bytes.mli"] [@@coq_plain_module]

  module Int32 : [%sig "v2/int32.mli"] [@@coq_plain_module]

  module Int64 : [%sig "v2/int64.mli"] [@@coq_plain_module]

  module Format : [%sig "v2/format.mli"] [@@coq_plain_module]

  module Hex : [%sig "v2/hex.mli"] [@@coq_plain_module]

  module Z : [%sig "v2/z.mli"] [@@coq_plain_module]

  module Lwt : [%sig "v2/lwt.mli"] [@@coq_plain_module]

  module Lwt_list : [%sig "v2/lwt_list.mli"] [@@coq_plain_module]

  module Data_encoding : [%sig "v2/data_encoding.mli"] [@@coq_plain_module]

  module Raw_hashes : [%sig "v2/raw_hashes.mli"] [@@coq_plain_module]

  module Compare : [%sig "v2/compare.mli"] [@@coq_plain_module]

  module Error_monad : [%sig "v2/error_monad.mli"] [@@coq_plain_module]

  open Error_monad

  module Logging : [%sig "v2/logging.mli"] [@@coq_plain_module]

  module Time : [%sig "v2/time.mli"] [@@coq_plain_module]

  module Option : [%sig "v2/option.mli"] [@@coq_plain_module]

  module TzEndian : [%sig "v2/tzEndian.mli"] [@@coq_plain_module]

  module Bits : [%sig "v2/bits.mli"] [@@coq_plain_module]

  module RPC_arg : [%sig "v2/RPC_arg.mli"] [@@coq_plain_module]

  module RPC_path : [%sig "v2/RPC_path.mli"] [@@coq_plain_module]

  module RPC_query : [%sig "v2/RPC_query.mli"] [@@coq_plain_module]

  module RPC_service : [%sig "v2/RPC_service.mli"] [@@coq_plain_module]

  module RPC_answer : [%sig "v2/RPC_answer.mli"] [@@coq_plain_module]

  module RPC_directory : [%sig "v2/RPC_directory.mli"] [@@coq_plain_module]

  module Base58 : [%sig "v2/base58.mli"] [@@coq_plain_module]

  module S : [%sig "v2/s.mli"] [@@coq_plain_module]

  module Set : [%sig "v2/set.mli"] [@@coq_plain_module]

  module Map : [%sig "v2/map.mli"] [@@coq_plain_module]

  module Blake2B : [%sig "v2/blake2B.mli"] [@@coq_plain_module]

  module Bls12_381 : [%sig "v2/bls12_381.mli"] [@@coq_plain_module]

  module Ed25519 : [%sig "v2/ed25519.mli"] [@@coq_plain_module]

  module Secp256k1 : [%sig "v2/secp256k1.mli"] [@@coq_plain_module]

  module P256 : [%sig "v2/p256.mli"] [@@coq_plain_module]

  module Chain_id : [%sig "v2/chain_id.mli"] [@@coq_plain_module]

  module Signature : [%sig "v2/signature.mli"] [@@coq_plain_module]

  module Block_hash : [%sig "v2/block_hash.mli"] [@@coq_plain_module]

  module Operation_hash : [%sig "v2/operation_hash.mli"] [@@coq_plain_module]

  module Operation_list_hash : [%sig "v2/operation_list_hash.mli"]
  [@@coq_plain_module]

  module Operation_list_list_hash : [%sig "v2/operation_list_list_hash.mli"]
  [@@coq_plain_module]

  module Protocol_hash : [%sig "v2/protocol_hash.mli"] [@@coq_plain_module]

  module Context_hash : [%sig "v2/context_hash.mli"] [@@coq_plain_module]

  module Pvss_secp256k1 : [%sig "v2/pvss_secp256k1.mli"] [@@coq_plain_module]

  module Sapling : [%sig "v2/sapling.mli"] [@@coq_plain_module]

  module Micheline : [%sig "v2/micheline.mli"] [@@coq_plain_module]

  module Block_header : [%sig "v2/block_header.mli"] [@@coq_plain_module]

  module Fitness : [%sig "v2/fitness.mli"] [@@coq_plain_module]

  module Operation : [%sig "v2/operation.mli"] [@@coq_plain_module]

  module Context : [%sig "v2/context.mli"] [@@coq_plain_module]

  module Updater : [%sig "v2/updater.mli"] [@@coq_plain_module]

  module RPC_context : [%sig "v2/RPC_context.mli"] [@@coq_plain_module]

  module Equality_witness : [%sig "v2/equality_witness.mli"]
  [@@coq_plain_module]
end
