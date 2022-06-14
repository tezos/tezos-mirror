open Tezos_protocol_environment_sigs_stdlib_compat.V_all

module type T = sig
  module Pervasives : [%sig "v5/pervasives.mli"] [@@coq_plain_module]

  open Pervasives

  module Either : [%sig "v5/either.mli"] [@@coq_plain_module]

  module String : [%sig "v5/string.mli"] [@@coq_plain_module]

  module Char : [%sig "v5/char.mli"] [@@coq_plain_module]

  module Bytes : [%sig "v5/bytes.mli"] [@@coq_plain_module]

  module Int32 : [%sig "v5/int32.mli"] [@@coq_plain_module]

  module Int64 : [%sig "v5/int64.mli"] [@@coq_plain_module]

  module Format : [%sig "v5/format.mli"] [@@coq_plain_module]

  module Logging : [%sig "v5/logging.mli"] [@@coq_plain_module]

  module Hex : [%sig "v5/hex.mli"] [@@coq_plain_module]

  module Z : [%sig "v5/z.mli"] [@@coq_plain_module]

  module Lwt : [%sig "v5/lwt.mli"] [@@coq_plain_module]

  module Data_encoding : [%sig "v5/data_encoding.mli"] [@@coq_plain_module]

  module Raw_hashes : [%sig "v5/raw_hashes.mli"] [@@coq_plain_module]

  module Compare : [%sig "v5/compare.mli"] [@@coq_plain_module]

  module Time : [%sig "v5/time.mli"] [@@coq_plain_module]

  module TzEndian : [%sig "v5/tzEndian.mli"] [@@coq_plain_module]

  module Bits : [%sig "v5/bits.mli"] [@@coq_plain_module]

  module Equality_witness : [%sig "v5/equality_witness.mli"]
  [@@coq_plain_module]

  module FallbackArray : [%sig "v5/fallbackArray.mli"] [@@coq_plain_module]

  module Error_monad : [%sig "v5/error_monad.mli"] [@@coq_plain_module]

  open Error_monad

  module Seq : [%sig "v5/seq.mli"] [@@coq_plain_module]

  module List : [%sig "v5/list.mli"] [@@coq_plain_module]

  module Set : [%sig "v5/set.mli"] [@@coq_plain_module]

  module Map : [%sig "v5/map.mli"] [@@coq_plain_module]

  module Option : [%sig "v5/option.mli"] [@@coq_plain_module]

  module Result : [%sig "v5/result.mli"] [@@coq_plain_module]

  module RPC_arg : [%sig "v5/RPC_arg.mli"] [@@coq_plain_module]

  module RPC_path : [%sig "v5/RPC_path.mli"] [@@coq_plain_module]

  module RPC_query : [%sig "v5/RPC_query.mli"] [@@coq_plain_module]

  module RPC_service : [%sig "v5/RPC_service.mli"] [@@coq_plain_module]

  module RPC_answer : [%sig "v5/RPC_answer.mli"] [@@coq_plain_module]

  module RPC_directory : [%sig "v5/RPC_directory.mli"] [@@coq_plain_module]

  module Base58 : [%sig "v5/base58.mli"] [@@coq_plain_module]

  module S : [%sig "v5/s.mli"] [@@coq_plain_module]

  module Blake2B : [%sig "v5/blake2B.mli"] [@@coq_plain_module]

  module Bls12_381 : [%sig "v5/bls12_381.mli"] [@@coq_plain_module]

  module Bls_signature : [%sig "v5/bls_signature.mli"] [@@coq_plain_module]

  module Ed25519 : [%sig "v5/ed25519.mli"] [@@coq_plain_module]

  module Secp256k1 : [%sig "v5/secp256k1.mli"] [@@coq_plain_module]

  module P256 : [%sig "v5/p256.mli"] [@@coq_plain_module]

  module Chain_id : [%sig "v5/chain_id.mli"] [@@coq_plain_module]

  module Signature : [%sig "v5/signature.mli"] [@@coq_plain_module]

  module Block_hash : [%sig "v5/block_hash.mli"] [@@coq_plain_module]

  module Operation_hash : [%sig "v5/operation_hash.mli"] [@@coq_plain_module]

  module Operation_list_hash : [%sig "v5/operation_list_hash.mli"]
  [@@coq_plain_module]

  module Operation_list_list_hash : [%sig "v5/operation_list_list_hash.mli"]
  [@@coq_plain_module]

  module Protocol_hash : [%sig "v5/protocol_hash.mli"] [@@coq_plain_module]

  module Context_hash : [%sig "v5/context_hash.mli"] [@@coq_plain_module]

  module Pvss_secp256k1 : [%sig "v5/pvss_secp256k1.mli"] [@@coq_plain_module]

  module Sapling : [%sig "v5/sapling.mli"] [@@coq_plain_module]

  module Timelock : [%sig "v5/timelock.mli"] [@@coq_plain_module]

  module Micheline : [%sig "v5/micheline.mli"] [@@coq_plain_module]

  module Block_header : [%sig "v5/block_header.mli"] [@@coq_plain_module]

  module Bounded : [%sig "v5/bounded.mli"] [@@coq_plain_module]

  module Fitness : [%sig "v5/fitness.mli"] [@@coq_plain_module]

  module Operation : [%sig "v5/operation.mli"] [@@coq_plain_module]

  module Context : [%sig "v5/context.mli"] [@@coq_plain_module]

  module Updater : [%sig "v5/updater.mli"] [@@coq_plain_module]

  module RPC_context : [%sig "v5/RPC_context.mli"] [@@coq_plain_module]
end
