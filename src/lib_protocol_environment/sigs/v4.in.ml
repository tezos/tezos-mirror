open Tezos_protocol_environment_sigs_stdlib_compat.V_all

module type T = sig
  module Pervasives : [%sig "v4/pervasives.mli"] [@@coq_plain_module]

  open Pervasives

  module String : [%sig "v4/string.mli"] [@@coq_plain_module]

  module Char : [%sig "v4/char.mli"] [@@coq_plain_module]

  module Bytes : [%sig "v4/bytes.mli"] [@@coq_plain_module]

  module Int32 : [%sig "v4/int32.mli"] [@@coq_plain_module]

  module Int64 : [%sig "v4/int64.mli"] [@@coq_plain_module]

  module Format : [%sig "v4/format.mli"] [@@coq_plain_module]

  module Logging : [%sig "v4/logging.mli"] [@@coq_plain_module]

  module Hex : [%sig "v4/hex.mli"] [@@coq_plain_module]

  module Z : [%sig "v4/z.mli"] [@@coq_plain_module]

  module Lwt : [%sig "v4/lwt.mli"] [@@coq_plain_module]

  module Data_encoding : [%sig "v4/data_encoding.mli"] [@@coq_plain_module]

  module Raw_hashes : [%sig "v4/raw_hashes.mli"] [@@coq_plain_module]

  module Compare : [%sig "v4/compare.mli"] [@@coq_plain_module]

  module Time : [%sig "v4/time.mli"] [@@coq_plain_module]

  module TzEndian : [%sig "v4/tzEndian.mli"] [@@coq_plain_module]

  module Bits : [%sig "v4/bits.mli"] [@@coq_plain_module]

  module Equality_witness : [%sig "v4/equality_witness.mli"]
  [@@coq_plain_module]

  module FallbackArray : [%sig "v4/fallbackArray.mli"] [@@coq_plain_module]

  module Error_monad : [%sig "v4/error_monad.mli"] [@@coq_plain_module]

  open Error_monad

  module Seq : [%sig "v4/seq.mli"] [@@coq_plain_module]

  module List : [%sig "v4/list.mli"] [@@coq_plain_module]

  module Set : [%sig "v4/set.mli"] [@@coq_plain_module]

  module Map : [%sig "v4/map.mli"] [@@coq_plain_module]

  module Option : [%sig "v4/option.mli"] [@@coq_plain_module]

  module Result : [%sig "v4/result.mli"] [@@coq_plain_module]

  module RPC_arg : [%sig "v4/RPC_arg.mli"] [@@coq_plain_module]

  module RPC_path : [%sig "v4/RPC_path.mli"] [@@coq_plain_module]

  module RPC_query : [%sig "v4/RPC_query.mli"] [@@coq_plain_module]

  module RPC_service : [%sig "v4/RPC_service.mli"] [@@coq_plain_module]

  module RPC_answer : [%sig "v4/RPC_answer.mli"] [@@coq_plain_module]

  module RPC_directory : [%sig "v4/RPC_directory.mli"] [@@coq_plain_module]

  module Base58 : [%sig "v4/base58.mli"] [@@coq_plain_module]

  module S : [%sig "v4/s.mli"] [@@coq_plain_module]

  module Blake2B : [%sig "v4/blake2B.mli"] [@@coq_plain_module]

  module Bls12_381 : [%sig "v4/bls12_381.mli"] [@@coq_plain_module]

  module Bls_signature : [%sig "v4/bls_signature.mli"] [@@coq_plain_module]

  module Ed25519 : [%sig "v4/ed25519.mli"] [@@coq_plain_module]

  module Secp256k1 : [%sig "v4/secp256k1.mli"] [@@coq_plain_module]

  module P256 : [%sig "v4/p256.mli"] [@@coq_plain_module]

  module Chain_id : [%sig "v4/chain_id.mli"] [@@coq_plain_module]

  module Signature : [%sig "v4/signature.mli"] [@@coq_plain_module]

  module Block_hash : [%sig "v4/block_hash.mli"] [@@coq_plain_module]

  module Operation_hash : [%sig "v4/operation_hash.mli"] [@@coq_plain_module]

  module Operation_list_hash : [%sig "v4/operation_list_hash.mli"]
  [@@coq_plain_module]

  module Operation_list_list_hash : [%sig "v4/operation_list_list_hash.mli"]
  [@@coq_plain_module]

  module Protocol_hash : [%sig "v4/protocol_hash.mli"] [@@coq_plain_module]

  module Context_hash : [%sig "v4/context_hash.mli"] [@@coq_plain_module]

  module Pvss_secp256k1 : [%sig "v4/pvss_secp256k1.mli"] [@@coq_plain_module]

  module Sapling : [%sig "v4/sapling.mli"] [@@coq_plain_module]

  module Timelock : [%sig "v4/timelock.mli"] [@@coq_plain_module]

  module Micheline : [%sig "v4/micheline.mli"] [@@coq_plain_module]

  module Block_header : [%sig "v4/block_header.mli"] [@@coq_plain_module]

  module Fitness : [%sig "v4/fitness.mli"] [@@coq_plain_module]

  module Operation : [%sig "v4/operation.mli"] [@@coq_plain_module]

  module Context : [%sig "v4/context.mli"] [@@coq_plain_module]

  module Updater : [%sig "v4/updater.mli"] [@@coq_plain_module]

  module RPC_context : [%sig "v4/RPC_context.mli"] [@@coq_plain_module]
end
