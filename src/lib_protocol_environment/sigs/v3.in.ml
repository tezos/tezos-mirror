open Tezos_protocol_environment_sigs_stdlib_compat.V_all

module type T = sig
  module Pervasives : [%sig "v3/pervasives.mli"] [@@coq_plain_module]

  open Pervasives

  module String : [%sig "v3/string.mli"] [@@coq_plain_module]

  module Char : [%sig "v3/char.mli"] [@@coq_plain_module]

  module Bytes : [%sig "v3/bytes.mli"] [@@coq_plain_module]

  module Int32 : [%sig "v3/int32.mli"] [@@coq_plain_module]

  module Int64 : [%sig "v3/int64.mli"] [@@coq_plain_module]

  module Format : [%sig "v3/format.mli"] [@@coq_plain_module]

  module Logging : [%sig "v3/logging.mli"] [@@coq_plain_module]

  module Hex : [%sig "v3/hex.mli"] [@@coq_plain_module]

  module Z : [%sig "v3/z.mli"] [@@coq_plain_module]

  module Lwt : [%sig "v3/lwt.mli"] [@@coq_plain_module]

  module Data_encoding : [%sig "v3/data_encoding.mli"] [@@coq_plain_module]

  module Raw_hashes : [%sig "v3/raw_hashes.mli"] [@@coq_plain_module]

  module Compare : [%sig "v3/compare.mli"] [@@coq_plain_module]

  module Time : [%sig "v3/time.mli"] [@@coq_plain_module]

  module TzEndian : [%sig "v3/tzEndian.mli"] [@@coq_plain_module]

  module Bits : [%sig "v3/bits.mli"] [@@coq_plain_module]

  module Equality_witness : [%sig "v3/equality_witness.mli"]
  [@@coq_plain_module]

  module FallbackArray : [%sig "v3/fallbackArray.mli"] [@@coq_plain_module]

  module Error_monad : [%sig "v3/error_monad.mli"] [@@coq_plain_module]

  open Error_monad

  module Seq : [%sig "v3/seq.mli"] [@@coq_plain_module]

  module List : [%sig "v3/list.mli"] [@@coq_plain_module]

  module Set : [%sig "v3/set.mli"] [@@coq_plain_module]

  module Map : [%sig "v3/map.mli"] [@@coq_plain_module]

  module Option : [%sig "v3/option.mli"] [@@coq_plain_module]

  module Result : [%sig "v3/result.mli"] [@@coq_plain_module]

  module RPC_arg : [%sig "v3/RPC_arg.mli"] [@@coq_plain_module]

  module RPC_path : [%sig "v3/RPC_path.mli"] [@@coq_plain_module]

  module RPC_query : [%sig "v3/RPC_query.mli"] [@@coq_plain_module]

  module RPC_service : [%sig "v3/RPC_service.mli"] [@@coq_plain_module]

  module RPC_answer : [%sig "v3/RPC_answer.mli"] [@@coq_plain_module]

  module RPC_directory : [%sig "v3/RPC_directory.mli"] [@@coq_plain_module]

  module Base58 : [%sig "v3/base58.mli"] [@@coq_plain_module]

  module S : [%sig "v3/s.mli"] [@@coq_plain_module]

  module Blake2B : [%sig "v3/blake2B.mli"] [@@coq_plain_module]

  module Bls12_381 : [%sig "v3/bls12_381.mli"] [@@coq_plain_module]

  module Ed25519 : [%sig "v3/ed25519.mli"] [@@coq_plain_module]

  module Secp256k1 : [%sig "v3/secp256k1.mli"] [@@coq_plain_module]

  module P256 : [%sig "v3/p256.mli"] [@@coq_plain_module]

  module Chain_id : [%sig "v3/chain_id.mli"] [@@coq_plain_module]

  module Signature : [%sig "v3/signature.mli"] [@@coq_plain_module]

  module Block_hash : [%sig "v3/block_hash.mli"] [@@coq_plain_module]

  module Operation_hash : [%sig "v3/operation_hash.mli"] [@@coq_plain_module]

  module Operation_list_hash : [%sig "v3/operation_list_hash.mli"]
  [@@coq_plain_module]

  module Operation_list_list_hash : [%sig "v3/operation_list_list_hash.mli"]
  [@@coq_plain_module]

  module Protocol_hash : [%sig "v3/protocol_hash.mli"] [@@coq_plain_module]

  module Context_hash : [%sig "v3/context_hash.mli"] [@@coq_plain_module]

  module Pvss_secp256k1 : [%sig "v3/pvss_secp256k1.mli"] [@@coq_plain_module]

  module Sapling : [%sig "v3/sapling.mli"] [@@coq_plain_module]

  module Timelock : [%sig "v3/timelock.mli"] [@@coq_plain_module]

  module Micheline : [%sig "v3/micheline.mli"] [@@coq_plain_module]

  module Block_header : [%sig "v3/block_header.mli"] [@@coq_plain_module]

  module Fitness : [%sig "v3/fitness.mli"] [@@coq_plain_module]

  module Operation : [%sig "v3/operation.mli"] [@@coq_plain_module]

  module Context : [%sig "v3/context.mli"] [@@coq_plain_module]

  module Updater : [%sig "v3/updater.mli"] [@@coq_plain_module]

  module RPC_context : [%sig "v3/RPC_context.mli"] [@@coq_plain_module]
end
