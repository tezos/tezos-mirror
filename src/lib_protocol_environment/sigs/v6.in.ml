open Tezos_protocol_environment_sigs_stdlib_compat.V_all

module type T = sig
  module Pervasives : [%sig "v6/pervasives.mli"] [@@coq_plain_module]

  open Pervasives

  module Either : [%sig "v6/either.mli"] [@@coq_plain_module]

  module String : [%sig "v6/string.mli"] [@@coq_plain_module]

  module Char : [%sig "v6/char.mli"] [@@coq_plain_module]

  module Bytes : [%sig "v6/bytes.mli"] [@@coq_plain_module]

  module Int32 : [%sig "v6/int32.mli"] [@@coq_plain_module]

  module Int64 : [%sig "v6/int64.mli"] [@@coq_plain_module]

  module Format : [%sig "v6/format.mli"] [@@coq_plain_module]

  module Logging : [%sig "v6/logging.mli"] [@@coq_plain_module]

  module Hex : [%sig "v6/hex.mli"] [@@coq_plain_module]

  module Z : [%sig "v6/z.mli"] [@@coq_plain_module]

  module Lwt : [%sig "v6/lwt.mli"] [@@coq_plain_module]

  module Data_encoding : [%sig "v6/data_encoding.mli"] [@@coq_plain_module]

  module Raw_hashes : [%sig "v6/raw_hashes.mli"] [@@coq_plain_module]

  module Compare : [%sig "v6/compare.mli"] [@@coq_plain_module]

  module Time : [%sig "v6/time.mli"] [@@coq_plain_module]

  module TzEndian : [%sig "v6/tzEndian.mli"] [@@coq_plain_module]

  module Bits : [%sig "v6/bits.mli"] [@@coq_plain_module]

  module Equality_witness : [%sig "v6/equality_witness.mli"]
  [@@coq_plain_module]

  module FallbackArray : [%sig "v6/fallbackArray.mli"] [@@coq_plain_module]

  module Error_monad : [%sig "v6/error_monad.mli"] [@@coq_plain_module]

  open Error_monad

  module Seq : [%sig "v6/seq.mli"] [@@coq_plain_module]

  module List : [%sig "v6/list.mli"] [@@coq_plain_module]

  module Set : [%sig "v6/set.mli"] [@@coq_plain_module]

  module Map : [%sig "v6/map.mli"] [@@coq_plain_module]

  module Option : [%sig "v6/option.mli"] [@@coq_plain_module]

  module Result : [%sig "v6/result.mli"] [@@coq_plain_module]

  module RPC_arg : [%sig "v6/RPC_arg.mli"] [@@coq_plain_module]

  module RPC_path : [%sig "v6/RPC_path.mli"] [@@coq_plain_module]

  module RPC_query : [%sig "v6/RPC_query.mli"] [@@coq_plain_module]

  module RPC_service : [%sig "v6/RPC_service.mli"] [@@coq_plain_module]

  module RPC_answer : [%sig "v6/RPC_answer.mli"] [@@coq_plain_module]

  module RPC_directory : [%sig "v6/RPC_directory.mli"] [@@coq_plain_module]

  module Base58 : [%sig "v6/base58.mli"] [@@coq_plain_module]

  module S : [%sig "v6/s.mli"] [@@coq_plain_module]

  module Blake2B : [%sig "v6/blake2B.mli"] [@@coq_plain_module]

  module Bls12_381 : [%sig "v6/bls12_381.mli"] [@@coq_plain_module]

  module Bls_signature : [%sig "v6/bls_signature.mli"] [@@coq_plain_module]

  module Ed25519 : [%sig "v6/ed25519.mli"] [@@coq_plain_module]

  module Secp256k1 : [%sig "v6/secp256k1.mli"] [@@coq_plain_module]

  module P256 : [%sig "v6/p256.mli"] [@@coq_plain_module]

  module Chain_id : [%sig "v6/chain_id.mli"] [@@coq_plain_module]

  module Signature : [%sig "v6/signature.mli"] [@@coq_plain_module]

  module Block_hash : [%sig "v6/block_hash.mli"] [@@coq_plain_module]

  module Operation_hash : [%sig "v6/operation_hash.mli"] [@@coq_plain_module]

  module Operation_list_hash : [%sig "v6/operation_list_hash.mli"]
  [@@coq_plain_module]

  module Operation_list_list_hash : [%sig "v6/operation_list_list_hash.mli"]
  [@@coq_plain_module]

  module Protocol_hash : [%sig "v6/protocol_hash.mli"] [@@coq_plain_module]

  module Context_hash : [%sig "v6/context_hash.mli"] [@@coq_plain_module]

  module Pvss_secp256k1 : [%sig "v6/pvss_secp256k1.mli"] [@@coq_plain_module]

  module Sapling : [%sig "v6/sapling.mli"] [@@coq_plain_module]

  module Timelock : [%sig "v6/timelock.mli"] [@@coq_plain_module]

  module Vdf : [%sig "v6/vdf.mli"] [@@coq_plain_module]

  module Micheline : [%sig "v6/micheline.mli"] [@@coq_plain_module]

  module Block_header : [%sig "v6/block_header.mli"] [@@coq_plain_module]

  module Bounded : [%sig "v6/bounded.mli"] [@@coq_plain_module]

  module Fitness : [%sig "v6/fitness.mli"] [@@coq_plain_module]

  module Operation : [%sig "v6/operation.mli"] [@@coq_plain_module]

  module Context : [%sig "v6/context.mli"] [@@coq_plain_module]

  module Updater : [%sig "v6/updater.mli"] [@@coq_plain_module]

  module RPC_context : [%sig "v6/RPC_context.mli"] [@@coq_plain_module]

  module Wasm_2_0_0 : [%sig "v6/wasm_2_0_0.mli"] [@@coq_plain_module]

  module Plonk : [%sig "v6/plonk.mli"] [@@coq_plain_module]
end
