module type T = sig
  module CamlinternalFormatBasics : module type of struct
    include Tezos_protocol_environment_sigs_internals.CamlinternalFormatBasics
  end

  module Pervasives : [%sig "v6/pervasives.mli"]

  open Pervasives

  module Either : [%sig "v6/either.mli"]

  module String : [%sig "v6/string.mli"]

  module Char : [%sig "v6/char.mli"]

  module Bytes : [%sig "v6/bytes.mli"]

  module Int32 : [%sig "v6/int32.mli"]

  module Int64 : [%sig "v6/int64.mli"]

  module Format : [%sig "v6/format.mli"]

  module Logging : [%sig "v6/logging.mli"]

  module Hex : [%sig "v6/hex.mli"]

  module Z : [%sig "v6/z.mli"]

  module Lwt : [%sig "v6/lwt.mli"]

  module Data_encoding : [%sig "v6/data_encoding.mli"]

  module Raw_hashes : [%sig "v6/raw_hashes.mli"]

  module Compare : [%sig "v6/compare.mli"]

  module Time : [%sig "v6/time.mli"]

  module TzEndian : [%sig "v6/tzEndian.mli"]

  module Bits : [%sig "v6/bits.mli"]

  module Equality_witness : [%sig "v6/equality_witness.mli"]

  module FallbackArray : [%sig "v6/fallbackArray.mli"]

  module Error_monad : [%sig "v6/error_monad.mli"]

  open Error_monad

  module Seq : [%sig "v6/seq.mli"]

  module List : [%sig "v6/list.mli"]

  module Set : [%sig "v6/set.mli"]

  module Map : [%sig "v6/map.mli"]

  module Option : [%sig "v6/option.mli"]

  module Result : [%sig "v6/result.mli"]

  module RPC_arg : [%sig "v6/RPC_arg.mli"]

  module RPC_path : [%sig "v6/RPC_path.mli"]

  module RPC_query : [%sig "v6/RPC_query.mli"]

  module RPC_service : [%sig "v6/RPC_service.mli"]

  module RPC_answer : [%sig "v6/RPC_answer.mli"]

  module RPC_directory : [%sig "v6/RPC_directory.mli"]

  module Base58 : [%sig "v6/base58.mli"]

  module S : [%sig "v6/s.mli"]

  module Blake2B : [%sig "v6/blake2B.mli"]

  module Bls12_381 : [%sig "v6/bls12_381.mli"]

  module Bls_signature : [%sig "v6/bls_signature.mli"]

  module Ed25519 : [%sig "v6/ed25519.mli"]

  module Secp256k1 : [%sig "v6/secp256k1.mli"]

  module P256 : [%sig "v6/p256.mli"]

  module Chain_id : [%sig "v6/chain_id.mli"]

  module Signature : [%sig "v6/signature.mli"]

  module Block_hash : [%sig "v6/block_hash.mli"]

  module Operation_hash : [%sig "v6/operation_hash.mli"]

  module Operation_list_hash : [%sig "v6/operation_list_hash.mli"]

  module Operation_list_list_hash : [%sig "v6/operation_list_list_hash.mli"]

  module Protocol_hash : [%sig "v6/protocol_hash.mli"]

  module Context_hash : [%sig "v6/context_hash.mli"]

  module Sapling : [%sig "v6/sapling.mli"]

  module Timelock : [%sig "v6/timelock.mli"]

  module Vdf : [%sig "v6/vdf.mli"]

  module Micheline : [%sig "v6/micheline.mli"]

  module Block_header : [%sig "v6/block_header.mli"]

  module Bounded : [%sig "v6/bounded.mli"]

  module Fitness : [%sig "v6/fitness.mli"]

  module Operation : [%sig "v6/operation.mli"]

  module Context : [%sig "v6/context.mli"]

  module Updater : [%sig "v6/updater.mli"]

  module RPC_context : [%sig "v6/RPC_context.mli"]

  module Wasm_2_0_0 : [%sig "v6/wasm_2_0_0.mli"]
end
