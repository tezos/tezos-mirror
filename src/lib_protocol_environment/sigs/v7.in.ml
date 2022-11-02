module type T = sig
  module CamlinternalFormatBasics : module type of struct
    include Tezos_protocol_environment_sigs_internals.CamlinternalFormatBasics
  end

  module Pervasives : [%sig "v7/pervasives.mli"]

  open Pervasives

  module Either : [%sig "v7/either.mli"]

  module String : [%sig "v7/string.mli"]

  module Char : [%sig "v7/char.mli"]

  module Bytes : [%sig "v7/bytes.mli"]

  module Int32 : [%sig "v7/int32.mli"]

  module Int64 : [%sig "v7/int64.mli"]

  module Format : [%sig "v7/format.mli"]

  module Logging : [%sig "v7/logging.mli"]

  module Hex : [%sig "v7/hex.mli"]

  module Z : [%sig "v7/z.mli"]

  module Q : [%sig "v7/q.mli"]

  module Lwt : [%sig "v7/lwt.mli"]

  module Data_encoding : [%sig "v7/data_encoding.mli"]

  module Raw_hashes : [%sig "v7/raw_hashes.mli"]

  module Compare : [%sig "v7/compare.mli"]

  module Time : [%sig "v7/time.mli"]

  module TzEndian : [%sig "v7/tzEndian.mli"]

  module Bits : [%sig "v7/bits.mli"]

  module Equality_witness : [%sig "v7/equality_witness.mli"]

  module FallbackArray : [%sig "v7/fallbackArray.mli"]

  module Error_monad : [%sig "v7/error_monad.mli"]

  open Error_monad

  module Seq : [%sig "v7/seq.mli"]

  module List : [%sig "v7/list.mli"]

  module Array : [%sig "v7/array.mli"]

  module Set : [%sig "v7/set.mli"]

  module Map : [%sig "v7/map.mli"]

  module Option : [%sig "v7/option.mli"]

  module Result : [%sig "v7/result.mli"]

  module RPC_arg : [%sig "v7/RPC_arg.mli"]

  module RPC_path : [%sig "v7/RPC_path.mli"]

  module RPC_query : [%sig "v7/RPC_query.mli"]

  module RPC_service : [%sig "v7/RPC_service.mli"]

  module RPC_answer : [%sig "v7/RPC_answer.mli"]

  module RPC_directory : [%sig "v7/RPC_directory.mli"]

  module Base58 : [%sig "v7/base58.mli"]

  module S : [%sig "v7/s.mli"]

  module Blake2B : [%sig "v7/blake2B.mli"]

  module Bls : [%sig "v7/bls.mli"]

  module Ed25519 : [%sig "v7/ed25519.mli"]

  module Secp256k1 : [%sig "v7/secp256k1.mli"]

  module P256 : [%sig "v7/p256.mli"]

  module Chain_id : [%sig "v7/chain_id.mli"]

  module Signature : [%sig "v7/signature.mli"]

  module Block_hash : [%sig "v7/block_hash.mli"]

  module Operation_hash : [%sig "v7/operation_hash.mli"]

  module Operation_list_hash : [%sig "v7/operation_list_hash.mli"]

  module Operation_list_list_hash : [%sig "v7/operation_list_list_hash.mli"]

  module Protocol_hash : [%sig "v7/protocol_hash.mli"]

  module Context_hash : [%sig "v7/context_hash.mli"]

  module Sapling : [%sig "v7/sapling.mli"]

  module Timelock : [%sig "v7/timelock.mli"]

  module Vdf : [%sig "v7/vdf.mli"]

  module Micheline : [%sig "v7/micheline.mli"]

  module Block_header : [%sig "v7/block_header.mli"]

  module Bounded : [%sig "v7/bounded.mli"]

  module Fitness : [%sig "v7/fitness.mli"]

  module Operation : [%sig "v7/operation.mli"]

  module Context : [%sig "v7/context.mli"]

  module Updater : [%sig "v7/updater.mli"]

  module RPC_context : [%sig "v7/RPC_context.mli"]

  module Wasm_2_0_0 : [%sig "v7/wasm_2_0_0.mli"]

  module Plonk : [%sig "v7/plonk.mli"]

  module Dal : [%sig "v7/dal.mli"]
end
