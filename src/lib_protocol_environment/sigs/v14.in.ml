module type T = sig
  module CamlinternalFormatBasics : module type of struct
    include Tezos_protocol_environment_sigs_internals.CamlinternalFormatBasics
  end

  module Pervasives : [%sig "v14/pervasives.mli"]

  open Pervasives

  module Either : [%sig "v14/either.mli"]

  module String : [%sig "v14/string.mli"]

  module Char : [%sig "v14/char.mli"]

  module Bytes : [%sig "v14/bytes.mli"]

  module Int32 : [%sig "v14/int32.mli"]

  module Int64 : [%sig "v14/int64.mli"]

  module Format : [%sig "v14/format.mli"]

  module Logging : [%sig "v14/logging.mli"]

  module Hex : [%sig "v14/hex.mli"]

  module Z : [%sig "v14/z.mli"]

  module Q : [%sig "v14/q.mli"]

  module Lwt : [%sig "v14/lwt.mli"]

  module Data_encoding : [%sig "v14/data_encoding.mli"]

  module Raw_hashes : [%sig "v14/raw_hashes.mli"]

  module Compare : [%sig "v14/compare.mli"]

  module Time : [%sig "v14/time.mli"]

  module TzEndian : [%sig "v14/tzEndian.mli"]

  module Bits : [%sig "v14/bits.mli"]

  module Equality_witness : [%sig "v14/equality_witness.mli"]

  module FallbackArray : [%sig "v14/fallbackArray.mli"]

  module Error_monad : [%sig "v14/error_monad.mli"]

  open Error_monad

  module Seq : [%sig "v14/seq.mli"]

  module List : [%sig "v14/list.mli"]

  module Array : [%sig "v14/array.mli"]

  module Set : [%sig "v14/set.mli"]

  module Map : [%sig "v14/map.mli"]

  module Bitset : [%sig "v14/bitset.mli"]

  module Option : [%sig "v14/option.mli"]

  module Result : [%sig "v14/result.mli"]

  module RPC_arg : [%sig "v14/RPC_arg.mli"]

  module RPC_path : [%sig "v14/RPC_path.mli"]

  module RPC_query : [%sig "v14/RPC_query.mli"]

  module RPC_service : [%sig "v14/RPC_service.mli"]

  module RPC_answer : [%sig "v14/RPC_answer.mli"]

  module RPC_directory : [%sig "v14/RPC_directory.mli"]

  module Base58 : [%sig "v14/base58.mli"]

  module S : [%sig "v14/s.mli"]

  module Blake2B : [%sig "v14/blake2B.mli"]

  module Bls : [%sig "v14/bls.mli"]

  module Ed25519 : [%sig "v14/ed25519.mli"]

  module Secp256k1 : [%sig "v14/secp256k1.mli"]

  module P256 : [%sig "v14/p256.mli"]

  module Chain_id : [%sig "v14/chain_id.mli"]

  module Signature : [%sig "v14/signature.mli"]

  module Block_hash : [%sig "v14/block_hash.mli"]

  module Operation_hash : [%sig "v14/operation_hash.mli"]

  module Operation_list_hash : [%sig "v14/operation_list_hash.mli"]

  module Operation_list_list_hash : [%sig "v14/operation_list_list_hash.mli"]

  module Protocol_hash : [%sig "v14/protocol_hash.mli"]

  module Context_hash : [%sig "v14/context_hash.mli"]

  module Sapling : [%sig "v14/sapling.mli"]

  module Timelock : [%sig "v14/timelock.mli"]

  module Vdf : [%sig "v14/vdf.mli"]

  module Micheline : [%sig "v14/micheline.mli"]

  module Block_header : [%sig "v14/block_header.mli"]

  module Bounded : [%sig "v14/bounded.mli"]

  module Fitness : [%sig "v14/fitness.mli"]

  module Operation : [%sig "v14/operation.mli"]

  module Context : [%sig "v14/context.mli"]

  module Updater : [%sig "v14/updater.mli"]

  module RPC_context : [%sig "v14/RPC_context.mli"]

  module Context_binary : [%sig "v14/context_binary.mli"]

  module Wasm_2_0_0 : [%sig "v14/wasm_2_0_0.mli"]

  module Plonk : [%sig "v14/plonk.mli"]

  module Dal : [%sig "v14/dal.mli"]

  module Skip_list : [%sig "v14/skip_list.mli"]

  module Smart_rollup : [%sig "v14/smart_rollup.mli"]
end
