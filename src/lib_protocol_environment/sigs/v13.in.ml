module type T = sig
  module CamlinternalFormatBasics : module type of struct
    include Tezos_protocol_environment_sigs_internals.CamlinternalFormatBasics
  end

  module Pervasives : [%sig "v13/pervasives.mli"]

  open Pervasives

  module Either : [%sig "v13/either.mli"]

  module String : [%sig "v13/string.mli"]

  module Char : [%sig "v13/char.mli"]

  module Bytes : [%sig "v13/bytes.mli"]

  module Int32 : [%sig "v13/int32.mli"]

  module Int64 : [%sig "v13/int64.mli"]

  module Format : [%sig "v13/format.mli"]

  module Logging : [%sig "v13/logging.mli"]

  module Hex : [%sig "v13/hex.mli"]

  module Z : [%sig "v13/z.mli"]

  module Q : [%sig "v13/q.mli"]

  module Lwt : [%sig "v13/lwt.mli"]

  module Data_encoding : [%sig "v13/data_encoding.mli"]

  module Raw_hashes : [%sig "v13/raw_hashes.mli"]

  module Compare : [%sig "v13/compare.mli"]

  module Time : [%sig "v13/time.mli"]

  module TzEndian : [%sig "v13/tzEndian.mli"]

  module Bits : [%sig "v13/bits.mli"]

  module Equality_witness : [%sig "v13/equality_witness.mli"]

  module FallbackArray : [%sig "v13/fallbackArray.mli"]

  module Error_monad : [%sig "v13/error_monad.mli"]

  open Error_monad

  module Seq : [%sig "v13/seq.mli"]

  module List : [%sig "v13/list.mli"]

  module Array : [%sig "v13/array.mli"]

  module Set : [%sig "v13/set.mli"]

  module Map : [%sig "v13/map.mli"]

  module Option : [%sig "v13/option.mli"]

  module Result : [%sig "v13/result.mli"]

  module RPC_arg : [%sig "v13/RPC_arg.mli"]

  module RPC_path : [%sig "v13/RPC_path.mli"]

  module RPC_query : [%sig "v13/RPC_query.mli"]

  module RPC_service : [%sig "v13/RPC_service.mli"]

  module RPC_answer : [%sig "v13/RPC_answer.mli"]

  module RPC_directory : [%sig "v13/RPC_directory.mli"]

  module Base58 : [%sig "v13/base58.mli"]

  module S : [%sig "v13/s.mli"]

  module Blake2B : [%sig "v13/blake2B.mli"]

  module Bls : [%sig "v13/bls.mli"]

  module Ed25519 : [%sig "v13/ed25519.mli"]

  module Secp256k1 : [%sig "v13/secp256k1.mli"]

  module P256 : [%sig "v13/p256.mli"]

  module Chain_id : [%sig "v13/chain_id.mli"]

  module Signature : [%sig "v13/signature.mli"]

  module Block_hash : [%sig "v13/block_hash.mli"]

  module Operation_hash : [%sig "v13/operation_hash.mli"]

  module Operation_list_hash : [%sig "v13/operation_list_hash.mli"]

  module Operation_list_list_hash : [%sig "v13/operation_list_list_hash.mli"]

  module Protocol_hash : [%sig "v13/protocol_hash.mli"]

  module Context_hash : [%sig "v13/context_hash.mli"]

  module Sapling : [%sig "v13/sapling.mli"]

  module Timelock : [%sig "v13/timelock.mli"]

  module Vdf : [%sig "v13/vdf.mli"]

  module Micheline : [%sig "v13/micheline.mli"]

  module Block_header : [%sig "v13/block_header.mli"]

  module Bounded : [%sig "v13/bounded.mli"]

  module Fitness : [%sig "v13/fitness.mli"]

  module Operation : [%sig "v13/operation.mli"]

  module Context : [%sig "v13/context.mli"]

  module Updater : [%sig "v13/updater.mli"]

  module RPC_context : [%sig "v13/RPC_context.mli"]

  module Context_binary : [%sig "v13/context_binary.mli"]

  module Wasm_2_0_0 : [%sig "v13/wasm_2_0_0.mli"]

  module Plonk : [%sig "v13/plonk.mli"]

  module Dal : [%sig "v13/dal.mli"]

  module Skip_list : [%sig "v13/skip_list.mli"]

  module Smart_rollup : [%sig "v13/smart_rollup.mli"]
end
