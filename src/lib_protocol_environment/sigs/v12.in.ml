module type T = sig
  module CamlinternalFormatBasics : module type of struct
    include Tezos_protocol_environment_sigs_internals.CamlinternalFormatBasics
  end

  module Pervasives : [%sig "v12/pervasives.mli"]

  open Pervasives

  module Either : [%sig "v12/either.mli"]

  module String : [%sig "v12/string.mli"]

  module Char : [%sig "v12/char.mli"]

  module Bytes : [%sig "v12/bytes.mli"]

  module Int32 : [%sig "v12/int32.mli"]

  module Int64 : [%sig "v12/int64.mli"]

  module Format : [%sig "v12/format.mli"]

  module Logging : [%sig "v12/logging.mli"]

  module Hex : [%sig "v12/hex.mli"]

  module Z : [%sig "v12/z.mli"]

  module Q : [%sig "v12/q.mli"]

  module Lwt : [%sig "v12/lwt.mli"]

  module Data_encoding : [%sig "v12/data_encoding.mli"]

  module Raw_hashes : [%sig "v12/raw_hashes.mli"]

  module Compare : [%sig "v12/compare.mli"]

  module Time : [%sig "v12/time.mli"]

  module TzEndian : [%sig "v12/tzEndian.mli"]

  module Bits : [%sig "v12/bits.mli"]

  module Equality_witness : [%sig "v12/equality_witness.mli"]

  module FallbackArray : [%sig "v12/fallbackArray.mli"]

  module Error_monad : [%sig "v12/error_monad.mli"]

  open Error_monad

  module Seq : [%sig "v12/seq.mli"]

  module List : [%sig "v12/list.mli"]

  module Array : [%sig "v12/array.mli"]

  module Set : [%sig "v12/set.mli"]

  module Map : [%sig "v12/map.mli"]

  module Option : [%sig "v12/option.mli"]

  module Result : [%sig "v12/result.mli"]

  module RPC_arg : [%sig "v12/RPC_arg.mli"]

  module RPC_path : [%sig "v12/RPC_path.mli"]

  module RPC_query : [%sig "v12/RPC_query.mli"]

  module RPC_service : [%sig "v12/RPC_service.mli"]

  module RPC_answer : [%sig "v12/RPC_answer.mli"]

  module RPC_directory : [%sig "v12/RPC_directory.mli"]

  module Base58 : [%sig "v12/base58.mli"]

  module S : [%sig "v12/s.mli"]

  module Blake2B : [%sig "v12/blake2B.mli"]

  module Bls : [%sig "v12/bls.mli"]

  module Ed25519 : [%sig "v12/ed25519.mli"]

  module Secp256k1 : [%sig "v12/secp256k1.mli"]

  module P256 : [%sig "v12/p256.mli"]

  module Chain_id : [%sig "v12/chain_id.mli"]

  module Signature : [%sig "v12/signature.mli"]

  module Block_hash : [%sig "v12/block_hash.mli"]

  module Operation_hash : [%sig "v12/operation_hash.mli"]

  module Operation_list_hash : [%sig "v12/operation_list_hash.mli"]

  module Operation_list_list_hash : [%sig "v12/operation_list_list_hash.mli"]

  module Protocol_hash : [%sig "v12/protocol_hash.mli"]

  module Context_hash : [%sig "v12/context_hash.mli"]

  module Sapling : [%sig "v12/sapling.mli"]

  module Timelock : [%sig "v12/timelock.mli"]

  module Vdf : [%sig "v12/vdf.mli"]

  module Micheline : [%sig "v12/micheline.mli"]

  module Block_header : [%sig "v12/block_header.mli"]

  module Bounded : [%sig "v12/bounded.mli"]

  module Fitness : [%sig "v12/fitness.mli"]

  module Operation : [%sig "v12/operation.mli"]

  module Context : [%sig "v12/context.mli"]

  module Updater : [%sig "v12/updater.mli"]

  module RPC_context : [%sig "v12/RPC_context.mli"]

  module Context_binary : [%sig "v12/context_binary.mli"]

  module Wasm_2_0_0 : [%sig "v12/wasm_2_0_0.mli"]

  module Plonk : [%sig "v12/plonk.mli"]

  module Dal : [%sig "v12/dal.mli"]

  module Skip_list : [%sig "v12/skip_list.mli"]

  module Smart_rollup : [%sig "v12/smart_rollup.mli"]
end
