module type T = sig
  module CamlinternalFormatBasics : module type of struct
    include Tezos_protocol_environment_sigs_internals.CamlinternalFormatBasics
  end

  module Pervasives : [%sig "v11/pervasives.mli"]

  open Pervasives

  module Either : [%sig "v11/either.mli"]

  module String : [%sig "v11/string.mli"]

  module Char : [%sig "v11/char.mli"]

  module Bytes : [%sig "v11/bytes.mli"]

  module Int32 : [%sig "v11/int32.mli"]

  module Int64 : [%sig "v11/int64.mli"]

  module Format : [%sig "v11/format.mli"]

  module Logging : [%sig "v11/logging.mli"]

  module Hex : [%sig "v11/hex.mli"]

  module Z : [%sig "v11/z.mli"]

  module Q : [%sig "v11/q.mli"]

  module Lwt : [%sig "v11/lwt.mli"]

  module Data_encoding : [%sig "v11/data_encoding.mli"]

  module Raw_hashes : [%sig "v11/raw_hashes.mli"]

  module Compare : [%sig "v11/compare.mli"]

  module Time : [%sig "v11/time.mli"]

  module TzEndian : [%sig "v11/tzEndian.mli"]

  module Bits : [%sig "v11/bits.mli"]

  module Equality_witness : [%sig "v11/equality_witness.mli"]

  module FallbackArray : [%sig "v11/fallbackArray.mli"]

  module Error_monad : [%sig "v11/error_monad.mli"]

  open Error_monad

  module Seq : [%sig "v11/seq.mli"]

  module List : [%sig "v11/list.mli"]

  module Array : [%sig "v11/array.mli"]

  module Set : [%sig "v11/set.mli"]

  module Map : [%sig "v11/map.mli"]

  module Option : [%sig "v11/option.mli"]

  module Result : [%sig "v11/result.mli"]

  module RPC_arg : [%sig "v11/RPC_arg.mli"]

  module RPC_path : [%sig "v11/RPC_path.mli"]

  module RPC_query : [%sig "v11/RPC_query.mli"]

  module RPC_service : [%sig "v11/RPC_service.mli"]

  module RPC_answer : [%sig "v11/RPC_answer.mli"]

  module RPC_directory : [%sig "v11/RPC_directory.mli"]

  module Base58 : [%sig "v11/base58.mli"]

  module S : [%sig "v11/s.mli"]

  module Blake2B : [%sig "v11/blake2B.mli"]

  module Bls : [%sig "v11/bls.mli"]

  module Ed25519 : [%sig "v11/ed25519.mli"]

  module Secp256k1 : [%sig "v11/secp256k1.mli"]

  module P256 : [%sig "v11/p256.mli"]

  module Chain_id : [%sig "v11/chain_id.mli"]

  module Signature : [%sig "v11/signature.mli"]

  module Block_hash : [%sig "v11/block_hash.mli"]

  module Operation_hash : [%sig "v11/operation_hash.mli"]

  module Operation_list_hash : [%sig "v11/operation_list_hash.mli"]

  module Operation_list_list_hash : [%sig "v11/operation_list_list_hash.mli"]

  module Protocol_hash : [%sig "v11/protocol_hash.mli"]

  module Context_hash : [%sig "v11/context_hash.mli"]

  module Sapling : [%sig "v11/sapling.mli"]

  module Timelock : [%sig "v11/timelock.mli"]

  module Vdf : [%sig "v11/vdf.mli"]

  module Micheline : [%sig "v11/micheline.mli"]

  module Block_header : [%sig "v11/block_header.mli"]

  module Bounded : [%sig "v11/bounded.mli"]

  module Fitness : [%sig "v11/fitness.mli"]

  module Operation : [%sig "v11/operation.mli"]

  module Context : [%sig "v11/context.mli"]

  module Updater : [%sig "v11/updater.mli"]

  module RPC_context : [%sig "v11/RPC_context.mli"]

  module Context_binary : [%sig "v11/context_binary.mli"]

  module Wasm_2_0_0 : [%sig "v11/wasm_2_0_0.mli"]

  module Plonk : [%sig "v11/plonk.mli"]

  module Dal : [%sig "v11/dal.mli"]

  module Skip_list : [%sig "v11/skip_list.mli"]

  module Smart_rollup : [%sig "v11/smart_rollup.mli"]
end
