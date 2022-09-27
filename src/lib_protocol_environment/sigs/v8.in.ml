module type T = sig
  module CamlinternalFormatBasics : module type of struct
    include Tezos_protocol_environment_sigs_internals.CamlinternalFormatBasics
  end

  module Pervasives : [%sig "v8/pervasives.mli"]

  open Pervasives

  module Either : [%sig "v8/either.mli"]

  module String : [%sig "v8/string.mli"]

  module Char : [%sig "v8/char.mli"]

  module Bytes : [%sig "v8/bytes.mli"]

  module Int32 : [%sig "v8/int32.mli"]

  module Int64 : [%sig "v8/int64.mli"]

  module Format : [%sig "v8/format.mli"]

  module Logging : [%sig "v8/logging.mli"]

  module Hex : [%sig "v8/hex.mli"]

  module Z : [%sig "v8/z.mli"]

  module Q : [%sig "v8/q.mli"]

  module Lwt : [%sig "v8/lwt.mli"]

  module Data_encoding : [%sig "v8/data_encoding.mli"]

  module Raw_hashes : [%sig "v8/raw_hashes.mli"]

  module Compare : [%sig "v8/compare.mli"]

  module Time : [%sig "v8/time.mli"]

  module TzEndian : [%sig "v8/tzEndian.mli"]

  module Bits : [%sig "v8/bits.mli"]

  module Equality_witness : [%sig "v8/equality_witness.mli"]

  module FallbackArray : [%sig "v8/fallbackArray.mli"]

  module Error_monad : [%sig "v8/error_monad.mli"]

  open Error_monad

  module Seq : [%sig "v8/seq.mli"]

  module List : [%sig "v8/list.mli"]

  module Array : [%sig "v8/array.mli"]

  module Set : [%sig "v8/set.mli"]

  module Map : [%sig "v8/map.mli"]

  module Option : [%sig "v8/option.mli"]

  module Result : [%sig "v8/result.mli"]

  module RPC_arg : [%sig "v8/RPC_arg.mli"]

  module RPC_path : [%sig "v8/RPC_path.mli"]

  module RPC_query : [%sig "v8/RPC_query.mli"]

  module RPC_service : [%sig "v8/RPC_service.mli"]

  module RPC_answer : [%sig "v8/RPC_answer.mli"]

  module RPC_directory : [%sig "v8/RPC_directory.mli"]

  module Base58 : [%sig "v8/base58.mli"]

  module S : [%sig "v8/s.mli"]

  module Blake2B : [%sig "v8/blake2B.mli"]

  module Bls : [%sig "v8/bls.mli"]

  module Ed25519 : [%sig "v8/ed25519.mli"]

  module Secp256k1 : [%sig "v8/secp256k1.mli"]

  module P256 : [%sig "v8/p256.mli"]

  module Chain_id : [%sig "v8/chain_id.mli"]

  module Signature : [%sig "v8/signature.mli"]

  module Block_hash : [%sig "v8/block_hash.mli"]

  module Operation_hash : [%sig "v8/operation_hash.mli"]

  module Operation_list_hash : [%sig "v8/operation_list_hash.mli"]

  module Operation_list_list_hash : [%sig "v8/operation_list_list_hash.mli"]

  module Protocol_hash : [%sig "v8/protocol_hash.mli"]

  module Context_hash : [%sig "v8/context_hash.mli"]

  module Sapling : [%sig "v8/sapling.mli"]

  module Timelock : [%sig "v8/timelock.mli"]

  module Vdf : [%sig "v8/vdf.mli"]

  module Micheline : [%sig "v8/micheline.mli"]

  module Block_header : [%sig "v8/block_header.mli"]

  module Bounded : [%sig "v8/bounded.mli"]

  module Fitness : [%sig "v8/fitness.mli"]

  module Operation : [%sig "v8/operation.mli"]

  module Context : [%sig "v8/context.mli"]

  module Updater : [%sig "v8/updater.mli"]

  module RPC_context : [%sig "v8/RPC_context.mli"]

  module Wasm_2_0_0 : [%sig "v8/wasm_2_0_0.mli"]

  module Plonk : [%sig "v8/plonk.mli"]

  module Dal : [%sig "v8/dal.mli"]
end
