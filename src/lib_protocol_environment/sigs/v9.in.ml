module type T = sig
  module CamlinternalFormatBasics : module type of struct
    include Tezos_protocol_environment_sigs_internals.CamlinternalFormatBasics
  end

  module Pervasives : [%sig "v9/pervasives.mli"]

  open Pervasives

  module Either : [%sig "v9/either.mli"]

  module String : [%sig "v9/string.mli"]

  module Char : [%sig "v9/char.mli"]

  module Bytes : [%sig "v9/bytes.mli"]

  module Int32 : [%sig "v9/int32.mli"]

  module Int64 : [%sig "v9/int64.mli"]

  module Format : [%sig "v9/format.mli"]

  module Logging : [%sig "v9/logging.mli"]

  module Hex : [%sig "v9/hex.mli"]

  module Z : [%sig "v9/z.mli"]

  module Q : [%sig "v9/q.mli"]

  module Lwt : [%sig "v9/lwt.mli"]

  module Data_encoding : [%sig "v9/data_encoding.mli"]

  module Raw_hashes : [%sig "v9/raw_hashes.mli"]

  module Compare : [%sig "v9/compare.mli"]

  module Time : [%sig "v9/time.mli"]

  module TzEndian : [%sig "v9/tzEndian.mli"]

  module Bits : [%sig "v9/bits.mli"]

  module Equality_witness : [%sig "v9/equality_witness.mli"]

  module FallbackArray : [%sig "v9/fallbackArray.mli"]

  module Error_monad : [%sig "v9/error_monad.mli"]

  open Error_monad

  module Seq : [%sig "v9/seq.mli"]

  module List : [%sig "v9/list.mli"]

  module Array : [%sig "v9/array.mli"]

  module Set : [%sig "v9/set.mli"]

  module Map : [%sig "v9/map.mli"]

  module Option : [%sig "v9/option.mli"]

  module Result : [%sig "v9/result.mli"]

  module RPC_arg : [%sig "v9/RPC_arg.mli"]

  module RPC_path : [%sig "v9/RPC_path.mli"]

  module RPC_query : [%sig "v9/RPC_query.mli"]

  module RPC_service : [%sig "v9/RPC_service.mli"]

  module RPC_answer : [%sig "v9/RPC_answer.mli"]

  module RPC_directory : [%sig "v9/RPC_directory.mli"]

  module Base58 : [%sig "v9/base58.mli"]

  module S : [%sig "v9/s.mli"]

  module Blake2B : [%sig "v9/blake2B.mli"]

  module Bls : [%sig "v9/bls.mli"]

  module Ed25519 : [%sig "v9/ed25519.mli"]

  module Secp256k1 : [%sig "v9/secp256k1.mli"]

  module P256 : [%sig "v9/p256.mli"]

  module Chain_id : [%sig "v9/chain_id.mli"]

  module Signature : [%sig "v9/signature.mli"]

  module Block_hash : [%sig "v9/block_hash.mli"]

  module Operation_hash : [%sig "v9/operation_hash.mli"]

  module Operation_list_hash : [%sig "v9/operation_list_hash.mli"]

  module Operation_list_list_hash : [%sig "v9/operation_list_list_hash.mli"]

  module Protocol_hash : [%sig "v9/protocol_hash.mli"]

  module Context_hash : [%sig "v9/context_hash.mli"]

  module Sapling : [%sig "v9/sapling.mli"]

  module Timelock : [%sig "v9/timelock.mli"]

  module Vdf : [%sig "v9/vdf.mli"]

  module Micheline : [%sig "v9/micheline.mli"]

  module Block_header : [%sig "v9/block_header.mli"]

  module Bounded : [%sig "v9/bounded.mli"]

  module Fitness : [%sig "v9/fitness.mli"]

  module Operation : [%sig "v9/operation.mli"]

  module Context : [%sig "v9/context.mli"]

  module Updater : [%sig "v9/updater.mli"]

  module RPC_context : [%sig "v9/RPC_context.mli"]

  module Wasm_2_0_0 : [%sig "v9/wasm_2_0_0.mli"]

  module Plonk : [%sig "v9/plonk.mli"]

  module Dal : [%sig "v9/dal.mli"]
end
