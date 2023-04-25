module type T = sig
  module CamlinternalFormatBasics : module type of struct
    include Tezos_protocol_environment_sigs_internals.CamlinternalFormatBasics
  end

  module Pervasives : [%sig "v10/pervasives.mli"]

  open Pervasives

  module Either : [%sig "v10/either.mli"]

  module String : [%sig "v10/string.mli"]

  module Char : [%sig "v10/char.mli"]

  module Bytes : [%sig "v10/bytes.mli"]

  module Int32 : [%sig "v10/int32.mli"]

  module Int64 : [%sig "v10/int64.mli"]

  module Format : [%sig "v10/format.mli"]

  module Logging : [%sig "v10/logging.mli"]

  module Hex : [%sig "v10/hex.mli"]

  module Z : [%sig "v10/z.mli"]

  module Q : [%sig "v10/q.mli"]

  module Lwt : [%sig "v10/lwt.mli"]

  module Data_encoding : [%sig "v10/data_encoding.mli"]

  module Raw_hashes : [%sig "v10/raw_hashes.mli"]

  module Compare : [%sig "v10/compare.mli"]

  module Time : [%sig "v10/time.mli"]

  module TzEndian : [%sig "v10/tzEndian.mli"]

  module Bits : [%sig "v10/bits.mli"]

  module Equality_witness : [%sig "v10/equality_witness.mli"]

  module FallbackArray : [%sig "v10/fallbackArray.mli"]

  module Error_monad : [%sig "v10/error_monad.mli"]

  open Error_monad

  module Seq : [%sig "v10/seq.mli"]

  module List : [%sig "v10/list.mli"]

  module Array : [%sig "v10/array.mli"]

  module Set : [%sig "v10/set.mli"]

  module Map : [%sig "v10/map.mli"]

  module Option : [%sig "v10/option.mli"]

  module Result : [%sig "v10/result.mli"]

  module RPC_arg : [%sig "v10/RPC_arg.mli"]

  module RPC_path : [%sig "v10/RPC_path.mli"]

  module RPC_query : [%sig "v10/RPC_query.mli"]

  module RPC_service : [%sig "v10/RPC_service.mli"]

  module RPC_answer : [%sig "v10/RPC_answer.mli"]

  module RPC_directory : [%sig "v10/RPC_directory.mli"]

  module Base58 : [%sig "v10/base58.mli"]

  module S : [%sig "v10/s.mli"]

  module Blake2B : [%sig "v10/blake2B.mli"]

  module Bls : [%sig "v10/bls.mli"]

  module Ed25519 : [%sig "v10/ed25519.mli"]

  module Secp256k1 : [%sig "v10/secp256k1.mli"]

  module P256 : [%sig "v10/p256.mli"]

  module Chain_id : [%sig "v10/chain_id.mli"]

  module Signature : [%sig "v10/signature.mli"]

  module Block_hash : [%sig "v10/block_hash.mli"]

  module Operation_hash : [%sig "v10/operation_hash.mli"]

  module Operation_list_hash : [%sig "v10/operation_list_hash.mli"]

  module Operation_list_list_hash : [%sig "v10/operation_list_list_hash.mli"]

  module Protocol_hash : [%sig "v10/protocol_hash.mli"]

  module Context_hash : [%sig "v10/context_hash.mli"]

  module Sapling : [%sig "v10/sapling.mli"]

  module Timelock : [%sig "v10/timelock.mli"]

  module Vdf : [%sig "v10/vdf.mli"]

  module Micheline : [%sig "v10/micheline.mli"]

  module Block_header : [%sig "v10/block_header.mli"]

  module Bounded : [%sig "v10/bounded.mli"]

  module Fitness : [%sig "v10/fitness.mli"]

  module Operation : [%sig "v10/operation.mli"]

  module Context : [%sig "v10/context.mli"]

  module Updater : [%sig "v10/updater.mli"]

  module RPC_context : [%sig "v10/RPC_context.mli"]

  module Wasm_2_0_0 : [%sig "v10/wasm_2_0_0.mli"]

  module Plonk : [%sig "v10/plonk.mli"]

  module Dal : [%sig "v10/dal.mli"]

  module Smart_rollup_address : [%sig "v10/smart_rollup_address.mli"]
end
