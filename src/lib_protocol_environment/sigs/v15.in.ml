module type T = sig
  module CamlinternalFormatBasics : module type of struct
    include Tezos_protocol_environment_sigs_internals.CamlinternalFormatBasics
  end

  module Pervasives : [%sig "v15/pervasives.mli"]

  open Pervasives

  module Either : [%sig "v15/either.mli"]

  module String : [%sig "v15/string.mli"]

  module Char : [%sig "v15/char.mli"]

  module Bytes : [%sig "v15/bytes.mli"]

  module Int32 : [%sig "v15/int32.mli"]

  module Int64 : [%sig "v15/int64.mli"]

  module Format : [%sig "v15/format.mli"]

  module Logging : [%sig "v15/logging.mli"]

  module Hex : [%sig "v15/hex.mli"]

  module Z : [%sig "v15/z.mli"]

  module Q : [%sig "v15/q.mli"]

  module Lwt : [%sig "v15/lwt.mli"]

  module Data_encoding : [%sig "v15/data_encoding.mli"]

  module Raw_hashes : [%sig "v15/raw_hashes.mli"]

  module Compare : [%sig "v15/compare.mli"]

  module Time : [%sig "v15/time.mli"]

  module TzEndian : [%sig "v15/tzEndian.mli"]

  module Bits : [%sig "v15/bits.mli"]

  module Equality_witness : [%sig "v15/equality_witness.mli"]

  module FallbackArray : [%sig "v15/fallbackArray.mli"]

  module Error_monad : [%sig "v15/error_monad.mli"]

  open Error_monad

  module Seq : [%sig "v15/seq.mli"]

  module List : [%sig "v15/list.mli"]

  module Array : [%sig "v15/array.mli"]

  module Set : [%sig "v15/set.mli"]

  module Map : [%sig "v15/map.mli"]

  module Bitset : [%sig "v15/bitset.mli"]

  module Option : [%sig "v15/option.mli"]

  module Result : [%sig "v15/result.mli"]

  module RPC_arg : [%sig "v15/RPC_arg.mli"]

  module RPC_path : [%sig "v15/RPC_path.mli"]

  module RPC_query : [%sig "v15/RPC_query.mli"]

  module RPC_service : [%sig "v15/RPC_service.mli"]

  module RPC_answer : [%sig "v15/RPC_answer.mli"]

  module RPC_directory : [%sig "v15/RPC_directory.mli"]

  module Base58 : [%sig "v15/base58.mli"]

  module S : [%sig "v15/s.mli"]

  module Blake2B : [%sig "v15/blake2B.mli"]

  module Bls : [%sig "v15/bls.mli"]

  module Ed25519 : [%sig "v15/ed25519.mli"]

  module Secp256k1 : [%sig "v15/secp256k1.mli"]

  module P256 : [%sig "v15/p256.mli"]

  module Chain_id : [%sig "v15/chain_id.mli"]

  module Signature : [%sig "v15/signature.mli"]

  module Block_hash : [%sig "v15/block_hash.mli"]

  module Operation_hash : [%sig "v15/operation_hash.mli"]

  module Operation_list_hash : [%sig "v15/operation_list_hash.mli"]

  module Operation_list_list_hash : [%sig "v15/operation_list_list_hash.mli"]

  module Protocol_hash : [%sig "v15/protocol_hash.mli"]

  module Context_hash : [%sig "v15/context_hash.mli"]

  module Sapling : [%sig "v15/sapling.mli"]

  module Timelock : [%sig "v15/timelock.mli"]

  module Vdf : [%sig "v15/vdf.mli"]

  module Micheline : [%sig "v15/micheline.mli"]

  module Block_header : [%sig "v15/block_header.mli"]

  module Bounded : [%sig "v15/bounded.mli"]

  module Fitness : [%sig "v15/fitness.mli"]

  module Operation : [%sig "v15/operation.mli"]

  module Context : [%sig "v15/context.mli"]

  module Updater : [%sig "v15/updater.mli"]

  module RPC_context : [%sig "v15/RPC_context.mli"]

  module Context_binary : [%sig "v15/context_binary.mli"]

  module Wasm_2_0_0 : [%sig "v15/wasm_2_0_0.mli"]

  module Plonk : [%sig "v15/plonk.mli"]

  module Dal : [%sig "v15/dal.mli"]

  module Skip_list : [%sig "v15/skip_list.mli"]

  module Smart_rollup : [%sig "v15/smart_rollup.mli"]

  module Riscv : [%sig "v15/riscv.mli"]
end
