module type T = sig
  module CamlinternalFormatBasics : module type of struct
    include Tezos_protocol_environment_sigs_internals.CamlinternalFormatBasics
  end

  module Pervasives : [%sig "v16/pervasives.mli"]

  open Pervasives

  module Either : [%sig "v16/either.mli"]

  module String : [%sig "v16/string.mli"]

  module Char : [%sig "v16/char.mli"]

  module Bytes : [%sig "v16/bytes.mli"]

  module Int32 : [%sig "v16/int32.mli"]

  module Int64 : [%sig "v16/int64.mli"]

  module Format : [%sig "v16/format.mli"]

  module Logging : [%sig "v16/logging.mli"]

  module Hex : [%sig "v16/hex.mli"]

  module Z : [%sig "v16/z.mli"]

  module Q : [%sig "v16/q.mli"]

  module Lwt : [%sig "v16/lwt.mli"]

  module Data_encoding : [%sig "v16/data_encoding.mli"]

  module Raw_hashes : [%sig "v16/raw_hashes.mli"]

  module Compare : [%sig "v16/compare.mli"]

  module Time : [%sig "v16/time.mli"]

  module TzEndian : [%sig "v16/tzEndian.mli"]

  module Bits : [%sig "v16/bits.mli"]

  module Equality_witness : [%sig "v16/equality_witness.mli"]

  module FallbackArray : [%sig "v16/fallbackArray.mli"]

  module Error_monad : [%sig "v16/error_monad.mli"]

  open Error_monad

  module Seq : [%sig "v16/seq.mli"]

  module List : [%sig "v16/list.mli"]

  module Array : [%sig "v16/array.mli"]

  module Set : [%sig "v16/set.mli"]

  module Map : [%sig "v16/map.mli"]

  module Bitset : [%sig "v16/bitset.mli"]

  module Option : [%sig "v16/option.mli"]

  module Result : [%sig "v16/result.mli"]

  module RPC_arg : [%sig "v16/RPC_arg.mli"]

  module RPC_path : [%sig "v16/RPC_path.mli"]

  module RPC_query : [%sig "v16/RPC_query.mli"]

  module RPC_service : [%sig "v16/RPC_service.mli"]

  module RPC_answer : [%sig "v16/RPC_answer.mli"]

  module RPC_directory : [%sig "v16/RPC_directory.mli"]

  module Base58 : [%sig "v16/base58.mli"]

  module S : [%sig "v16/s.mli"]

  module Blake2B : [%sig "v16/blake2B.mli"]

  module Mldsa44 : [%sig "v16/mldsa44.mli"]

  module Bls : [%sig "v16/bls.mli"]

  module Ed25519 : [%sig "v16/ed25519.mli"]

  module Secp256k1 : [%sig "v16/secp256k1.mli"]

  module P256 : [%sig "v16/p256.mli"]

  module Chain_id : [%sig "v16/chain_id.mli"]

  module Signature : [%sig "v16/signature.mli"]

  module Block_hash : [%sig "v16/block_hash.mli"]

  module Operation_hash : [%sig "v16/operation_hash.mli"]

  module Operation_list_hash : [%sig "v16/operation_list_hash.mli"]

  module Operation_list_list_hash : [%sig "v16/operation_list_list_hash.mli"]

  module Protocol_hash : [%sig "v16/protocol_hash.mli"]

  module Context_hash : [%sig "v16/context_hash.mli"]

  module Sapling : [%sig "v16/sapling.mli"]

  module Timelock : [%sig "v16/timelock.mli"]

  module Vdf : [%sig "v16/vdf.mli"]

  module Micheline : [%sig "v16/micheline.mli"]

  module Block_header : [%sig "v16/block_header.mli"]

  module Bounded : [%sig "v16/bounded.mli"]

  module Fitness : [%sig "v16/fitness.mli"]

  module Operation : [%sig "v16/operation.mli"]

  module Context : [%sig "v16/context.mli"]

  module Updater : [%sig "v16/updater.mli"]

  module RPC_context : [%sig "v16/RPC_context.mli"]

  module Context_binary : [%sig "v16/context_binary.mli"]

  module Wasm_2_0_0 : [%sig "v16/wasm_2_0_0.mli"]

  module Plonk : [%sig "v16/plonk.mli"]

  module Dal : [%sig "v16/dal.mli"]

  module Skip_list : [%sig "v16/skip_list.mli"]

  module Smart_rollup : [%sig "v16/smart_rollup.mli"]

  module Riscv : [%sig "v16/riscv.mli"]
end
