module type T = sig
  module CamlinternalFormatBasics : module type of struct
    include Tezos_protocol_environment_sigs_internals.CamlinternalFormatBasics
  end

  module Pervasives : [%sig "v17/pervasives.mli"]

  open Pervasives

  module Either : [%sig "v17/either.mli"]

  module String : [%sig "v17/string.mli"]

  module Char : [%sig "v17/char.mli"]

  module Bytes : [%sig "v17/bytes.mli"]

  module Int32 : [%sig "v17/int32.mli"]

  module Int64 : [%sig "v17/int64.mli"]

  module Format : [%sig "v17/format.mli"]

  module Logging : [%sig "v17/logging.mli"]

  module Hex : [%sig "v17/hex.mli"]

  module Z : [%sig "v17/z.mli"]

  module Q : [%sig "v17/q.mli"]

  module Lwt : [%sig "v17/lwt.mli"]

  module Data_encoding : [%sig "v17/data_encoding.mli"]

  module Raw_hashes : [%sig "v17/raw_hashes.mli"]

  module Compare : [%sig "v17/compare.mli"]

  module Time : [%sig "v17/time.mli"]

  module TzEndian : [%sig "v17/tzEndian.mli"]

  module Bits : [%sig "v17/bits.mli"]

  module Equality_witness : [%sig "v17/equality_witness.mli"]

  module FallbackArray : [%sig "v17/fallbackArray.mli"]

  module Error_monad : [%sig "v17/error_monad.mli"]

  open Error_monad

  module Seq : [%sig "v17/seq.mli"]

  module List : [%sig "v17/list.mli"]

  module Array : [%sig "v17/array.mli"]

  module Set : [%sig "v17/set.mli"]

  module Map : [%sig "v17/map.mli"]

  module Bitset : [%sig "v17/bitset.mli"]

  module Option : [%sig "v17/option.mli"]

  module Result : [%sig "v17/result.mli"]

  module RPC_arg : [%sig "v17/RPC_arg.mli"]

  module RPC_path : [%sig "v17/RPC_path.mli"]

  module RPC_query : [%sig "v17/RPC_query.mli"]

  module RPC_service : [%sig "v17/RPC_service.mli"]

  module RPC_answer : [%sig "v17/RPC_answer.mli"]

  module RPC_directory : [%sig "v17/RPC_directory.mli"]

  module Base58 : [%sig "v17/base58.mli"]

  module S : [%sig "v17/s.mli"]

  module Blake2B : [%sig "v17/blake2B.mli"]

  module Mldsa44 : [%sig "v17/mldsa44.mli"]

  module Bls : [%sig "v17/bls.mli"]

  module Ed25519 : [%sig "v17/ed25519.mli"]

  module Secp256k1 : [%sig "v17/secp256k1.mli"]

  module P256 : [%sig "v17/p256.mli"]

  module Chain_id : [%sig "v17/chain_id.mli"]

  module Signature : [%sig "v17/signature.mli"]

  module Block_hash : [%sig "v17/block_hash.mli"]

  module Operation_hash : [%sig "v17/operation_hash.mli"]

  module Operation_list_hash : [%sig "v17/operation_list_hash.mli"]

  module Operation_list_list_hash : [%sig "v17/operation_list_list_hash.mli"]

  module Protocol_hash : [%sig "v17/protocol_hash.mli"]

  module Context_hash : [%sig "v17/context_hash.mli"]

  module Sapling : [%sig "v17/sapling.mli"]

  module Timelock : [%sig "v17/timelock.mli"]

  module Vdf : [%sig "v17/vdf.mli"]

  module Micheline : [%sig "v17/micheline.mli"]

  module Block_header : [%sig "v17/block_header.mli"]

  module Bounded : [%sig "v17/bounded.mli"]

  module Fitness : [%sig "v17/fitness.mli"]

  module Operation : [%sig "v17/operation.mli"]

  module Context : [%sig "v17/context.mli"]

  module Updater : [%sig "v17/updater.mli"]

  module RPC_context : [%sig "v17/RPC_context.mli"]

  module Context_binary : [%sig "v17/context_binary.mli"]

  module Plonk : [%sig "v17/plonk.mli"]

  module Dal : [%sig "v17/dal.mli"]

  module Skip_list : [%sig "v17/skip_list.mli"]

  module Smart_rollup : [%sig "v17/smart_rollup.mli"]

  module Wasm_2_0_0 : [%sig "v17/wasm_2_0_0.mli"]

  module Riscv : [%sig "v17/riscv.mli"]
end
