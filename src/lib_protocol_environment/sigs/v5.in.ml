module type T = sig
  module CamlinternalFormatBasics : module type of struct
    include Tezos_protocol_environment_sigs_internals.CamlinternalFormatBasics
  end

  module Pervasives : [%sig "v5/pervasives.mli"]

  open Pervasives

  module Either : [%sig "v5/either.mli"]

  module String : [%sig "v5/string.mli"]

  module Char : [%sig "v5/char.mli"]

  module Bytes : [%sig "v5/bytes.mli"]

  module Int32 : [%sig "v5/int32.mli"]

  module Int64 : [%sig "v5/int64.mli"]

  module Format : [%sig "v5/format.mli"]

  module Logging : [%sig "v5/logging.mli"]

  module Hex : [%sig "v5/hex.mli"]

  module Z : [%sig "v5/z.mli"]

  module Lwt : [%sig "v5/lwt.mli"]

  module Data_encoding : [%sig "v5/data_encoding.mli"]

  module Raw_hashes : [%sig "v5/raw_hashes.mli"]

  module Compare : [%sig "v5/compare.mli"]

  module Time : [%sig "v5/time.mli"]

  module TzEndian : [%sig "v5/tzEndian.mli"]

  module Bits : [%sig "v5/bits.mli"]

  module Equality_witness : [%sig "v5/equality_witness.mli"]

  module FallbackArray : [%sig "v5/fallbackArray.mli"]

  module Error_monad : [%sig "v5/error_monad.mli"]

  open Error_monad

  module Seq : [%sig "v5/seq.mli"]

  module List : [%sig "v5/list.mli"]

  module Set : [%sig "v5/set.mli"]

  module Map : [%sig "v5/map.mli"]

  module Option : [%sig "v5/option.mli"]

  module Result : [%sig "v5/result.mli"]

  module RPC_arg : [%sig "v5/RPC_arg.mli"]

  module RPC_path : [%sig "v5/RPC_path.mli"]

  module RPC_query : [%sig "v5/RPC_query.mli"]

  module RPC_service : [%sig "v5/RPC_service.mli"]

  module RPC_answer : [%sig "v5/RPC_answer.mli"]

  module RPC_directory : [%sig "v5/RPC_directory.mli"]

  module Base58 : [%sig "v5/base58.mli"]

  module S : [%sig "v5/s.mli"]

  module Blake2B : [%sig "v5/blake2B.mli"]

  module Bls12_381 : [%sig "v5/bls12_381.mli"]

  module Bls_signature : [%sig "v5/bls_signature.mli"]

  module Ed25519 : [%sig "v5/ed25519.mli"]

  module Secp256k1 : [%sig "v5/secp256k1.mli"]

  module P256 : [%sig "v5/p256.mli"]

  module Chain_id : [%sig "v5/chain_id.mli"]

  module Signature : [%sig "v5/signature.mli"]

  module Block_hash : [%sig "v5/block_hash.mli"]

  module Operation_hash : [%sig "v5/operation_hash.mli"]

  module Operation_list_hash : [%sig "v5/operation_list_hash.mli"]

  module Operation_list_list_hash : [%sig "v5/operation_list_list_hash.mli"]

  module Protocol_hash : [%sig "v5/protocol_hash.mli"]

  module Context_hash : [%sig "v5/context_hash.mli"]

  module Sapling : [%sig "v5/sapling.mli"]

  module Timelock : [%sig "v5/timelock.mli"]

  module Micheline : [%sig "v5/micheline.mli"]

  module Block_header : [%sig "v5/block_header.mli"]

  module Bounded : [%sig "v5/bounded.mli"]

  module Fitness : [%sig "v5/fitness.mli"]

  module Operation : [%sig "v5/operation.mli"]

  module Context : [%sig "v5/context.mli"]

  module Updater : [%sig "v5/updater.mli"]

  module RPC_context : [%sig "v5/RPC_context.mli"]
end
