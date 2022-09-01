module type T = sig
  module CamlinternalFormatBasics : module type of struct
    include Tezos_protocol_environment_sigs_internals.CamlinternalFormatBasics
  end

  module Pervasives : [%sig "v3/pervasives.mli"]

  open Pervasives

  module String : [%sig "v3/string.mli"]

  module Char : [%sig "v3/char.mli"]

  module Bytes : [%sig "v3/bytes.mli"]

  module Int32 : [%sig "v3/int32.mli"]

  module Int64 : [%sig "v3/int64.mli"]

  module Format : [%sig "v3/format.mli"]

  module Logging : [%sig "v3/logging.mli"]

  module Hex : [%sig "v3/hex.mli"]

  module Z : [%sig "v3/z.mli"]

  module Lwt : [%sig "v3/lwt.mli"]

  module Data_encoding : [%sig "v3/data_encoding.mli"]

  module Raw_hashes : [%sig "v3/raw_hashes.mli"]

  module Compare : [%sig "v3/compare.mli"]

  module Time : [%sig "v3/time.mli"]

  module TzEndian : [%sig "v3/tzEndian.mli"]

  module Bits : [%sig "v3/bits.mli"]

  module Equality_witness : [%sig "v3/equality_witness.mli"]

  module FallbackArray : [%sig "v3/fallbackArray.mli"]

  module Error_monad : [%sig "v3/error_monad.mli"]

  open Error_monad

  module Seq : [%sig "v3/seq.mli"]

  module List : [%sig "v3/list.mli"]

  module Set : [%sig "v3/set.mli"]

  module Map : [%sig "v3/map.mli"]

  module Option : [%sig "v3/option.mli"]

  module Result : [%sig "v3/result.mli"]

  module RPC_arg : [%sig "v3/RPC_arg.mli"]

  module RPC_path : [%sig "v3/RPC_path.mli"]

  module RPC_query : [%sig "v3/RPC_query.mli"]

  module RPC_service : [%sig "v3/RPC_service.mli"]

  module RPC_answer : [%sig "v3/RPC_answer.mli"]

  module RPC_directory : [%sig "v3/RPC_directory.mli"]

  module Base58 : [%sig "v3/base58.mli"]

  module S : [%sig "v3/s.mli"]

  module Blake2B : [%sig "v3/blake2B.mli"]

  module Bls12_381 : [%sig "v3/bls12_381.mli"]

  module Ed25519 : [%sig "v3/ed25519.mli"]

  module Secp256k1 : [%sig "v3/secp256k1.mli"]

  module P256 : [%sig "v3/p256.mli"]

  module Chain_id : [%sig "v3/chain_id.mli"]

  module Signature : [%sig "v3/signature.mli"]

  module Block_hash : [%sig "v3/block_hash.mli"]

  module Operation_hash : [%sig "v3/operation_hash.mli"]

  module Operation_list_hash : [%sig "v3/operation_list_hash.mli"]

  module Operation_list_list_hash : [%sig "v3/operation_list_list_hash.mli"]

  module Protocol_hash : [%sig "v3/protocol_hash.mli"]

  module Context_hash : [%sig "v3/context_hash.mli"]

  module Sapling : [%sig "v3/sapling.mli"]

  module Timelock : [%sig "v3/timelock.mli"]

  module Micheline : [%sig "v3/micheline.mli"]

  module Block_header : [%sig "v3/block_header.mli"]

  module Fitness : [%sig "v3/fitness.mli"]

  module Operation : [%sig "v3/operation.mli"]

  module Context : [%sig "v3/context.mli"]

  module Updater : [%sig "v3/updater.mli"]

  module RPC_context : [%sig "v3/RPC_context.mli"]
end
