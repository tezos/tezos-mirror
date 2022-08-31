module type T = sig
  module CamlinternalFormatBasics : module type of struct
    include Tezos_protocol_environment_sigs_internals.CamlinternalFormatBasics
  end

  module Pervasives : [%sig "v4/pervasives.mli"]

  open Pervasives

  module String : [%sig "v4/string.mli"]

  module Char : [%sig "v4/char.mli"]

  module Bytes : [%sig "v4/bytes.mli"]

  module Int32 : [%sig "v4/int32.mli"]

  module Int64 : [%sig "v4/int64.mli"]

  module Format : [%sig "v4/format.mli"]

  module Logging : [%sig "v4/logging.mli"]

  module Hex : [%sig "v4/hex.mli"]

  module Z : [%sig "v4/z.mli"]

  module Lwt : [%sig "v4/lwt.mli"]

  module Data_encoding : [%sig "v4/data_encoding.mli"]

  module Raw_hashes : [%sig "v4/raw_hashes.mli"]

  module Compare : [%sig "v4/compare.mli"]

  module Time : [%sig "v4/time.mli"]

  module TzEndian : [%sig "v4/tzEndian.mli"]

  module Bits : [%sig "v4/bits.mli"]

  module Equality_witness : [%sig "v4/equality_witness.mli"]

  module FallbackArray : [%sig "v4/fallbackArray.mli"]

  module Error_monad : [%sig "v4/error_monad.mli"]

  open Error_monad

  module Seq : [%sig "v4/seq.mli"]

  module List : [%sig "v4/list.mli"]

  module Set : [%sig "v4/set.mli"]

  module Map : [%sig "v4/map.mli"]

  module Option : [%sig "v4/option.mli"]

  module Result : [%sig "v4/result.mli"]

  module RPC_arg : [%sig "v4/RPC_arg.mli"]

  module RPC_path : [%sig "v4/RPC_path.mli"]

  module RPC_query : [%sig "v4/RPC_query.mli"]

  module RPC_service : [%sig "v4/RPC_service.mli"]

  module RPC_answer : [%sig "v4/RPC_answer.mli"]

  module RPC_directory : [%sig "v4/RPC_directory.mli"]

  module Base58 : [%sig "v4/base58.mli"]

  module S : [%sig "v4/s.mli"]

  module Blake2B : [%sig "v4/blake2B.mli"]

  module Bls12_381 : [%sig "v4/bls12_381.mli"]

  module Bls_signature : [%sig "v4/bls_signature.mli"]

  module Ed25519 : [%sig "v4/ed25519.mli"]

  module Secp256k1 : [%sig "v4/secp256k1.mli"]

  module P256 : [%sig "v4/p256.mli"]

  module Chain_id : [%sig "v4/chain_id.mli"]

  module Signature : [%sig "v4/signature.mli"]

  module Block_hash : [%sig "v4/block_hash.mli"]

  module Operation_hash : [%sig "v4/operation_hash.mli"]

  module Operation_list_hash : [%sig "v4/operation_list_hash.mli"]

  module Operation_list_list_hash : [%sig "v4/operation_list_list_hash.mli"]

  module Protocol_hash : [%sig "v4/protocol_hash.mli"]

  module Context_hash : [%sig "v4/context_hash.mli"]

  module Sapling : [%sig "v4/sapling.mli"]

  module Timelock : [%sig "v4/timelock.mli"]

  module Micheline : [%sig "v4/micheline.mli"]

  module Block_header : [%sig "v4/block_header.mli"]

  module Fitness : [%sig "v4/fitness.mli"]

  module Operation : [%sig "v4/operation.mli"]

  module Context : [%sig "v4/context.mli"]

  module Updater : [%sig "v4/updater.mli"]

  module RPC_context : [%sig "v4/RPC_context.mli"]
end
