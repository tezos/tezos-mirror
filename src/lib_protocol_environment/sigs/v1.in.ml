module type T = sig
  module CamlinternalFormatBasics : module type of struct
    include Tezos_protocol_environment_sigs_internals.CamlinternalFormatBasics
  end

  module Pervasives : [%sig "v1/pervasives.mli"]

  open Pervasives

  module List : [%sig "v1/list.mli"]

  module String : [%sig "v1/string.mli"]

  module Char : [%sig "v1/char.mli"]

  module Bytes : [%sig "v1/bytes.mli"]

  module Int32 : [%sig "v1/int32.mli"]

  module Int64 : [%sig "v1/int64.mli"]

  module Format : [%sig "v1/format.mli"]

  module Hex : [%sig "v1/hex.mli"]

  module Z : [%sig "v1/z.mli"]

  module Lwt : [%sig "v1/lwt.mli"]

  module Lwt_list : [%sig "v1/lwt_list.mli"]

  module Data_encoding : [%sig "v1/data_encoding.mli"]

  module Raw_hashes : [%sig "v1/raw_hashes.mli"]

  module Compare : [%sig "v1/compare.mli"]

  module Error_monad : [%sig "v1/error_monad.mli"]

  open Error_monad

  module Logging : [%sig "v1/logging.mli"]

  module Time : [%sig "v1/time.mli"]

  module Option : [%sig "v1/option.mli"]

  module TzEndian : [%sig "v1/tzEndian.mli"]

  module RPC_arg : [%sig "v1/RPC_arg.mli"]

  module RPC_path : [%sig "v1/RPC_path.mli"]

  module RPC_query : [%sig "v1/RPC_query.mli"]

  module RPC_service : [%sig "v1/RPC_service.mli"]

  module RPC_answer : [%sig "v1/RPC_answer.mli"]

  module RPC_directory : [%sig "v1/RPC_directory.mli"]

  module Base58 : [%sig "v1/base58.mli"]

  module S : [%sig "v1/s.mli"]

  module Set : [%sig "v1/set.mli"]

  module Map : [%sig "v1/map.mli"]

  module Blake2B : [%sig "v1/blake2B.mli"]

  module Bls12_381 : [%sig "v1/bls12_381.mli"]

  module Ed25519 : [%sig "v1/ed25519.mli"]

  module Secp256k1 : [%sig "v1/secp256k1.mli"]

  module P256 : [%sig "v1/p256.mli"]

  module Chain_id : [%sig "v1/chain_id.mli"]

  module Signature : [%sig "v1/signature.mli"]

  module Block_hash : [%sig "v1/block_hash.mli"]

  module Operation_hash : [%sig "v1/operation_hash.mli"]

  module Operation_list_hash : [%sig "v1/operation_list_hash.mli"]

  module Operation_list_list_hash : [%sig "v1/operation_list_list_hash.mli"]

  module Protocol_hash : [%sig "v1/protocol_hash.mli"]

  module Context_hash : [%sig "v1/context_hash.mli"]

  module Sapling : [%sig "v1/sapling.mli"]

  module Micheline : [%sig "v1/micheline.mli"]

  module Block_header : [%sig "v1/block_header.mli"]

  module Fitness : [%sig "v1/fitness.mli"]

  module Operation : [%sig "v1/operation.mli"]

  module Context : [%sig "v1/context.mli"]

  module Updater : [%sig "v1/updater.mli"]

  module RPC_context : [%sig "v1/RPC_context.mli"]
end
