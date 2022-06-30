module type T = sig
  module CamlinternalFormatBasics : module type of struct
    include Tezos_protocol_environment_sigs_internals.CamlinternalFormatBasics
  end

  module Pervasives : [%sig "v0/pervasives.mli"]

  open Pervasives

  module List : [%sig "v0/list.mli"]

  module String : [%sig "v0/string.mli"]

  module Int32 : [%sig "v0/int32.mli"]

  module Int64 : [%sig "v0/int64.mli"]

  module Format : [%sig "v0/format.mli"]

  module MBytes : [%sig "v0/mBytes.mli"]

  module Z : [%sig "v0/z.mli"]

  module Lwt : [%sig "v0/lwt.mli"]

  module Lwt_list : [%sig "v0/lwt_list.mli"]

  module Raw_hashes : [%sig "v0/raw_hashes.mli"]

  module Compare : [%sig "v0/compare.mli"]

  module Data_encoding : [%sig "v0/data_encoding.mli"]

  module Error_monad : [%sig "v0/error_monad.mli"]

  open Error_monad

  module Logging : [%sig "v0/logging.mli"]

  module Time : [%sig "v0/time.mli"]

  module Option : [%sig "v0/option.mli"]

  module RPC_arg : [%sig "v0/RPC_arg.mli"]

  module RPC_path : [%sig "v0/RPC_path.mli"]

  module RPC_query : [%sig "v0/RPC_query.mli"]

  module RPC_service : [%sig "v0/RPC_service.mli"]

  module RPC_answer : [%sig "v0/RPC_answer.mli"]

  module RPC_directory : [%sig "v0/RPC_directory.mli"]

  module Base58 : [%sig "v0/base58.mli"]

  module S : [%sig "v0/s.mli"]

  module Set : [%sig "v0/set.mli"]

  module Map : [%sig "v0/map.mli"]

  module Blake2B : [%sig "v0/blake2B.mli"]

  module Ed25519 : [%sig "v0/ed25519.mli"]

  module Secp256k1 : [%sig "v0/secp256k1.mli"]

  module P256 : [%sig "v0/p256.mli"]

  module Chain_id : [%sig "v0/chain_id.mli"]

  module Signature : [%sig "v0/signature.mli"]

  module Block_hash : [%sig "v0/block_hash.mli"]

  module Operation_hash : [%sig "v0/operation_hash.mli"]

  module Operation_list_hash : [%sig "v0/operation_list_hash.mli"]

  module Operation_list_list_hash : [%sig "v0/operation_list_list_hash.mli"]

  module Protocol_hash : [%sig "v0/protocol_hash.mli"]

  module Context_hash : [%sig "v0/context_hash.mli"]

  module Micheline : [%sig "v0/micheline.mli"]

  module Block_header : [%sig "v0/block_header.mli"]

  module Fitness : [%sig "v0/fitness.mli"]

  module Operation : [%sig "v0/operation.mli"]

  module Context : [%sig "v0/context.mli"]

  module Updater : [%sig "v0/updater.mli"]

  module RPC_context : [%sig "v0/RPC_context.mli"]
end
