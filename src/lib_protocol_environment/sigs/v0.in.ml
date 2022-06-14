open Tezos_protocol_environment_sigs_stdlib_compat.V_all

module type T = sig
  module Pervasives : [%sig "v0/pervasives.mli"] [@@coq_plain_module]

  open Pervasives

  module List : [%sig "v0/list.mli"] [@@coq_plain_module]

  module String : [%sig "v0/string.mli"] [@@coq_plain_module]

  module Int32 : [%sig "v0/int32.mli"] [@@coq_plain_module]

  module Int64 : [%sig "v0/int64.mli"] [@@coq_plain_module]

  module Format : [%sig "v0/format.mli"] [@@coq_plain_module]

  module MBytes : [%sig "v0/mBytes.mli"] [@@coq_plain_module]

  module Z : [%sig "v0/z.mli"] [@@coq_plain_module]

  module Lwt : [%sig "v0/lwt.mli"] [@@coq_plain_module]

  module Lwt_list : [%sig "v0/lwt_list.mli"] [@@coq_plain_module]

  module Raw_hashes : [%sig "v0/raw_hashes.mli"] [@@coq_plain_module]

  module Compare : [%sig "v0/compare.mli"] [@@coq_plain_module]

  module Data_encoding : [%sig "v0/data_encoding.mli"] [@@coq_plain_module]

  module Error_monad : [%sig "v0/error_monad.mli"] [@@coq_plain_module]

  open Error_monad

  module Logging : [%sig "v0/logging.mli"] [@@coq_plain_module]

  module Time : [%sig "v0/time.mli"] [@@coq_plain_module]

  module Option : [%sig "v0/option.mli"] [@@coq_plain_module]

  module RPC_arg : [%sig "v0/RPC_arg.mli"] [@@coq_plain_module]

  module RPC_path : [%sig "v0/RPC_path.mli"] [@@coq_plain_module]

  module RPC_query : [%sig "v0/RPC_query.mli"] [@@coq_plain_module]

  module RPC_service : [%sig "v0/RPC_service.mli"] [@@coq_plain_module]

  module RPC_answer : [%sig "v0/RPC_answer.mli"] [@@coq_plain_module]

  module RPC_directory : [%sig "v0/RPC_directory.mli"] [@@coq_plain_module]

  module Base58 : [%sig "v0/base58.mli"] [@@coq_plain_module]

  module S : [%sig "v0/s.mli"] [@@coq_plain_module]

  module Set : [%sig "v0/set.mli"] [@@coq_plain_module]

  module Map : [%sig "v0/map.mli"] [@@coq_plain_module]

  module Blake2B : [%sig "v0/blake2B.mli"] [@@coq_plain_module]

  module Ed25519 : [%sig "v0/ed25519.mli"] [@@coq_plain_module]

  module Secp256k1 : [%sig "v0/secp256k1.mli"] [@@coq_plain_module]

  module P256 : [%sig "v0/p256.mli"] [@@coq_plain_module]

  module Chain_id : [%sig "v0/chain_id.mli"] [@@coq_plain_module]

  module Signature : [%sig "v0/signature.mli"] [@@coq_plain_module]

  module Block_hash : [%sig "v0/block_hash.mli"] [@@coq_plain_module]

  module Operation_hash : [%sig "v0/operation_hash.mli"] [@@coq_plain_module]

  module Operation_list_hash : [%sig "v0/operation_list_hash.mli"]
  [@@coq_plain_module]

  module Operation_list_list_hash : [%sig "v0/operation_list_list_hash.mli"]
  [@@coq_plain_module]

  module Protocol_hash : [%sig "v0/protocol_hash.mli"] [@@coq_plain_module]

  module Context_hash : [%sig "v0/context_hash.mli"] [@@coq_plain_module]

  module Micheline : [%sig "v0/micheline.mli"] [@@coq_plain_module]

  module Block_header : [%sig "v0/block_header.mli"] [@@coq_plain_module]

  module Fitness : [%sig "v0/fitness.mli"] [@@coq_plain_module]

  module Operation : [%sig "v0/operation.mli"] [@@coq_plain_module]

  module Context : [%sig "v0/context.mli"] [@@coq_plain_module]

  module Updater : [%sig "v0/updater.mli"] [@@coq_plain_module]

  module RPC_context : [%sig "v0/RPC_context.mli"] [@@coq_plain_module]
end
