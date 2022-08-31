module type T = sig
  module CamlinternalFormatBasics : module type of struct
    include Tezos_protocol_environment_sigs_internals.CamlinternalFormatBasics
  end

  module Pervasives : [%sig "v2/pervasives.mli"]

  open Pervasives

  module Seq : [%sig "v2/seq.mli"]

  module List : [%sig "v2/list.mli"]

  module String : [%sig "v2/string.mli"]

  module Char : [%sig "v2/char.mli"]

  module Bytes : [%sig "v2/bytes.mli"]

  module Int32 : [%sig "v2/int32.mli"]

  module Int64 : [%sig "v2/int64.mli"]

  module Format : [%sig "v2/format.mli"]

  module Hex : [%sig "v2/hex.mli"]

  module Z : [%sig "v2/z.mli"]

  module Lwt : [%sig "v2/lwt.mli"]

  module Lwt_list : [%sig "v2/lwt_list.mli"]

  module Data_encoding : [%sig "v2/data_encoding.mli"]

  module Raw_hashes : [%sig "v2/raw_hashes.mli"]

  module Compare : [%sig "v2/compare.mli"]

  module Error_monad : [%sig "v2/error_monad.mli"]

  open Error_monad

  module Logging : [%sig "v2/logging.mli"]

  module Time : [%sig "v2/time.mli"]

  module Option : [%sig "v2/option.mli"]

  module TzEndian : [%sig "v2/tzEndian.mli"]

  module Bits : [%sig "v2/bits.mli"]

  module RPC_arg : [%sig "v2/RPC_arg.mli"]

  module RPC_path : [%sig "v2/RPC_path.mli"]

  module RPC_query : [%sig "v2/RPC_query.mli"]

  module RPC_service : [%sig "v2/RPC_service.mli"]

  module RPC_answer : [%sig "v2/RPC_answer.mli"]

  module RPC_directory : [%sig "v2/RPC_directory.mli"]

  module Base58 : [%sig "v2/base58.mli"]

  module S : [%sig "v2/s.mli"]

  module Set : [%sig "v2/set.mli"]

  module Map : [%sig "v2/map.mli"]

  module Blake2B : [%sig "v2/blake2B.mli"]

  module Bls12_381 : [%sig "v2/bls12_381.mli"]

  module Ed25519 : [%sig "v2/ed25519.mli"]

  module Secp256k1 : [%sig "v2/secp256k1.mli"]

  module P256 : [%sig "v2/p256.mli"]

  module Chain_id : [%sig "v2/chain_id.mli"]

  module Signature : [%sig "v2/signature.mli"]

  module Block_hash : [%sig "v2/block_hash.mli"]

  module Operation_hash : [%sig "v2/operation_hash.mli"]

  module Operation_list_hash : [%sig "v2/operation_list_hash.mli"]

  module Operation_list_list_hash : [%sig "v2/operation_list_list_hash.mli"]

  module Protocol_hash : [%sig "v2/protocol_hash.mli"]

  module Context_hash : [%sig "v2/context_hash.mli"]

  module Sapling : [%sig "v2/sapling.mli"]

  module Micheline : [%sig "v2/micheline.mli"]

  module Block_header : [%sig "v2/block_header.mli"]

  module Fitness : [%sig "v2/fitness.mli"]

  module Operation : [%sig "v2/operation.mli"]

  module Context : [%sig "v2/context.mli"]

  module Updater : [%sig "v2/updater.mli"]

  module RPC_context : [%sig "v2/RPC_context.mli"]

  module Equality_witness : [%sig "v2/equality_witness.mli"]
end
