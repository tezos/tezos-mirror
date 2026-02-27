(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Environment_context
open Environment_protocol_T

module type T = sig
  include
    Tezos_protocol_environment_sigs.V16.T
      with type Format.formatter = Format.formatter
       and type 'a Seq.node = 'a Seq.node
       and type 'a Seq.t = unit -> 'a Seq.node
      (* This ['a Seq.t = unit -> 'a Seq.node] cannot be replaced by the
         simpler ['a Seq.t = 'a Seq.t] (which one could think is equivalent)
         because this makes [Seq.t] abstract in the protocol environment.
         Specifically, with the [= 'a Seq.t] constraints sequences can be
         passed to [fold], [iter] and other such functions, but they cannot
         be traversed manually. *)
       and type 'a Data_encoding.t = 'a Data_encoding.t
       and type 'a Data_encoding.Compact.t = 'a Data_encoding.Compact.t
       and type 'a Data_encoding.lazy_t = 'a Data_encoding.lazy_t
       and type 'a Lwt.t = 'a Lwt.t
       and type ('a, 'b) Pervasives.result = ('a, 'b) result
       and type Chain_id.t = Chain_id.t
       and type Block_hash.t = Block_hash.t
       and type Operation_hash.t = Operation_hash.t
       and type Operation_list_hash.t = Operation_list_hash.t
       and type Operation_list_list_hash.t = Operation_list_list_hash.t
       and type Context.t = Context.t
       and type Context.cache_key = Environment_context.Context.cache_key
       and type Context.cache_value = Environment_context.Context.cache_value
       and type Context_hash.t = Context_hash.t
       and type Context_hash.Version.t = Context_hash.Version.t
       and type Context.config = Tezos_context_sigs.Config.t
       and module Context.Proof = Environment_context.Context.Proof
       and type Context_binary.t = Tezos_context_memory.Context_binary.t
       and type Context_binary.tree = Tezos_context_memory.Context_binary.tree
       and type Protocol_hash.t = Protocol_hash.t
       and type Time.t = Time.Protocol.t
       and type Operation.shell_header = Operation.shell_header
       and type Operation.t = Operation.t
       and type Block_header.shell_header = Block_header.shell_header
       and type Block_header.t = Block_header.t
       and type 'a RPC_directory.t = 'a Tezos_rpc.Directory.t
       and type Ed25519.Public_key_hash.t =
        Tezos_crypto.Signature.Ed25519.Public_key_hash.t
       and type Ed25519.Public_key.t =
        Tezos_crypto.Signature.Ed25519.Public_key.t
       and type Ed25519.t = Tezos_crypto.Signature.Ed25519.t
       and type Secp256k1.Public_key_hash.t =
        Tezos_crypto.Signature.Secp256k1.Public_key_hash.t
       and type Secp256k1.Public_key.t =
        Tezos_crypto.Signature.Secp256k1.Public_key.t
       and type Secp256k1.t = Tezos_crypto.Signature.Secp256k1.t
       and type P256.Public_key_hash.t =
        Tezos_crypto.Signature.P256.Public_key_hash.t
       and type P256.Public_key.t = Tezos_crypto.Signature.P256.Public_key.t
       and type P256.t = Tezos_crypto.Signature.P256.t
       and type Bls.Public_key_hash.t =
        Tezos_crypto.Signature.Bls.Public_key_hash.t
       and type Bls.Public_key.t = Tezos_crypto.Signature.Bls.Public_key.t
       and type Bls.t = Tezos_crypto.Signature.Bls.t
       and type Mldsa44.Public_key_hash.t =
        Tezos_crypto.Signature.Mldsa44.Public_key_hash.t
       and type Mldsa44.Public_key.t =
        Tezos_crypto.Signature.Mldsa44.Public_key.t
       and type Mldsa44.t = Tezos_crypto.Signature.Mldsa44.t
       and type Signature.public_key_hash =
        Tezos_crypto.Signature.V3.public_key_hash
       and type Signature.public_key = Tezos_crypto.Signature.V3.public_key
       and type Signature.signature = Tezos_crypto.Signature.V3.signature
       and type Signature.t = Tezos_crypto.Signature.V3.t
       and type Signature.watermark = Tezos_crypto.Signature.V3.watermark
       and type Micheline.canonical_location = Micheline.canonical_location
       and type 'a Micheline.canonical = 'a Micheline.canonical
       and type Z.t = Z.t
       and type Q.t = Q.t
       and type Bitset.t = Tezos_base.Bitset.t
       and type ('a, 'b) Micheline.node = ('a, 'b) Micheline.node
       and type Data_encoding.json_schema = Data_encoding.json_schema
       and type ('a, 'b) RPC_path.t = ('a, 'b) Tezos_rpc.Path.t
       and type RPC_service.meth = Tezos_rpc.Service.meth
       and type (+'m, 'pr, 'p, 'q, 'i, 'o) RPC_service.t =
        ('m, 'pr, 'p, 'q, 'i, 'o) Tezos_rpc.Service.t
       and type Error_monad.shell_tztrace = Error_monad.tztrace
       and type 'a Error_monad.shell_tzresult = ('a, Error_monad.tztrace) result
       and type Timelock.chest = Tezos_crypto.Timelock.chest
       and type Timelock.chest_key = Tezos_crypto.Timelock.chest_key
       and type Timelock.opening_result = Tezos_crypto.Timelock.opening_result
       and module Sapling = Tezos_sapling.Core.Validator
       and type ('a, 'b) Either.t = ('a, 'b) Stdlib.Either.t
       and type Bls.Primitive.Fr.t = Bls12_381.Fr.t
       and type Plonk.proof = Tezos_protocol_environment_structs.V16.Plonk.proof
       and type Plonk.public_parameters =
        Tezos_protocol_environment_structs.V16.Plonk.verifier_public_parameters
       and type Dal.parameters = Tezos_crypto_dal.Cryptobox.Verifier.parameters
       and type Dal.commitment = Tezos_crypto_dal.Cryptobox.Verifier.commitment
       and type Dal.commitment_proof =
        Tezos_crypto_dal.Cryptobox.Verifier.commitment_proof
       and type Dal.page_proof = Tezos_crypto_dal.Cryptobox.Verifier.page_proof
       and type Dal.share = Tezos_crypto_dal.Cryptobox.Verifier.share
       and type Dal.shard = Tezos_crypto_dal.Cryptobox.Verifier.shard
       and type Dal.shard_proof =
        Tezos_crypto_dal.Cryptobox.Verifier.shard_proof
       and type Bounded.Non_negative_int32.t =
        Tezos_base.Bounded.Non_negative_int32.t
       and type Wasm_2_0_0.version = Tezos_scoru_wasm.Wasm_pvm_state.version
       and type Wasm_2_0_0.input = Tezos_scoru_wasm.Wasm_pvm_state.input_info
       and type Wasm_2_0_0.output = Tezos_scoru_wasm.Wasm_pvm_state.output_info
       and type Wasm_2_0_0.reveal = Tezos_scoru_wasm.Wasm_pvm_state.reveal
       and type Wasm_2_0_0.input_request =
        Tezos_scoru_wasm.Wasm_pvm_state.input_request
       and type Wasm_2_0_0.info = Tezos_scoru_wasm.Wasm_pvm_state.info
       and type Wasm_2_0_0.Wasm_pvm_machine.context =
        Tezos_smart_rollup_wasm_in_memory.Wasm_pvm_in_memory.context
       and type Wasm_2_0_0.Wasm_pvm_machine.state =
        Tezos_smart_rollup_wasm_in_memory.Wasm_pvm_in_memory.state
       and type Wasm_2_0_0.Wasm_pvm_machine.proof =
        Tezos_smart_rollup_wasm_in_memory.Wasm_pvm_in_memory.proof
       and module Skip_list = Tezos_base.Skip_list
       and type Smart_rollup.Address.t =
        Tezos_crypto.Hashed.Smart_rollup_address.t
       and type Smart_rollup.Commitment_hash.t =
        Tezos_crypto.Hashed.Smart_rollup_commitment_hash.t
       and type Smart_rollup.State_hash.t =
        Tezos_crypto.Hashed.Smart_rollup_state_hash.t
       and type Smart_rollup.Inbox_hash.t =
        Tezos_crypto.Hashed.Smart_rollup_inbox_hash.t
       and type Smart_rollup.Merkelized_payload_hashes_hash.t =
        Tezos_crypto.Hashed.Smart_rollup_merkelized_payload_hashes_hash.t

  (** An [Ecoproto_error e] is a shell error that carry a protocol error.

      Each protocol has its own error-monad (instantiated when this module here
      is applied) with a fresh extensible error type. This protocol-specific
      error type is incompatible with the shell's. The [Ecoproto_error]
      constructor belongs to the shell's error type and it carries the errors of
      the protocol's specific error type back into the shell's.

      The function [wrap_tz*] below provide wrappers for three different levels:
      errors, traces, and tzresults. They are used within the implementation of
      the environment to translate some return values from the protocol's error
      monad into the shell's. They are exported because they can be useful for
      writing tests for the protocol (i.e., for the tests located in
      [src/proto_*/lib_protocol/test/]) and for writing protocol-specific
      support libraries and binaries (i.e., for the code in
      [src/proto_*/lib_{client,delegate,etc.}]). *)
  type error += Ecoproto_error of Error_monad.error

  (** [wrap_tzerror e] is a shell error wrapping the protocol error [e].
      (It is [Ecoproto_error e].) *)
  val wrap_tzerror : Error_monad.error -> error

  (** [wrap_tztrace t] is a shell trace composed of the wrapped errors of the
      protocol trace [t]. *)
  val wrap_tztrace : Error_monad.error Error_monad.trace -> error trace

  (** [wrap_tzresult r] is a shell tzresult that carries the same result as or a
      wrapped trace of the protocol tzresult [r].
      (It is [Ok x] if [r] is [Ok x], it is [Error (wrap_tztrace t)] if [r] is
      [Error t].) *)
  val wrap_tzresult : 'a Error_monad.tzresult -> 'a tzresult

  module Lift (P : Updater.PROTOCOL) :
    PROTOCOL
      with type block_header_data = P.block_header_data
       and type block_header_metadata = P.block_header_metadata
       and type block_header = P.block_header
       and type operation_data = P.operation_data
       and type operation_receipt = P.operation_receipt
       and type operation = P.operation
       and type validation_state = P.validation_state
       and type application_state = P.application_state

  class ['chain, 'block] proto_rpc_context :
    Tezos_rpc.Context.t ->
    (unit, (unit * 'chain) * 'block) RPC_path.t ->
    ['chain * 'block] RPC_context.simple

  class ['block] proto_rpc_context_of_directory :
    ('block -> RPC_context.t) ->
    RPC_context.t RPC_directory.t ->
    ['block] RPC_context.simple
end

module Make
    (Param : sig
      val name : string
    end)
    () :
  T
    with type Updater.validation_result = validation_result
     and type Updater.quota = quota
     and type Updater.rpc_context = rpc_context
