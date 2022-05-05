(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

module type V1 = sig
  include
    Tezos_protocol_environment_sigs.V1.T
      with type Format.formatter = Format.formatter
       and type 'a Data_encoding.t = 'a Data_encoding.t
       and type 'a Data_encoding.lazy_t = 'a Data_encoding.lazy_t
       and type 'a Lwt.t = 'a Lwt.t
       and type ('a, 'b) Pervasives.result = ('a, 'b) result
       and type Chain_id.t = Chain_id.t
       and type Block_hash.t = Block_hash.t
       and type Operation_hash.t = Operation_hash.t
       and type Operation_list_hash.t = Operation_list_hash.t
       and type Operation_list_list_hash.t = Operation_list_list_hash.t
       and type Context.t = Context.t
       and type Context_hash.t = Context_hash.t
       and type Protocol_hash.t = Protocol_hash.t
       and type Time.t = Time.Protocol.t
       and type Operation.shell_header = Operation.shell_header
       and type Operation.t = Operation.t
       and type Block_header.shell_header = Block_header.shell_header
       and type Block_header.t = Block_header.t
       and type 'a RPC_directory.t = 'a RPC_directory.t
       and type Ed25519.Public_key_hash.t = Ed25519.Public_key_hash.t
       and type Ed25519.Public_key.t = Ed25519.Public_key.t
       and type Ed25519.t = Ed25519.t
       and type Secp256k1.Public_key_hash.t = Secp256k1.Public_key_hash.t
       and type Secp256k1.Public_key.t = Secp256k1.Public_key.t
       and type Secp256k1.t = Secp256k1.t
       and type P256.Public_key_hash.t = P256.Public_key_hash.t
       and type P256.Public_key.t = P256.Public_key.t
       and type P256.t = P256.t
       and type Signature.public_key_hash = Signature.public_key_hash
       and type Signature.public_key = Signature.public_key
       and type Signature.t = Signature.t
       and type Signature.watermark = Signature.watermark
       and type Pvss_secp256k1.Commitment.t = Pvss_secp256k1.Commitment.t
       and type Pvss_secp256k1.Encrypted_share.t =
            Pvss_secp256k1.Encrypted_share.t
       and type Pvss_secp256k1.Clear_share.t = Pvss_secp256k1.Clear_share.t
       and type Pvss_secp256k1.Public_key.t = Pvss_secp256k1.Public_key.t
       and type Pvss_secp256k1.Secret_key.t = Pvss_secp256k1.Secret_key.t
       and type 'a Micheline.canonical = 'a Micheline.canonical
       and type Z.t = Z.t
       and type ('a, 'b) Micheline.node = ('a, 'b) Micheline.node
       and type Data_encoding.json_schema = Data_encoding.json_schema
       and type ('a, 'b) RPC_path.t = ('a, 'b) RPC_path.t
       and type RPC_service.meth = RPC_service.meth
       and type (+'m, 'pr, 'p, 'q, 'i, 'o) RPC_service.t =
            ('m, 'pr, 'p, 'q, 'i, 'o) RPC_service.t
       and type Error_monad.shell_error = Error_monad.error
       and module Sapling = Tezos_sapling.Core.Validator_legacy

  type error += Ecoproto_error of Error_monad.error

  val wrap_error : 'a Error_monad.tzresult -> 'a tzresult

  module Lift (P : Updater.PROTOCOL) :
    PROTOCOL
      with type block_header_data = P.block_header_data
       and type block_header_metadata = P.block_header_metadata
       and type block_header = P.block_header
       and type operation_data = P.operation_data
       and type operation_receipt = P.operation_receipt
       and type operation = P.operation
       and type validation_state = P.validation_state

  class ['chain, 'block] proto_rpc_context :
    Tezos_rpc.RPC_context.t
    -> (unit, (unit * 'chain) * 'block) RPC_path.t
    -> ['chain * 'block] RPC_context.simple

  class ['block] proto_rpc_context_of_directory :
    ('block -> RPC_context.t)
    -> RPC_context.t RPC_directory.t
    -> ['block] RPC_context.simple
end

module MakeV1 (Param : sig
  val name : string
end)
() :
  V1
    with type Context.t = Context.t
     and type Updater.validation_result = validation_result
     and type Updater.quota = quota
     and type Updater.rpc_context = rpc_context
