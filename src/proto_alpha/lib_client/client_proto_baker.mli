(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

open Protocol
open Alpha_context
open Protocol_client_context

type transfer_to_scripted = {
  amount : Tez.t;
  destination : Contract.t;
  entrypoint : string;
  parameter : Script.expr;
  parameter_type : Script.expr;
}

type action =
  | Transfer_to_implicit of Tez.t * Signature.Public_key_hash.t
  | Transfer_to_scripted of transfer_to_scripted
  | Submit_proposals of Protocol_hash.t list
  | Submit_ballot of Protocol_hash.t * Vote.ballot
  | Set_active of bool
  | Toggle_delegations of bool
  | Set_consensus_key of Signature.Public_key.t
  | Set_owner_keys of Z.t * Signature.Public_key.t list
  | Set_pvss_key of Pvss_secp256k1.Public_key.t
  | Generic of Script.expr

type multisig_prepared_action = {
  bytes : Bytes.t;
  threshold : Z.t;
  keys : public_key list;
  counter : Z.t;
}

type error += More_than_one_key

val generic_entrypoint : string

val generic_lambda_type : string

val prepare_transfer_action :
  ?arg:string ->
  ?entrypoint:string ->
  full ->
  amount:Tez.t ->
  destination:Contract.contract ->
  action tzresult Lwt.t

val prepare_generic_action :
  full -> Michelson_v1_parser.parsed -> action tzresult Lwt.t

val mk_payload : stored_counter:Z.t -> action:action -> Script.node

val mk_bytes_to_sign :
  chain_id:Chain_id.t -> payload:Script.node -> Contract.t -> bytes

val mk_singlesig_script_param :
  payload:Script.node -> signature:Signature.t -> Script.expr

val mk_multisig_script_param :
  payload:Script.node -> signatures:Signature.t option list -> Script.expr

val call_singlesig :
  full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?branch:int ->
  source:public_key_hash ->
  src_pk:public_key ->
  src_sk:Client_keys.sk_uri ->
  baker:Baker_hash.t ->
  action:action ->
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  ?counter:Z.t ->
  fee_parameter:Injection.fee_parameter ->
  unit ->
  (Kind.transaction Kind.manager Injection.result * Contract.t list) tzresult
  Lwt.t

val call_multisig :
  full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?branch:int ->
  source:public_key_hash ->
  src_pk:public_key ->
  src_sk:Client_keys.sk_uri ->
  baker:Baker_hash.t ->
  signatures:Signature.t list ->
  action:action ->
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  ?counter:Z.t ->
  fee_parameter:Injection.fee_parameter ->
  unit ->
  (Kind.transaction Kind.manager Injection.result * Contract.t list) tzresult
  Lwt.t

val prepare_multisig_transaction :
  full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  baker:Baker_hash.t ->
  action:action ->
  unit ->
  multisig_prepared_action tzresult Lwt.t
