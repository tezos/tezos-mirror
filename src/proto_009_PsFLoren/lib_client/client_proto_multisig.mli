(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type multisig_action =
  | Transfer of Tez.t * Contract.t
  | Change_delegate of public_key_hash option
  | Change_keys of Z.t * public_key list

type multisig_prepared_action = {
  bytes : Bytes.t;
  threshold : Z.t;
  keys : public_key list;
  counter : Z.t;
}

val known_multisig_hashes : Script_expr_hash.t list

val originate_multisig :
  full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?branch:int ->
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  ?verbose_signing:bool ->
  delegate:public_key_hash option ->
  threshold:Z.t ->
  keys:public_key list ->
  balance:Tez.t ->
  source:public_key_hash ->
  src_pk:public_key ->
  src_sk:Client_keys.sk_uri ->
  fee_parameter:Injection.fee_parameter ->
  unit ->
  (Kind.origination Kind.manager Injection.result * Contract.t) tzresult Lwt.t

val prepare_multisig_transaction :
  full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  multisig_contract:Contract.t ->
  action:multisig_action ->
  unit ->
  multisig_prepared_action tzresult Lwt.t

val call_multisig :
  full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?branch:int ->
  source:public_key_hash ->
  src_pk:public_key ->
  src_sk:Client_keys.sk_uri ->
  multisig_contract:Contract.t ->
  action:multisig_action ->
  signatures:Signature.t list ->
  amount:Tez.t ->
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  ?counter:Z.t ->
  fee_parameter:Injection.fee_parameter ->
  unit ->
  (Kind.transaction Kind.manager Injection.result * Contract.t list) tzresult
  Lwt.t

val call_multisig_on_bytes :
  full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?branch:int ->
  source:public_key_hash ->
  src_pk:public_key ->
  src_sk:Client_keys.sk_uri ->
  multisig_contract:Contract.t ->
  bytes:Bytes.t ->
  signatures:Signature.t list ->
  amount:Tez.t ->
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  ?counter:Z.t ->
  fee_parameter:Injection.fee_parameter ->
  unit ->
  (Kind.transaction Kind.manager Injection.result * Contract.t list) tzresult
  Lwt.t
