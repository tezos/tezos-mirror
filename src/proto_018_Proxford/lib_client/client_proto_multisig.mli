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

(* The script of the recommended version of the multisig contract.
   This is the script originated by [originate_multisig]. *)
val multisig_script : Script.expr

(* A description of the possible actions that a multisig contract can run. *)
type multisig_action =
  | Transfer of {
      amount : Tez.t;
      destination : Contract.t;
      entrypoint : Entrypoint.t;
      parameter_type : Script.expr;
      parameter : Script.expr;
    }
  | Change_delegate of public_key_hash option
  | Lambda of Script.expr
  | Change_keys of Z.t * public_key list

(* A prepared action is the byte sequence that needs to be signed to run the
   action together with some data that can be useful for the user and to call
   the multisig contracts once enough signatures are provided. *)
type multisig_prepared_action = {
  (* The sequence of bytes to be signed. *)
  bytes : Bytes.t;
  (* Information reported to the user so that she knows who can sign and how
     many signatures are required. *)
  threshold : Z.t;
  keys : public_key list;
  (* Information needed to execute the action ones enough signatures have been
     gathered. *)
  counter : Z.t;
  entrypoint : Entrypoint.t option;
  generic : bool;
}

(* The client will refuse to interact with a multisig contract if the hash of
   its script is not in this list. *)
val known_multisig_hashes : Script_expr_hash.t list

(* Originate a new multisig contract *)
val originate_multisig :
  full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?branch:int ->
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?safety_guard:Gas.Arith.integral ->
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

(* Prepare an action, see [multisig_prepared_action]. *)
val prepare_multisig_transaction :
  full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  multisig_contract:Contract_hash.t ->
  action:multisig_action ->
  unit ->
  multisig_prepared_action tzresult Lwt.t

(* Call a multisig contract, requesting it to run an action. *)
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
  multisig_contract:Contract_hash.t ->
  action:multisig_action ->
  signatures:Signature.t list ->
  amount:Tez.t ->
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?safety_guard:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  ?counter:Manager_counter.t ->
  fee_parameter:Injection.fee_parameter ->
  unit ->
  (Kind.transaction Kind.manager Injection.result * Contract_hash.t list)
  tzresult
  Lwt.t

(* Same as [call_multisig] but the action to be performed is reconstructed from
   the byte sequence that was signed. *)
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
  multisig_contract:Contract_hash.t ->
  bytes:Bytes.t ->
  signatures:Signature.t list ->
  amount:Tez.t ->
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?safety_guard:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  ?counter:Manager_counter.t ->
  fee_parameter:Injection.fee_parameter ->
  unit ->
  (Kind.transaction Kind.manager Injection.result * Contract_hash.t list)
  tzresult
  Lwt.t
