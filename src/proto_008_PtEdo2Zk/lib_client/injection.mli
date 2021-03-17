(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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
open Apply_results

type _ annotated_manager_operation =
  | Manager_info : {
      fee : Tez.t option;
      operation : 'kind manager_operation;
      gas_limit : Gas.Arith.integral option;
      storage_limit : Z.t option;
    }
      -> 'kind annotated_manager_operation

type packed_annotated_manager_operation =
  | Annotated_manager_operation :
      'kind annotated_manager_operation
      -> packed_annotated_manager_operation

(** The [annotated_manager_operation_list] type helps making
    [contents_list] from a list of [manager_operation]s.
    Its construction mimics [contents_list] in order to keep
    consistent types when calling [inject_manager_operation]
    and [inject_operation].*)
type _ annotated_manager_operation_list =
  | Single_manager :
      'kind annotated_manager_operation
      -> 'kind annotated_manager_operation_list
  | Cons_manager :
      'kind annotated_manager_operation
      * 'rest annotated_manager_operation_list
      -> ('kind * 'rest) annotated_manager_operation_list

type packed_annotated_manager_operation_list =
  | Manager_list :
      'kind annotated_manager_operation_list
      -> packed_annotated_manager_operation_list

val manager_of_list :
  packed_annotated_manager_operation list ->
  packed_annotated_manager_operation_list

val manager_to_list :
  packed_annotated_manager_operation_list ->
  packed_annotated_manager_operation list

type 'kind preapply_result =
  Operation_hash.t * 'kind operation * 'kind operation_metadata

type fee_parameter = {
  minimal_fees : Tez.t;
  minimal_nanotez_per_byte : Q.t;
  minimal_nanotez_per_gas_unit : Q.t;
  force_low_fee : bool;
  fee_cap : Tez.t;
  burn_cap : Tez.t;
}

val dummy_fee_parameter : fee_parameter

val preapply :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?verbose_signing:bool ->
  ?fee_parameter:fee_parameter ->
  ?branch:int ->
  ?src_sk:Client_keys.sk_uri ->
  'kind contents_list ->
  'kind preapply_result tzresult Lwt.t

type 'kind result_list =
  Operation_hash.t * 'kind contents_list * 'kind contents_result_list

val inject_operation :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?simulation:bool ->
  ?branch:int ->
  ?src_sk:Client_keys.sk_uri ->
  ?verbose_signing:bool ->
  fee_parameter:fee_parameter ->
  ?compute_fee:bool ->
  'kind contents_list ->
  'kind result_list tzresult Lwt.t

type 'kind result = Operation_hash.t * 'kind contents * 'kind contents_result

val prepare_manager_operation :
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  'kind manager_operation ->
  'kind annotated_manager_operation

val inject_manager_operation :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?branch:int ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?simulation:bool ->
  source:Signature.Public_key_hash.t ->
  src_pk:Signature.public_key ->
  src_sk:Client_keys.sk_uri ->
  ?fee:Tez.t ->
  ?gas_limit:Gas.Arith.integral ->
  ?storage_limit:Z.t ->
  ?counter:Z.t ->
  fee_parameter:fee_parameter ->
  'kind annotated_manager_operation_list ->
  'kind Kind.manager result_list tzresult Lwt.t

val originated_contracts :
  'kind contents_result_list -> Contract.t list tzresult
