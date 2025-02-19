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

(** Perform simulation of the given operations and return the corresponding
   [preapply_result]s. *)
val simulate :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?successor_level:bool ->
  ?branch:int ->
  ?latency:int ->
  'kind contents_list ->
  'kind preapply_result tzresult Lwt.t

type 'kind result_list =
  Operation_hash.t * 'kind contents_list * 'kind contents_result_list

(** /!\ [inject_operation] does not perform automatic patching of
    gas, storage and fees; use [inject_manager_operation] to inject
    manager operations. *)
val inject_operation :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?simulation:bool ->
  ?successor_level:bool ->
  ?branch:int ->
  ?src_sk:Client_keys.sk_uri ->
  ?verbose_signing:bool ->
  ?fee_parameter:fee_parameter ->
  'kind contents_list ->
  'kind result_list tzresult Lwt.t

type 'kind result = Operation_hash.t * 'kind contents * 'kind contents_result

val prepare_manager_operation :
  fee:Tez.t Limit.t ->
  gas_limit:Gas.Arith.integral Limit.t ->
  storage_limit:Z.t Limit.t ->
  'kind manager_operation ->
  'kind Annotated_manager_operation.t

val inject_manager_operation :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?successor_level:bool ->
  ?branch:int ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  ?simulation:bool ->
  ?force:bool ->
  ?safety_guard:Gas.Arith.integral ->
  source:Signature.Public_key_hash.t ->
  src_pk:Signature.public_key ->
  src_sk:Client_keys.sk_uri ->
  fee:Tez.t Limit.t ->
  gas_limit:Gas.Arith.integral Limit.t ->
  storage_limit:Z.t Limit.t ->
  ?counter:Manager_counter.t ->
  ?replace_by_fees:bool ->
  fee_parameter:fee_parameter ->
  'kind Annotated_manager_operation.annotated_list ->
  (Operation_hash.t
  * packed_operation
  * 'kind Kind.manager contents_list
  * 'kind Kind.manager contents_result_list)
  tzresult
  Lwt.t

(** Collects the addresses of all contracts originated by a batch of operations
    by looking at the operation results. Fails if an operation in the batch is
    failed unless [force] is given. *)
val originated_contracts :
  force:bool -> 'kind contents_result_list -> Contract_hash.t list tzresult
