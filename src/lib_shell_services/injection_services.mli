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

open Tezos_rpc.Context

(** While injecting several operations (see
   {!val:private_operations}), if one injection failed, we have to
   report the error. To avoid using recursive errors we do the
   reporting as follows:

   - We wrap the error into [Injection_operations_error]

   - If injecting the operation [oph] succeeded we use
   [Injection_operation_succeed_case oph]

   - If injecting the operation [oph] failed with [err], we use
   [Injection_operation_error_case oph] followed by [err].  *)
type Error_monad.error += Injection_operations_error

type Error_monad.error += Injection_operation_succeed_case of Operation_hash.t

type Error_monad.error += Injection_operation_error_case of Operation_hash.t

(** [block cctxt ?async ?force raw_block] tries to inject
    [raw_block] inside the node. If [?async] is [true], [raw_block]
    will be validated before the result is returned. If [?force] is
    true, the block will be injected even on non strictly increasing
    fitness. *)
val block :
  #simple ->
  ?async:bool ->
  ?force:bool ->
  ?chain:Chain_services.chain ->
  Bytes.t ->
  Operation.t list list ->
  Block_hash.t tzresult Lwt.t

val operation :
  #simple ->
  ?async:bool ->
  ?chain:Chain_services.chain ->
  Bytes.t ->
  Operation_hash.t tzresult Lwt.t

val private_operation :
  #simple ->
  ?async:bool ->
  ?chain:Chain_services.chain ->
  Bytes.t ->
  Operation_hash.t tzresult Lwt.t

(** [private_operations] injects multiple operations. The [private_]
   prefix is because the service is bound to the /private/ path-prefix
   which is intended for tests only. See the ~description argument in
   the definition in the ml file for more information.  *)
val private_operations :
  #simple ->
  ?async:bool ->
  ?force:bool ->
  ?chain:Chain_services.chain ->
  Bytes.t list ->
  Operation_hash.t list tzresult Lwt.t

val protocol :
  #simple -> ?async:bool -> Protocol.t -> Protocol_hash.t tzresult Lwt.t

module S : sig
  val block :
    ( [`POST],
      unit,
      unit,
      < async : bool ; force : bool ; chain : Chain_services.chain option >,
      Bytes.t * Operation.t list list,
      Block_hash.t )
    Tezos_rpc.Service.t

  val operation :
    ( [`POST],
      unit,
      unit,
      < async : bool ; chain : Chain_services.chain option >,
      Bytes.t,
      Operation_hash.t )
    Tezos_rpc.Service.t

  val private_operation :
    ( [`POST],
      unit,
      unit,
      < async : bool ; chain : Chain_services.chain option >,
      Bytes.t,
      Operation_hash.t )
    Tezos_rpc.Service.t

  val private_operations :
    ( [`POST],
      unit,
      unit,
      < async : bool ; force : bool ; chain : Chain_services.chain option >,
      Bytes.t list,
      Operation_hash.t list )
    Tezos_rpc.Service.t

  val protocol :
    ( [`POST],
      unit,
      unit,
      < async : bool >,
      Protocol.t,
      Protocol_hash.t )
    Tezos_rpc.Service.t
end
