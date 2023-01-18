(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
open Protocol_client_context.Alpha_block_services

type error += Cannot_read_block_metadata of Block_hash.t

type 'accu successful_operation_processor = {
  apply :
    'kind.
    'accu ->
    source:public_key_hash ->
    'kind manager_operation ->
    'kind Apply_results.successful_manager_operation_result ->
    'accu;
  apply_internal :
    'kind.
    'accu ->
    source:public_key_hash ->
    'kind Apply_internal_results.internal_operation ->
    'kind Apply_internal_results.successful_internal_operation_result ->
    'accu;
}

type 'accu operation_processor = {
  apply :
    'kind.
    'accu ->
    source:public_key_hash ->
    'kind manager_operation ->
    'kind Apply_results.manager_operation_result ->
    'accu;
  apply_internal :
    'kind.
    'accu ->
    source:public_key_hash ->
    'kind Apply_internal_results.internal_operation ->
    'kind Apply_internal_results.internal_operation_result ->
    'accu;
}

(** [process_manager_operations accu operations operator] folds over the list of
    manager operations in [operations] applying [operator] to transform [accu]
    along the way. *)
val process_manager_operations :
  'a -> operation list list -> 'a operation_processor -> 'a

(** [process_applied_manager_operations accu operations operator] folds over the
    list of applied manager operations in [operations] applying [operator] to
    transform [accu] along the way. *)
val process_applied_manager_operations :
  'a -> operation list list -> 'a successful_operation_processor -> 'a
