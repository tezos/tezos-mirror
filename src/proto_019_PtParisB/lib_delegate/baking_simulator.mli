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

type incremental = {
  predecessor : Baking_state.block_info;
  context : Tezos_protocol_environment.Context.t;
  state : validation_state * application_state option;
  rev_operations : Operation.packed list;
  header : Tezos_base.Block_header.shell_header;
}

val load_context :
  context_path:string -> Abstract_context_index.t tzresult Lwt.t

(** Make sure that the given context is consistent by trying to read in it *)
val check_context_consistency :
  Abstract_context_index.t -> Context_hash.t -> unit tzresult Lwt.t

(** [begin_construction ~timestamp ~protocol_data ~force_apply abstract_context
    predecessor chain_id] creates a new [incremental] value with an empty
    operation list. A [context] is recovered from the [abstract_index] and the
    resulting_context_hash from [predecessor]. This context is used to create a
    [validation_state] and an [application_state] (if [force_apply] is set). A
    partial [shell_header] is created from [predecessor] information and
    [timestamp]. *)
val begin_construction :
  timestamp:Time.Protocol.t ->
  protocol_data:block_header_data ->
  force_apply:bool ->
  pred_resulting_context_hash:Context_hash.t ->
  Abstract_context_index.t ->
  Baking_state.block_info ->
  Chain_id.t ->
  incremental tzresult Lwt.t

(** [add_operation incremental op] validates [op] in
    [incremental.validation_state] without checking its signature. Indeed, the
    operation has already been validated in the node so it has a correct
    signature. We still need to validate it again because the context may be
    different. [op] is also applied if [incremental] has been created with
    [force_apply] set. This function returns an [incremental] with updated
    operations list and [validation_state] (and [application_state]). *)
val add_operation :
  incremental ->
  Operation.packed ->
  (incremental * operation_receipt option) tzresult Lwt.t

(** [finalize_construction incremental] calls the [finalize_validation] of the
    protocol on the [validation_state] from [incremental]. If [incremental] has
    been created with [force_apply] set, [finalize_application] is also called
    and its results returned. *)
val finalize_construction :
  incremental ->
  (Tezos_protocol_environment.validation_result * block_header_metadata) option
  tzresult
  Lwt.t
