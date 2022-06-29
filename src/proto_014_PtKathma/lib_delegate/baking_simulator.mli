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
  state : validation_state;
  rev_operations : Operation.packed list;
  header : Tezos_base.Block_header.shell_header;
}

val load_context :
  context_path:string -> Abstract_context_index.t tzresult Lwt.t

(** Make sure that the given context is consistent by trying to read in it *)
val check_context_consistency :
  Abstract_context_index.t -> Context_hash.t -> unit tzresult Lwt.t

val begin_construction :
  timestamp:Time.Protocol.t ->
  ?protocol_data:block_header_data ->
  Abstract_context_index.t ->
  Baking_state.block_info ->
  Chain_id.t ->
  incremental tzresult Lwt.t

val add_operation :
  incremental ->
  Operation.packed ->
  (incremental * operation_receipt) tzresult Lwt.t

val finalize_construction :
  incremental ->
  (Tezos_protocol_environment.validation_result * block_header_metadata)
  tzresult
  Lwt.t
