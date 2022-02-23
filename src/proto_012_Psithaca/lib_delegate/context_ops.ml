(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* Backend-agnostic operations on the context *)

let mem (context : Environment_context.Context.t) key =
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} -> Context.mem ctxt key
  | Context {kind = Memory_context.Context; ctxt; _} ->
      Tezos_context_memory.Context.mem ctxt key
  | Context t ->
      Environment_context.err_implementation_mismatch
        ~expected:"shell or memory"
        ~got:t.impl_name

let get_protocol (context : Environment_context.Context.t) =
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} -> Context.get_protocol ctxt
  | Context {kind = Memory_context.Context; ctxt; _} ->
      Tezos_context_memory.Context.get_protocol ctxt
  | Context t ->
      Environment_context.err_implementation_mismatch
        ~expected:"shell or memory"
        ~got:t.impl_name

let add_predecessor_block_metadata_hash
    (context : Environment_context.Context.t) hash =
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      Context.add_predecessor_block_metadata_hash ctxt hash
      >|= Shell_context.wrap_disk_context
  | Context {kind = Memory_context.Context; ctxt; _} ->
      Tezos_context_memory.Context.add_predecessor_block_metadata_hash ctxt hash
      >|= Memory_context.wrap_memory_context
  | Context t ->
      Environment_context.err_implementation_mismatch
        ~expected:"shell or memory"
        ~got:t.impl_name

let add_predecessor_ops_metadata_hash (context : Environment_context.Context.t)
    hash =
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      Context.add_predecessor_ops_metadata_hash ctxt hash
      >|= Shell_context.wrap_disk_context
  | Context {kind = Memory_context.Context; ctxt; _} ->
      Tezos_context_memory.Context.add_predecessor_ops_metadata_hash ctxt hash
      >|= Memory_context.wrap_memory_context
  | Context t ->
      Environment_context.err_implementation_mismatch
        ~expected:"shell or memory"
        ~got:t.impl_name

let hash ~time ?message (context : Environment_context.Context.t) =
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      Context.hash ~time ?message ctxt
  | Context {kind = Memory_context.Context; ctxt; _} ->
      Tezos_context_memory.Context.hash ~time ?message ctxt
  | Context t ->
      Environment_context.err_implementation_mismatch
        ~expected:"shell or memory"
        ~got:t.impl_name

let get_test_chain (context : Environment_context.Context.t) =
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      Context.get_test_chain ctxt
  | Context {kind = Memory_context.Context; _} ->
      Lwt.return Test_chain_status.Not_running
  | Context t ->
      Environment_context.err_implementation_mismatch
        ~expected:"shell or memory"
        ~got:t.impl_name

let add_test_chain (context : Environment_context.Context.t) status =
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      Context.add_test_chain ctxt status >|= Shell_context.wrap_disk_context
  | Context {kind = Memory_context.Context; ctxt; _} ->
      Tezos_context_memory.Context.add_test_chain ctxt status
      >|= Memory_context.wrap_memory_context
  | Context t ->
      Environment_context.err_implementation_mismatch
        ~expected:"shell or memory"
        ~got:t.impl_name
