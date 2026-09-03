(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

open Tezos_scoru_wasm
open Tezos_webassembly_interpreter

(** [preload_kernel ~hooks durable] can be used to parse and load in the module
    cache the kernel from [durable], as a way to speed-up further executions
    involving this kernel. *)
val preload_kernel : hooks:Hooks.t -> Durable.t -> unit Lwt.t

(** [compute ~nds ~nds_host_functions_enabled durable buffers] executes
    [wasm_entrypoint] once. Defaults to {!Constants.wasm_entrypoint} when
    omitted.  [nds] is the NDS handle of a [Dual] storage ([None] on
    [Durable_only]); the [nds_*] host functions mutate it in place, so
    only the new [durable] is returned.  [nds_host_functions_enabled] is
    [true] when NDS is activated on chain at the level of this
    [kernel_run]. *)
val compute :
  ?wasm_entrypoint:string ->
  hooks:Hooks.t ->
  version:Wasm_pvm_state.version ->
  nds:Octez_riscv_nds_common.Nds.t option ->
  nds_host_functions_enabled:bool ->
  reveal_builtins:Builtins.reveals ->
  write_debug:Builtins.write_debug ->
  Durable.t ->
  Eval.buffers ->
  Durable.t Lwt.t
