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

open Tezos_webassembly_interpreter
open Tezos_wasmer
open Tezos_scoru_wasm

(** Host state environment for Fast Execution.

    [durable] is persistent, so mutating host functions write the new
    value back into the field.  [nds] is the NDS handle of a [Dual]
    storage, mutated in place by the [nds_*] host functions; it is
    [None] on [Durable_only] storage. *)
type host_state = {
  retrieve_mem : unit -> Memory.t;
  buffers : Eval.buffers;
  mutable durable : Durable.t;
  nds : Octez_riscv_nds_common.Nds.t option;
}

(** [make ~version ~nds_host_functions_enabled ~reveal_builtins
    ~write_debug host_state] generates the list of host functions a
    WebAssembly module may import, for the PVM [version].
    [nds_host_functions_enabled] is [true] when NDS is activated on
    chain. When NDS is not activated on chain, every [nds_*] function
    answers [Host_funcs.nds_disabled_code] and touches neither the memory
    nor the NDS, as on the interpreter. A negative key length raises
    [Invalid_argument] instead. *)
val make :
  version:Wasm_pvm_state.version ->
  nds_host_functions_enabled:bool ->
  reveal_builtins:Builtins.reveals ->
  write_debug:Builtins.write_debug ->
  host_state ->
  (string * string * extern) list
