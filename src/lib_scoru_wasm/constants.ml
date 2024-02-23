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

(* The name by which the module is registered. This can be anything as long
   as we use the same name to lookup from the registry. *)
let wasm_main_module_name = "main"

(* This is the name of the main function of the module. We require the
   kernel to expose a function named [kernel_run]. *)
let wasm_entrypoint = "kernel_run"

(* The name of the WASM module which exports the host functions to the
   WASM kernel. *)
let wasm_host_funcs_virual_module = "smart_rollup_core"

(* TODO: https://gitlab.com/tezos/tezos/-/issues/3157
   Find an appropriate number of reboots per inputs.
*)
let maximum_reboots_per_input = Z.of_int 1_000

(* Flag used in the durable storage by the kernel to ask a reboot from the PVM
   without consuming an input. *)
let reboot_flag_key = Durable.key_of_string_exn "/kernel/env/reboot"

(* The path to where the WASM kernel is stored. *)
let kernel_key = Durable.key_of_string_exn "/kernel/boot.wasm"

(* The path to where the fallback WASM kernel is stored. When in evaluation,
   this corresponds to the currently running kernel.
*)
let kernel_fallback_key = Durable.key_of_string_exn "/readonly/kernel/boot.wasm"

let stuck_flag_key = Durable.key_of_string_exn "/readonly/kernel/env/stuck"

let upgrade_error_flag_key =
  Durable.key_of_string_exn "/readonly/kernel/env/upgrade_error"

let too_many_reboot_flag_key =
  Durable.key_of_string_exn "/readonly/kernel/env/too_many_reboot"

(* The path to where the WASM PVM exposes the remaining reboots a
   kernel can do with a given inbox. *)
let reboot_counter_key =
  Durable.key_of_string_exn "/readonly/kernel/env/reboot_counter"

let version_key = Durable.key_of_string_exn "/readonly/wasm_version"

let stack_size_limit = 300

let oxford_name = "oxford_018"

let paris_name = "paris_019"

let proto_alpha_name = "alpha_current"
