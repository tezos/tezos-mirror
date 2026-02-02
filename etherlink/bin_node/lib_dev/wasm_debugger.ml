(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Pvm_types

let config = Octez_smart_rollup_wasm_debugger_lib.Config.config

module Bare_context = struct
  module Tree = Irmin_context.Tree

  type t = Irmin_context.rw

  type index = Irmin_context.rw_index

  type nonrec tree = Irmin_context.tree

  let init ?patch_context:_ ?readonly:_ ?index_log_size:_ path =
    let open Lwt_syntax in
    let* res =
      Irmin_context.load ~cache_size:100_000 ~async_domain:true Read_write path
    in
    match res with
    | Ok res -> return res
    | Error _ -> Lwt.fail_with "could not initialize the context"

  let empty index = Irmin_context.empty index
end

(* We mostly need the utils, execution is last resort: we normally use the
   runtime rather than the PVM for execution.

   The default [Wasm_utils] implementation has an explicit dependency on Wasmer
   because it relies on the Fast Exec backend to speed-up execution. In our
   case, this speed-up is implemented by the WASM Runtime.

   To avoid introducing a dependency to Wasmer, we redefine a `Wasm_utils`
   module which uses the (slow) PVM both for fast and slow execution. *)
module Ctx = Tezos_tree_encoding.Encodings_util.Make (Bare_context)
module Slow_pvm = Tezos_scoru_wasm.Wasm_pvm.Make_machine (Ctx.Tree)
module Wasm_utils = Wasm_utils_functor.Make (Ctx) (Slow_pvm) (Slow_pvm)

module Wasm =
  Octez_smart_rollup_wasm_debugger_lib.Wasm_debugger.Make (Wasm_utils)

let read_kernel = function
  | On_disk path ->
      Octez_smart_rollup_wasm_debugger_lib.Wasm_debugger.read_kernel_from_file
        path
  | In_memory binary -> Lwt_result.return (binary, true)

let check_kernel =
  Octez_smart_rollup_wasm_debugger_lib.Wasm_debugger.check_kernel

let get_function_symbols = Wasm.Commands.get_function_symbols

let set_durable_value = Wasm.set_durable_value

let start ~state version = function
  | On_disk kernel -> Wasm.start ~state version kernel
  | In_memory kernel ->
      Wasm.handle_module ~state version true "installer.wasm" kernel

let find_key_in_durable = Wasm.Commands.find_key_in_durable

let wrap_as_durable_storage = Wasm_utils.wrap_as_durable_storage

let eval = Wasm.Commands.eval

let profile = Wasm.Commands.profile

let encode =
  Ctx.Tree_encoding_runner.encode Tezos_scoru_wasm.Wasm_pvm.pvm_state_encoding

let decode =
  Ctx.Tree_encoding_runner.decode Tezos_scoru_wasm.Wasm_pvm.pvm_state_encoding

let get_wasm_version = Wasm_utils.Wasm.get_wasm_version
