(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Bare_context = struct
  module Tree = Irmin_context.Tree

  type t = Irmin_context.rw

  type index = Irmin_context.rw_index

  type nonrec tree = Irmin_context.tree

  let init ?patch_context:_ ?readonly:_ ?index_log_size:_ path =
    let open Lwt_syntax in
    let* res = Irmin_context.load ~cache_size:100_000 Read_write path in
    match res with
    | Ok res -> return res
    | Error _ -> Lwt.fail_with "could not initialize the context"

  let empty index = Irmin_context.empty index
end

module Ctx = Tezos_tree_encoding.Encodings_util.Make (Bare_context)
module Wasm_utils =
  Wasm_utils.Make_with_pvms (Ctx) (Tezos_scoru_wasm.Wasm_pvm.Make (Ctx.Tree))
    (Tezos_scoru_wasm.Wasm_pvm.Make (Ctx.Tree))

module Wasm =
  Octez_smart_rollup_wasm_debugger_lib.Wasm_debugger.Make (Wasm_utils)

let read_kernel_from_file =
  Octez_smart_rollup_wasm_debugger_lib.Wasm_debugger.read_kernel_from_file

let check_kernel =
  Octez_smart_rollup_wasm_debugger_lib.Wasm_debugger.check_kernel

let profile = Wasm.Commands.profile

let set_durable_value = Wasm.set_durable_value

let start = Wasm.start

let find_key_in_durable = Wasm.Commands.find_key_in_durable

let wrap_as_durable_storage = Wasm_utils.wrap_as_durable_storage

let eval = Wasm.Commands.eval

let encode =
  Ctx.Tree_encoding_runner.encode Tezos_scoru_wasm.Wasm_pvm.pvm_state_encoding

let decode =
  Ctx.Tree_encoding_runner.decode Tezos_scoru_wasm.Wasm_pvm.pvm_state_encoding

let get_wasm_version = Wasm_utils.Wasm.get_wasm_version
