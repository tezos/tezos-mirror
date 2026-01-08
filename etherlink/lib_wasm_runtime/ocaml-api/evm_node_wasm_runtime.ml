(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let () = Wasm_runtime_callbacks.register ()

include Wasm_runtime_gen

let wasm_runtime_run ~scope ~trace_host_funs ~context ~preimages_dir
    ?preimages_endpoint ~native_execution ~entrypoint ~tree ~rollup_address
    ~level inputs =
  wasm_runtime_run
    context
    preimages_dir
    preimages_endpoint
    native_execution
    entrypoint
    scope
    trace_host_funs
    tree
    rollup_address
    level
    inputs
