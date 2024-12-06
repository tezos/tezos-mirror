(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let () = Wasm_runtime_callbacks.register ()

include Wasm_runtime_gen

let wasm_runtime_run ~preimages_dir ?preimages_endpoint ~native_execution
    ~entrypoint ctxt =
  wasm_runtime_run
    ctxt
    preimages_dir
    preimages_endpoint
    native_execution
    entrypoint
