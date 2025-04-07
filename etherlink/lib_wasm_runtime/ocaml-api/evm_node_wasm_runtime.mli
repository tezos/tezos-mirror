(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type context

external wasm_runtime_new_context : unit -> context = "wasm_runtime_new_context"

val wasm_runtime_run :
  preimages_dir:string ->
  ?preimages_endpoint:string ->
  native_execution:bool ->
  entrypoint:string ->
  context ->
  Irmin_context.tree ->
  bytes ->
  int32 ->
  string list ->
  Irmin_context.tree

val wasm_runtime_preload_kernel : context -> Irmin_context.tree -> unit

val wasm_runtime_logger_init : unit -> unit
