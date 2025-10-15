(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type context

external wasm_runtime_new_context : unit -> context = "wasm_runtime_new_context"

val wasm_runtime_run :
  scope:Wasm_runtime_callbacks.scope ->
  trace_host_funs:bool ->
  context:context ->
  preimages_dir:string ->
  ?preimages_endpoint:string ->
  native_execution:bool ->
  entrypoint:string ->
  tree:Irmin_context.tree ->
  rollup_address:bytes ->
  level:int32 ->
  string list ->
  Irmin_context.tree

(** Returns [false] if the kernel was loaded from the cache. *)
val wasm_runtime_preload_kernel : context -> Irmin_context.tree -> bool

val wasm_runtime_logger_init : unit -> unit
