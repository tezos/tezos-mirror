(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Constructs a configuration for rollup execution. *)
val config :
  ?sender:Tezos_protocol_alpha.Protocol.Contract_hash.t ->
  ?source:Signature.Public_key_hash.t ->
  ?destination:Tezos_protocol_alpha.Protocol.Alpha_context.Sc_rollup.Address.t ->
  ?preimage_directory:string ->
  ?preimage_endpoint:Uri.t ->
  ?dal_pages_directory:string ->
  ?kernel_debug:bool ->
  ?flamecharts_directory:string ->
  ?timings_file:string ->
  ?trace_host_funs:bool ->
  unit ->
  Pvm_types.config

(** [read_kernel kernel] returns a tuple consisting of the kernel code
    [content] and a boolean [is_binary], where [is_binary] is [true] if
    [content] is a WASM blob, and [false] if it is a wat file (WebAssembly text
    format). *)
val read_kernel : Pvm_types.kernel -> (string * bool) tzresult Lwt.t

val check_kernel :
  binary:bool ->
  name:string ->
  Tezos_scoru_wasm.Wasm_pvm_state.version ->
  string ->
  unit tzresult Lwt.t

val set_durable_value :
  ?edit_readonly:bool ->
  Irmin_context.tree ->
  string ->
  string ->
  Irmin_context.tree Lwt.t

val start :
  tree:Irmin_context.tree ->
  Tezos_scoru_wasm.Wasm_pvm_state.version ->
  Pvm_types.kernel ->
  Irmin_context.tree tzresult Lwt.t

val find_key_in_durable :
  Irmin_context.tree ->
  Tezos_scoru_wasm.Durable.key ->
  Tezos_lazy_containers.Chunked_byte_vector.t option Lwt.t

val wrap_as_durable_storage :
  Irmin_context.tree -> Tezos_webassembly_interpreter.Durable_storage.t Lwt.t

val eval :
  ?hooks:Tezos_scoru_wasm.Hooks.t ->
  ?migrate_to:Tezos_scoru_wasm.Pvm_input_kind.protocol ->
  write_debug:Tezos_scoru_wasm.Builtins.write_debug ->
  wasm_entrypoint:string ->
  int32 ->
  string trace Seq.t ->
  Pvm_types.config ->
  Octez_smart_rollup_wasm_debugger_lib.Commands.eval_step ->
  Irmin_context.tree ->
  (Irmin_context.tree * int64 * string trace Seq.t * int32) tzresult Lwt.t

val profile :
  ?migrate_to:Tezos_scoru_wasm.Pvm_input_kind.protocol ->
  ?hooks:Tezos_scoru_wasm.Hooks.t ->
  collapse:bool ->
  with_time:bool ->
  no_reboot:bool ->
  int32 ->
  string trace Seq.t ->
  Pvm_types.config ->
  string Octez_smart_rollup_wasm_debugger_lib.Custom_section.FuncMap.t ->
  Irmin_context.tree ->
  (Irmin_context.tree * string trace Seq.t * int32) tzresult Lwt.t

val encode :
  Tezos_scoru_wasm.Wasm_pvm_state.Internal_state.pvm_state ->
  Irmin_context.tree ->
  Irmin_context.tree Lwt.t

val decode :
  Irmin_context.tree ->
  Tezos_scoru_wasm.Wasm_pvm_state.Internal_state.pvm_state Lwt.t

val get_wasm_version :
  Irmin_context.tree -> Tezos_scoru_wasm.Wasm_pvm_state.version Lwt.t

val get_function_symbols :
  Irmin_context.tree ->
  string Octez_smart_rollup_wasm_debugger_lib.Custom_section.FuncMap.t tzresult
  Lwt.t
