(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Describe where the kernel code can be found: either in-memory from a
    buffer, or on-disk using a given path. *)
type kernel = In_memory of string | On_disk of string

val read_kernel_from_file :
  Lwt_io.file_name -> (string * bool, tztrace) result Lwt.t

val check_kernel :
  binary:bool ->
  name:string ->
  Tezos_scoru_wasm.Wasm_pvm_state.version ->
  string ->
  unit tzresult Lwt.t

val profile :
  ?migrate_to:Tezos_scoru_wasm.Pvm_input_kind.protocol ->
  collapse:bool ->
  with_time:bool ->
  no_reboot:bool ->
  int32 ->
  string trace Seq.t ->
  Config.config ->
  string Custom_section.FuncMap.t ->
  Irmin_context.tree ->
  (Irmin_context.tree * string trace Seq.t * int32) tzresult Lwt.t

val set_durable_value :
  ?edit_readonly:bool ->
  Irmin_context.tree ->
  string ->
  string ->
  Irmin_context.tree Lwt.t

val start :
  tree:Irmin_context.tree ->
  Tezos_scoru_wasm.Wasm_pvm_state.version ->
  kernel ->
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
  Config.config ->
  Commands.eval_step ->
  Irmin_context.tree ->
  (Irmin_context.tree * int64 * string trace Seq.t * int32) tzresult Lwt.t

val encode :
  Tezos_scoru_wasm.Wasm_pvm_state.Internal_state.pvm_state ->
  Irmin_context.tree ->
  Irmin_context.tree Lwt.t

val decode :
  Irmin_context.tree ->
  Tezos_scoru_wasm.Wasm_pvm_state.Internal_state.pvm_state Lwt.t

val get_wasm_version :
  Irmin_context.tree -> Tezos_scoru_wasm.Wasm_pvm_state.version Lwt.t
