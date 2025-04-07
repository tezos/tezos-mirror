(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [dump_durable_storage ~block ~data_dir ~file] writes to [file] the current
    state of the WASM PVM from [data_dir], that is the state of the WASM PVM
    for the given [block]. *)
val dump_durable_storage :
  block:Tezos_shell_services.Block_services.block ->
  data_dir:string ->
  file:string ->
  unit tzresult Lwt.t

(** [patch_durable_storage ~data_dir ~key ~value] adds [value] to
    [key] into the durable storage of PVM state. If the value exists
    it's replaced. *)
val patch_durable_storage :
  data_dir:string -> key:string -> value:string -> unit tzresult Lwt.t

(** Hooks to be used for the WASM PVM. *)
val hooks : check_invalid_kernel:bool -> Tezos_scoru_wasm.Hooks.t
