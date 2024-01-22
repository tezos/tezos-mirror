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
