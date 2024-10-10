(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [run entrypoint tree rollup_address blueprint] calls the WASM runtime over
    [tree], and computing the next [tree]. *)
val run :
  preimages_dir:string ->
  entrypoint:string ->
  Irmin_context.tree ->
  Address.t ->
  string list ->
  Irmin_context.tree Lwt.t
