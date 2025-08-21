(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [run ?l1_timestamp ~preimages_dir ?preimages_endpoint
    ~native_execution_policy ~entrypoint tree rollup_address
    blueprint] calls the WASM runtime over [tree], and computing the
    next [tree].

    When [l1_timestamp] is provided it's used in [ipl], default value
    is `epoch`.
*)
val run :
  ?l1_timestamp:Time.Protocol.t ->
  preimages_dir:string ->
  ?preimages_endpoint:Uri.t ->
  native_execution:bool ->
  entrypoint:string ->
  Irmin_context.tree ->
  Address.t ->
  string list ->
  Irmin_context.tree Lwt.t

val preload_kernel : Irmin_context.tree -> bool Lwt.t
