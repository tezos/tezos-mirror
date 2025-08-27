(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type kernel_input = [`Inbox of string trace | `Skip_stage_one]

(** [run ?l1_timestamp ~preimages_dir ?preimages_endpoint
    ~native_execution_policy ~entrypoint tree rollup_address
    blueprint] calls the WASM runtime over [tree], and computing the
    next [tree].

    When [l1_timestamp] is provided it's used in [ipl], default value
    is `epoch`.
*)
val run :
  pool:Lwt_domain.pool ->
  ?trace_host_funs:bool ->
  ?l1_timestamp:Time.Protocol.t ->
  preimages_dir:string ->
  ?preimages_endpoint:Uri.t ->
  native_execution:bool ->
  entrypoint:string ->
  Irmin_context.tree ->
  Address.t ->
  kernel_input ->
  Irmin_context.tree Lwt.t

val preload_kernel : pool:Lwt_domain.pool -> Irmin_context.tree -> bool Lwt.t
