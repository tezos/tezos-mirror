(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** Set Opentelemetry global options and registers the Lwt compatible ambient
    context. *)
val setup :
  data_dir:string ->
  service_namespace:string ->
  service_name:string ->
  Opentelemetry_config.t ->
  unit Lwt.t
