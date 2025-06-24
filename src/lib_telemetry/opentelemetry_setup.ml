(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Opentelemetry

let setup ~service_name {Opentelemetry_config.enable; config; _} =
  Globals.service_name := service_name ;
  Opentelemetry_ambient_context.set_storage_provider
    (Opentelemetry_ambient_context_lwt.storage ()) ;
  Opentelemetry_client_cohttp_lwt.setup ~enable ~config ()
