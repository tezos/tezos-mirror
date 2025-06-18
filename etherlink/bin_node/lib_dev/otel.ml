(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Opentelemetry

let initialize_telemetry ~service_name ~enable config =
  Globals.service_name := service_name ;
  Opentelemetry_ambient_context.set_storage_provider
    (Opentelemetry_ambient_context_lwt.storage ()) ;
  Opentelemetry_client_cohttp_lwt.setup ~enable ~config ()
