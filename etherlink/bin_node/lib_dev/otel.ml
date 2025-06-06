(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Opentelemetry

let initialize_telemetry ~service_name config =
  Globals.service_name := service_name ;
  Opentelemetry_client_cohttp_lwt.setup ~config ()
