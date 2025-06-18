(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type t = {enable : bool; config : Opentelemetry_client_cohttp_lwt.Config.t}

let default =
  {enable = false; config = Opentelemetry_client_cohttp_lwt.Config.make ()}

let encoding =
  let open Data_encoding in
  let open Opentelemetry_client_cohttp_lwt.Config in
  conv
    (fun {
           enable;
           config =
             {debug; url_traces; headers; batch_traces; batch_timeout_ms; _};
         } ->
      ( enable,
        Some debug,
        Some url_traces,
        Some headers,
        Some batch_traces,
        Some batch_timeout_ms ))
    (fun (enable, debug, url_traces, headers, batch_traces, batch_timeout_ms) ->
      let config =
        Opentelemetry_client_cohttp_lwt.Config.make
          ?debug
          ?url_traces
          ?headers
          ?batch_traces
          ?batch_timeout_ms
          ()
      in
      {enable; config})
  @@ obj6
       (dft "enable" ~description:"Enable opentelemetry profiling" bool false)
       (opt "debug" ~description:"Enable debug mode" bool)
       (opt "url_traces" ~description:"URL to send traces" string)
       (opt
          "headers"
          ~description:"API headers sent to the endpoint"
          (list (tup2 string string)))
       (opt "batch_traces" ~description:"Batch traces" (option int31))
       (opt
          "batch_timeout_ms"
          ~description:
            "Milliseconds after which we emit a batch, even incomplete"
          int31)
