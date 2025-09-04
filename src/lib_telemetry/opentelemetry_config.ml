(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type t = {
  enable : bool;
  instance_id : string option;
  environment : string option;
  config : Opentelemetry_client_cohttp_lwt.Config.t;
}

let default =
  {
    enable = false;
    instance_id = None;
    environment = Sys.getenv_opt "DD_ENV";
    config = Opentelemetry_client_cohttp_lwt.Config.make ();
  }

let detailed_encoding =
  let open Data_encoding in
  let open Opentelemetry_client_cohttp_lwt.Config in
  conv
    (fun {
           enable;
           instance_id;
           environment;
           config =
             {
               debug;
               url_traces;
               url_logs;
               headers;
               batch_traces;
               batch_logs;
               batch_timeout_ms;
               _;
             };
         }
       ->
      ( enable,
        instance_id,
        environment,
        Some debug,
        Some url_traces,
        Some url_logs,
        Some headers,
        Some batch_traces,
        Some batch_logs,
        Some batch_timeout_ms ))
    (fun ( enable,
           instance_id,
           environment,
           debug,
           url_traces,
           url_logs,
           headers,
           batch_traces,
           batch_logs,
           batch_timeout_ms )
       ->
      let config =
        Opentelemetry_client_cohttp_lwt.Config.make
          ?debug
          ?url_traces
          ?url_logs
          ?headers
          ?batch_traces
          ?batch_logs
          ?batch_timeout_ms
          ()
      in
      {enable; instance_id; environment; config})
  @@ obj10
       (dft "enable" ~description:"Enable opentelemetry profiling" bool true)
       (opt
          "instance_id"
          ~description:
            "Instance id to identify the node in Opentelemetry traces. Takes \
             precedence over <data_dir>/telemetry_id."
          string)
       (dft
          "environment"
          ~description:
            "Deployment environment, used for Datadog tagging, will be \
             extracted from env variable DD_ENV if null. See \
             https://docs.datadoghq.com/getting_started/tagging/unified_service_tagging."
          (option string)
          default.environment)
       (opt "debug" ~description:"Enable debug mode" bool)
       (opt "url_traces" ~description:"URL to send traces" string)
       (opt "url_logs" ~description:"URL to send logs" string)
       (opt
          "headers"
          ~description:"API headers sent to the endpoint"
          (list (tup2 string string)))
       (opt "batch_traces" ~description:"Batch traces" (option int31))
       (opt "batch_logs" ~description:"Batch logs" (option int31))
       (opt
          "batch_timeout_ms"
          ~description:
            "Milliseconds after which we emit a batch, even incomplete"
          int31)

let encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"detailed_opentelemetry_config"
        (Tag 0)
        detailed_encoding
        Option.some
        Fun.id;
      case
        ~title:"opentelemetry_boolean"
        (Tag 1)
        bool
        (Fun.const None)
        (fun enable -> {default with enable});
      case
        ~title:"opentelemetry_null"
        (Tag 2)
        null
        (Fun.const None)
        (Fun.const default);
    ]
