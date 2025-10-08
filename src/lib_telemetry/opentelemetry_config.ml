(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type gc_telemetry = {
  enable : bool;
  filter : (Runtime_events.runtime_phase -> bool) option;
  min_duration_ms : float option;
}

type t = {
  enable : bool;
  instance_id : string option;
  environment : string option;
  config : Opentelemetry_client_cohttp_lwt.Config.t;
  gc_telemetry : gc_telemetry;
}

let default =
  {
    enable = false;
    instance_id = None;
    environment = Sys.getenv_opt "DD_ENV";
    config = Opentelemetry_client_cohttp_lwt.Config.make ();
    gc_telemetry = {enable = false; filter = None; min_duration_ms = Some 1.};
  }

let gc_runtime_phases =
  Runtime_events.
    [
      EV_EXPLICIT_GC_SET;
      EV_EXPLICIT_GC_STAT;
      EV_EXPLICIT_GC_MINOR;
      EV_EXPLICIT_GC_MAJOR;
      EV_EXPLICIT_GC_FULL_MAJOR;
      EV_EXPLICIT_GC_COMPACT;
      EV_MAJOR;
      EV_MAJOR_SWEEP;
      EV_MAJOR_MARK_ROOTS;
      EV_MAJOR_MARK;
      EV_MINOR;
      EV_MINOR_LOCAL_ROOTS;
      EV_MINOR_FINALIZED;
      EV_EXPLICIT_GC_MAJOR_SLICE;
      EV_FINALISE_UPDATE_FIRST;
      EV_FINALISE_UPDATE_LAST;
      EV_INTERRUPT_REMOTE;
      EV_MAJOR_EPHE_MARK;
      EV_MAJOR_EPHE_SWEEP;
      EV_MAJOR_FINISH_MARKING;
      EV_MAJOR_GC_CYCLE_DOMAINS;
      EV_MAJOR_GC_PHASE_CHANGE;
      EV_MAJOR_GC_STW;
      EV_MAJOR_MARK_OPPORTUNISTIC;
      EV_MAJOR_SLICE;
      EV_MAJOR_FINISH_CYCLE;
      EV_MINOR_CLEAR;
      EV_MINOR_FINALIZERS_OLDIFY;
      EV_MINOR_GLOBAL_ROOTS;
      EV_MINOR_LEAVE_BARRIER;
      EV_STW_API_BARRIER;
      EV_STW_HANDLER;
      EV_STW_LEADER;
      EV_MAJOR_FINISH_SWEEPING;
      EV_MINOR_FINALIZERS_ADMIN;
      EV_MINOR_REMEMBERED_SET;
      EV_MINOR_REMEMBERED_SET_PROMOTE;
      EV_MINOR_LOCAL_ROOTS_PROMOTE;
      EV_DOMAIN_RESIZE_HEAP_RESERVATION;
      EV_COMPACT;
      EV_COMPACT_EVACUATE;
      EV_COMPACT_FORWARD;
      EV_COMPACT_RELEASE;
    ]

let gc_runtime_phase_encoding =
  Data_encoding.string_enum
  @@ List.map
       (fun e -> (Runtime_events.runtime_phase_name e, e))
       gc_runtime_phases

let gc_events_filter_encoding =
  let open Data_encoding in
  conv
    (fun f -> List.filter f gc_runtime_phases)
    (fun events x -> List.memq x events)
  @@ list gc_runtime_phase_encoding

let gc_telemetry_encoding =
  let open Data_encoding in
  conv
    (fun {enable; filter; min_duration_ms} -> (enable, filter, min_duration_ms))
    (fun (enable, filter, min_duration_ms) -> {enable; filter; min_duration_ms})
  @@ obj3
       (dft "enable" bool default.gc_telemetry.enable)
       (opt
          "filter"
          ~description:"Filter for which GC events to emit traces"
          gc_events_filter_encoding)
       (dft
          "min_duration_ms"
          ~description:"Minimal span duration for a GC event to be emitted"
          (option float)
          default.gc_telemetry.min_duration_ms)

let otel_config_encoding =
  let open Data_encoding in
  let open Opentelemetry_client.Config in
  conv
    (fun {
           debug;
           url_traces;
           url_logs;
           headers;
           batch_traces;
           batch_logs;
           batch_timeout_ms;
           _;
         }
       ->
      ( Some debug,
        Some url_traces,
        Some url_logs,
        Some headers,
        Some batch_traces,
        Some batch_logs,
        Some batch_timeout_ms ))
    (fun ( debug,
           url_traces,
           url_logs,
           headers,
           batch_traces,
           batch_logs,
           batch_timeout_ms )
       ->
      Opentelemetry_client_cohttp_lwt.Config.make
        ?debug
        ?url_traces
        ?url_logs
        ?headers
        ?batch_traces
        ?batch_logs
        ?batch_timeout_ms
        ())
  @@ obj7
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

let detailed_encoding =
  let open Data_encoding in
  conv
    (fun {enable; instance_id; environment; config; gc_telemetry} ->
      ((enable, instance_id, environment), (config, gc_telemetry)))
    (fun ((enable, instance_id, environment), (config, gc_telemetry)) ->
      {enable; instance_id; environment; config; gc_telemetry})
  @@ merge_objs
       (obj3
          (dft "enable" ~description:"Enable opentelemetry profiling" bool true)
          (opt
             "instance_id"
             ~description:
               "Instance id to identify the node in Opentelemetry traces. \
                Takes precedence over <data_dir>/telemetry_id."
             string)
          (dft
             "environment"
             ~description:
               "Deployment environment, used for Datadog tagging, will be \
                extracted from env variable DD_ENV if null. See \
                https://docs.datadoghq.com/getting_started/tagging/unified_service_tagging."
             (option string)
             default.environment))
       (merge_objs
          otel_config_encoding
          (obj1
             (dft
                "gc_telemetry"
                ~description:"Enable traces for GC events (costly)"
                gc_telemetry_encoding
                default.gc_telemetry)))

let extended_encoding extra_encoding default_extra =
  let open Data_encoding in
  union
    [
      case
        ~title:"detailed_opentelemetry_config"
        (Tag 0)
        (merge_objs detailed_encoding extra_encoding)
        Option.some
        Fun.id;
      case
        ~title:"opentelemetry_boolean"
        (Tag 1)
        bool
        (Fun.const None)
        (fun enable -> ({default with enable}, default_extra));
      case
        ~title:"opentelemetry_null"
        (Tag 2)
        null
        (Fun.const None)
        (Fun.const (default, default_extra));
    ]

let encoding =
  let open Data_encoding in
  conv (fun c -> (c, ())) (fun (c, ()) -> c) @@ extended_encoding empty ()

let enable c b = {c with enable = b}
