(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(**
 This module is responsible for collecting and reporting garbage collection (GC)
 traces using the [Runtime_events] library and OpenTelemetry. It tracks the
 duration of different GC phases and emits them as spans.
*)

(** Initializes the GC telemetry collection.

 This function starts the [Runtime_events] listener, which allows capturing
 detailed information about the OCaml garbage collector's activity. It then sets
 up a polling mechanism to periodically collect these events and transform them
 into OpenTelemetry traces.

 The optional [?filter] argument allows for selectively tracing specific GC
 phases. By default, it uses a predefined filter that excludes noisy events like
 [EV_DOMAIN_CONDITION_WAIT].

 The optional [?min_duration_ms] argument allows to only emit spans for GC
 events whose duration is greater than this amount of milliseconds.

 NOTE: Enabling this instrumentation can generate a significant volume of
 trace data. It is recommended to use it for debugging or performance
 analysis purposes rather than in a production environment where it might
 introduce overhead.
*)
val instrument :
  ?filter:(Runtime_events.runtime_phase -> bool) ->
  ?min_duration_ms:float ->
  unit ->
  unit
