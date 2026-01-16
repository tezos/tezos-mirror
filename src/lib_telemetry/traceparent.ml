(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t = Opentelemetry.Trace_id.t * Opentelemetry.Span_id.t

type 'a instrumented = {origin : t option; data : 'a}

let from_scope ({trace_id; span_id; _} : Opentelemetry.Scope.t) =
  (trace_id, span_id)

let to_string (trace_id, span_id) =
  Opentelemetry.Trace_context.Traceparent.to_value
    ~trace_id
    ~parent_id:span_id
    ()

let from_string = Opentelemetry.Trace_context.Traceparent.of_value

let encoding = Data_encoding.(conv_with_guard to_string from_string string)

let current () =
  match Opentelemetry.Scope.get_ambient_scope () with
  | Some scope -> Some (from_scope scope)
  | None -> None

let instrument ?origin data =
  {data; origin = Option.either (Option.map from_scope origin) (current ())}

let propagate ?service_name ?attrs ?kind ?links ~span_name {data; origin} k =
  let trace_id, parent =
    match origin with
    | Some (tid, sid) -> (Some tid, Some sid)
    | None -> (None, None)
  in
  Opentelemetry_lwt.Trace.with_
    ?service_name
    ?trace_id
    ?parent
    ?attrs:(Option.map (fun f -> f data) attrs)
    ?kind
    ?links
    (span_name data)
  @@ fun scope -> k scope data
