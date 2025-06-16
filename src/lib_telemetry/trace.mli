(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** A convenient wrapper for {!Opentelemetry_lwt.Trace.with_} dealing with the
    error monad. *)
val with_tzresult :
  ?message_on_success:('a -> string) ->
  ?force_new_trace_id:bool ->
  ?trace_state:string ->
  ?service_name:string ->
  ?attrs:(string * Opentelemetry.value) trace ->
  ?kind:Opentelemetry_lwt.Span.kind ->
  ?trace_id:Opentelemetry.Trace_id.t ->
  ?parent:Opentelemetry.Span_id.t ->
  ?scope:Opentelemetry.Scope.t ->
  ?links:Opentelemetry_lwt.Span_link.t trace ->
  string ->
  (Opentelemetry.Scope.t -> 'a tzresult Lwt.t) ->
  'a tzresult Lwt.t
