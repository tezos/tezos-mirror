(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** A traceparent describes the position of the incoming request in its trace
    graph in a portable, fixed-length format.

    See https://www.w3.org/TR/trace-context/#design-overview *)
type t

(** A piece of data with an attached origin from distributed trace graph. *)
type 'a instrumented = {origin : t option; data : 'a}

val encoding : t Data_encoding.encoding

val to_string : t -> string

val from_string : string -> (t, string) result

(** [from_scope scope] constructs a traceparent that can be used to link a new
    span to [scope]. *)
val from_scope : Opentelemetry.Scope.t -> t

(** [instrument ?origin x] attach a traceparent to provided origin. If [origin]
    is omitted, the current scope from the ambient context is used. *)
val instrument : ?origin:Opentelemetry.Scope.t -> 'a -> 'a instrumented

(** [current ()] returns the traceparent identifiying the current scope from
    the ambient context. *)
val current : unit -> t option

(** [propagate span_name x k] creates a span for the execution of [k x.data]. *)
val propagate :
  ?service_name:string ->
  ?attrs:('a -> (string * Opentelemetry.value) trace) ->
  ?kind:Opentelemetry_proto.Trace.span_span_kind ->
  ?links:Opentelemetry_proto.Trace.span_link trace ->
  span_name:('a -> string) ->
  'a instrumented ->
  (Opentelemetry.Scope.t -> 'a -> 'b Lwt.t) ->
  'b Lwt.t
