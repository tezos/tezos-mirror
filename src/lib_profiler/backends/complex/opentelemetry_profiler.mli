(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** [initialize ?unique_identifier service_name] initializes the Opentelemetry client to output the
    traces. [?unique_identifier] can be used to discriminate a service used
    in multiple binaries (for example multiple `octez-node` in the network). *)
val initialize : ?unique_identifier:string -> string -> unit

(** [op_hash_to_trace_id op_hash] returns a valid [Opentelemetry.Trace_id.t] that can be
    used when tracing. When tracing an operation application, using the
    operation hash as trace id can serve to produce a consistent trace amongst
    multiple nodes to view the operation propagation. *)
val op_hash_to_trace_id :
  Tezos_crypto.Hashed.Operation_hash.t -> Opentelemetry.Trace_id.t

(** [add_event ?attrs name] adds an event to the currently opened scope. If no
    scope has been opened yet (i.e. not called in the continuation of
    [Opentelemetry.Trace.with_]), the function does nothing. *)
val add_event : ?attrs:Opentelemetry.key_value list -> string -> unit

(** [trace id k] is [Opentelemetry.Trace.with_ id (fun _ -> k ())]. It doesn't
    give the scope to the callback, as it is meant to be called by custom
    functions. *)
val trace :
  ?force_new_trace_id:bool ->
  ?trace_state:string ->
  ?service_name:string ->
  ?attrs:(string * Opentelemetry.value) list ->
  ?kind:Opentelemetry_proto.Trace.span_span_kind ->
  ?trace_id:Opentelemetry.Trace_id.t ->
  ?parent:Opentelemetry.Span_id.t ->
  ?scope:Opentelemetry.Scope.t ->
  ?links:Opentelemetry_proto.Trace.span_link list ->
  string ->
  (unit -> 'a) ->
  'a

(** [trace_operation op id k] uses the operation hash as a [trace_id] and calls
    [trace ~trace_id id k]. *)
val trace_operation :
  [< `Hash of Operation_hash.t | `Operation of Operation.t] ->
  ?attrs:(string * Opentelemetry.value) trace ->
  ?force_new_trace_id:bool ->
  ?trace_state:string ->
  ?service_name:string ->
  ?kind:Opentelemetry_proto.Trace.span_span_kind ->
  ?parent:Opentelemetry.Span_id.t ->
  ?scope:Opentelemetry.Scope.t ->
  ?links:Opentelemetry_proto.Trace.span_link list ->
  string ->
  (unit -> 'a) ->
  'a

(** [update_scope s f] updates the Opentelemetry scope in the ambient context
    before calling [f]. *)
val update_scope : Opentelemetry.Scope.t option -> (unit -> 'a) -> 'a

type config = {service_name : string; verbosity : Profiler.verbosity}

(** Mocked driver, that serves only to register the opentelemetry as a valid
    profiler and enable relevant functions with the ppx. *)
module Driver : Profiler.DRIVER with type config = config

val opentelemetry : config Profiler.driver
