(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** A table to store tracking information per domain ID. *)
module DomainTable = Hashtbl.Make (Int)

(** A table to store ongoing events per runtime phase. *)
module EventsTable = Hashtbl.Make (struct
  type t = Runtime_events.runtime_phase

  let equal = ( = )

  let hash = Hashtbl.hash
end)

(** State for tracking GC events and telemetry scopes for a single domain. *)
type tracking = {
  events :
    (Runtime_events.Timestamp.t
    * Opentelemetry.Scope.t
    * Opentelemetry.Span_id.t option)
    EventsTable.t;
      (** A table to keep track of ongoing runtime events. *)
  mutable scopes : (Opentelemetry.Scope.t * Opentelemetry.Span_id.t option) list;
      (** A stack of active scopes, used to manage parent-child relationships in
          spans. *)
}

(** The main state for the GC telemetry module. *)
type t = {
  domains : tracking DomainTable.t;
      (** A table to store tracking information for each domain. *)
  filter : Runtime_events.runtime_phase -> bool;
      (** A function to filter which runtime events to trace. *)
  min_duration : int64 option;
      (** Minimal duration in nanoseconds for the event to be emitted. Events
          shorter than this duration will be filtered out. *)
  offset : int64;
      (** The offset between the runtime events' monotonic clock and the
          system's monotonic clock.  *)
}

(** Retrieves or creates a [tracking] record for a given domain ID. *)
let get_domain_tracking t id =
  match DomainTable.find_opt t.domains id with
  | Some tr -> tr
  | None ->
      let tr = {events = EventsTable.create 77; scopes = []} in
      DomainTable.replace t.domains id tr ;
      tr

(** A callback function that is called when a runtime event begins. It creates a
    new OpenTelemetry span and scope for the event. *)
let runtime_begin t domain_id timestamp phase =
  if t.filter phase then (
    let tr = get_domain_tracking t domain_id in
    (* Determine the trace ID and parent span ID from the scopes stack. *)
    let trace_id, parent =
      match tr.scopes with
      | [] -> (Opentelemetry.Trace_id.create (), None)
      | (Opentelemetry.Scope.{trace_id; span_id; _}, _) :: _ ->
          (trace_id, Some span_id)
    in
    let span_id = Opentelemetry.Span_id.create () in
    let scope = Opentelemetry.Scope.make ~trace_id ~span_id () in
    (* Store the event's start time, scope, and parent span. *)
    EventsTable.add tr.events phase (timestamp, scope, parent) ;
    (* Add the new scope on top of active scopes. *)
    tr.scopes <- (scope, parent) :: tr.scopes)

(** Removes a scope from the list of active scopes when its corresponding event
    ends. *)
let close_scope del_span_id tr =
  let rec remove acc = function
    | [] -> List.rev acc
    | ((Opentelemetry.Scope.{span_id; _}, _) as s) :: scopes ->
        if span_id = del_span_id then scopes
        else (remove [@tailcall]) (s :: acc) scopes
  in
  tr.scopes <- remove [] tr.scopes

(** Checks if the duration of an event is greater than the configured minimum
    duration. *)
let duration_filter_ok t ~start_time_ns ~end_time_ns =
  match t.min_duration with
  | None -> true
  | Some min ->
      let duration_ns = Int64.sub end_time_ns start_time_ns in
      duration_ns >= min

(** A callback function that is called when a runtime event ends. It closes the
    corresponding OpenTelemetry span and emits it. *)
let runtime_end t domain_id end_time phase =
  if t.filter phase then
    match DomainTable.find_opt t.domains domain_id with
    | None -> ()
    | Some tr -> (
        match EventsTable.find_opt tr.events phase with
        | None -> ()
        | Some (start_time, scope, parent) ->
            (* Remove the event from the tracking table. *)
            EventsTable.remove tr.events phase ;
            (* Close the corresponding scope. *)
            close_scope scope.span_id tr ;
            (* Convert timestamps to nanoseconds and apply the offset. *)
            let start_time_ns = Runtime_events.Timestamp.to_int64 start_time in
            let end_time_ns = Runtime_events.Timestamp.to_int64 end_time in
            (* Only emit if passes min duration filter. *)
            if duration_filter_ok t ~start_time_ns ~end_time_ns then (
              let end_time = Int64.add end_time_ns t.offset in
              let start_time = Int64.add start_time_ns t.offset in
              (* Create and emit the span with domain ID as an attribute. *)
              let span, _ =
                Opentelemetry.Span.create
                  ?parent
                  ~id:scope.span_id
                  ~trace_id:scope.trace_id
                  ~start_time
                  ~end_time
                  ~attrs:[("ocaml.domain_id", `Int domain_id)]
                  (Runtime_events.runtime_phase_name phase)
              in
              Opentelemetry.Trace.emit ~service_name:"Gc" [span] ;
              ()))

(** A custom user event tag to mark the startup time. *)
type Runtime_events.User.tag += Startup

(** Computes the offset between the monotonic clock of [Runtime_events] and the
    system's monotonic clock used by OpenTelemetry. *)
let compute_offset cursor =
  (* TODO: use Runtime_events.Timestamp.get_current when switching to Ocaml
     5.4 *)
  let startup_timestamp = ref (-1L) in
  (* Register a custom event to capture the startup timestamp. *)
  let startup_event =
    Runtime_events.User.register "startup" Startup Runtime_events.Type.unit
  in
  let callbacks =
    Runtime_events.Callbacks.add_user_event
      Runtime_events.Type.unit
      (fun _domain_id timestamp _event () ->
        startup_timestamp := Runtime_events.Timestamp.to_int64 timestamp)
      (Runtime_events.Callbacks.create ())
  in
  (* Write the startup event to get the current runtime events timestamp. *)
  Runtime_events.User.write startup_event () ;
  let now = Opentelemetry.Timestamp_ns.now_unix_ns () in
  (* Read the event to populate [startup_timestamp]. *)
  let (_nb : int) = Runtime_events.read_poll cursor callbacks None in
  assert (!startup_timestamp <> -1L) ;
  (* Calculate the offset. *)
  Int64.sub now !startup_timestamp

(** A default filter for runtime events. This filter is used to exclude events
    that are not interesting for GC analysis, such as waiting for a condition
    variable. *)
let runtime_events_filter = function
  | Runtime_events.EV_DOMAIN_CONDITION_WAIT -> false
  | _ -> true

(** Polls for new runtime events and processes them with the given callbacks. *)
let poll cursor callbacks =
  let (_nb : int) = Runtime_events.read_poll cursor callbacks None in
  ()

(** Initializes the GC telemetry collection. This function starts the
    [Runtime_events] listener, computes the timestamp offset, and sets up a
    polling mechanism to collect and emit GC traces.  *)
let instrument ?(filter = runtime_events_filter) ?min_duration_ms () =
  if Opentelemetry.Collector.has_backend () then (
    (* Start listening for runtime events. *)
    Runtime_events.start () ;
    let cursor = Runtime_events.create_cursor None in
    (* Compute the timestamp offset. *)
    let offset = compute_offset cursor in
    (* Convert min_duration_ms to nanoseconds. *)
    let min_duration =
      Option.map (fun f -> f *. 1_000_000. |> Int64.of_float) min_duration_ms
    in
    let t =
      {
        (* Create domain table with a size hint based on recommended domain
           count. *)
        domains = DomainTable.create (Domain.recommended_domain_count ());
        filter;
        min_duration;
        offset;
      }
    in
    (* Register the callbacks for runtime events. *)
    let callbacks =
      Runtime_events.Callbacks.create
        ~runtime_begin:(runtime_begin t)
        ~runtime_end:(runtime_end t)
        ()
    in
    (* Make the Opentelemetry collector call the poll function (in addition to
       other tick callbacks) on every tick. *)
    Opentelemetry.Collector.on_tick (fun () -> poll cursor callbacks))
