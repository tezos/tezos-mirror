(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** This module defines a sink for {!Internal_event} that sends events to an
    OpenTelemetry collector. The sink is configured using a URI, which allows
    specifying filtering rules based on event levels and sections. *)

module Sink : Internal_event.SINK = struct
  (** The sink's internal state, which primarily consists of a filter to
      determine which events to handle. *)
  type t = {
    filter :
      [ `Level_at_least of Internal_event.Level.t
      | `Per_section_prefix of
        (Internal_event.Section.t * Internal_event.Level.t option) list ];
  }

  let uri_scheme = "otel"

  let fail_parsing uri fmt =
    Format.kasprintf
      (failwith "Parsing Otel event sink URI: %s: %s" (Uri.to_string uri))
      fmt

  (** [configure uri] parses a URI to configure the sink. The URI can specify
      filtering rules. Two types of filtering are supported:
      - [level-at-least=LEVEL]: Only events with a level at or above [LEVEL]
        are handled.
      - [section-prefix=SECTION:LEVEL,...]: A comma-separated list of rules
        that specify the minimum level for events in sections with a given
        prefix. If [LEVEL] is omitted, events from that section are disabled. *)
  let configure uri =
    let open Lwt_result_syntax in
    let section_prefixes =
      let all =
        List.filter_map
          (function "section-prefix", l -> Some l | _ -> None)
          (Uri.query uri)
      in
      match all with [] -> None | more -> Some (List.concat more)
    in
    let* filter =
      match (Uri.get_query_param uri "level-at-least", section_prefixes) with
      | None, None -> return (`Level_at_least Internal_event.Level.default)
      | Some l, None -> (
          match Internal_event.Level.of_string_exn l with
          | Some l -> return (`Level_at_least l)
          | None | (exception Internal_event.Level.Not_a_level _) ->
              fail_parsing uri "Wrong level: %S" l)
      | base_level, Some l -> (
          try
            let sections =
              let parse_section s =
                match String.split_on_char ':' s with
                | [one] ->
                    ( Internal_event.Section.make_sanitized
                        (String.split_on_char '.' one),
                      Some Internal_event.Level.default )
                | [one; two] ->
                    let lvl =
                      match
                        Internal_event.Level.of_string_exn
                          (String.lowercase_ascii two)
                      with
                      | level -> level
                      | exception Internal_event.Level.Not_a_level _ ->
                          Format.kasprintf
                            Stdlib.failwith
                            "Wrong level name: %S in argument %S"
                            two
                            s
                    in
                    let section =
                      match one with
                      | "" -> Internal_event.Section.empty
                      | _ ->
                          Internal_event.Section.make_sanitized
                            (String.split_on_char '.' one)
                    in
                    (section, lvl)
                | _ ->
                    Format.kasprintf
                      Stdlib.failwith
                      "Wrong section-level entry: %S"
                      s
              in
              let pairs = List.map parse_section l in
              match base_level with
              | None -> pairs
              | Some lvl -> (
                  match Internal_event.Level.of_string_exn lvl with
                  | level ->
                      (* establish default for all sections *)
                      pairs @ [(Internal_event.Section.empty, level)]
                  | exception Internal_event.Level.Not_a_level _ ->
                      Format.kasprintf
                        Stdlib.failwith
                        "Wrong level name %S in level-at-least argument"
                        lvl)
            in
            return (`Per_section_prefix sections)
          with Failure s -> fail_parsing uri "%s" s)
    in
    return {filter}

  (** [should_handle ~section sink event_definition] checks if an event should be
      handled by the sink based on the configured filters. It compares the
      event's level and section against the rules defined in the sink's
      state. *)
  let should_handle (type a) ?(section = Internal_event.Section.empty)
      {filter; _} m =
    let module M = (val m : Internal_event.EVENT_DEFINITION with type t = a) in
    match filter with
    | `Level_at_least level_at_least ->
        Internal_event.Level.compare M.level level_at_least >= 0
    | `Per_section_prefix kvl -> (
        match
          List.find
            (fun (prefix, _) ->
              Internal_event.Section.is_prefix
                ~prefix
                (Internal_event.Section.append section M.simple_name))
            kvl
        with
        | None ->
            (* default *)
            Internal_event.Level.compare M.level Internal_event.Level.default
            >= 0
        | Some (_, None) -> (* exclude list *) false
        | Some (_, Some lvl) -> Internal_event.Level.compare M.level lvl >= 0)

  (** [json_to_value json] recursively converts an Ezjsonm.value to an
      OpenTelemetry `any_value`. This is used to structure the event data in a
      format that OpenTelemetry can understand. *)
  let rec json_to_value (j : Ezjsonm.value) :
      Opentelemetry.Proto.Common.any_value option =
    match j with
    | `Null -> None
    | `Bool b -> Some (Bool_value b)
    | `Float f ->
        let i = Int64.of_float f in
        Some (if Int64.to_float i = f then Int_value i else Double_value f)
    | `String s -> Some (String_value s)
    | `O kv ->
        Some
          (Kvlist_value
             {
               values =
                 List.map
                   (fun (key, j) ->
                     Opentelemetry.Proto.Common.{key; value = json_to_value j})
                   kv;
             })
    | `A l -> Some (Array_value {values = List.filter_map json_to_value l})

  (** [handle sink event_definition ~section event] is the core function of the
      sink. It processes an event by converting it into an OpenTelemetry log
      record and emitting it. The log record includes metadata such as
      timestamp, severity, trace and span IDs, and the event data as a structured
      body and attributes. *)
  let handle (type a) _ m ?(section = Internal_event.Section.empty) (event : a)
      =
    let open Lwt_result_syntax in
    let module M = (val m : Internal_event.EVENT_DEFINITION with type t = a) in
    let time = Opentelemetry.Timestamp_ns.now_unix_ns () in
    let severity =
      match M.level with
      | Internal_event.Debug -> Opentelemetry.Logs.Severity_number_trace
      | Info -> Severity_number_debug
      | Notice -> Severity_number_info
      | Warning -> Severity_number_warn
      | Error -> Severity_number_error
      | Fatal -> Severity_number_fatal
    in
    let log_level = Internal_event.Level.to_string M.level in
    let trace_id, span_id =
      match Opentelemetry.Scope.get_ambient_scope () with
      | None -> (None, None)
      | Some {trace_id; span_id; _} -> (Some trace_id, Some span_id)
    in
    let log =
      Format.asprintf "%a" (M.pp ~all_fields:false ~block:false) event
    in
    let v_event =
      let (`O [(_, json)] | json) =
        Data_encoding.Json.construct M.encoding event
      in
      json_to_value json
    in
    let section_str =
      Internal_event.Section.to_string_list section |> String.concat "."
    in
    let attributes : Opentelemetry_proto.Common.key_value list =
      [
        {key = "section"; value = Some (String_value section_str)};
        {key = "name"; value = Some (String_value M.name)};
        {key = "event"; value = v_event};
      ]
    in

    let log =
      Opentelemetry.Proto.Logs.default_log_record
        ~time_unix_nano:time
        ~observed_time_unix_nano:time
        ~severity_number:severity
        ~severity_text:log_level
        ?trace_id:(Option.map Opentelemetry.Trace_id.to_bytes trace_id)
        ?span_id:(Option.map Opentelemetry.Span_id.to_bytes span_id)
        ~body:(Some (String_value log))
        ~attributes
        ()
    in
    Opentelemetry.Logs.emit [log] ;
    return_unit

  let close (_ : t) : unit tzresult Lwt.t = Lwt_result_syntax.return_unit
end

let sink : Sink.t Internal_event.sink_definition =
  (module Sink : Internal_event.SINK with type t = Sink.t)

let () = Internal_event.All_sinks.register sink

(** [activate ?level ?sections ()] activates the OpenTelemetry sink.
    This is a convenience function that constructs the appropriate URI and
    activates the sink.
    - [level]: The minimum level of events to log. Defaults to [Info].
    - [sections]: A list of [(section, level)] pairs for more granular
      filtering. *)
let activate ?(level = Internal_event.Info) ?sections () =
  let query =
    ("level-at-least", [Internal_event.Level.to_string level])
    ::
    (match sections with
    | None -> []
    | Some sections ->
        [
          ( "section-prefix",
            List.map
              (fun (section, level) ->
                Format.asprintf
                  "%a:%s"
                  Internal_event.Section.pp
                  section
                  (Internal_event.Level.to_string level))
              sections );
        ])
  in
  let uri = Uri.make ~scheme:Sink.uri_scheme ~query () in
  Internal_event.All_sinks.activate uri
