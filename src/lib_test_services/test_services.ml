(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** [test_services] collects Alcotest testable definitions for base OCaml types
   and Tezos specific types. *)

include Alcotest
include Test_services_base

let trace : error trace testable = testable pp_print_error ( = )

let tzresults (type a) (t : a testable) : a tzresult testable = result t trace

let p2p_peer_id : P2p_peer.Id.t Alcotest.testable =
  Alcotest.testable P2p_peer.Id.pp P2p_peer.Id.equal

let json : Data_encoding.json testable = of_pp Data_encoding.Json.pp

(** Transorm a function running in the error monad into an Alcotest, taking
    care of failing the test if the function results is an error.
    Note that the given function must still take care of test assertions. *)
let tztest name speed f =
  Alcotest_lwt.test_case name speed (fun _sw () ->
      f () >>= function
      | Ok () -> Lwt.return_unit
      | Error err ->
          Tezos_stdlib_unix.Internal_event_unix.close () >>= fun () ->
          Format.printf "@\n%a@." pp_print_error err ;
          Lwt.fail Alcotest.Test_error)

module Alcotest_extra = struct
  let check_any ?(msg = "No value in the list satifies the condition.") f l =
    if not (List.exists f l) then Alcotest.fail msg
end

(* A mock event sink that records handled events for verifying test
   expectations. *)
module Mock_sink : sig
  include Internal_event.SINK

  (** A [filter] can be applied when asserting the contents of the sink. This
      restricts assertions to events from a specific section or to those that
      have no section. The value [filter] has the following meaning:
       - [filter = None]: only keep events that have no section.
       - [filter = Some s]: only keep events with section [s].

       Note that the filter is always passed as an optional parameter. It can be
       omited to keep all events.
   *)
  type filter = Internal_event.Section.t option

  type event = {
    level : Internal_event.Level.t;
    section : Internal_event.Section.t option;
    name : string;
    message : string;
    json : Data_encoding.json;
  }

  (** Describes an event pattern to search for in the sink. *)
  module Pattern : sig
    (** An event pattern to search the sink for. Checks for the event's name,
        and optionally also for level and section. Passing None as each of
        these fields results in that field not being checked. *)
    type t = {
      level : Internal_event.level option;
      section : Internal_event.Section.t option option;
      name : string;
    }

    (** Returns true if event matches the pattern; false otherwise. *)
    val match_event : t -> event -> bool

    (** An Alcotest assertion that event matches the pattern. *)
    val assert_event : t -> event -> unit
  end

  type pattern = Pattern.t

  (** [assert_has_event str filter strict e] asserts that Mock_sink has recorded
     an event [e], and fails with the assertion message [str] if not. If
     [filter] is set, only the events whose section is [filter] are
     searched. If [strict] is set, then [e] must be the only event recorded
     (subject to [filter]).  *)
  val assert_has_event :
    string -> ?filter:filter -> ?strict:bool -> pattern -> unit

  (** [assert_has_events str filter strict es] asserts that Mock_sink has
     recorded the list of events [es], and fails with the assertion message
     [str] if not. If [filter] is set, only the events whose section is
     [filter] are searched. If [strict] is set, then [es] must be the only
     events recorded (subject to [filter]).  *)
  val assert_has_events :
    string -> ?filter:filter -> ?strict:bool -> pattern list -> unit

  (** Clears recorded events, typically called in the initialization of each
   test. *)
  val clear_events : unit -> unit

  (** Returns true if this sink has been activated *)
  val is_activated : unit -> bool

  (** Pretty prints the list of recorded events *)
  val pp_state : ?filter:filter -> unit -> unit

  val pp_events_json : Format.formatter -> event list -> unit

  val get_events : ?filter:filter -> unit -> event list

  val testable_event : event testable

  val testable_level : Internal_event.Level.t testable

  val testable_section : Internal_event.Section.t testable
end = struct
  type filter = Internal_event.Section.t option

  type t = unit

  type event = {
    level : Internal_event.Level.t;
    section : Internal_event.Section.t option;
    name : string;
    message : string;
    json : Data_encoding.json;
  }

  let pp_event fmt e =
    Format.fprintf
      fmt
      "Event[%s](%a:%s) %a"
      (Internal_event.Level.to_string e.level)
      Format.(
        pp_print_option
        @@ pp_print_list
             ~pp_sep:(fun fmt () -> pp_print_string fmt ":")
             pp_print_string)
      (Option.map Internal_event.Section.to_string_list e.section)
      e.name
      Data_encoding.Json.pp
      e.json

  module Pattern = struct
    type t = {
      level : Internal_event.level option;
      section : Internal_event.Section.t option option;
      name : string;
    }

    let pp fmt pattern =
      Format.fprintf
        fmt
        "Pattern[%a](%a:%s)"
        Format.(pp_print_option pp_print_string)
        (Option.map Internal_event.Level.to_string pattern.level)
        Format.(
          pp_print_option ~none:(fun fmt () -> pp_print_string fmt "<ANY>")
          @@ pp_print_option
          @@ pp_print_list
               ~pp_sep:(fun fmt () -> pp_print_string fmt ":")
               pp_print_string)
        (Option.map
           (Option.map Internal_event.Section.to_string_list)
           pattern.section)
        pattern.name

    let is_unspecified_or_equal_to pattern value =
      Option.fold ~none:true ~some:(( = ) value) pattern

    let match_event (pattern : t) (event : event) =
      is_unspecified_or_equal_to pattern.level event.level
      && is_unspecified_or_equal_to pattern.section event.section
      && pattern.name = event.name

    let assert_event pattern event =
      let msg =
        Format.asprintf
          "event %a does not match pattern %a"
          pp_event
          event
          pp
          pattern
      in
      check bool msg (match_event pattern event) true
  end

  type pattern = Pattern.t

  let recorded_events : event list ref = ref []

  let activated : bool ref = ref false

  let uri_scheme = "mock-log"

  let configure _ =
    activated := true ;
    return ()

  let is_activated () = !activated

  let close (_ : t) : unit tzresult Lwt.t = return ()

  let handle (type a) (_ : t) m ?section (f : unit -> a) =
    let module M = (val m : Internal_event.EVENT_DEFINITION with type t = a) in
    let ev = f () in
    let event =
      {
        level = M.level ev;
        section;
        name = M.name;
        message = Format.asprintf "%a" (M.pp ~short:false) ev;
        json = Data_encoding.Json.construct M.encoding ev;
      }
    in
    recorded_events := !recorded_events @ [event] ;
    return ()

  (** testing stuff *)

  (** [get_events filter] returns the list of recorded events, in chronological
      order. *)
  let get_events ?filter () : event list =
    match filter with
    | None -> !recorded_events
    | Some f -> List.filter (fun {section; _} -> section = f) !recorded_events

  let clear_events () : unit = recorded_events := []

  (** Pretty prints the list of recorded events *)
  let pp_state ?filter () =
    Format.printf "-- State of Mock_sink --\n" ;
    List.iteri
      (fun i {level = lvl; message = msg; section; _} ->
        let section_pp =
          match section with
          | None -> "[~]"
          | Some section ->
              Format.asprintf "%a" Internal_event.Section.pp section
        in
        let lvl_pp = Internal_event.Level.to_string lvl in
        Format.printf
          "Registered event{%d}:\n   %s (%s): \"%s\"\n"
          i
          section_pp
          lvl_pp
          msg)
      (get_events ?filter ()) ;
    Format.print_flush ()

  let pp_events_json =
    Format.(
      pp_print_list
        ~pp_sep:(fun fmt () -> pp_print_string fmt ";\n")
        (fun fmt {json; _} -> Data_encoding.Json.pp fmt json))

  let testable_level : Internal_event.Level.t testable =
    let level_pp = Fmt.of_to_string Internal_event.Level.to_string in
    let level_eq l1 l2 = 0 == Internal_event.Level.compare l1 l2 in
    testable level_pp level_eq

  let testable_section : Internal_event.Section.t testable =
    of_pp Internal_event.Section.pp

  let testable_event : event testable =
    (module struct
      type t = event

      let equal (e1 : event) (e2 : event) =
        e1.level = e2.level && e1.section = e2.section && e1.name = e2.name
        && e1.json = e2.json

      let pp fmt (event : event) =
        Format.fprintf
          fmt
          "(%s, %a, %a)"
          (Internal_event.Level.to_string event.level)
          Format.(pp_print_list pp_print_string)
          (Option.fold
             ~some:Internal_event.Section.to_string_list
             ~none:[]
             event.section)
          Data_encoding.Json.pp
          event.json
    end : Alcotest.TESTABLE
      with type t = event)

  let assert_has_events msg ?filter ?(strict = true) (pats : Pattern.t list) =
    let events = get_events ?filter () in
    if strict then
      match List.combine_with_leftovers pats events with
      | (pes, None) -> List.iter (fun (p, e) -> Pattern.assert_event p e) pes
      | (_, Some (`Left pats)) ->
          Alcotest.fail ?here:None ?pos:None
          @@ Format.asprintf
               "Missing events in sink: %a"
               (Format.pp_print_list Pattern.pp)
               pats
      | (_, Some (`Right events)) ->
          Alcotest.fail ?here:None ?pos:None
          @@ Format.asprintf
               "Excess events in sink: %a"
               (Format.pp_print_list pp_event)
               events
    else
      List.iter
        (fun pattern ->
          Alcotest_extra.check_any ~msg (Pattern.match_event pattern) events)
        pats

  let assert_has_event msg ?filter ?(strict = true) (pattern : Pattern.t) =
    let log = get_events ?filter () in
    if strict then assert_has_events msg ?filter ~strict [pattern]
    else Alcotest_extra.check_any ~msg (Pattern.match_event pattern) log
end

let mock_sink : Mock_sink.t Internal_event.sink_definition =
  (module Mock_sink : Internal_event.SINK with type t = Mock_sink.t)

(** [with_empty_mock_sink f] executes f after activating or clearing a Mock_sink
    sink.

  Sinks can only be registered and activated once, and not removed thereafter.
 *)
let with_empty_mock_sink (f : unit -> unit Lwt.t) : unit Lwt.t =
  ignore
    (if not (Mock_sink.is_activated ()) then (
     Internal_event.All_sinks.register mock_sink ;
     Internal_event.All_sinks.activate (Uri.of_string "mock-log://")
     >>= function
     | Ok _ -> Lwt.return_unit
     | Error errors ->
         Format.printf
           "Could not initialize mock sink:\n   %a\n"
           pp_print_error
           errors ;
         Format.print_flush () ;
         Lwt.return_unit)
    else (
      Mock_sink.clear_events () ;
      Lwt.return_unit)) ;
  f ()
