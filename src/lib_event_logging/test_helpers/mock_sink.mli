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

(** A mock event sink that records handled events for verifying test
   expectations. *)

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

val testable_event : event Alcotest.testable

val testable_level : Internal_event.Level.t Alcotest.testable

val testable_section : Internal_event.Section.t Alcotest.testable
