(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** This profiling library declares a high-level interface meant to
    be used to instrument code in order to measure the time spent in
    the different parts in such a way to yield a (human-)processable
    report. This module declares a generic interface (driver) that
    will provide an API to the developer to instrument the code. When
    the profiling data is recorded, the abstracted profiler will feed
    it to its "plugged" backend (instance) which will process the
    different profiler's nodes in order to produce the
    reports. Reports may also be combined to interwine different
    components' traces.

    The provided API is intentionally simplistic to simplify its
    usage. The basic usage is to call [record <symbol>] before the
    desired section to profile and [stop ()] when we exit it. Nested
    calls are also supported and, given that the backend supports it,
    will be displayed as a callgraph. The API is also augmented with
    higher-level combinators in order to avoid mismatched [stop]s and
    to support [Lwt] function calls. *)

module StringMap : Map.S with type key = string

type time = {
  wall : float;  (** Wall-clock time: total time elapsed. *)
  cpu : float;  (** CPU time: time elapsed in the CPU. *)
}

type span = Span of time

val zero_time : span

val ( -* ) : time -> time -> time

val ( +* ) : time -> time -> time

(** The level of detail of report sections. The driver can choose to
    use this information to skip or aggregate sections below a given
    level. The driver could also record everything, including the
    level of detail, and let a post processor skip or aggregate at
    display time. *)
type lod = Terse | Detailed | Verbose

(** An aggregate node registers multiple calls to a section and sum
    their occurences and time. It also recursively aggregate its
    sub-aggregation nodes. *)
type aggregated_node = {
  count : int;
  total : span;
  children : aggregated_node StringMap.t;
  node_lod : lod;
}

(** A sequence item registers one section with potential sub-reports
    and registers elapsed-time. *)
type seq_item = {
  start : time;
  duration : span;
  contents : report;
  item_lod : lod;
}

and report = {
  aggregated : aggregated_node StringMap.t;
  recorded : (string * seq_item) list;
}

val report_encoding : report Data_encoding.t

type (_, _) kind = ..

module type DRIVER = sig
  (** Parameters to launch an instance of the driver. *)
  type config

  (** Internal state of an instance of the driver. *)
  type state

  (** A typed kind for downcasting. *)
  val kind : (config, state) kind

  (** Create an instance from a config. *)
  val create : config -> state

  (** Gives the current time in seconds. *)
  val time : state -> time

  (** Open a sequence in the current sequence.
      If currently aggregating (not all aggregation scopes are closed),
      this has the same semantics as {!aggregate} instead. *)
  val record : state -> lod -> string -> unit

  (** Open an aggregation node in the current sequence. *)
  val aggregate : state -> lod -> string -> unit

  (** Close the most recently opened sequence or aggregation scope. *)
  val stop : state -> unit

  (** Record a timestamp in the most recently opened sequence. *)
  val stamp : state -> lod -> string -> unit

  (** Count this event's occurences in the most recent sequence. *)
  val mark : state -> lod -> string list -> unit

  (** Sum the time spent in this event in the most recent sequence. *)
  val span : state -> lod -> span -> string list -> unit

  (** Include a report in the current sequence. *)
  val inc : state -> report -> unit

  (** Consume the last toplevel report, if any. *)
  val report : state -> report option

  (** Flush and/or close output. *)
  val close : state -> unit
end

type 'a driver = (module DRIVER with type config = 'a)

type instance

val instance : 'a driver -> 'a -> instance

val time : instance -> time

val report : instance -> report option

val report_s : instance -> report Lwt.t

val close : instance -> unit

type profiler

val plug : profiler -> instance -> unit

val unplug : profiler -> instance -> unit

val close_and_unplug : profiler -> instance -> unit

val close_and_unplug_all : profiler -> unit

val plugged : profiler -> instance list

val record : profiler -> ?lod:lod -> string -> unit

val aggregate : profiler -> ?lod:lod -> string -> unit

val stop : profiler -> unit

val stamp : profiler -> ?lod:lod -> string -> unit

val mark : profiler -> ?lod:lod -> string list -> unit

val span : profiler -> ?lod:lod -> span -> string list -> unit

val inc : profiler -> report -> unit

val record_f : profiler -> ?lod:lod -> string -> (unit -> 'a) -> 'a

val record_s : profiler -> ?lod:lod -> string -> (unit -> 'a Lwt.t) -> 'a Lwt.t

val aggregate_f : profiler -> ?lod:lod -> string -> (unit -> 'a) -> 'a

val aggregate_s :
  profiler -> ?lod:lod -> string -> (unit -> 'a Lwt.t) -> 'a Lwt.t

val span_f : profiler -> ?lod:lod -> string list -> (unit -> 'a) -> 'a

val span_s :
  profiler -> ?lod:lod -> string list -> (unit -> 'a Lwt.t) -> 'a Lwt.t

val with_new_profiler : 'a driver -> 'a -> (profiler -> 'r) -> 'r * report list

val with_new_profiler_s :
  'a driver -> 'a -> (profiler -> 'r Lwt.t) -> ('r * report list) Lwt.t

val unplugged : unit -> profiler

val main : profiler

module type GLOBAL_PROFILER = sig
  type nonrec lod = lod = Terse | Detailed | Verbose

  val plug : instance -> unit

  val unplug : instance -> unit

  val close_and_unplug : instance -> unit

  val close_and_unplug_all : unit -> unit

  val plugged : unit -> instance list

  val record : ?lod:lod -> string -> unit

  val aggregate : ?lod:lod -> string -> unit

  val stop : unit -> unit

  val stamp : ?lod:lod -> string -> unit

  val mark : ?lod:lod -> string list -> unit

  val span : ?lod:lod -> span -> string list -> unit

  val inc : report -> unit

  val record_f : ?lod:lod -> string -> (unit -> 'a) -> 'a

  val record_s : ?lod:lod -> string -> (unit -> 'a Lwt.t) -> 'a Lwt.t

  val aggregate_f : ?lod:lod -> string -> (unit -> 'a) -> 'a

  val aggregate_s : ?lod:lod -> string -> (unit -> 'a Lwt.t) -> 'a Lwt.t

  val span_f : ?lod:lod -> string list -> (unit -> 'a) -> 'a

  val span_s : ?lod:lod -> string list -> (unit -> 'a Lwt.t) -> 'a Lwt.t
end

val wrap : profiler -> (module GLOBAL_PROFILER)
