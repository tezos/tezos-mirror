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

(** {1:doc-profiler Profiler} *)

(** {2 Summary}

    This profiling library declares a high-level interface meant to
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

(** {2 Types and utility functions} *)

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

(** {2:driver Driver}

    The [Driver] is a signature that, when instantiated, specifies the behaviour
    of any profiler. This means that all profilers should implement this set of
    functions with their specifities.

    Example: this driver writes text files in a unix filesystem *)
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

(** {2:instance Instance}

    A specific instance of a {!section-driver} implementation.

    Example: this driver that writes text files in a unix filesystem will
    write them in this specific file with this specific level of details *)
type instance

(** [instance driver params] will instantiate the [driver] with
    the given [params] *)
val instance : 'a driver -> 'a -> instance

val time : instance -> time

val report : instance -> report option

val report_s : instance -> report Lwt.t

val close : instance -> unit

(** {2 Profiler}

    The profiler is an API that needs to be attached to a backend.
    A backend is composed of:
    - a {!section-driver}
    - a {!section-instance} *)
type profiler

(** [plug profiler instance] plugs [profiler] to [instance] *)
val plug : profiler -> instance -> unit

(** [unplug profiler instance] unplugs [profiler] from [instance] *)
val unplug : profiler -> instance -> unit

(** [close_and_unplug profiler instance] closes [profiler] and
    unplugs it from [instance] *)
val close_and_unplug : profiler -> instance -> unit

(** [close_and_unplug_all profiler] closes [profiler] and
    unplugs it from all instances it was plugged to *)
val close_and_unplug_all : profiler -> unit

(** [plugged profiler] returns all the instances plugged to [profiler] *)
val plugged : profiler -> instance list

(** Open a sequence in the current sequence.
    If currently aggregating (not all aggregation scopes are closed),
    this has the same semantics as {!aggregate} instead. *)
val record : profiler -> ?lod:lod -> string -> unit

(** Open an aggregation node in the current sequence. *)
val aggregate : profiler -> ?lod:lod -> string -> unit

(** Close the most recently opened sequence or aggregation scope. *)
val stop : profiler -> unit

(** Record a timestamp in the most recently opened sequence. *)
val stamp : profiler -> ?lod:lod -> string -> unit

(** Count this event's occurences in the most recent sequence. *)
val mark : profiler -> ?lod:lod -> string list -> unit

(** Sum the time spent in this event in the most recent sequence. *)
val span : profiler -> ?lod:lod -> span -> string list -> unit

(** Include a report in the current sequence. *)
val inc : profiler -> report -> unit

(** [record_f profiler ?lod label f] will call:
    {[
      record profiler ?lod name;
      f ();
      stop ();
    ]} *)
val record_f : profiler -> ?lod:lod -> string -> (unit -> 'a) -> 'a

(** Same as {!record_f} but for Lwt function *)
val record_s : profiler -> ?lod:lod -> string -> (unit -> 'a Lwt.t) -> 'a Lwt.t

(** [aggregate_f profiler ?lod label f] will call:
    {[
      aggregate profiler ?lod name;
      f ();
      stop ();
    ]} *)
val aggregate_f : profiler -> ?lod:lod -> string -> (unit -> 'a) -> 'a

(** Same as {!aggregate_f} but for Lwt functions *)
val aggregate_s :
  profiler -> ?lod:lod -> string -> (unit -> 'a Lwt.t) -> 'a Lwt.t

(** [span_f profiler ?lod label_list f] will compute [span] but
    specifically around [f] *)
val span_f : profiler -> ?lod:lod -> string list -> (unit -> 'a) -> 'a

(** Same as {!span_f} but for Lwt functions *)
val span_s :
  profiler -> ?lod:lod -> string list -> (unit -> 'a Lwt.t) -> 'a Lwt.t

val with_new_profiler : 'a driver -> 'a -> (profiler -> 'r) -> 'r * report list

val with_new_profiler_s :
  'a driver -> 'a -> (profiler -> 'r Lwt.t) -> ('r * report list) Lwt.t

(** [unplugged ()] returns a new profiler *)
val unplugged : unit -> profiler

val main : profiler

(** {2 Global Profiler} *)
module type GLOBAL_PROFILER = sig
  type nonrec lod = lod = Terse | Detailed | Verbose

  (** {3 Plugging} *)

  (** [plug instance] plugs the profiler stored in the module to [instance] *)
  val plug : instance -> unit

  (** [unplug instance] unplugs the profiler stored in the module
      from [instance] *)
  val unplug : instance -> unit

  (** [close_and_unplug instance] closes the profiler stored in the module and
      unplugs it from [instance] *)
  val close_and_unplug : instance -> unit

  (** [close_and_unplug_all ()] closes the profiler stored in the module and
      unplugs it from all instances it was plugged to *)
  val close_and_unplug_all : unit -> unit

  (** [plugged ()] returns all the instances plugged to the profiler stored
      in the module *)
  val plugged : unit -> instance list

  (** {3 Sequences} *)

  (** Open a sequence in the current sequence.
      If currently aggregating (not all aggregation scopes are closed),
      this has the same semantics as {!aggregate} instead. *)
  val record : ?lod:lod -> string -> unit

  (** Open an aggregation node in the current sequence. *)
  val aggregate : ?lod:lod -> string -> unit

  (** Close the most recently opened sequence or aggregation scope. *)
  val stop : unit -> unit

  (** {3 Profiling} *)

  (** Record a timestamp in the most recently opened sequence. *)
  val stamp : ?lod:lod -> string -> unit

  (** Count this event's occurences in the most recent sequence. *)
  val mark : ?lod:lod -> string list -> unit

  (** Sum the time spent in this event in the most recent sequence. *)
  val span : ?lod:lod -> span -> string list -> unit

  (** Include a report in the current sequence. *)
  val inc : report -> unit

  (** [record_f ?lod label f] will call:
      {[
        record ?lod name;
        f ();
        stop ();
      ]} *)
  val record_f : ?lod:lod -> string -> (unit -> 'a) -> 'a

  (** Same as {!record_f} but for Lwt function *)
  val record_s : ?lod:lod -> string -> (unit -> 'a Lwt.t) -> 'a Lwt.t

  (** [aggregate_f ?lod label f] will call:
      {[
        aggregate ?lod name;
        f ();
        stop ();
      ]} *)
  val aggregate_f : ?lod:lod -> string -> (unit -> 'a) -> 'a

  (** Same as {!aggregate_f} but for Lwt functions *)
  val aggregate_s : ?lod:lod -> string -> (unit -> 'a Lwt.t) -> 'a Lwt.t

  (** [span_f ?lod label_list f] will compute [span] but specifically
      around [f] *)
  val span_f : ?lod:lod -> string list -> (unit -> 'a) -> 'a

  (** Same as {!span_f} but for Lwt functions *)
  val span_s : ?lod:lod -> string list -> (unit -> 'a Lwt.t) -> 'a Lwt.t
end

(** [wrap profiler] stores [profiler] in a {!GLOBAL_PROFILER} module
    allowing to use the [profiler] functions without having to provide it
    as a parameter *)
val wrap : profiler -> (module GLOBAL_PROFILER)
