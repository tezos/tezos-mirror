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

(** Type used in order to (optionally) add information to a
    profiling call in the most generic way possible, allowing for
    different backends to need different data.

    For instance, a prometheus-based profiler (which can't handle as many
    different metrics as the file-based ones) would rely on the
    ["prometheus"] key being present or not in metadata in order to
    determine if the function should be monitored or not. *)
type metadata = (string * string) list

(** Name of a metric, with (possibly empty) metadata attached. *)
type id = string * metadata

(** Stack of names used for nested metrics (the head is the name of the
    metric, the second element is the name of the parent section, the
    third element the parent of the parent, etc...) with (possibly empty)
    metadata attached. *)
type ids = string list * metadata

module IdMap : Map.S with type key = id

(** {2 Types and utility functions} *)

type time = {
  wall : float;  (** Wall-clock time: total time elapsed. *)
  cpu : float option;  (** CPU time: time elapsed in the CPU. *)
}

type span = Span of time

(** [compute_cpu ~cpu] returns true if CPU profiling should be enabled.

    Computing CPU times is slow, if the overhead when profiling is too important,
    consider disabling cpu profiling globally or locally on functions
    that are called a lot.

    This depends on the value of the CPU_PROFILING environment variable
    and [cpu].

    [CPU_PROFILING] value dictates what kind of value is returned:
    - [true] means cpu profiling will be enabled everywhere
    - [false] means cpu profiling will be disabled everywhere
    - [default_true] means cpu profiling will be enabled except for
      the profiling parts where [cpu:false] is explicit
    - [default_false] means cpu profiling will be disabled except for
      the profiling parts where [cpu:true] is explicit. *)
val compute_cpu : cpu:bool option -> bool

val zero_time : cpu:bool option -> span

val ( -* ) : time -> time -> time

val ( +* ) : time -> time -> time

(** The level of detail of report sections. The driver can choose to
    use this information to skip or aggregate sections below a given
    level. The driver could also record everything, including the
    level of detail, and let a post processor skip or aggregate at
    display time. *)
type verbosity = Notice | Info | Debug

(** An aggregate node registers multiple calls to a section and sum
    their occurences and time. It also recursively aggregate its
    sub-aggregation nodes. *)
type aggregated_node = {
  count : int;
  total : span;
  children : aggregated_node IdMap.t;
  node_verbosity : verbosity;
}

(** A sequence item registers one section with potential sub-reports
    and registers elapsed-time. *)
type seq_item = {
  start : time;
  duration : span;
  contents : report;
  item_verbosity : verbosity;
}

and report = {
  aggregated : aggregated_node IdMap.t;
  recorded : (id * seq_item) list;
}

val report_encoding : report Data_encoding.t

type (_, _) kind = ..

type view = View : ('config, 'state) kind -> view

(** {2:driver Driver}

    The [Driver] is a signature that, when instantiated, specifies the behaviour
    of any profiler. This means that all profilers should implement this set of
    functions with their specifities. *)
module type DRIVER = sig
  (** Parameters to launch an instance of the driver. *)
  type config

  (** Internal state of an instance of the driver. *)
  type state

  (** A typed kind for downcasting. *)
  val kind : (config, state) kind

  (** An encoding to represent high-level informations about the driver *)
  val encoding_case : view Data_encoding.case

  (** Create an instance from a config. *)
  val create : config -> state

  (** Gives the current time in seconds. *)
  val time : cpu:bool option -> state -> time

  (** Open a sequence in the current sequence.
      If currently aggregating (not all aggregation scopes are closed),
      this has the same semantics as {!aggregate} instead. *)
  val record : cpu:bool option -> state -> verbosity -> id -> unit

  (** Open an aggregation node in the current sequence. *)
  val aggregate : cpu:bool option -> state -> verbosity -> id -> unit

  (** Close the most recently opened sequence or aggregation scope. *)
  val stop : state -> unit

  (** Record a timestamp in the most recently opened sequence. *)
  val stamp : cpu:bool option -> state -> verbosity -> id -> unit

  (** Count this event's occurences in the most recent sequence. *)
  val mark : state -> verbosity -> ids -> unit

  (** Sum the time spent in this event in the most recent sequence. *)
  val span : cpu:bool option -> state -> verbosity -> span -> ids -> unit

  (** Include a report in the current sequence. *)
  val inc : state -> report -> unit

  (** Consume the last toplevel report, if any. *)
  val report : cpu:bool option -> state -> report option

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

val time : cpu:bool option -> instance -> time

val report : cpu:bool option -> instance -> report option

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
val record : cpu:bool option -> profiler -> verbosity -> id -> unit

(** Open an aggregation node in the current sequence. *)
val aggregate : cpu:bool option -> profiler -> verbosity -> id -> unit

(** Close the most recently opened sequence or aggregation scope. *)
val stop : profiler -> unit

(** Record a timestamp in the most recently opened sequence. *)
val stamp : cpu:bool option -> profiler -> verbosity -> id -> unit

(** Count this event's occurences in the most recent sequence. *)
val mark : profiler -> verbosity -> ids -> unit

(** Sum the time spent in this event in the most recent sequence. *)
val span : cpu:bool option -> profiler -> verbosity -> span -> ids -> unit

(** Include a report in the current sequence. *)
val inc : profiler -> report -> unit

(** [record_f profiler verbosity label f] will call:
    {[
      record profiler verbosity name;
      f ();
      stop ();
    ]} *)
val record_f :
  cpu:bool option -> profiler -> verbosity -> id -> (unit -> 'a) -> 'a

(** Same as {!record_f} but for Lwt function *)
val record_s :
  cpu:bool option ->
  profiler ->
  verbosity ->
  id ->
  (unit -> 'a Lwt.t) ->
  'a Lwt.t

(** [aggregate_f profiler verbosity label f] will call:
    {[
      aggregate profiler verbosity name;
      f ();
      stop ();
    ]} *)
val aggregate_f :
  cpu:bool option -> profiler -> verbosity -> id -> (unit -> 'a) -> 'a

(** Same as {!aggregate_f} but for Lwt functions *)
val aggregate_s :
  cpu:bool option ->
  profiler ->
  verbosity ->
  id ->
  (unit -> 'a Lwt.t) ->
  'a Lwt.t

(** [span_f profiler verbosity label_list f] will compute [span] but
    specifically around [f] *)
val span_f :
  cpu:bool option -> profiler -> verbosity -> ids -> (unit -> 'a) -> 'a

(** Same as {!span_f} but for Lwt functions *)
val span_s :
  cpu:bool option ->
  profiler ->
  verbosity ->
  ids ->
  (unit -> 'a Lwt.t) ->
  'a Lwt.t

(** [unplugged ()] returns a new profiler *)
val unplugged : unit -> profiler

val main : profiler

(** {2 Global Profiler} *)
module type GLOBAL_PROFILER = sig
  type nonrec metadata = metadata

  type nonrec id = id

  type nonrec ids = ids

  type nonrec verbosity = verbosity = Notice | Info | Debug

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
  val record : cpu:bool option -> verbosity -> id -> unit

  (** Open an aggregation node in the current sequence. *)
  val aggregate : cpu:bool option -> verbosity -> id -> unit

  (** Close the most recently opened sequence or aggregation scope. *)
  val stop : unit -> unit

  (** {3 Profiling} *)

  (** Record a timestamp in the most recently opened sequence. *)
  val stamp : cpu:bool option -> verbosity -> id -> unit

  (** Count this event's occurences in the most recent sequence. *)
  val mark : verbosity -> ids -> unit

  (** Sum the time spent in this event in the most recent sequence. *)
  val span : cpu:bool option -> verbosity -> span -> ids -> unit

  (** Include a report in the current sequence. *)
  val inc : report -> unit

  (** [record_f verbosity label f] will call:
      {[
        record verbosity name;
        f ();
        stop ();
      ]} *)
  val record_f : cpu:bool option -> verbosity -> id -> (unit -> 'a) -> 'a

  (** Same as {!record_f} but for Lwt function *)
  val record_s :
    cpu:bool option -> verbosity -> id -> (unit -> 'a Lwt.t) -> 'a Lwt.t

  (** [aggregate_f verbosity label f] will call:
      {[
        aggregate verbosity name;
        f ();
        stop ();
      ]} *)
  val aggregate_f : cpu:bool option -> verbosity -> id -> (unit -> 'a) -> 'a

  (** Same as {!aggregate_f} but for Lwt functions *)
  val aggregate_s :
    cpu:bool option -> verbosity -> id -> (unit -> 'a Lwt.t) -> 'a Lwt.t

  (** [span_f verbosity label_list f] will compute [span] but specifically
      around [f] *)
  val span_f : cpu:bool option -> verbosity -> ids -> (unit -> 'a) -> 'a

  (** Same as {!span_f} but for Lwt functions *)
  val span_s :
    cpu:bool option -> verbosity -> ids -> (unit -> 'a Lwt.t) -> 'a Lwt.t
end

(** [wrap profiler] stores [profiler] in a {!GLOBAL_PROFILER} module
    allowing to use the [profiler] functions without having to provide it
    as a parameter *)
val wrap : profiler -> (module GLOBAL_PROFILER)

type 'a section_maker = 'a * metadata -> unit

(** [section_maker equal to_string profiler]
    Creates a function to open a new section (and close the one opened before) using [record] function
    when a new entity is encountered.

    @param verbosity - usual verbosity argument. Defaults to [Notice]
    @param equal - used for entities comparison.
    @param to_string - used for labeling the entity when using the [record] function.
    @param profiler - profiler instance used to track sections and record profiling data. *)
val section_maker :
  ?verbosity:verbosity ->
  ?cpu:bool ->
  ('a -> 'a -> bool) ->
  ('a -> string) ->
  profiler ->
  'a section_maker
