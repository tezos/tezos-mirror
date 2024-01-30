(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Parameters of the [Daemon.Make] functor. *)
module type PARAMETERS = sig
  (** Data to store whether a daemon is running or not. *)
  type persistent_state

  (** Data to store when a daemon is running. *)
  type session_state

  (** Basis for the default value for the [?name] argument of [create].

      Examples: ["node"] or ["accuser"]. *)
  val base_default_name : string

  (** Cycle of default values for the [?color] argument of [create]. *)
  val default_colors : Log.Color.t array
end

module Level : sig
  type default_level = [`Debug | `Info | `Notice]

  type level = [default_level | `Warning | `Error | `Fatal]

  val to_string : level -> string
end

(** Functor for the common parts of all Tezos daemons: node, baker,
    endorser and accuser. Handles event handling in particular. *)
module Make : functor (X : PARAMETERS) -> sig
  (** Exception raised by [wait_for] functions if the daemon terminates before the event.

      You may catch or let it propagate to cause the test to fail.
      [daemon] is the name of the daemon.
      [event] is the name of the event.
      [where] is an additional optional constraint, such as ["level >= 10"]. *)
  exception
    Terminated_before_event of {
      daemon : string;
      event : string;
      where : string option;
    }

  (** When a daemon is running, we store:
     - its process, so that we can terminate it for instance;
     - the event loop promise, which reads events and cleans them up when
       the daemon terminates;
     - some information about the state of the daemon so that users can query them.

     The event loop promise is particularly important as when we terminate
     the daemon we must also wait for the event loop to finish cleaning up before
     we start the daemon again. The event loop is also responsible to set the status
     of the daemon to [Not_running], which is another reason to wait for it to
     finish before restarting a daemon. Otherwise we could have a [Not_running]
     daemon which would be actually running. *)
  type session_status = {
    process : Process.t;
    stdin : Lwt_io.output_channel;
    session_state : X.session_state;
    mutable event_loop_promise : unit Lwt.t option;
  }

  (** Daemon status *)
  type status = Not_running | Running of session_status

  type event_handler =
    | Event_handler : {
        filter : JSON.t -> 'a option;
        resolver : 'a option Lwt.u;
      }
        -> event_handler

  (** Raw events. *)
  type event = {name : string; value : JSON.t; timestamp : float}

  (** Daemon states. *)
  type t = {
    name : string;
    color : Log.Color.t;
    path : string;
    persistent_state : X.persistent_state;
    mutable status : status;
    event_pipe : string;
    mutable stdout_handlers : (string -> unit) list;
    mutable stderr_handlers : (string -> unit) list;
    mutable persistent_event_handlers : (event -> unit) list;
    mutable one_shot_event_handlers : event_handler list String_map.t;
  }

  (** Get the name of a daemon. *)
  val name : t -> string

  (** Send SIGTERM to a daemon and wait for it to terminate.

      Default [timeout] is 30 seconds, after which SIGKILL is sent. *)
  val terminate : ?timeout:float -> t -> unit Lwt.t

  (** Send SIGKILL to a daemon and wait for it to terminate. *)
  val kill : t -> unit Lwt.t

  (** Send SIGSTOP to a daemon. *)
  val stop : t -> unit Lwt.t

  (** Send SIGCONT to a daemon. *)
  val continue : t -> unit Lwt.t

  (** Generate a fresh indentifier based on [X.base_default_name].
      This function ensures that a same name can't be returned twice. *)
  val fresh_name : unit -> string

  (** Evaluates in a different color at each call. *)
  val get_next_color : unit -> Log.Color.t

  (** Create a daemon.

    The standard output and standard error output of the daemon will
    be logged with prefix [name] and color [color].

    Default [event_pipe] is a temporary file
    whose name is derived from [name]. It will be created
    as a named pipe so that daemon events can be received.

    If [runner] is specified, the daemon will be spawned on this
    runner using SSH. *)
  val create :
    path:string ->
    ?runner:Runner.t ->
    ?name:string ->
    ?color:Log.Color.t ->
    ?event_pipe:string ->
    X.persistent_state ->
    t

  (** Takes the given JSON full event of the following form
      and evaluates in an event using [<name>] and
      [<value>]:

      {[{
        "fd-sink-item.v0": {
          [...]
            "event": { <name>:<value> }
        }
      }]}

      If the given JSON does not match the right structure,
      and in particular if the value of the field ["event"]
      is not a one-field object, the function evaluates in
      None. *)
  val get_event_from_full_event : JSON.t -> event option

  (** Spawn a daemon.

      If [capture_stderr] is [true] (default to [false]), then functions like
      {!Process.check_and_read_stderr} or {!Process.check_error} will not work
      as expected with the process of the daemon (as stored in its
      [session_status]). *)
  val run :
    ?env:string String_map.t ->
    ?runner:Runner.t ->
    ?on_terminate:(Unix.process_status -> unit Lwt.t) ->
    ?event_level:Level.default_level ->
    ?event_sections_levels:(string * Level.level) list ->
    ?capture_stderr:bool ->
    t ->
    X.session_state ->
    string list ->
    unit Lwt.t

  (** Wait for a custom event to occur.

      Usage: [wait_for_full daemon name filter]

      If an event named [name] occurs, apply [filter] to its
      whole json, which is of the form:
      {[{
        "fd-sink-item.v0": {
          "hostname": "...",
                      "time_stamp": ...,
                      "section": [ ... ],
                      "event": { <name>: ... }
                               }
        }]}
      If [filter] returns [None], continue waiting.
      If [filter] returns [Some x], return [x].

      [where] is used as the [where] field of the [Terminated_before_event] exception
      if the daemon terminates. It should describe the constraint that [filter] applies,
      such as ["field level exists"].

      It is advised to register such event handlers before starting the daemon,
      as if they occur before being registered, they will not trigger your handler.
      For instance, you can define a promise with
      [let x_event = wait_for daemon "x" (fun x -> Some x)]
      and bind it later with [let* x = x_event]. *)
  val wait_for_full :
    ?where:string -> t -> string -> (JSON.t -> 'a option) -> 'a Lwt.t

  (** Same as [wait_for_full] but ignore metadata from the file descriptor sink.

      More precisely, [filter] is applied to the value of field
      ["fd-sink-item.v0"."event".<name>].

      If the daemon receives a JSON value that does not match the right
      JSON structure, it is not given to [filter] and the event is
      ignored. See [wait_for_full] to know what the JSON value must
      look like. *)
  val wait_for :
    ?where:string -> t -> string -> (JSON.t -> 'a option) -> 'a Lwt.t

  (** Add a callback to be called whenever the daemon emits an event.

      Contrary to [wait_for] functions, this callback is never removed.

      Listening to events with [on_event] will not prevent [wait_for] promises
      to be fulfilled. You can also have multiple [on_event] handlers, although
      the order in which they trigger is unspecified. *)
  val on_event : t -> (event -> unit) -> unit

  (** Add a callback to be called whenever the daemon prints to its stdout.

      Contrary to [wait_for] functions, this callback is never removed.

      Listening to events with [on_stdout] will not prevent [wait_for] promises
      to be fulfilled. You can also have multiple [on_stdout] handlers,
      although the order in which they trigger is unspecified. *)
  val on_stdout : t -> (string -> unit) -> unit

  (** Add a callback to be called whenever the daemon prints to its stderr.
      {!run} must have been called with [capture_stderr] flag set to true,
      to call callbacks registered this way.

      Contrary to [wait_for] functions, this callback is never removed.

      Listening to events with [on_stderr] will not prevent [wait_for] promises
      to be fulfilled. You can also have multiple [on_stderr] handlers,
      although the order in which they trigger is unspecified. *)
  val on_stderr : t -> (string -> unit) -> unit

  (** Register an event handler that logs all events.

      Use this when you need to debug or reverse engineer incoming events.
      Usually you do not want to keep that in the final versions of your tests.

      The [max_length] optional parameter can be used to limit the
      length of the output of each event; outputs longer than the
      limit are truncated at the limit and "[...]" is appended to
      them to mark the truncation. *)
  val log_events : ?max_length:int -> t -> unit

  (** Values returned by {!memory_consumption}. *)
  type observe_memory_consumption = Observe of (unit -> int option Lwt.t)

  (** Observe memory consumption of the daemon.

      This function requires [perf] and [heaptrack] in the PATH
      and [kernel.perf_event_paranoid] to be permissive enough.
      Otherwise, the observation will always return [None].

      The returned function gives the peak of memory consumption
      observed since the observation has started.

      [memory_consumption daemon] starts the observation and returns
      [Some (Observe get)]. [get ()] stops the observation and
      returns the observation memory consumption.
  *)
  val memory_consumption : t -> observe_memory_consumption Lwt.t
end

(** {2 Filter combinators } *)

(** Transform an event filter into one that waits for several events.

    Usage: [n_events n filter]

    [n] represents the number of events that the resulting filter must
    wait for.

    [filter] is the initial filter that must be transformed. It is
    expressed as a function that takes a serialized input (for example
    a [JSON.t]) and returns some deserialized output (an event) if no
    error occured during the decoding and input matched some acceptance
    criteria.

    The function evaluates into a new filter, i.e. a new function that
    takes a serialized input and returns [None] until [n] calls where
    the given input passes the initial [filter] are performed.

    On the [nth] successful call, the resulting filter will return some
    list of the [n] successuful outputs of [filter]. The order of the
    given inputs is preserved in the list of outputs.

    The behavior of the resulting filter is unspecified after the list
    has been returned. *)
val n_events : int -> ('a -> 'b option) -> 'a -> 'b list option

(** Tranform an event filter into one that waits for several events to
    return the last one.

   Similar to [n_events] but only returns the last successful event
   instead of the whole list. *)
val nth_event : int -> ('a -> 'b option) -> 'a -> 'b option
