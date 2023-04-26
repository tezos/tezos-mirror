(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Configure the event-logging framework for UNIx-based applications. *)

(** The JSON-file-friendly definition of the configuration of the
    internal-events framework. It allows one to activate registered
    event sinks.  *)

open Error_monad

module Configuration : sig
  include module type of struct
    include Tezos_base.Internal_event_config
  end

  (** Parse a json file at [path] into a configuration. *)
  val of_file : string -> t tzresult Lwt.t
end

(** This is the environment variable name that contains the a list of
    URIs on which events can be reported (see {!val:init}). *)
val env_var_name : string

(** Initialize the internal-event sinks by looking at the
    [?internal_events] argument and then at the (whitespace separated) list
    of URIs in the ["TEZOS_EVENTS_CONFIG"] environment variable, if an URI
    does not have a scheme it is expected to be a path to a configuration
    JSON file (cf. {!Configuration.of_file}), e.g.:
    [export TEZOS_EVENTS_CONFIG="unix-files:///tmp/events-unix debug://"], or
    [export TEZOS_EVENTS_CONFIG="debug://  /path/to/config.json"].

    The function also initializes the {!Logs_simple_config} module
    (corresponding to the ["TEZOS_LOG"] environment variable).

    - [internal_events] is the value of configured sinks of command calling
    init.

    - [log_cfg] is the configuration specific to the default sinks if
    applicable.
*)
val init :
  ?log_cfg:Logs_simple_config.cfg ->
  ?internal_events:Internal_event_config.t ->
  unit ->
  unit Lwt.t

(** Call [close] on all the sinks. *)
val close : unit -> unit Lwt.t

(** [make_with_defaults ?enable_default_daily_logs_at ?internal_events ] creates
    internal event configuration using default values depending on parameters.

    - If [internal_events] is provided, nothing is modified. Otherwise default
    values are used.

    - [enable_default_daily_logs_at] adds daily rotating sink at the given path
    with the following value:
    ["file-descriptor-path:///<daily_logs_path>/daily.log
    ?create-dirs=true&daily-logs=7&section-prefix=info&format=pp"] *)

val make_with_defaults :
  ?internal_events:Internal_event_config.t ->
  ?enable_default_daily_logs_at:string ->
  unit ->
  Internal_event_config.t

(** Calls [init] with the same arguments as [make_with_defaults] *)
val init_with_defaults :
  ?internal_events:Internal_event_config.t ->
  ?enable_default_daily_logs_at:string ->
  ?log_cfg:Logs_simple_config.cfg ->
  unit ->
  unit Lwt.t
