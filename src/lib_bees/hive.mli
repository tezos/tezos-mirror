(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com       *)
(*                                                                           *)
(*****************************************************************************)

(** Hive is used to abstract and launch a pool of worker bees. A single hive
    should be used per process. Must be used within [Event_loop.main_run]
    continuation. *)

(** [launch_worker worker bee_name domains worker_loop] starts [domains] domains
    running the worker_loop to handle worker requests. [worker_loop] takes the
    domain number.
    If no [switch] is provided, the Event_loop's main switch will be used.
    *)
val launch_worker :
  ?switch:Eio.Switch.t ->
  'worker ->
  bee_name:string ->
  domains:int ->
  (int -> 'worker -> [`Stop_daemon]) ->
  unit

(** [get_error bee_name] returns any eio-related error associated with the worker
    [bee_name]. *)
val get_error : string -> exn option

(** Delegate the execution of an Lwt promise to a specialized worker. Beware of
    the fact that the Lwt.t promise won't be resolved as soon as [async_lwt]
    returns, but (as suggested by the function name) asynchronously.

    This means that [try async_lwt closure with _ -> ...] will not catch an
    exception raised by the execution of the closure.
    If the closure raises an exception, it will break the internal lwt loop
    and prevent any subsequent asynchronous call to be executed. It's
    consequently advised to handle exceptions directly in the closure and return
    a [result].

    Warning: Promises added to this loop are expected to perform only very short
    computations (typically just sending events). Longer computations may
    congest promise scheduling and cause blocking [async_lwt] calls.*)
val async_lwt : (unit -> unit Lwt.t) -> unit

(** Schedule [f] to run on the Event_loop main switch and return its result.
    Callers can invoke this from any domain to ensure main-domain execution. *)
val run_on_main : (unit -> 'a) -> 'a
