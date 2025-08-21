(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com       *)
(*                                                                           *)
(*****************************************************************************)

(** This module provides an easy-to-use generic worker in order to run
    computations in parallel. It uses a dedicated pool of domains that is shared
    amongst all components of the same process.
    Important: the task worker inherits the main event loop switch that is
    available when it is created. If this switch is terminated, it will result
    in an inoperative task worker.
 *)

type 'a message_error =
  | Closed of error list option
  | Request_error of 'a
  | Any of exn

(* Exposes the number of domains that is currently used to execute parallel
   computations. *)
val number_of_domains : int

(** [launch_task_and_wait name func ?on_completion arg] create a request named
    [name] and executing [func args], and push it to a [worker] queue. [worker]
    is created at toplevel and shared amongst all the components. Returns a
    promise for the request result. An optional callback [on_completion] can be
    used to trigger specific computations once the request is completed. *)
val launch_task_and_wait :
  string ->
  ('a -> 'b) ->
  ?on_completion:('b -> unit) ->
  'a ->
  ('b, 'c message_error) result Eio.Promise.t

(** [launch_tasks_and_wait ?max_fibers name func ?on_completion args] runs
    {!val-launch_task_and_wait} for each each [arg] in [args], in parallel.
    [max_fibers] sets the maximum number of fibers to run concurrently (default
    = max_int). *)
val launch_tasks_and_wait :
  ?max_fibers:int ->
  string ->
  ('a -> 'b) ->
  ?on_completion:('b -> unit) ->
  'a list ->
  ('b, 'c message_error) result list

(** [launch_task name func ?on_completion arg] create a request named [name] and
    executing [func args], and push it to [worker] queue, true if the request
    has been pushed successfully and false otherwise. An optional callback
    [on_completion] can be used to trigger specific computations once the
    request is completed *)
val launch_task :
  string -> ('a -> 'b) -> ?on_completion:('b -> unit) -> 'a -> bool
