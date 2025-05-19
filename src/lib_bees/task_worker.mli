(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com       *)
(*                                                                           *)
(*****************************************************************************)

(** This module provides an easy-to-use generic worker in order to run
    computations in parallel.
 *)

type task_worker

type launch_error

type 'a message_error =
  | Closed of error list option
  | Request_error of 'a
  | Any of exn

(** [worker name domains] creates a worker to be used with {!val-launch_task}
    and {!val-launch_tasks} functions.

    Typically, one worker should be initialised at program launch, and reused
    during the whole program lifetime for parallelised computation. *)
val worker : string -> int -> (task_worker, launch_error) result

(** [launch_task_and_wait worker name func ?on_completion arg] create a request named
    [name] and executing [func args], and push it to [worker] queue, returning a
    promise for the request result. An optional callback [on_completion] can be
    used to trigger specific computations once the request is completed. *)
val launch_task_and_wait :
  task_worker ->
  string ->
  ('a -> 'b) ->
  ?on_completion:('b -> unit) ->
  'a ->
  ('b, 'c message_error) result Eio.Promise.t

(** [launch_tasks_and_wait worker name func ?on_completion args] runs
    {!val-launch_task_and_wait} for each each [arg] in [args], in parallel. *)
val launch_tasks_and_wait :
  task_worker ->
  string ->
  ('a -> 'b) ->
  ?on_completion:('b -> unit) ->
  'a list ->
  ('b, 'c message_error) result list

(** [launch_task worker name func ?on_completion arg] create a request named
    [name] and executing [func args], and push it to [worker] queue, true if the
    request has been pushed successfully and false otherwise. An optional
    callback [on_completion] can be used to trigger specific computations once
    the request is completed *)
val launch_task :
  task_worker ->
  string ->
  ('a -> 'b) ->
  ?on_completion:('b -> unit) ->
  'a ->
  bool
