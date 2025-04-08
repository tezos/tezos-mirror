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

(** [launch_task worker name func arg] create a request named [name] and
    executing [func args], and push it to [worker] queue, returning a promise
    for the request result. *)
val launch_task :
  task_worker ->
  string ->
  ('a -> 'b) ->
  'a ->
  ('b, 'c message_error) result Lwt.t

(** [launch_tasks worker name func args] runs {!val-launch_task} for each
    each [arg] in [args], in parallel. *)
val launch_tasks :
  task_worker ->
  string ->
  ('a -> 'b) ->
  'a list ->
  ('b, 'c message_error) result list Lwt.t
