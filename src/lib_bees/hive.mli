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
