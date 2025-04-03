(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Scheduler that mimics cron functionality. *)

(** The scheduler *)
type t

(** A scheduler task. *)
type task

(** [task ~name ~tm ~action ?randomized_delay ()] returns a new task
    named [name] performing [action] according to the time [tm].

    The [tm] time format follows the standard cron syntax with five
    space-separated fields: minute, hour, day of month, month, and day
    of week. Each field can be either a specific number within its
    valid range (minute: 0-59, hour: 0-23, day: 1-31, month: 1-12, day
    of week: 0-6 where 0 is Sunday) or an asterisk '*' to indicate
    "any value".

    Relies on UTC (Coordinated Universal Time), also known as GMT for
    time. Paris operates on Central European Time (CET), which is
    UTC+1 during standard time (winter months). During daylight saving
    time (summer months), Paris switches to Central European Summer
    Time (CEST), which is UTC+2.

    For example, "30 2 * * 1" means "2:30 AM every Monday (GMT)".

    Currently, the implementation only supports single values or
    asterisks - ranges, lists and step values are not yet supported.

    [?randomized_delay] delays the timer by a randomly selected amount
    of time between 0 and the specified value. Defaults to 0,
    indicating that no randomized delay shall be applied. Each timer
    unit will determine this delay randomly before each iteration.
    This setting is useful to stretch dispatching of similarly
    configured timer events over a certain time interval, to prevent
    them from firing all at the same time, possibly resulting in
    resource congestion. Similar to to what is proposed here:
    https://www.freedesktop.org/software/systemd/man/latest/systemd.timer.html#RandomizedDelaySec=
*)
val task :
  name:string ->
  tm:string ->
  action:(unit -> unit Lwt.t) ->
  ?randomized_delay:int ->
  unit ->
  task

(** [init ~tasks] creates an chronos scheduler performing the given
    [tasks]. *)
val init : tasks:task list -> t

(** [start t] starts the scheduler. *)
val start : t -> unit

(** [shutdown t] triggers a shutdown of the scheduler. *)
val shutdown : t -> unit
