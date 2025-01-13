(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Scheduler that mimics cron functionality. *)

(** The scheduler *)
type t

(** Create an empty chronos scheduler *)
val zero : unit -> t

(** [register t ~time ~action] adds a new task to the scheduler.

    The time format follows the standard cron syntax with five
    space-separated fields: minute, hour, day of month, month, and day
    of week. Each field can be either a specific number within its
    valid range (minute: 0-59, hour: 0-23, day: 1-31, month: 1-12, day
    of week: 0-6 where 0 is Sunday) or an asterisk '*' to indicate
    "any value".

    Relies on UTC (Coordinated Universal Time), also known as GMT for
    time. Remember Paris is (UTC+1).

    For example, "30 2 * * 1" means "2:30 AM every Monday (GMT)".

    Currently, the implementation only supports single values or
    asterisks - ranges, lists and step values are not yet supported.

    @raise Failure if the time specification is invalid.
    @raise Failure if the time string format is invalid. *)
val register : t -> tm:string -> action:(unit -> unit Lwt.t) -> unit

(** [start t] starts the scheduler. *)
val start : t -> unit Lwt.t

(** [shutdown t] triggers a shutdown of the scheduler. *)
val shutdown : t -> unit
