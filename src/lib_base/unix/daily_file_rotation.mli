(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Shared utilities for daily file rotation.

    Provides date-stamped filename generation, date-based file matching,
    and old file cleanup used by both the log sink and profiling backends. *)

(** A calendar date as [(year, month, day)]. *)
type day = int * int * int

(** [current_day ()] returns today's date. *)
val current_day : unit -> day

(** [day_of_ptime t] extracts the calendar date from a [Ptime.t]. *)
val day_of_ptime : Ptime.t -> day

(** [string_of_day day] formats a day as ["YYYYMMDD"]. *)
val string_of_day : day -> string

(** [filename_insert_before_ext ~path s] inserts [s] before the file
    extension in [path].
    E.g. [filename_insert_before_ext ~path:"node.log" "20260323"]
    returns ["node-20260323.log"]. *)
val filename_insert_before_ext : path:string -> string -> string

(** [check_file_format_with_date base_filename candidate] returns [true]
    if [candidate] matches the pattern [{name}-YYYYMMDD{.ext}] derived
    from [base_filename]. *)
val check_file_format_with_date : string -> string -> bool

(** [list_rotation_files base_path] lists files in the directory of
    [base_path] that match the dated rotation pattern. *)
val list_rotation_files : string -> string list

(** [remove_older_files base_path ~days_kept] removes dated rotation
    files beyond the [days_kept] most recent ones. Synchronous. *)
val remove_older_files : string -> days_kept:int -> unit

(** [list_rotation_files_lwt base_path] is the Lwt variant of
    {!list_rotation_files}. *)
val list_rotation_files_lwt : string -> string list Lwt.t

(** [remove_older_files_lwt base_path ~days_kept] is the Lwt variant
    of {!remove_older_files}. *)
val remove_older_files_lwt : string -> days_kept:int -> unit Lwt.t
