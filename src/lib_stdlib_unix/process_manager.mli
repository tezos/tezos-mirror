(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Alternative implementation of [Lwt_process] API which does not rely on
    [Unix.fork].

    This module provides functions for creating and managing external processes.
    It uses [Lwt_domain] to spawn processes, making it a safer alternative to
    [Lwt_process] in multi-threaded applications that use system threads.

    The API mirrors [Lwt_process], providing functions to open processes with
    different I/O configurations ([open_process_in], [open_process_out], etc.)
    and corresponding "with" functions ([with_process_in], etc.) that handle
    resource cleanup automatically.

    {b Note:} This module does not support the Windows platform. *)

(** {1 Opening Processes} *)

(** [open_process_in (command, args)] starts the given command. The standard
    output of the process is connected to a pipe, which can be read via the
    [stdout] method of the returned object.

    @param command A tuple containing the command name and its arguments.
    @return A promise that resolves to a [Lwt_process.process_in] object. *)
val open_process_in : string * string array -> Lwt_process.process_in Lwt.t

(** [open_process_out (command, args)] starts the given command. The standard
    input of the process is connected to a pipe, which can be written to via
    the [stdin] method of the returned object.

    @param command A tuple containing the command name and its arguments.
    @return A promise that resolves to a [Lwt_process.process_out] object. *)
val open_process_out : string * string array -> Lwt_process.process_out Lwt.t

(** [open_process (command, args)] starts the given command. The standard
    input and standard output of the process are connected to pipes, which can
    be accessed via the [stdin] and [stdout] methods of the returned object.

    @param command A tuple containing the command name and its arguments.
    @return A promise that resolves to a [Lwt_process.process] object. *)
val open_process : string * string array -> Lwt_process.process Lwt.t

(** [open_process_full (command, args)] starts the given command. The standard
    input, standard output, and standard error of the process are connected to
    pipes, accessible via the [stdin], [stdout], and [stderr] methods of the
    returned object.

    @param command A tuple containing the command name and its arguments.
    @return A promise that resolves to a [Lwt_process.process_full] object. *)
val open_process_full : string * string array -> Lwt_process.process_full Lwt.t

(** [open_process_none (command, args)] starts the given command. The standard
    input, standard output, and standard error are redirected to the ones of the
    caller.

    @param command A tuple containing the command name and its arguments.
    @return A promise that resolves to a [Lwt_process.process_full] object. *)
val open_process_none : string * string array -> Lwt_process.process_none Lwt.t

(** {1 Managing Processes}

    These are helper functions that manage the lifecycle of a process. They
    start a process, apply a given function, and ensure the process is
    properly closed afterward, even if the function raises an exception.
    This is the recommended way to interact with processes. *)

(** [with_process_in cmd f] is a safe wrapper for [open_process_in]. It
    starts a process, applies [f] to the process object, and guarantees
    cleanup.

    @param cmd The command and arguments to execute.
    @param f A function to apply to the created process.
    @return The result of [f proc]. *)
val with_process_in :
  string * string array -> (Lwt_process.process_in -> 'a Lwt.t) -> 'a Lwt.t

(** [with_process_out cmd f] is a safe wrapper for [open_process_out]. It
    starts a process, applies [f] to the process object, and guarantees
    cleanup.

    @param cmd The command and arguments to execute.
    @param f A function to apply to the created process.
    @return The result of [f proc]. *)
val with_process_out :
  string * string array -> (Lwt_process.process_out -> 'a Lwt.t) -> 'a Lwt.t

(** [with_process cmd f] is a safe wrapper for [open_process]. It
    starts a process, applies [f] to the process object, and guarantees
    cleanup.

    @param cmd The command and arguments to execute.
    @param f A function to apply to the created process.
    @return The result of [f proc]. *)
val with_process :
  string * string array -> (Lwt_process.process -> 'a Lwt.t) -> 'a Lwt.t

(** [with_process_full cmd f] is a safe wrapper for [open_process_full]. It
    starts a process, applies [f] to the process object, and guarantees
    cleanup.

    @param cmd The command and arguments to execute.
    @param f A function to apply to the created process.
    @return The result of [f proc]. *)
val with_process_full :
  string * string array -> (Lwt_process.process_full -> 'a Lwt.t) -> 'a Lwt.t

(** [with_process_none cmd f] is a safe wrapper for [open_process_none]. It
    starts a process, applies [f] to the process object, and guarantees
    cleanup.

    @param cmd The command and arguments to execute.
    @param f A function to apply to the created process.
    @return The result of [f proc]. *)
val with_process_none :
  string * string array -> (Lwt_process.process_none -> 'a Lwt.t) -> 'a Lwt.t
