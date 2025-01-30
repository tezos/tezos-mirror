(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Run external programs and capture their output. *)

open Misc

(** Run an external program.

    [command executable arguments] runs [executable] with [arguments]
    as command-line arguments.

    If [working_directory] is specified, the program is run in this working directory
    instead of the current one. The working directory is restored once [command] returns.

    The outputs of the program (its [stdout] and [stderr]) are read line by line.
    Each line is passed to [on_read_line].
    By default, [on_read_line] outputs back each line on [stdout], prefixed with
    the name of the executable in brackets. *)
val command :
  ?working_directory:string ->
  ?on_read_line:(string -> unit) ->
  string ->
  string list ->
  (unit, [> `failed] error) result

(** Run an external program and read its output as a list of lines.

    Same as {!command} but returns the list of output lines.
    The program output is not printed to [stdout], contrary to the default
    behavior of {!command}. *)
val command_lines :
  ?working_directory:string ->
  string ->
  string list ->
  (string list, [> `failed] error) result
