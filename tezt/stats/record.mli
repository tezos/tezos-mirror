(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type summed_durations = {total_time : int64; count : int}

type test = {
  file : string;
  title : string;
  tags : string list;
  successful_runs : summed_durations;
  failed_runs : summed_durations;
  peak_memory_usage : int option;
}

(** Get the average duration of successful runs, in nanoseconds. *)
val duration_ns : test -> int64

(** Get the average duration of successful runs, in minutes. *)
val duration_minutes : test -> float

type t = test list

(** Input some record files.

    If one of the requested files is actually a directory,
    input all JSON files from it, and if [recursive] is [true],
    recurse into its subdirectories. *)
val input : recursive:bool -> string list -> t

(** Test whether a test satisfies a TSL predicate. *)
val matches : Tezt_core.TSL_AST.t -> test -> bool
