(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** CPU information. *)

(** Get the number of CPU cores from [/proc/cpuinfo].

    Always returns at least 1.
    Prints a warning if [/proc/cpuinfo] cannot be read. *)
val get_cpu_count : unit -> int
