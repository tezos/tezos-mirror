(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Driver registering its report in a promotheus collector whenever a
    toplevel section ends.
    Enable it using [PROFILING_BACKEND=prometheus] environment variable.

    This backend relies on the presence of a "prometheus" attribute
    in the metadata field of profiler calls. If this attribute is set to
    the empty string, the id passed to the call will be used as metric
    label. If the attribute is a non-empty string, that value will be
    used instead of the one passed to the profiling function.

    You MUST avoid metric names that are too unique such as block or
    operation hashes because prometheus will quickly fail at handling
    a too high metrics cardinality otherwise. *)
val prometheus : (string * Profiler.verbosity) Profiler.driver
