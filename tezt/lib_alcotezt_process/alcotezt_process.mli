(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Process-isolated test execution for Alcotezt with Tezos error monad support.

    This module provides fork-exec based test isolation using OS-level process
    separation. Tests are executed via Unix.create_process_env, ensuring:

    1. Complete isolation between tests (no shared memory, domains, or state)
    2. Clean initialization of Eio/Lwt runtime per test
    3. Proper cleanup via process exit (no lingering resources)
    4. Timeout enforcement at the OS level
    5. Support for Tezos [tzresult] types

    Both [test_case] and [test_case_lwt] provide FULL process isolation
    via separate OS processes. *)

open Tezos_error_monad.Error_monad

(** [maybe_run_child ()] should be called at the start of test programs
    that use process isolation. It checks if the current process is a child
    process spawned for test execution and runs the appropriate test if so. *)
val maybe_run_child : unit -> unit

(** [test_case ?process_name ?timeout label speed_level body] creates a test
    that runs in a separate OS process with Eio runtime.

    This function provides FULL PROCESS ISOLATION via fork-exec. The test body
    executes in a fresh child process with clean Eio environment.

    @param process_name Optional name for the process (defaults to label)
    @param timeout Optional timeout in seconds (defaults to 10s)
    @param label Test label
    @param speed_level Test speed ([`Quick] or [`Slow])
    @param body Eio-based test function returning [unit tzresult]
    @return An Alcotest test_case tuple *)
val test_case :
  ?process_name:string ->
  ?timeout:float ->
  string ->
  [`Quick | `Slow] ->
  (unit -> unit tzresult) ->
  string * [`Quick | `Slow] * (unit -> unit)

(** [test_case_lwt ?process_name ?timeout label speed_level body] creates a test
    that runs in a separate OS process with Lwt runtime.

    This function provides FULL PROCESS ISOLATION via fork-exec, just like
    [test_case]. The only difference is it uses Lwt runtime (via Octez Event_loop)
    instead of Eio runtime.

    @param process_name Optional name for the process (defaults to label)
    @param timeout Optional timeout in seconds (defaults to 10s)
    @param label Test label
    @param speed_level Test speed ([`Quick] or [`Slow])
    @param body Lwt-based test function returning [unit tzresult Lwt.t]
    @return An Alcotest test_case tuple *)
val test_case_lwt :
  ?process_name:string ->
  ?timeout:float ->
  string ->
  [`Quick | `Slow] ->
  (unit -> unit tzresult Lwt.t) ->
  string * [`Quick | `Slow] * (unit -> unit)

(** [run ?__FILE__ ?tags name test_suites] runs the test suites with process isolation.

    This is a convenience wrapper that automatically calls [maybe_run_child ()]
    first, then registers tests with Tezt. Use this instead of [Alcotest.run]
    when your tests use [test_case] or [test_case_lwt] for process isolation.

    @param __FILE__ Optional file name for test output (defaults to "")
    @param tags Optional additional tags for all tests
    @param name Test suite name
    @param test_suites List of test suites (name, test list) pairs *)
val run :
  ?__FILE__:string ->
  ?tags:string list ->
  string ->
  (string * (string * [`Quick | `Slow] * (unit -> unit)) list) list ->
  unit
