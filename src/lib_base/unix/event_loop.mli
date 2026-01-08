(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** The [Event_loop] module provides an abstraction layer on top of promises
    for use in the Octez codebase.

    Some components will use [eio*] libs, and if your library or binary uses
    these components, you *MUST* use this module instead of the traditional
    [Lwt_main.run].
*)

exception Not_initialized

(** Retrieve the Eio environment for the current [main_run] being executed.
    The returned environment must not escape the scope of this [main_run]
    execution. *)
val env : unit -> Eio_unix.Stdenv.base option

(** Same as [env], but raises [Not_initialized] if called outside of
    [main_run]. *)
val env_exn : unit -> Eio_unix.Stdenv.base

(** Retrieve the main switch for the current [main_run] being executed.
    The returned switch must not escape the scope of this [main_run]
    execution. Call only from the main domain; code running off-main should
    schedule work via {!Tezos_bees.Hive.run_on_main} or an equivalent helper to
    avoid deadlocking while waiting for the switch.

    Ideally, an Eio-based function that needs to allocate resources locally
    should create its own switch to have better control over resource usage.
    In some cases, such as when a worker is spawned with
    [Eio.Fiber.fork_daemon], you actually need a switch that will persist
    for the entire program's lifetime. That is where the main switch comes in
    handy. *)
val main_switch : unit -> Eio.Switch.t option

(** Same as [main_switch] but raises [Not_initialized] if called outside
    of [main_run] execution. *)
val main_switch_exn : unit -> Eio.Switch.t

(** [on_main_run callback] registers a callback to be called after
    initializing each [main_run] call. *)
val on_main_run : (Eio_unix.Stdenv.base -> Eio.Switch.t -> unit) -> unit

(** [main_run] should be used as a replacement for [Lwt_main.run], as it also
    handles `Eio` env initialization internal calls to the `Eio` event loop
    if [~eio] is set to [true] ([false] by default).

    You can't nest [main_run] calls. *)
val main_run : ?eio:bool -> process_name:string -> (unit -> 'a Lwt.t) -> 'a

(** [main_run_eio] should be used if the code you are running is using
    `Eio` libraries only. It will initialize the `Eio` event loop and run the
    promise. *)
val main_run_eio : (Eio_unix.Stdenv.base -> 'a) -> 'a
