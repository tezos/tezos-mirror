(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023-2024 Nomadic Labs <contact@nomadic-labs.com>           *)
(*                                                                           *)
(*****************************************************************************)

module type NAME = sig
  val base : string list

  val component : string list
end

module type EVENTS = sig
  open Internal_event.Simple

  val emit : 'a t -> 'a -> unit Lwt.t

  val shutting_down_process : unit t

  val process_started : int t

  val process_exited_abnormally : (int * Unix.process_status) t

  val cannot_start_process : string t

  val waiting_for_process_restart : float t
end

module MakeEvent : functor (N : NAME) -> EVENTS

type 'a t

(** [create ~parameters ~parameters_encoding] creates a watchdog
    state, ready to be passed to the [Daemon] runner. *)
val create : parameters:'a -> parameters_encoding:'a encoding -> 'a t

val get_init_socket_path :
  socket_dir:string -> ?socket_prefix:string -> pid:int -> unit -> string

module Daemon : functor (Event : EVENTS) -> sig
  (** [stop t] stops the process handled by the watchdog daemon as
      soon as it is called. *)
  val stop : 'a t -> unit Lwt.t

  (** [run_process t ~binary_path ~arguments ()] starts a
      [Lwt_process.process_none] thanks to the given [binary_path] and
      its [arguments].
      Note that the arguments are taken directly from the argument,
      thus, the [binary_path] is not added to the argument list. Refer
      to the [Lwt_process] documentation for more details. *)
  val run_process :
    'a t ->
    binary_path:string ->
    arguments:string array ->
    unit ->
    'a t tzresult Lwt.t

  (** [run_process_with_sockets t ~process_name ?socket_prefix
      ?executable_name ~handshake ()] starts a
      [Lwt_process.process_none] depending on the given [process_name]
      and [executable_name] parameters. If [executable_name] is
      passed, then the process will be run thanks to the path to this
      binary. Otherwise, the current binary name will be used as a
      forked process. [process_name] aims to be the entry point of the
      binary, that may differ from the [executable_name] in case of
      fork. The [stdout] and [stderr] are redirected to the default
      Unix streams.

      [socket_prefix] and [handshake] are used to setup the
      communication, through a socket, with the created process. The
      values ares expected to be defined accordingly to both parts. *)
  val run_process_with_sockets :
    'a t ->
    process_name:string ->
    ?socket_prefix:string ->
    ?executable_name:string ->
    handshake:bytes ->
    unit ->
    'a t tzresult Lwt.t

  (** [watch_dog ~start_new_server t] takes a running process [t] and
      make sure it runs well. If it crashed, it will restart the
      process using the given [start_new_server] callback. *)
  val watch_dog :
    start_new_server:(unit -> 'a t tzresult Lwt.t) ->
    'a t ->
    (unit, tztrace) result Lwt.t
end
