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
  val stop : 'a t -> unit Lwt.t

  val run_process :
    'a t ->
    process_name:string ->
    ?socket_prefix:string ->
    handshake:bytes ->
    unit ->
    'a t tzresult Lwt.t

  val run_with_backoff :
    backoff:Ptime.t * float ->
    f:(unit -> 'a tzresult Lwt.t) ->
    'a tzresult Lwt.t

  val watch_dog :
    start_new_server:(unit -> 'a t tzresult Lwt.t) ->
    'a t ->
    (unit, tztrace) result Lwt.t
end
