(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

exception Could_not_connect

exception Connection_closed

(** Type of a websocket client  *)
type t

(** [connect ?runner ?hook ?name url] connects to a websocket server and returns
    the client. *)
val connect :
  ?runner:Runner.t ->
  ?hooks:Process_hooks.t ->
  ?name:string ->
  string ->
  t Lwt.t

(** Terminate the client. *)
val close : t -> unit Lwt.t

(** Send a JSON object on the websocket. *)
val send : t -> JSON.t -> unit Lwt.t

(** Send a raw string on the websocket. *)
val send_raw : t -> string -> unit Lwt.t

(** Receive a JSON object on the websocket. *)
val recv : ?timeout:float -> t -> JSON.t Lwt.t

(** Send and receive response on websocket. *)
val send_recv : ?timeout:float -> t -> JSON.t -> JSON.t Lwt.t

(** Send SIGSTOP to websocket client process. *)
val pause : t -> unit

(** Send SIGCONT to websocket client process. *)
val resume : t -> unit
