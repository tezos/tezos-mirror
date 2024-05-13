(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t

(** Create a bundler instance connecting to the sequencer at [endpoint].*)
val bundler :
  ?runner:Runner.t ->
  ?rpc_addr:string ->
  ?rpc_port:int ->
  endpoint:string ->
  unit ->
  t

(** Create a sequencer sidecar instance. *)
val sequencer :
  ?runner:Runner.t -> ?rpc_addr:string -> ?rpc_port:int -> unit -> t

(** Returns the endpoint exposed by the DSN node. *)
val endpoint : t -> string

(** Starts the given instance of the dsn node. *)
val start : t -> unit Lwt.t
