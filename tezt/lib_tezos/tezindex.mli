(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Tezindex process wrapper for Tezt tests. *)

(** A running tezindex instance. *)
type t

(** [run ?runner ?path ?name ~node ?rpc_port ()] spawns a tezindex process
    connected to [node]. A fresh temporary directory is used for
    [--base-dir] and a fresh port for the RPC server unless [rpc_port]
    is provided. *)
val run :
  ?runner:Runner.t ->
  ?path:string ->
  ?name:string ->
  node:Node.t ->
  ?rpc_port:int ->
  unit ->
  t

(** [wait_for_ready ?attempts t] polls the [/health] endpoint until it
    responds (up to [attempts] times, default 30, with 1s delay). *)
val wait_for_ready : ?attempts:int -> t -> unit Lwt.t

(** [get_v1_rewards_split t ~baker ~cycle] queries the
    [/v1/rewards/split/{baker}/{cycle}] endpoint and returns the parsed
    JSON response. *)
val get_v1_rewards_split : t -> baker:string -> cycle:int -> JSON.t Lwt.t
