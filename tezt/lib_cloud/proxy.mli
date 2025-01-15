(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Create a configuration for the proxy *)
val make_config : unit -> Agent.Configuration.t

(** [get_agent agents] returns the proxy agent. It raises [Not_found] if the
    proxy agent was not found. This function should be safe to call when
    [Env.mode] is [`Orchestrator] or [`Host]. *)
val get_agent : Agent.t list -> Agent.t

(** [copy_files agent] copies all the necessary files for the proxy orchestrator
  to run correctly. *)
val copy_files :
  Agent.t ->
  scenario_files:string list ->
  proxy_deployement:string ->
  unit Lwt.t
