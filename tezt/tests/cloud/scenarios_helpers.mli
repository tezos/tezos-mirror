(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Shorthand for [Log.info ~prefix:"TOP" ~color:Log.Color.FG.green] *)
val toplog : ('a, Format.formatter, unit, unit) format4 -> 'a

(** [init_teztale cloud agent]
    Run a teztale server on [agent] and add the corresponding service to
    [cloud]. [agent] name is used to prefix the prometheus target name. *)
val init_teztale : Cloud.t -> Agent.t -> Tezos.Teztale.t Lwt.t

(** [add_prometheus_source ?dal_node ?sc_rollup_node ?evm_node ?node cloud agent]
    Uses the metrics endpoints of given [?dal_node], [?sc_rollup_node],
    [?evm_node] and [?node] for registering prometheus sources in [cloud]. *)
val add_prometheus_source :
  ?dal_node:Dal_node.t ->
  ?sc_rollup_node:Sc_rollup_node.t ->
  ?evm_node:Tezt_etherlink.Evm_node.t ->
  ?node:Node.t ->
  Cloud.t ->
  Agent.t ->
  string ->
  unit Lwt.t

(** [init_explorus cloud node] uses [node]'s rpc endpoint to register explorus as a service *)
val init_explorus : Cloud.t -> Node.t -> unit Lwt.t

val refutation_game_minimal_rolling_history_mode : Node.argument

val default_page_size : int

val default_slot_size : int
