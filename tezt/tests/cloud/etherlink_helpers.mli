(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** This module provides helpers for initializing Etherlink rollup operators, producers
    and DAL integration within Tezt-cloud scenarios.
*)

type etherlink_configuration = {
  etherlink_sequencer : bool;
  etherlink_producers : int;
  etherlink_dal_slots : int list;
  chain_id : int option;
  tezlink : bool;
}

type etherlink_operator_setup = {
  node : Node.t;
  client : Client.t;
  sc_rollup_node : Sc_rollup_node.t;
  evm_node : Tezt_etherlink.Evm_node.t;
  is_sequencer : bool;
  sc_rollup_address : string;
  account : Account.key;
  batching_operators : Account.key list;
}

type etherlink = {
  configuration : etherlink_configuration;
  operator : etherlink_operator_setup;
  accounts : Tezt_etherlink.Eth_account.t Array.t;
}

(** Compute the combined Tez balance of all the given Etherlink operator accounts. *)
val total_operator_balance :
  client:Client.t -> operators:Account.key list -> Tez.t Lwt.t

(** Initialize the operator and batching accounts required for Etherlink. *)
val init_etherlink_operators :
  client:Client.t -> 'a option -> (Account.key list * Account.key list) Lwt.t

(** Initialize the Etherlink rollup stack which includes the EVM rollup node,
    DAL if enabled and a configurable number of producers. *)
val init_etherlink :
  data_dir:string option ->
  simulate_network:Network_simulation.t ->
  external_rpc:bool ->
  network:Network.t ->
  snapshot:Snapshot_helpers.t ->
  ppx_profiling_verbosity:string option ->
  ppx_profiling_backends:string list ->
  memtrace:bool ->
  node_p2p_endpoint:string ->
  dal_node_p2p_endpoint:string option ->
  next_agent:(name:string -> Agent.t Lwt.t) ->
  cloud:Cloud.t ->
  Account.key option ->
  Account.key list ->
  etherlink_configuration option ->
  etherlink option Lwt.t
