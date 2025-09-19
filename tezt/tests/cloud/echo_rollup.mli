(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** This module handles the initialization and orchestration of an Echo rollup
    within a Tezt-cloud scenario.

    An Echo rollup is a smart rollup running a WASM kernel that echoes back
    messages published via DAL slots.
*)

type operator = {
  node : Node.t;
  client : Client.t;
  sc_rollup_node : Sc_rollup_node.t;
  sc_rollup_address : string;
  operator : Account.key;
  origination_level : int;
}

(** Initialize an Echo rollup. *)
val init_echo_rollup :
  Cloud.t ->
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
  Dal_node_helpers.producer list ->
  int ->
  Account.key ->
  operator Lwt.t

(** [fetch_echo_rollup_data ~echo_rollup ~data_node_producers ~level]
    fetches payload size in bytes for each DAL slot given by [~data_node_producers]
    written by the [~echo_rollup] at a given L1 [~level]. *)
val fetch_echo_rollup_data :
  echo_rollup:operator option ->
  dal_node_producers:int list ->
  level:int ->
  (int, int) Hashtbl.t Lwt.t
