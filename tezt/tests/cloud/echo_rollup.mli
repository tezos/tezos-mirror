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

type operator

(** [init_echo_rollup_account ~client ~echo_rollup ~alias_prefix] generates
    a new rollup operator key if [~echo_rollup] is [true]. The key alias will
    start with [~alias_prefix]. *)
val init_echo_rollup_account :
  client:Client.t ->
  echo_rollup:bool ->
  alias_prefix:string ->
  Account.key option Lwt.t

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
  Account.key option ->
  operator option Lwt.t
