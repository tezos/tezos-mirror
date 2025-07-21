(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** This module provides high-level utilities for initializing
    Tezos L1 nodes in both public and private network configurations,
    as used in Tezt cloud-based scenarios.

    It wraps the lower-level [Node] and [Node.Agent] modules with additional
    logic for snapshot handling, identity propagation, DAL configuration,
    "yes-crypto" simulation, and automatic bootstrapping behavior.
*)

(** Shortcut to create a "yes-wallet" for the given agent. *)
val yes_wallet : Agent.t -> Yes_wallet.t Lwt.t

(** Initialize an L1 node for the given configuration.

    If a [~snapshot] is provided and [?data_dir] is omitted, the node will be
    bootstrapped using the snapshot. Otherwise, a normal configuration
    initialization is used.

    In public networks, the node listens on all interfaces ([::]),
    whereas for private networks, it binds to [127.0.0.1].
*)

val init :
  ?arguments:Node.argument list ->
  ?data_dir:string ->
  ?identity_file:string ->
  ?dal_config:Tezos_crypto_dal_octez_dal_config.Dal_config.t ->
  ?env:string String_map.t ->
  rpc_external:bool ->
  name:string ->
  Network.t ->
  with_yes_crypto:bool ->
  snapshot:Snapshot_helpers.t ->
  ?ppx_profiling:bool ->
  Cloud.t ->
  Agent.t ->
  Node.t Lwt.t
