(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

val isolated_config :
  auto_synchronisation_threshold:bool ->
  auto_connections:bool ->
  no_bootstrap_peers:bool ->
  peers:string list ->
  network:Network.t ->
  delay:int ->
  Node.argument list

val isolated_args : private_mode:bool -> string list -> Node.argument list

(** Initialize an L1 node for the given configuration.

    If a [~snapshot] is provided and [?data_dir] is omitted, the node will be
    bootstrapped using the snapshot. Otherwise, a normal configuration
    initialization is used.

    In public networks, the node listens on all interfaces ([::]),
    whereas for private networks, it binds to [127.0.0.1].
*)

(** [may_add_migration_offset_to_config node snapshot ~migration_offset ~network] may add an
    entry in the configuration file of [node] to trigger a UAU at level [~migration_offset]
    to upgrade to the next protocol of [~network]. This entry is is parametrised by the
    information obtained from [snapshot]. *)
val may_add_migration_offset_to_config :
  Node.t ->
  string ->
  migration_offset:int option ->
  network:Network.t ->
  unit Lwt.t

val init :
  ?arguments:Node.argument list ->
  ?data_dir:string ->
  ?identity_file:string ->
  ?dal_config:Tezos_crypto_dal_octez_dal_config.Dal_config.t ->
  ?env:string String_map.t ->
  ?migration_offset:int ->
  rpc_external:bool ->
  name:string ->
  Network.t ->
  with_yes_crypto:bool ->
  snapshot:Snapshot_helpers.t ->
  ppx_profiling_verbosity:string option ->
  ppx_profiling_backends:string list ->
  Cloud.t ->
  Agent.t ->
  Node.t Lwt.t
