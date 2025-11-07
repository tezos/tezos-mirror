(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** This module provides high-level helper functions and types for setting up
    Data Availability Layer (DAL) nodes in Tezt-cloud scenarios.

    It covers two main roles:
    - DAL producers: which create and publish shards
    - DAL observers: which observe shards and attest availability
*)

type observer = {
  node : Node.t;
  dal_node : Dal_node.t;
  topic : [`Pkh of string | `Slot_indexes of int list];
}

type archiver = {
  node : Node.t;
  dal_node : Dal_node.t;
  topic : [`Slot_indexes of int list];
}

type producer = {
  node : Node.t;
  dal_node : Dal_node.t;
  client : Client.t;
  account : Account.key;
  is_ready : unit Lwt.t;
  slot_index : int;
}

(** [may_copy_dal_node_identity_file agent dal_node identity_file_opt]
    copies the identity file to the DAL node if a source path is provided. *)
val may_copy_dal_node_identity_file :
  Agent.t -> Dal_node.t -> string option -> unit Lwt.t

(** [fund_producers_accounts ~client ~fundraiser accounts] funds the given list of
    DAL producer accounts using the specified fundraiser key. All transfers are
    batched and injected together. *)
val fund_producers_accounts :
  client:Client.t ->
  fundraiser:string option ->
  (Account.key * int) list ->
  unit Lwt.t

(** [init_producer_accounts ~client ~producer_key ~dal_node_producers] creates or imports
    accounts for each producer. *)
val init_producer_accounts :
  client:Client.t ->
  producer_key:string option ->
  dal_node_producers:'a list ->
  Account.key list Lwt.t

(** Initialize a DAL producer node and DAL node. *)
val init_producer :
  Cloud.t ->
  data_dir:string option ->
  simulate_network:Network_simulation.t ->
  external_rpc:bool ->
  network:Network.t ->
  snapshot:Snapshot_helpers.t ->
  memtrace:bool ->
  ppx_profiling_verbosity:string option ->
  ppx_profiling_backends:string list ->
  ignore_pkhs:string list ->
  disable_shard_validation:bool ->
  disable_amplification:bool ->
  node_p2p_endpoint:string ->
  dal_node_p2p_endpoint:string option ->
  Tezos.Teztale.t option ->
  Account.key ->
  int ->
  int ->
  Agent.t ->
  producer Lwt.t

(** [produce_slot ~client ~producers ~network ~producer_key
    ~some_node_rpc_endpoint ~producers_delay ~slot_size level index]
    attempts to produce and publish a DAL slot commitment for the given [level]
    and producer [index]. *)
val produce_slot :
  Cloud.t ->
  client:Client.t ->
  producers:producer list ->
  network:Network.t ->
  producer_key:string option ->
  some_node_rpc_endpoint:Endpoint.t ->
  producers_delay:int ->
  slot_size:int ->
  int ->
  int ->
  unit Lwt.t

(** [producers_not_ready ~producers] returns [true] if at least one of the
    given [producers] has not yet completed its [is_ready] promise. *)
val producers_not_ready : producers:producer list -> bool

(** Initialize a DAL observer node and DAL node. *)
val init_observer :
  Cloud.t ->
  data_dir:string option ->
  simulate_network:Network_simulation.t ->
  external_rpc:bool ->
  network:Network.t ->
  snapshot:Snapshot_helpers.t ->
  memtrace:bool ->
  ppx_profiling_verbosity:string option ->
  ppx_profiling_backends:string list ->
  disable_shard_validation:bool ->
  disable_amplification:bool ->
  node_p2p_endpoint:string ->
  dal_node_p2p_endpoint:string option ->
  Tezos.Teztale.t option ->
  topic:[`Pkh of string | `Slot_indexes of int list] ->
  int ->
  Agent.t ->
  observer Lwt.t

(** Initialize a DAL archiver node and DAL node. *)
val init_archiver :
  Cloud.t ->
  data_dir:string option ->
  simulate_network:Network_simulation.t ->
  external_rpc:bool ->
  network:Network.t ->
  snapshot:Snapshot_helpers.t ->
  memtrace:bool ->
  ppx_profiling_verbosity:string option ->
  ppx_profiling_backends:string list ->
  disable_shard_validation:bool ->
  disable_amplification:bool ->
  node_p2p_endpoint:string ->
  dal_node_p2p_endpoint:string option ->
  Tezos.Teztale.t option ->
  topic:[`Slot_indexes of int list] ->
  int ->
  Agent.t ->
  archiver Lwt.t
