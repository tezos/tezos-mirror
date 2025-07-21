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
  topic : [`Pkh of string | `Slot_index of int];
}

type producer = {
  node : Node.t;
  dal_node : Dal_node.t;
  client : Client.t;
  account : Account.key;
  is_ready : unit Lwt.t;
  slot_index : int;
}

(** "Status" of an attester at some level.
    There are 5 cases:
    - The attester is in the DAL committee and sent a dal_attestation -> With_DAL
    - The attester is in the DAL committee and sent an attestation without DAL -> Without_DAL
    - The attester is in the DAL committee and sent no attestation -> Expected_to_DAL_attest
    - The attester is out of the DAL committee (but in the Tenderbake committee) and
      sent an attestation -> Out_of_committee
    - The attester is out of the DAL committee and did not send an attestation
      (this case can happen either because they are out of the Tenderbake committee or
      because their baker had an issue at this level) -> Those bakers will not be in the
      `attestations` field of the `per_level_infos` crafted at the current level.
*)
type dal_status =
  | With_DAL of Z.t
  | Without_DAL
  | Out_of_committee
  | Expected_to_DAL_attest

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
  simulate_network:Scenarios_cli.network_simulation_config ->
  external_rpc:bool ->
  network:Network.t ->
  snapshot:Snapshot_helpers.t ->
  memtrace:bool ->
  ppx_profiling:bool ->
  ppx_profiling_backends:string list ->
  ignore_pkhs:string list ->
  disable_shard_validation:bool ->
  node_p2p_endpoint:string ->
  dal_node_p2p_endpoint:string option ->
  Tezos.Teztale.t option ->
  Account.key ->
  int ->
  int ->
  Agent.t ->
  producer Lwt.t

(** Initialize a DAL observer node and DAL node. *)
val init_observer :
  Cloud.t ->
  data_dir:string option ->
  simulate_network:Scenarios_cli.network_simulation_config ->
  external_rpc:bool ->
  network:Network.t ->
  snapshot:Snapshot_helpers.t ->
  memtrace:bool ->
  ppx_profiling:bool ->
  ppx_profiling_backends:string list ->
  disable_shard_validation:bool ->
  node_p2p_endpoint:string ->
  dal_node_p2p_endpoint:string option ->
  Tezos.Teztale.t option ->
  topic:[`Pkh of string | `Slot_index of int] ->
  int ->
  Agent.t ->
  observer Lwt.t
