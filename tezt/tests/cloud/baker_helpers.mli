(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** Helpers for setting up Tezos bakers (with optional DAL nodes) in
    tezt‑cloud scenarios. *)

type baker_account = {
  delegate : Account.key;
  consensus_key : Account.key option;
}

(** A running baker *)
type baker = {
  node : Node.t;
  dal_node : Dal_node.t option;
  baker : Agnostic_baker.t;
  accounts : baker_account list;
  stake : int;
}

(** A summary of a single baker’s DAL performance at one block,
    used in monitoring *)
type per_baker_dal_summary = {
  attestable_slots : int;
  attested_slots : int;
  in_committee : bool;
  (* [attestation_with_dal] is [None] if one is out of the DAL committee or did
     not send any attestation.
     Otherwise, it is [Some the_sent_attestation_is_"with_dal"]. *)
  attestation_with_dal : bool option;
}

(** Initialize a whole fleet of bakers. *)
val init_bakers :
  bakers:string list ->
  stake:int list Lwt.t ->
  data_dir:string option ->
  simulate_network:Network_simulation.t ->
  external_rpc:bool ->
  network:Network.t ->
  snapshot:Snapshot_helpers.t ->
  ppx_profiling_verbosity:string option ->
  ppx_profiling_backends:string list ->
  memtrace:bool ->
  with_dal:bool ->
  disable_shard_validation:bool ->
  node_p2p_endpoint:string ->
  dal_node_p2p_endpoint:string option ->
  Cloud.t ->
  Tezos.Teztale.t option ->
  baker_accounts:baker_account list list ->
  (name:string -> Agent.t Lwt.t) ->
  baker list Lwt.t
