(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module DAL : sig
  type t = {
    blocks_history : int option;
    producer_key : string option;
    fundraiser : string option;
    network : Network.t option;
    simulate_network : Network_simulation.t option;
    snapshot : Snapshot_helpers.t option;
    bootstrap : bool option;
    stake : Network.stake_repartition option;
    bakers : string list;
    stake_machine_type : string list;
    dal_producers_slot_indices : int list;
    producers : int option;
    producers_delay : int option;
    producer_machine_type : string option;
    observer_slot_indices : int list;
    observer_pkhs : string list;
    protocol : Protocol.t option;
    data_dir : string option;
    etherlink : bool option;
    etherlink_sequencer : bool option;
    etherlink_producers : int option;
    etherlink_chain_id : int option;
    echo_rollup : bool option;
    disconnect : (int * int) option;
    etherlink_dal_slots : int list;
    teztale : bool option;
    octez_release : string option;
    memtrace : bool option;
    bootstrap_node_identity_file : string option;
    bootstrap_dal_node_identity_file : string option;
    refresh_binaries : bool option;
    node_external_rpc_server : bool option;
    with_dal : bool option;
    proxy_localhost : bool option;
    disable_shard_validation : bool option;
    ignore_pkhs : string list;
    ppx_profiling_verbosity : string option;
    ppx_profiling_backends : string list;
    enable_network_health_monitoring : bool option;
    tezlink : bool option;
    slot_size : int option;
    number_of_slots : int option;
  }

  val encoding : t Data_encoding.t
end
