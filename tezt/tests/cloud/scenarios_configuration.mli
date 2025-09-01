(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** [network_simulation_configuration] allows to configure the simulation of a
   network, relying on the actual distribution of rights that will be found in
   the imported data (data-dir or snapshot). It requires yes crypto to be
   enabled.
   The simulate option has three modes:
     - scatter(x,y): selects the [x] biggest bakers found, and scatters their
       baking rights, in a round robin fashion, on [y] baker daemons. This is
       particularly useful to scatter the baking power across several baker
       daemons,
     - map(x,y,z): maps [y] keys from the biggest bakers found onto [y] baker
       daemons (theses daemons are handling a single key) and scatters the
       remaining [x-y] keys to [z] baker daemons. This is particularly useful to
       simulate the behaviour of an actual network,
     - disabled: no simulation, we rely on the configuration.stake parameter.
   For example:
     - scatter(10,2): [[0;2;4;6;8];[1;3;5;7;9]]
     - map(10,2,1):[[0];[1];[2;3;4;5;6;7;8;9]]
     - map(10,2,2):[[0];[1];[2;5;6;8];[3;5;8;9]] *)

type network_simulation_config =
  | Scatter of int * int
  | Map of int * int * int
  | Disabled

val network_simulation_config_encoding :
  network_simulation_config Data_encoding.t

val simulate_network_to_string : network_simulation_config -> string

module DAL : sig
  type t = {
    blocks_history : int option;
    producer_key : string option;
    fundraiser : string option;
    network : Network.t option;
    simulate_network : network_simulation_config option;
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
