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
    stake : Stake_repartition.Dal.t option;
    bakers : string list;
    stake_machine_type : string list;
    dal_producers_slot_indices : int list;
    producers : int option;
    producers_delay : int option;
    producer_machine_type : string option;
    observers_slot_indices : int list list;
    observer_machine_type : string list;
    archivers_slot_indices : int list list;
    observer_pkhs : string list;
    protocol : Protocol.t option;
    data_dir : string option;
    etherlink : bool option;
    etherlink_sequencer : bool option;
    etherlink_producers : int option;
    etherlink_chain_id : int option;
    echo_rollups : int option;
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
    disable_amplification : bool option;
    ignore_pkhs : string list;
    ppx_profiling_verbosity : string option;
    ppx_profiling_backends : string list;
    enable_network_health_monitoring : bool option;
    tezlink : bool option;
    slot_size : int option;
    number_of_slots : int option;
    attestation_lag : int option;
    traps_fraction : Q.t option;
    publish_slots_regularly : bool;
    stresstest : Stresstest.t option;
  }

  (** If [seed] is omitted from the JSON config, a random seed is chosen
      at decode time. *)
  val encoding : t Data_encoding.t
end

module LAYER1 : sig
  module Default : sig
    val maintenance_delay : int

    val ppx_profiling_backends : string list

    val without_dal : bool

    val auto_faketime : bool
  end

  (** Scenario configuration

    - [snapshot]: local path or URL of the snapshot to use for the experiment.
      local path implies to [scp] the snapshot to all the vms.

    - [stake]: stake repartition between baking nodes, numbers are relatives.

      [Manual [2,1,1]] runs 3 bakers, aggregate delegates from the network,
      spreading them in 3 pools representing roughly 50%, 25% and 25% of the
      total stake of the network. The same stake repartition using the same
      snapshot will result in the same delegate repartition.

      There is a special case using a single number instead of a list, which
      gives the number of bakers to use. Delegates will be distributed as
      evenly as possible between these bakers.

      [Auto] will spawn one baker per delegate, and the list of delegates will
      be automatically retrieved from the provided snapshot.

    - [maintenance_delay]: number of level which will be multiplied by the
      position in the list of the bakers to define the store merge delay.
      We want it to be the same for two runs with same parameters (not
      random) and we want it not to occur at the same time on every baker.
      Default value is 1.
      Use 0 for disabling delay and have all the bakers to merge their
      store at the beginning of cycles.

      - [migration]: is the parameters for protocol migration. The level offset
      (can be expressed in cycle) that dictates after how many levels a protocol
      upgrade will be performed via a UAU. And the number of level or cycle that
      should be baked after the migration until the experiment stops

    - [stresstest]: See the description of [stresstest_conf]

    - [auto_faketime]: is set to [true], use the snapshot info in order to
      calculate the right FAKETIME offset to use.
  *)
  type t = {
    stake : Stake_repartition.Layer1.t;
    network : Network.t;
    snapshot : Snapshot_helpers.t;
    stresstest : Stresstest.t option;
    without_dal : bool;
    dal_node_producers : int list option;
    maintenance_delay : int;
    migration : Protocol_migration.t option;
    ppx_profiling_verbosity : string option;
    ppx_profiling_backends : string list;
    signing_delay : (float * float) option;
    fixed_random_seed : int option;
    octez_release : string option;
    auto_faketime : bool;
  }

  val encoding : t Data_encoding.t
end
