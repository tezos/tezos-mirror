(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module DAL = struct
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

  let encoding =
    let open Data_encoding in
    let q_encoding =
      conv
        (fun Q.{num; den} -> (num, den))
        (fun (num, den) -> Q.make num den)
        (obj2 (req "numerator" z) (req "denominator" z))
    in
    conv
      (fun {
             blocks_history;
             producer_key;
             fundraiser;
             network;
             simulate_network;
             snapshot;
             bootstrap;
             stake;
             bakers;
             stake_machine_type;
             dal_producers_slot_indices;
             producers;
             producers_delay;
             producer_machine_type;
             observers_slot_indices;
             observer_machine_type;
             archivers_slot_indices;
             observer_pkhs;
             protocol;
             data_dir;
             etherlink;
             etherlink_sequencer;
             etherlink_producers;
             etherlink_chain_id;
             echo_rollups;
             disconnect;
             etherlink_dal_slots;
             teztale;
             octez_release;
             memtrace;
             bootstrap_node_identity_file;
             bootstrap_dal_node_identity_file;
             refresh_binaries;
             node_external_rpc_server;
             with_dal;
             proxy_localhost;
             disable_shard_validation;
             disable_amplification;
             ignore_pkhs;
             ppx_profiling_verbosity;
             ppx_profiling_backends;
             enable_network_health_monitoring;
             tezlink;
             slot_size;
             number_of_slots;
             attestation_lag;
             traps_fraction;
             publish_slots_regularly;
             stresstest;
           }
         ->
        ( ( ( ( blocks_history,
                producer_key,
                fundraiser,
                network,
                simulate_network,
                snapshot,
                bootstrap,
                stake,
                bakers,
                stake_machine_type ),
              ( ( dal_producers_slot_indices,
                  producers,
                  producers_delay,
                  producer_machine_type,
                  observers_slot_indices,
                  observer_machine_type,
                  archivers_slot_indices,
                  observer_pkhs ),
                (protocol, data_dir, etherlink, etherlink_sequencer) ) ),
            ( ( etherlink_producers,
                etherlink_chain_id,
                echo_rollups,
                disconnect,
                etherlink_dal_slots,
                teztale,
                octez_release,
                memtrace,
                bootstrap_node_identity_file,
                bootstrap_dal_node_identity_file ),
              ( ( refresh_binaries,
                  node_external_rpc_server,
                  with_dal,
                  proxy_localhost,
                  disable_shard_validation,
                  disable_amplification,
                  ignore_pkhs,
                  ppx_profiling_verbosity,
                  ppx_profiling_backends,
                  enable_network_health_monitoring ),
                tezlink ) ) ),
          ( slot_size,
            number_of_slots,
            attestation_lag,
            traps_fraction,
            publish_slots_regularly,
            stresstest ) ))
      (fun ( ( ( ( blocks_history,
                   producer_key,
                   fundraiser,
                   network,
                   simulate_network,
                   snapshot,
                   bootstrap,
                   stake,
                   bakers,
                   stake_machine_type ),
                 ( ( dal_producers_slot_indices,
                     producers,
                     producers_delay,
                     producer_machine_type,
                     observers_slot_indices,
                     observer_machine_type,
                     archivers_slot_indices,
                     observer_pkhs ),
                   (protocol, data_dir, etherlink, etherlink_sequencer) ) ),
               ( ( etherlink_producers,
                   etherlink_chain_id,
                   echo_rollups,
                   disconnect,
                   etherlink_dal_slots,
                   teztale,
                   octez_release,
                   memtrace,
                   bootstrap_node_identity_file,
                   bootstrap_dal_node_identity_file ),
                 ( ( refresh_binaries,
                     node_external_rpc_server,
                     with_dal,
                     proxy_localhost,
                     disable_shard_validation,
                     disable_amplification,
                     ignore_pkhs,
                     ppx_profiling_verbosity,
                     ppx_profiling_backends,
                     enable_network_health_monitoring ),
                   tezlink ) ) ),
             ( slot_size,
               number_of_slots,
               attestation_lag,
               traps_fraction,
               publish_slots_regularly,
               stresstest ) )
         ->
        {
          blocks_history;
          producer_key;
          fundraiser;
          network;
          simulate_network;
          snapshot;
          bootstrap;
          stake;
          bakers;
          stake_machine_type;
          dal_producers_slot_indices;
          producers;
          producers_delay;
          producer_machine_type;
          observers_slot_indices;
          observer_machine_type;
          archivers_slot_indices;
          observer_pkhs;
          protocol;
          data_dir;
          etherlink;
          etherlink_sequencer;
          etherlink_producers;
          etherlink_chain_id;
          echo_rollups;
          disconnect;
          etherlink_dal_slots;
          teztale;
          octez_release;
          memtrace;
          bootstrap_node_identity_file;
          bootstrap_dal_node_identity_file;
          refresh_binaries;
          node_external_rpc_server;
          with_dal;
          proxy_localhost;
          disable_shard_validation;
          disable_amplification;
          ignore_pkhs;
          ppx_profiling_verbosity;
          ppx_profiling_backends;
          enable_network_health_monitoring;
          tezlink;
          slot_size;
          number_of_slots;
          attestation_lag;
          traps_fraction;
          publish_slots_regularly;
          stresstest;
        })
      (merge_objs
         (merge_objs
            (merge_objs
               (obj10
                  (opt "blocks_history" int31)
                  (opt "producer_key" string)
                  (opt "fundraiser" string)
                  (opt "network" Network.encoding)
                  (opt "simulate_network" Network_simulation.encoding)
                  (opt "snapshot" Snapshot_helpers.encoding)
                  (opt "bootstrap" bool)
                  (opt "stake" Stake_repartition.Dal.encoding)
                  (dft "bakers" (list string) [])
                  (dft "stake_machine_type" (list string) []))
               (merge_objs
                  (obj8
                     (dft "dal_producers_slot_indices" (list int31) [])
                     (opt "producers" int31)
                     (opt "producers_delay" int31)
                     (opt "producer_machine_type" string)
                     (dft "observers_slot_indices" (list (list int31)) [])
                     (dft "observer_machine_type" (list string) [])
                     (dft "archivers_slot_indices" (list (list int31)) [])
                     (dft "observer_pkhs" (list string) []))
                  (obj4
                     (opt "protocol" Protocol.encoding)
                     (opt "data_dir" string)
                     (opt "etherlink" bool)
                     (opt "etherlink_sequencer" bool))))
            (merge_objs
               (obj10
                  (opt "etherlink_producers" int31)
                  (opt "etherlink_chain_id" int31)
                  (opt "echo_rollups" int31)
                  (opt "disconnect" (tup2 int31 int31))
                  (dft "etherlink_dal_slots" (list int31) [])
                  (opt "teztale" bool)
                  (opt "octez_release" string)
                  (opt "memtrace" bool)
                  (opt "bootstrap_node_identity_file" string)
                  (opt "bootstrap_dal_node_identity_file" string))
               (merge_objs
                  (obj10
                     (opt "refresh_binaries" bool)
                     (opt "node_external_rpc_server" bool)
                     (opt "with_dal" bool)
                     (opt "proxy_localhost" bool)
                     (opt "disable_shard_validation" bool)
                     (opt "disable_amplification" bool)
                     (dft "ignore_pkhs" (list string) [])
                     (opt "ppx_profiling_verbosity" string)
                     (dft "ppx_profiling_backends" (list string) [])
                     (opt "enable_network_health_monitoring" bool))
                  (obj1 (opt "tezlink" bool)))))
         (obj6
            (opt "slot_size" int31)
            (opt "number_of_slots" int31)
            (opt "attestation_lag" int31)
            (opt "traps_fraction" q_encoding)
            (dft "publish_slots_regularly" bool false)
            (opt "stresstest" Stresstest.encoding)))
end

module LAYER1 = struct
  module Default = struct
    let maintenance_delay = 1

    let ppx_profiling_backends = ["txt"]

    let without_dal = false

    let auto_faketime = true
  end

  type t = {
    stake : Stake_repartition.Layer1.t;
    network : Network.t;
    snapshot : Snapshot_helpers.t;
    stresstest : Stresstest.t option;
    without_dal : bool;
    dal_node_producers : int list option;
    maintenance_delay : int;
    migration_offset : int option;
    ppx_profiling_verbosity : string option;
    ppx_profiling_backends : string list;
    signing_delay : (float * float) option;
    fixed_random_seed : int option;
    octez_release : string option;
    auto_faketime : bool;
  }

  let encoding =
    let open Data_encoding in
    conv
      (fun {
             stake;
             network;
             snapshot;
             stresstest;
             without_dal;
             dal_node_producers;
             maintenance_delay;
             migration_offset;
             ppx_profiling_verbosity;
             ppx_profiling_backends;
             signing_delay;
             fixed_random_seed;
             octez_release;
             auto_faketime;
           }
         ->
        ( ( stake,
            network,
            snapshot,
            stresstest,
            without_dal,
            dal_node_producers,
            maintenance_delay,
            migration_offset,
            ppx_profiling_verbosity,
            ppx_profiling_backends ),
          (signing_delay, fixed_random_seed, octez_release, auto_faketime) ))
      (fun ( ( stake,
               network,
               snapshot,
               stresstest,
               without_dal,
               dal_node_producers,
               maintenance_delay,
               migration_offset,
               ppx_profiling_verbosity,
               ppx_profiling_backends ),
             (signing_delay, fixed_random_seed, octez_release, auto_faketime) )
         ->
        {
          stake;
          network;
          snapshot;
          stresstest;
          without_dal;
          dal_node_producers;
          maintenance_delay;
          migration_offset;
          ppx_profiling_verbosity;
          ppx_profiling_backends;
          signing_delay;
          fixed_random_seed;
          octez_release;
          auto_faketime;
        })
      (merge_objs
         (obj10
            (dft "stake" Stake_repartition.Layer1.encoding (Manual [1]))
            (req "network" Network.encoding)
            (req "snapshot" Snapshot_helpers.encoding)
            (opt "stresstest" Stresstest.encoding)
            (dft "without_dal" bool Default.without_dal)
            (opt "dal_node_producers" (list int31))
            (dft "maintenance_delay" int31 Default.maintenance_delay)
            (opt "migration_offset" int31)
            (opt "ppx_profiling_verbosity" string)
            (dft
               "ppx_profiling_backends"
               (list string)
               Default.ppx_profiling_backends))
         (obj4
            (opt "signing_delay" (tup2 float float))
            (opt "fixed_random_seed" int31)
            (opt "octez_release" string)
            (dft "auto_faketime" bool Default.auto_faketime)))
end
