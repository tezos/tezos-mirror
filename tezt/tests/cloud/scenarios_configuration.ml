(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type network_simulation_config =
  | Scatter of int * int
  | Map of int * int * int
  | Disabled

let network_simulation_config_encoding =
  let open Data_encoding in
  union
    [
      case
        (Tag 1)
        ~title:"scatter"
        (tup2 int31 int31)
        (function
          | Scatter (nb_keys, nb_daemons) -> Some (nb_keys, nb_daemons)
          | _ -> None)
        (fun (nb_keys, nb_daemons) -> Scatter (nb_keys, nb_daemons));
      case
        (Tag 2)
        ~title:"map"
        (tup3 int31 int31 int31)
        (function
          | Map (nb_keys, nb_alone_bakers, nb_additional_daemons) ->
              Some (nb_keys, nb_alone_bakers, nb_additional_daemons)
          | _ -> None)
        (fun (nb_keys, nb_alone_bakers, nb_additional_daemons) ->
          Map (nb_keys, nb_alone_bakers, nb_additional_daemons));
      case
        (Tag 3)
        ~title:"disabled"
        empty
        (function Disabled -> Some () | _ -> None)
        (fun () -> Disabled);
    ]

let simulate_network_to_string = function
  | Scatter (x, y) -> Format.sprintf "scatter(%d,%d)" x y
  | Map (x, y, z) -> Format.sprintf "map(%d,%d,%d)" x y z
  | Disabled -> Format.sprintf "disabled"

module DAL = struct
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
    ppx_profiling : bool option;
    ppx_profiling_backends : string list;
    enable_network_health_monitoring : bool option;
    tezlink : bool option;
  }

  let encoding =
    let open Data_encoding in
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
             observer_slot_indices;
             observer_pkhs;
             protocol;
             data_dir;
             etherlink;
             etherlink_sequencer;
             etherlink_producers;
             etherlink_chain_id;
             echo_rollup;
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
             ignore_pkhs;
             ppx_profiling;
             ppx_profiling_backends;
             enable_network_health_monitoring;
             tezlink;
           }
         ->
        ( ( ( blocks_history,
              producer_key,
              fundraiser,
              network,
              simulate_network,
              snapshot,
              bootstrap,
              stake,
              bakers,
              stake_machine_type ),
            ( dal_producers_slot_indices,
              producers,
              producers_delay,
              producer_machine_type,
              observer_slot_indices,
              observer_pkhs,
              protocol,
              data_dir,
              etherlink,
              etherlink_sequencer ) ),
          ( ( etherlink_producers,
              etherlink_chain_id,
              echo_rollup,
              disconnect,
              etherlink_dal_slots,
              teztale,
              octez_release,
              memtrace,
              bootstrap_node_identity_file,
              bootstrap_dal_node_identity_file ),
            ( refresh_binaries,
              node_external_rpc_server,
              with_dal,
              proxy_localhost,
              disable_shard_validation,
              ignore_pkhs,
              ppx_profiling,
              ppx_profiling_backends,
              enable_network_health_monitoring,
              tezlink ) ) ))
      (fun ( ( ( blocks_history,
                 producer_key,
                 fundraiser,
                 network,
                 simulate_network,
                 snapshot,
                 bootstrap,
                 stake,
                 bakers,
                 stake_machine_type ),
               ( dal_producers_slot_indices,
                 producers,
                 producers_delay,
                 producer_machine_type,
                 observer_slot_indices,
                 observer_pkhs,
                 protocol,
                 data_dir,
                 etherlink,
                 etherlink_sequencer ) ),
             ( ( etherlink_producers,
                 etherlink_chain_id,
                 echo_rollup,
                 disconnect,
                 etherlink_dal_slots,
                 teztale,
                 octez_release,
                 memtrace,
                 bootstrap_node_identity_file,
                 bootstrap_dal_node_identity_file ),
               ( refresh_binaries,
                 node_external_rpc_server,
                 with_dal,
                 proxy_localhost,
                 disable_shard_validation,
                 ignore_pkhs,
                 ppx_profiling,
                 ppx_profiling_backends,
                 enable_network_health_monitoring,
                 tezlink ) ) )
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
          observer_slot_indices;
          observer_pkhs;
          protocol;
          data_dir;
          etherlink;
          etherlink_sequencer;
          etherlink_producers;
          etherlink_chain_id;
          echo_rollup;
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
          ignore_pkhs;
          ppx_profiling;
          ppx_profiling_backends;
          enable_network_health_monitoring;
          tezlink;
        })
      (merge_objs
         (merge_objs
            (obj10
               (opt "blocks_history" int31)
               (opt "producer_key" string)
               (opt "fundraiser" string)
               (opt "network" Network.encoding)
               (opt "simulate_network" network_simulation_config_encoding)
               (opt "snapshot" Snapshot_helpers.encoding)
               (opt "bootstrap" bool)
               (opt "stake" Network.stake_repartition_encoding)
               (dft "bakers" (list string) [])
               (dft "stake_machine_type" (list string) []))
            (obj10
               (dft "dal_producers_slot_indices" (list int31) [])
               (opt "producers" int31)
               (opt "producers_delay" int31)
               (opt "producer_machine_type" string)
               (dft "observer_slot_indices" (list int31) [])
               (dft "observer_pkhs" (list string) [])
               (opt "protocol" Protocol.encoding)
               (opt "data_dir" string)
               (opt "etherlink" bool)
               (opt "etherlink_sequencer" bool)))
         (merge_objs
            (obj10
               (opt "etherlink_producers" int31)
               (opt "etherlink_chain_id" int31)
               (opt "echo_rollup" bool)
               (opt "disconnect" (tup2 int31 int31))
               (dft "etherlink_dal_slots" (list int31) [])
               (opt "teztale" bool)
               (opt "octez_release" string)
               (opt "memtrace" bool)
               (opt "bootstrap_node_identity_file" string)
               (opt "bootstrap_dal_node_identity_file" string))
            (obj10
               (opt "refresh_binaries" bool)
               (opt "node_external_rpc_server" bool)
               (opt "with_dal" bool)
               (opt "proxy_localhost" bool)
               (opt "disable_shard_validation" bool)
               (dft "ignore_pkhs" (list string) [])
               (opt "ppx_profiling" bool)
               (dft "ppx_profiling_backends" (list string) [])
               (opt "enable_network_health_monitoring" bool)
               (opt "tezlink" bool))))
end
