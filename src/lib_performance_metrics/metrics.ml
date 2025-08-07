(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Prometheus
open Sigs

let namespace = Tezos_version.Octez_node_version.namespace

module type REGISTRY = sig
  val registry : CollectorRegistry.t

  val subsystem : string

  val directories : data_dir_element list
end

module Make (R : REGISTRY) = struct
  include R

  let v_counter = Counter.v ~registry ~namespace ~subsystem

  let v_gauge = Gauge.v ~registry ~namespace ~subsystem

  let virtual_ = v_gauge ~help:"Size Memory Stats" "performance_virtual"

  let resident = v_gauge ~help:"Resident Memory Stats" "performance_resident"

  let memp = v_gauge ~help:"Memory Percentage" "performance_mem_percentage"

  let cpu = v_gauge ~help:"CPU Percentage" "performance_cpu_percentage"

  let elapsed_time =
    v_counter
      ~help:"Number of seconds since the node is running"
      "performance_elapsed_time"

  let data = v_gauge ~help:"Disk Usage" "performance_data"

  let percentage =
    v_gauge ~help:"Disk Usage Percentage" "performance_disk_percentage"

  let file_descriptors =
    v_gauge ~help:"Open file descriptors" "performance_file_descriptors"

  let connections = v_gauge ~help:"Open connections" "performance_connections"

  let directories_gauges =
    let open Format in
    List.map
      (fun data_dir_elem ->
        ( data_dir_elem.path,
          v_gauge
            ~help:(sprintf "Disk usage: %s" data_dir_elem.path)
            (sprintf "performance_%s" data_dir_elem.metrics_suffix) ))
      directories
end
