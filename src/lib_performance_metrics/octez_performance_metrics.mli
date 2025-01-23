(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type data_dir_element = {path : string; metrics_suffix : string}

val data_dir_element : ?metrics_suffix:string -> string -> data_dir_element

module type REGISTRY = sig
  val registry : Prometheus.CollectorRegistry.t

  val subsystem : string

  val directories : data_dir_element list
end

module Make : functor (R : REGISTRY) -> sig
  val set_memory_cpu_stats : unit -> unit Lwt.t

  val set_disk_usage_stats : data_dir:string -> unit Lwt.t

  val set_file_descriptors : unit -> unit Lwt.t

  val set_stats : data_dir:string -> unit Lwt.t
end

(** [supports_performance_metrics ()] returns [true] if [du], [ps] and [lsof]
    are available on the platform. *)
val supports_performance_metrics : unit -> bool Lwt.t
