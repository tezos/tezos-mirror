(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type data_dir_element = {path : string; metrics_suffix : string}

let data_dir_element ?metrics_suffix path =
  let metrics_suffix = Option.value ~default:path metrics_suffix in
  {path; metrics_suffix}

module type REGISTRY = sig
  val registry : Prometheus.CollectorRegistry.t

  val subsystem : string

  val directories : data_dir_element list
end

module type MONAD = sig
  type 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val return : 'a -> 'a t

  val both : 'a t -> 'b t -> ('a * 'b) t

  val list_map_p : ('a -> 'b t) -> 'a list -> 'b list t
end

module type PROCESS = sig
  type 'a monad

  val get_ps : int -> (float * float * int * int) option monad

  val directory_size : string -> int64 option monad

  val get_disk_usage_percentage : string -> int64 option monad

  val get_file_descriptors : int -> (int * int) option monad
end

module type S = sig
  type 'a monad

  module Make : functor (R : REGISTRY) -> sig
    val set_memory_cpu_stats : unit -> unit monad

    val set_disk_usage_stats : data_dir:string -> unit monad

    val set_file_descriptors : unit -> unit monad

    val set_stats : data_dir:string -> unit monad
  end

  (** [supports_performance_metrics ()] returns [true] if [du], [ps] and [lsof]
    are available on the platform. *)
  val supports_performance_metrics : unit -> bool monad
end
