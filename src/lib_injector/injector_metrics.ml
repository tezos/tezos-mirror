(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Prometheus

module type P = sig
  module Tag : Injector_sigs.TAG

  val registry : Prometheus.CollectorRegistry.t
end

module Make (P : P) = struct
  let produce_metrics = ref false

  let wrap f = if !produce_metrics then f ()

  let produce_metrics b = produce_metrics := b

  let registry = P.registry

  let namespace = Tezos_version.Octez_node_version.namespace

  let subsystem = "injector"

  (** Registers a labeled counter in [injector_registry] *)
  let v_label_counter = Counter.v_label ~registry ~namespace ~subsystem

  (** Registers a gauge in [injector_registry] *)
  let v_gauge = Gauge.v ~registry ~namespace ~subsystem

  (** Registers a labeled gauge in [injector_registry] *)
  let v_label_gauge = Gauge.v_labels ~registry ~namespace ~subsystem

  module Tags = Injector_tags.Make (P.Tag)

  module Gauge_table = Hashtbl.Make (struct
    include Tags

    let hash tags = Tags.fold (fun t acc -> (7 * acc) + P.Tag.hash t) tags 0
  end)

  let concat_tags tags =
    String.concat ", "
    @@ List.map (fun t -> Format.asprintf "%a" P.Tag.pp t) tags

  let batchers_balance_gauge_table = Gauge_table.create 7

  let queue_gauge_table = Gauge_table.create 7

  let included_operations_gauge_table = Gauge_table.create 7

  let injected_operations_gauge_table = Gauge_table.create 7

  let add_gauge i tags =
    let tags_str = concat_tags tags in
    let tags = Tags.of_list tags in
    let _label =
      v_label_counter
        ~help:(Format.sprintf "Injector for %s (%d)" tags_str i)
        ~label_name:"tags"
        (Format.sprintf "%d_tags" i)
        tags_str
    in
    let queue_gauge =
      v_gauge
        ~help:(Format.asprintf "injector worker %s (%d) queue size" tags_str i)
        (Format.asprintf "%d_queue_size" i)
    in
    Gauge_table.add queue_gauge_table tags queue_gauge ;
    let injected_operations_gauge =
      v_gauge
        ~help:
          (Format.asprintf
             "injector %s (%d) injected operations size"
             tags_str
             i)
        (Format.asprintf "%d_injected_size" i)
    in
    Gauge_table.add
      injected_operations_gauge_table
      tags
      injected_operations_gauge ;
    let included_operations_gauge =
      v_gauge
        ~help:
          (Format.asprintf
             "injector %s (%d) included operations size"
             tags_str
             i)
        (Format.asprintf "%d_included_size" i)
    in
    Gauge_table.add
      included_operations_gauge_table
      tags
      included_operations_gauge ;
    let batchers_balance_gauge =
      v_label_gauge
        ~help:(Format.sprintf "injector %s (%d) batcher balance" tags_str i)
        ~label_names:["pkh"; "tags"]
        (Format.asprintf "%d_batcher_balance" i)
    in
    Gauge_table.add batchers_balance_gauge_table tags batchers_balance_gauge

  let set_gauge_batcher_balance tags key balance =
    Option.iter
      (fun labeled_gauge ->
        Gauge.set
          (Gauge.labels labeled_gauge [key; concat_tags tags])
          (Int64.to_float balance /. 1_000_000.))
      (Gauge_table.find_opt batchers_balance_gauge_table (Tags.of_list tags))

  let set_gauge_find_opt table tags len =
    Option.iter
      (fun gauge -> Gauge.set gauge (Int.to_float len))
      (Gauge_table.find_opt table (Tags.of_list tags))

  let set_queue_size = set_gauge_find_opt queue_gauge_table

  let set_injected_operations_size =
    set_gauge_find_opt injected_operations_gauge_table

  let set_included_operations_size =
    set_gauge_find_opt included_operations_gauge_table
end
