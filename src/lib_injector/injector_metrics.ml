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

  let signer_balance_gauge_table = Gauge_table.create 7

  let queue_gauge_table = Gauge_table.create 7

  let included_operations_gauge_table = Gauge_table.create 7

  let injected_operations_gauge_table = Gauge_table.create 7

  let queue_gauge =
    v_label_gauge
      ~help:"injector worker queue size"
      ~label_names:["tags"; "index"]
      "queue_size"

  let injected_operations_gauge =
    v_label_gauge
      ~help:"injector injected operations size"
      ~label_names:["tags"; "index"]
      "injected_size"

  let included_operations_gauge =
    v_label_gauge
      ~help:"injector included operations size"
      ~label_names:["tags"; "index"]
      "included_size"

  let signer_balance_gauge =
    v_label_gauge
      ~help:"injector signer balance"
      ~label_names:["tags"; "index"; "pkh"]
      "signer_balance"

  let add_gauge i tags =
    let tags = Tags.of_list tags in
    Gauge_table.add queue_gauge_table tags (i, queue_gauge) ;
    Gauge_table.add
      injected_operations_gauge_table
      tags
      (i, injected_operations_gauge) ;
    Gauge_table.add
      included_operations_gauge_table
      tags
      (i, included_operations_gauge) ;
    Gauge_table.add signer_balance_gauge_table tags (i, signer_balance_gauge)

  let set_gauge_signer_balance tags key balance =
    Option.iter
      (fun (i, labeled_gauge) ->
        Gauge.set
          (Gauge.labels labeled_gauge [concat_tags tags; Int.to_string i; key])
          (Int64.to_float balance /. 1_000_000.))
      (Gauge_table.find_opt signer_balance_gauge_table (Tags.of_list tags))

  let set_labeled_gauge_find_opt table tags len =
    Option.iter
      (fun (i, labeled_gauge) ->
        Gauge.set
          (Gauge.labels labeled_gauge [concat_tags tags; Int.to_string i])
          (Int.to_float len))
      (Gauge_table.find_opt table (Tags.of_list tags))

  let set_queue_size = set_labeled_gauge_find_opt queue_gauge_table

  let set_injected_operations_size =
    set_labeled_gauge_find_opt injected_operations_gauge_table

  let set_included_operations_size =
    set_labeled_gauge_find_opt included_operations_gauge_table
end
