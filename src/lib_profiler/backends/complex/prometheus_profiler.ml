(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Profiler

type prometheus_state = {name : string; mutable state : Simple_profiler.state}

type prometheus_config = string * verbosity

type (_, _) kind += Prometheus : (prometheus_config, prometheus_state) kind

module Prometheus : DRIVER with type config = prometheus_config = struct
  type nonrec state = prometheus_state

  type config = prometheus_config

  let kind = Prometheus

  let encoding_case =
    Data_encoding.(
      case
        Json_only
        ~title:"prometheus"
        ~description:"Prometheus driver"
        (constant "prometheus")
        (function View Prometheus -> Some () | _ -> None)
        (fun () -> View Prometheus))

  let create (name, verbosity) : state =
    {state = Simple_profiler.empty verbosity; name}

  include Simple_profiler.Base (struct
    type t = prometheus_state

    let get_state t = t.state

    let set_state t s = t.state <- s

    (* prometheus backend will throw an error if you register the same name twice,
       hence the need for reusing metrics already defined *)
    let metric
        (maker :
          namespace:string ->
          subsystem:string ->
          label_name:string ->
          help:string ->
          string ->
          'a) help suffix =
      let ht = Stdlib.Hashtbl.create 0 in
      fun ~namespace ~subsystem ->
        let k = (namespace, subsystem) in
        match Stdlib.Hashtbl.find_opt ht k with
        | Some metric -> metric
        | None ->
            let metric =
              maker ~namespace ~subsystem ~label_name:"id" ~help suffix
            in
            Stdlib.Hashtbl.add ht k metric ;
            metric

    let counter =
      let maker ~namespace ~subsystem ~label_name ~help name =
        Prometheus.Counter.v_label ~namespace ~subsystem ~label_name ~help name
      in
      metric maker "Number of times a function is called" "count"

    let summary =
      let maker ~namespace ~subsystem ~label_name ~help name =
        Prometheus.Summary.v_label ~namespace ~subsystem ~label_name ~help name
      in
      metric
        maker
        "Total time passed in a function and number of times it has been called"
        "time"

    (** Because prometheus can't handle high cardinality in metrics names,
        we treat only those who are specifically marked for the prometheus backend. *)
    let output_entry subsystem (id, metadata) n d =
      let namespace = "profiling" in
      match Stdlib.List.assoc_opt "prometheus" metadata with
      | None -> ()
      | Some id' ->
          let id = if id' = "" then id else id' in
          if d.wall = 0. then
            Prometheus.Counter.inc
              (counter ~namespace ~subsystem id)
              (Float.of_int n)
          else
            Prometheus.Summary.observe
              (summary ~namespace ~subsystem id)
              ~n:(Float.of_int n)
              d.wall

    let output_report =
      let output t r =
        let rec output {recorded; aggregated} =
          (* For each node in both [aggregated] and [recorded] lists,
             we only output the children nodes if the parent has been marked for
             promotheus backend. This is similar to verbosity filtering where a child is
             only recorded if the parent is. *)
          IdMap.iter
            (fun id {count = n; total = Span d; node_verbosity = _; children} ->
              output_entry t.name id n d ;
              output {recorded = []; aggregated = children})
            aggregated ;
          List.iter
            (fun ( id,
                   {start = _; duration = Span d; contents; item_verbosity = _}
                 )
               ->
              output_entry t.name id 1 d ;
              output contents)
            recorded
        in
        output r
      in
      Some output
  end)

  let close _ = ()
end

let prometheus : prometheus_config Profiler.driver =
  (module Prometheus : DRIVER with type config = prometheus_config)

let instance_maker driver ~verbosity ~directory:_ ~name =
  Profiler.instance driver (name, verbosity)

let () =
  Profiler_instance.register_backend ["prometheus"] instance_maker prometheus
