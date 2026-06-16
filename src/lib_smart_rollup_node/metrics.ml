(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Prometheus

let sc_rollup_node_registry = CollectorRegistry.create ()

let namespace = Tezos_version.Octez_node_version.namespace

let subsystem = "sc_rollup_node"

(** Registers a labeled counter in [sc_rollup_node_registry] *)
let v_labels_counter =
  Counter.v_labels ~registry:sc_rollup_node_registry ~namespace ~subsystem

let v_label_counter =
  Counter.v_label ~registry:sc_rollup_node_registry ~namespace ~subsystem

(** Registers a gauge in [sc_rollup_node_registry] *)
let v_gauge = Gauge.v ~registry:sc_rollup_node_registry ~namespace ~subsystem

let set_gauge help name f =
  let m = v_gauge ~help name in
  fun x -> Gauge.set m @@ f x

let v_label_gauge ~label_names ~help name =
  Gauge.v_labels
    ~registry:sc_rollup_node_registry
    ~namespace
    ~subsystem
    ~label_names
    ~help
    name

(** Registers a labeled gauge in [sc_rollup_node_registry] *)
let set_labeled_gauge ~family f ?(labels = []) x =
  Gauge.set (Gauge.labels family labels) (f x)

let process_metrics = ref false

let active_metrics (configuration : Configuration.t) =
  process_metrics := Option.is_some configuration.metrics_addr

let wrap f = if !process_metrics then f ()

let wrap_lwt f =
  if !process_metrics then f () else Lwt_result_syntax.return_unit

module Cohttp (Server : Cohttp_lwt.S.Server) = struct
  let callback _conn req _body =
    let open Cohttp in
    let open Lwt_syntax in
    let uri = Request.uri req in
    match (Request.meth req, Uri.path uri) with
    | `GET, "/metrics" ->
        let* data_sc = CollectorRegistry.(collect sc_rollup_node_registry) in
        let* data_injector = CollectorRegistry.(collect Injector.registry) in
        let data_merged =
          MetricFamilyMap.merge
            (fun _ v1 v2 -> match v1 with Some v1 -> Some v1 | _ -> v2)
            data_sc
            data_injector
        in
        let body =
          Fmt.to_to_string Prometheus_app.TextFormat_0_0_4.output data_merged
        in
        let headers =
          Header.init_with "Content-Type" "text/plain; version=0.0.4"
        in
        Server.respond_string ~status:`OK ~headers ~body ()
    | _ -> Server.respond_error ~status:`Bad_request ~body:"Bad request" ()
end

module Metrics_server = Cohttp (Cohttp_lwt_unix.Server)

let metrics_serve metrics_addr =
  let open Lwt_result_syntax in
  match metrics_addr with
  | Some metrics_addr ->
      let* addrs =
        Octez_node_config.Config_file.resolve_metrics_addrs
          ~default_metrics_port:Configuration.default_metrics_port
          metrics_addr
      in
      let*! () =
        List.iter_p
          (fun (addr, port) ->
            let host = Ipaddr.V6.to_string addr in
            let*! () = Event.starting_metrics_server ~host ~port in
            let*! ctx = Conduit_lwt_unix.init ~src:host () in
            let ctx = Cohttp_lwt_unix.Net.init ~ctx () in
            let mode = `TCP (`Port port) in
            let callback = Metrics_server.callback in
            Cohttp_lwt_unix.Server.create
              ~ctx
              ~mode
              (Cohttp_lwt_unix.Server.make ~callback ()))
          addrs
      in
      return_unit
  | None -> return_unit

let metric_type_to_string = function
  | Counter -> "Counter"
  | Gauge -> "Gauge"
  | Summary -> "Summary"
  | Histogram -> "Histogram"

let pp_label_names fmt =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt ";")
    (fun fmt v -> Format.fprintf fmt "%a" LabelName.pp v)
    fmt

let print_csv_metrics ppf metrics =
  Format.fprintf ppf "@[<v>Name,Type,Description,Labels" ;
  List.iter
    (fun (v, _) ->
      Format.fprintf
        ppf
        "@,@[%a@],%s,\"%s\",%a"
        MetricName.pp
        v.MetricInfo.name
        (metric_type_to_string v.MetricInfo.metric_type)
        v.MetricInfo.help
        pp_label_names
        v.MetricInfo.label_names)
    (MetricFamilyMap.to_list metrics) ;
  Format.fprintf ppf "@]@."

module Refutation = struct
  type state = OurTurn | TheirTurn | Timeout

  let state_to_float = function
    | OurTurn -> 0.
    | TheirTurn -> 1.
    | Timeout -> -1.

  let set_number_of_conflict =
    set_gauge "Number of conflicts" "number_of_conflicts" Int.to_float

  let family_state_of_refutation_game =
    v_label_gauge
      ~label_names:["opponent"; "start_level"]
      ~help:"State of refutation game"
      "state_of_refutation_game"

  let family_set_block_timeout =
    v_label_gauge
      ~label_names:["opponent"; "start_level"]
      ~help:"Number of block before player timeout"
      "block_timeout"

  let set_state_refutation_game =
    set_labeled_gauge ~family:family_state_of_refutation_game state_to_float

  let set_block_timeout =
    set_labeled_gauge ~family:family_set_block_timeout Int.to_float

  let clear_state_refutation_game labels =
    Gauge.clear_specific family_state_of_refutation_game labels ;
    Gauge.clear_specific family_set_block_timeout labels
end

module Info = struct
  open Tezos_version

  let set_genesis_level =
    set_gauge "Rollup genesis level" "genesis_level" Int32.to_float

  let node_general_info =
    v_labels_counter
      ~help:"General information on the node"
      ~label_names:["version"; "commit_hash"; "commit_date"]
      "node_info"

  let rollup_node_info =
    let help = "Rollup node info" in
    v_labels_counter
      ~help
      ~label_names:
        [
          "rollup_address";
          "mode";
          "genesis_level";
          "genesis_hash";
          "pvm_kind";
          "history_mode";
        ]
      "rollup_node_info"

  let protocol_label =
    let help = "Rollup node current protocol" in
    v_label_counter ~help ~label_name:"protocol" "protocol"

  let init_rollup_node_info (configuration : Configuration.t) ~genesis_level
      ~genesis_hash ~pvm_kind ~history_mode =
    let addr =
      Tezos_crypto.Hashed.Smart_rollup_address.to_b58check
        configuration.sc_rollup_address
    in
    let mode = Configuration.string_of_mode configuration.mode in
    let () = set_genesis_level genesis_level in
    let genesis_level = Int32.to_string genesis_level in
    let genesis_hash = Commitment.Hash.to_b58check genesis_hash in
    let history_mode = Configuration.string_of_history_mode history_mode in
    ignore
    @@ Counter.labels
         rollup_node_info
         [addr; mode; genesis_level; genesis_hash; pvm_kind; history_mode] ;
    ()

  let () =
    let version = Version.to_string Current_git_info.octez_version in
    let commit_hash = Current_git_info.commit_hash in
    let commit_date = Current_git_info.committer_date in
    let _ =
      Counter.labels node_general_info [version; commit_hash; commit_date]
    in
    ()

  let set_lcc_level_l1 =
    set_gauge
      "Last cemented commitment level on L1"
      "lcc_level_l1"
      Int32.to_float

  let set_lcc_level_local =
    set_gauge
      "Last cemented commitment level locally"
      "lcc_level_local"
      Int32.to_float

  let set_lpc_level_l1 =
    let gauge =
      v_gauge ~help:"Last published commitment on L1" "lpc_level_l1"
    in
    fun lpc ->
      let new_lpc = Int32.to_float lpc in
      let old_lpc = Gauge.read gauge in
      Gauge.set gauge (max new_lpc old_lpc)

  let set_lpc_level_local =
    let gauge =
      v_gauge ~help:"Last published commitment by operator" "lpc_level_local"
    in
    fun lpc ->
      let new_lpc = Int32.to_float lpc in
      let old_lpc = Gauge.read gauge in
      Gauge.set gauge (max new_lpc old_lpc)

  let set_commitment_period =
    set_gauge "Current commitment period" "commitment_period" float_of_int

  let set_challenge_window =
    set_gauge "Current challenge window" "challenge_window" float_of_int

  let set_dal_enabled =
    set_gauge "DAL enabled in protocol" "dal_enabled" (function
      | true -> 1.
      | false -> 0.)

  let set_dal_attestation_lag =
    set_gauge "DAL attestation lag" "dal_attestation_lag" float_of_int

  let set_dal_number_of_slots =
    set_gauge "DAL number of slots" "dal_number_of_slots" float_of_int

  let set_proto_info protocol (constants : Rollup_constants.protocol_constants)
      =
    let proto = Protocol_hash.to_b58check protocol in
    ignore (protocol_label proto) ;
    set_commitment_period constants.sc_rollup.commitment_period_in_blocks ;
    set_challenge_window constants.sc_rollup.challenge_window_in_blocks ;
    set_dal_enabled constants.dal.feature_enable ;
    set_dal_attestation_lag constants.dal.attestation_lag ;
    set_dal_number_of_slots constants.dal.number_of_slots ;
    ()

  let () =
    let version = Version.to_string Current_git_info.octez_version in
    let commit_hash = Current_git_info.commit_hash in
    let commit_date = Current_git_info.committer_date in
    let _ =
      Counter.labels node_general_info [version; commit_hash; commit_date]
    in
    ()
end

module Inbox = struct
  let set_head_level =
    set_gauge "Level of last inbox" "inbox_level" Int32.to_float

  let internal_messages_number =
    v_gauge
      ~help:"Number of internal messages in inbox"
      "inbox_internal_messages_number"

  let external_messages_number =
    v_gauge
      ~help:"Number of external messages in inbox"
      "inbox_external_messages_number"

  let set_messages ~is_internal l =
    let internal, external_ =
      List.fold_left
        (fun (internal, external_) x ->
          if is_internal x then (internal +. 1., external_)
          else (internal, external_ +. 1.))
        (0., 0.)
        l
    in
    Gauge.set internal_messages_number internal ;
    Gauge.set external_messages_number external_

  let set_process_time =
    set_gauge
      "The time the rollup node spent processing the head"
      "inbox_process_time"
      Ptime.Span.to_float_s

  let set_fetch_time =
    set_gauge
      "The time the rollup node spent fetching the inbox"
      "inbox_fetch_time"
      Ptime.Span.to_float_s

  let set_total_time =
    set_gauge
      "The total time the rollup node spent handling the inbox"
      "inbox_total_time"
      Ptime.Span.to_float_s
end

module GC = struct
  let set_process_time =
    set_gauge "GC processing time" "gc_process_time" Ptime.Span.to_float_s

  let set_oldest_available_level =
    set_gauge
      "Oldest Available Level after GC"
      "gc_oldest_available_level"
      Int32.to_float
end

module Batcher = struct
  let set_get_time =
    set_gauge "Time to fetch batches" "batcher_get_time" Ptime.Span.to_float_s

  let set_inject_time =
    set_gauge
      "Time to inject batches"
      "batcher_inject_time"
      Ptime.Span.to_float_s

  let set_messages_queue_size =
    set_gauge
      "Batcher message queue size"
      "batcher_message_queue_size"
      Int.to_float

  let set_messages_size =
    set_gauge
      "Batcher messages size in batches"
      "batcher_messages_size"
      Int.to_float

  let set_batches_size =
    set_gauge "Batcher batches sent" "batcher_batches_size" Int.to_float

  let set_last_batch_level =
    set_gauge "Last batch level" "batcher_last_batch_level" Int32.to_float

  let set_last_batch_time =
    set_gauge "Last batch time" "batcher_last_batch_time" Ptime.to_float_s
end

module DAL_batcher = struct
  let set_dal_batcher_queue_length =
    set_gauge
      "Number of messages waiting for publication on the DAL"
      "dal_batcher_queue_length"
      Int.to_float

  let set_dal_injections_queue_length =
    set_gauge
      "Number of recently published DAL slots, who have not yet been forgotten"
      "dal_injections_queue_length"
      Int.to_float
end

module Riscv = struct
  let live_states_metric =
    MetricInfo.v
      ~metric_type:Gauge
      ~namespace
      ~subsystem
      ~label_names:[LabelName.v "kind"]
      ~help:"Number of live RISC-V PVM states"
      "riscv_states"

  let live_states_collector () =
    let immutable =
      Octez_riscv_pvm.Storage.State.get_live_count () |> float_of_int
    in
    let mutable_ =
      Octez_riscv_pvm.Storage.Mutable_state.get_live_count () |> float_of_int
    in
    LabelSetMap.of_list
      [
        (["immutable"], [Sample_set.sample immutable]);
        (["mutable"], [Sample_set.sample mutable_]);
      ]

  (* Register collector: values are sampled fresh on each /metrics scrape *)
  let () =
    CollectorRegistry.register
      sc_rollup_node_registry
      live_states_metric
      live_states_collector
end

module Performance_metrics_config = struct
  open Octez_performance_metrics

  let registry = sc_rollup_node_registry

  let subsystem = "sc_rollup_node"

  let directories =
    [
      data_dir_element "storage";
      data_dir_element "context";
      data_dir_element ~metrics_suffix:"wasm" "wasm_2_0_0";
      data_dir_element ~metrics_suffix:"logs" "daily_logs";
    ]
end

module type PERFORMANCE = sig
  val set_stats : data_dir:string -> unit Lwt.t
end

let performance_metrics : (module PERFORMANCE) Lazy.t =
  lazy
    (let module M = Octez_performance_metrics.Make (Performance_metrics_config) in
    (module M : PERFORMANCE))

let listing ~disable_performance_metrics =
  let open Lwt_syntax in
  let* () =
    if not disable_performance_metrics then (
      let+ supports =
        Octez_performance_metrics.supports_performance_metrics ()
      in
      if supports then ignore (Lazy.force_val performance_metrics))
    else return_unit
  in
  let* data_sc = CollectorRegistry.(collect sc_rollup_node_registry) in
  let+ data_injector = CollectorRegistry.(collect Injector.registry) in
  let data_merged =
    MetricFamilyMap.merge
      (fun _ v1 v2 -> match v1 with Some v1 -> Some v1 | _ -> v2)
      data_sc
      data_injector
  in
  let body =
    Fmt.to_to_string Prometheus_app.TextFormat_0_0_4.output data_merged
  in
  let metrics =
    String.split_on_char '\n' body
    |> List.filter (String.starts_with ~prefix:"#")
  in
  String.concat "\n" metrics
