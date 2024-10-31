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
    set_gauge "Last published commitment on L1" "lpc_level_l1" Int32.to_float

  let set_lpc_level_local =
    set_gauge
      "Last published commitment by operator"
      "lpc_level_local"
      Int32.to_float

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

module Performance = struct
  let virtual_ = v_gauge ~help:"Size Memory Stats" "performance_virtual"

  let resident = v_gauge ~help:"Resident Memory Stats" "performance_resident"

  let memp = v_gauge ~help:"Memory Percentage" "performance_mem_percentage"

  let cpu = v_gauge ~help:"CPU Percentage" "performance_cpu_percentage"

  let get_ps pid =
    Lwt.catch
      (fun () ->
        let open Lwt_syntax in
        let+ s =
          Lwt_process.with_process_in
            ~env:[|"LC_ALL=C"|]
            ("ps", [|"ps"; "-p"; string_of_int pid; "-o"; "%cpu,%mem,vsz,rss"|])
            (fun pc ->
              let* s = Lwt_io.read_line_opt pc#stdout in
              match s with
              | None -> return_none
              | Some _ ->
                  (* skip header *)
                  Lwt_io.read_line_opt pc#stdout)
        in
        match Option.map (String.split_no_empty ' ') s with
        | Some [cpu; memp; virt; res] -> (
            try
              Some
                ( float_of_string cpu,
                  float_of_string memp,
                  int_of_string virt,
                  int_of_string res )
            with _ -> None)
        | _ -> None)
      (function _exn -> Lwt.return None)

  let set_memory_cpu_stats () =
    let open Lwt_syntax in
    let pid = Unix.getpid () in
    let+ stats = get_ps pid in
    Option.iter
      (fun (cpu_percent, mem_percent, virt, res) ->
        (* ps results are in kB, we show them as GB *)
        Gauge.set virtual_ (float virt /. (1024. *. 1024.)) ;
        Gauge.set resident (float res /. (1024. *. 1024.)) ;
        Gauge.set cpu cpu_percent ;
        Gauge.set memp mem_percent)
      stats

  let directory_size path =
    Lwt.catch
      (fun () ->
        let open Lwt_syntax in
        let+ s =
          Lwt_process.with_process_in
            ~env:[|"LC_ALL=C"|]
            ("du", [|"du"; "-sk"; path|])
            (fun pc -> Lwt_io.read_line pc#stdout)
        in
        match String.split_on_char '\t' s with
        | [] -> None
        | h :: _ -> Int64.of_string_opt h)
      (function _exn -> Lwt.return None)

  let get_disk_usage_percentage path =
    Lwt.catch
      (fun () ->
        let open Lwt_syntax in
        let+ s =
          Lwt_process.with_process_in
            ("df", [|"df"; path|])
            (fun pc ->
              let _ = Lwt_io.read_line pc#stdout in
              Lwt_io.read_line pc#stdout)
        in
        let l = Str.split (Str.regexp "[ ]+") s in
        let h = List.nth_opt l 4 in
        match h with
        | Some str ->
            let len = String.length str in
            let e = String.sub str 0 (len - 1) in
            Int64.of_string_opt e
        | None -> None)
      (function _exn -> Lwt.return None)

  let storage = v_gauge ~help:"Storage Disk Usage" "performance_storage"

  let context = v_gauge ~help:"Context Disk Usage" "performance_context"

  let logs = v_gauge ~help:"Logs Disk Usage" "performance_logs"

  let data = v_gauge ~help:"Data Disk Usage" "performance_data"

  let wasm = v_gauge ~help:"Wasm Disk Usage" "performance_wasm"

  let percentage =
    v_gauge ~help:"Disk Usage Percentage" "performance_disk_percentage"

  let set_disk_usage_stats data_dir =
    let open Lwt_syntax in
    let* disk_percentage = get_disk_usage_percentage data_dir in
    let* storage_size = directory_size @@ Filename.concat data_dir "storage" in
    let* context_size = directory_size @@ Filename.concat data_dir "context" in
    let* daily_logs_size =
      directory_size @@ Filename.concat data_dir "daily_logs"
    in
    let* preimages_size =
      directory_size @@ Filename.concat data_dir "wasm_2_0_0"
    in
    let total_size =
      List.fold_left
        (fun acc size ->
          match size with None -> acc | Some size -> Int64.add acc size)
        0L
        [storage_size; context_size; daily_logs_size; preimages_size]
    in
    (* du results are in kB, we show them as GB *)
    let aux gauge s =
      Gauge.set gauge @@ (Int64.to_float s /. (1024. *. 1024.))
    in
    Option.iter (aux storage) storage_size ;
    Option.iter (aux context) context_size ;
    Option.iter (aux logs) daily_logs_size ;
    Option.iter (aux wasm) preimages_size ;
    Option.iter
      (fun s -> Gauge.set percentage @@ Int64.to_float s)
      disk_percentage ;
    aux data total_size ;
    return_unit

  let file_descriptors =
    v_gauge ~help:"Open file descriptors" "performance_file_descriptors"

  let connections = v_gauge ~help:"Open connections" "performance_connections"

  let get_file_descriptors pid =
    Lwt.catch
      (fun () ->
        let open Lwt_syntax in
        let+ fd, conn =
          Lwt_process.with_process_in
            ~env:[|"LC_ALL=C"|]
            ("lsof", [|"lsof"; "-wap"; string_of_int pid|])
            (fun pc ->
              let rec count fd conn =
                Lwt.catch
                  (fun () ->
                    let* s = Lwt_io.read_line pc#stdout in
                    let l = String.split_on_char ' ' s in
                    let conn =
                      if List.mem ~equal:String.equal "TCP" l then conn + 1
                      else conn
                    in
                    count (fd + 1) conn)
                  (fun _ -> return (fd, conn))
              in
              count 0 0)
        in
        Some (fd - 1, conn))
      (function _exn -> Lwt.return None)

  let set_file_descriptors () =
    let open Lwt_syntax in
    let pid = Unix.getpid () in
    let+ r = get_file_descriptors pid in
    Option.iter
      (fun (fd, conn) ->
        Gauge.set file_descriptors @@ Float.of_int fd ;
        Gauge.set connections @@ Float.of_int conn)
      r

  let set_stats data_dir =
    let open Lwt_syntax in
    let* () = set_memory_cpu_stats ()
    and* () = set_disk_usage_stats data_dir
    and* () = set_file_descriptors () in
    return_unit
end
