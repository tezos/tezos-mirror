(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Prometheus

let namespace = Tezos_version.Octez_node_version.namespace

type data_dir_element = {path : string; metrics_suffix : string}

let data_dir_element ?metrics_suffix path =
  let metrics_suffix = Option.value ~default:path metrics_suffix in
  {path; metrics_suffix}

module type REGISTRY = sig
  val registry : CollectorRegistry.t

  val subsystem : string

  val directories : data_dir_element list
end

let run_without_error cmd args =
  let open Lwt_syntax in
  let+ status =
    Lwt_process.with_process_full
      (cmd, Array.of_list (cmd :: args))
      (fun pc -> pc#status)
  in
  match status with Unix.WEXITED 0 -> true | _ -> false

let rec get_children pid =
  let open Lwt_syntax in
  let* children =
    Lwt_process.with_process_full
      ("pgrep", [|"pgrep"; "-P"; pid|])
      (fun pc -> Lwt_io.read_lines pc#stdout |> Lwt_stream.to_list)
  in
  List.fold_left_s
    (fun acc pid ->
      let+ grandchildren = get_children pid in
      acc @ grandchildren)
    [pid]
    children

let get_pids pid = get_children (string_of_int pid)

let get_pids_str ?(sep = ",") pid =
  let open Lwt_syntax in
  let+ pids = get_pids pid in
  String.concat sep pids

let supports_performance_metrics () =
  let open Lwt_syntax in
  let* pids_str = get_pids_str (Unix.getpid ()) in
  let+ cmd_support =
    Lwt.all
      [
        run_without_error "which" ["lsof"];
        (* `ps` with BusyBox (used by Alpine) does not support the `-p` option,
           so `which` is not enough. *)
        run_without_error "ps" ["-p"; pids_str; "-o"; "%cpu=,%mem=,vsz=,rss="];
        run_without_error "which" ["du"];
      ]
  in
  List.for_all Fun.id cmd_support

module Make (R : REGISTRY) = struct
  include R

  let start_time = Time.System.now ()

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

  let get_ps pid =
    let open Lwt_syntax in
    Lwt.catch
      (fun () ->
        let* pids_str = get_pids_str pid in
        Lwt_process.with_process_in
          ~env:[|"LC_ALL=C"|]
          ("ps", [|"ps"; "-p"; pids_str; "-o"; "%cpu=,%mem=,vsz=,rss="|])
        @@ fun pc ->
        let lines = Lwt_io.read_lines pc#stdout in
        Lwt_stream.fold
          (fun s acc ->
            match String.split_no_empty ' ' s with
            | [cpu; memp; virt; res] -> (
                try
                  let pcpu, pmemp, pvirt, pres =
                    match acc with None -> (0., 0., 0, 0) | Some acc -> acc
                  in
                  Some
                    ( pcpu +. float_of_string cpu,
                      pmemp +. float_of_string memp,
                      pvirt + int_of_string virt,
                      pres + int_of_string res )
                with _ -> acc)
            | _ -> acc)
          lines
          None)
      (function _exn -> return_none)

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
          Lwt_process.with_process_full
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
          Lwt_process.with_process_full
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

  let directories_gauges =
    let open Format in
    List.map
      (fun data_dir_elem ->
        ( data_dir_elem.path,
          v_gauge
            ~help:(sprintf "Disk usage: %s" data_dir_elem.path)
            (sprintf "performance_%s" data_dir_elem.metrics_suffix) ))
      directories

  let data = v_gauge ~help:"Disk Usage" "performance_data"

  let percentage =
    v_gauge ~help:"Disk Usage Percentage" "performance_disk_percentage"

  let set_disk_usage_stats ~data_dir =
    let open Lwt_syntax in
    let* disk_percentage = get_disk_usage_percentage data_dir in
    (* du results are in kB, we show them as GB *)
    let aux gauge s =
      Gauge.set gauge @@ (Int64.to_float s /. (1024. *. 1024.))
    in
    let* disk_sizes =
      List.map_p
        (fun (dir, gauge) ->
          let+ res = directory_size (Filename.concat data_dir dir) in
          Option.iter (aux gauge) res ;
          res)
        directories_gauges
    in
    let total_size =
      List.fold_left
        (fun acc size ->
          match size with None -> acc | Some size -> Int64.add acc size)
        0L
        disk_sizes
    in
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
        let* pids_str = get_pids_str pid in
        let+ fd, conn =
          Lwt_process.with_process_in
            ~env:[|"LC_ALL=C"|]
            ("lsof", [|"lsof"; "-wap"; pids_str|])
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

  let set_elapsed_time () =
    let new_elapsed_time =
      Ptime.Span.to_float_s @@ Ptime.diff (Time.System.now ()) start_time
    in
    Counter.set elapsed_time new_elapsed_time

  let set_stats ~data_dir =
    let open Lwt_syntax in
    set_elapsed_time () ;
    let* () = set_memory_cpu_stats ()
    and* () = set_disk_usage_stats ~data_dir
    and* () = set_file_descriptors () in
    return_unit
end
