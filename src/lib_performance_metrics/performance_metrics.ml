(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Prometheus
open Sigs
open Metrics

module Make (Monad : MONAD) (Process : PROCESS with type 'a monad := 'a Monad.t) =
struct
  let ( let* ) = Monad.bind

  let ( and* ) = Monad.both

  let return = Monad.return

  module Make (R : REGISTRY) = struct
    include R
    include Metrics.Make (R)

    let start_time = Time.System.now ()

    let set_memory_cpu_stats () =
      let pid = Unix.getpid () in
      let* stats = Process.get_ps pid in
      Option.iter
        (fun (cpu_percent, mem_percent, virt, res) ->
          (* ps results are in kB, we show them as GB *)
          Gauge.set virtual_ (float virt /. (1024. *. 1024.)) ;
          Gauge.set resident (float res /. (1024. *. 1024.)) ;
          Gauge.set cpu cpu_percent ;
          Gauge.set memp mem_percent)
        stats ;
      return ()

    let set_disk_usage_stats ~data_dir =
      let* disk_percentage = Process.get_disk_usage_percentage data_dir in
      (* du results are in kB, we show them as GB *)
      let aux gauge s =
        Gauge.set gauge @@ (Int64.to_float s /. (1024. *. 1024.))
      in
      let* disk_sizes =
        Monad.list_map_p
          (fun (dir, gauge) ->
            let* res = Process.directory_size (Filename.concat data_dir dir) in
            Option.iter (aux gauge) res ;
            return res)
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
      return ()

    let set_file_descriptors () =
      let pid = Unix.getpid () in
      let* r = Process.get_file_descriptors pid in
      Option.iter
        (fun (fd, conn) ->
          Gauge.set file_descriptors @@ Float.of_int fd ;
          Gauge.set connections @@ Float.of_int conn)
        r ;
      return ()

    let set_elapsed_time () =
      let new_elapsed_time =
        Ptime.Span.to_float_s @@ Ptime.diff (Time.System.now ()) start_time
      in
      Counter.set elapsed_time new_elapsed_time

    let set_stats ~data_dir =
      set_elapsed_time () ;
      let* () = set_memory_cpu_stats ()
      and* () = set_disk_usage_stats ~data_dir
      and* () = set_file_descriptors () in
      return ()
  end
end
