(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

module Monad = struct
  include Lwt

  let list_map_p = List.map_p
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

module Process = struct
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
end

include Performance_metrics.Make (Monad) (Process)
