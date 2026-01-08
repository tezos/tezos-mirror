(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

module Monad = struct
  type 'a t = 'a

  let bind a f = f a

  let return x = x

  let both a b = (a, b)

  let list_map_p = List.map
end

let run_without_error cmd args =
  let ((o, _, e) as p) =
    Unix.open_process_args_full
      cmd
      (Array.of_list (cmd :: args))
      (Unix.environment ())
  in
  (* Consume outputs *)
  In_channel.input_all o |> ignore ;
  In_channel.input_all e |> ignore ;
  match Unix.close_process_full p with Unix.WEXITED 0 -> true | _ -> false

let rec get_children pid =
  let ic = Unix.open_process_args_in "pgrep" [|"pgrep"; "-P"; pid|] in
  let children = In_channel.input_lines ic in
  Unix.close_process_in ic |> ignore ;
  List.fold_left
    (fun acc pid ->
      let grandchildren = get_children pid in
      acc @ grandchildren)
    [pid]
    children

let get_pids pid = get_children (string_of_int pid)

let get_pids_str ?(sep = ",") pid =
  let pids = get_pids pid in
  String.concat sep pids

let supports_performance_metrics () =
  let pids_str = get_pids_str (Unix.getpid ()) in
  List.for_all
    Fun.id
    [
      run_without_error "which" ["lsof"];
      (* `ps` with BusyBox (used by Alpine) does not support the `-p` option,
         so `which` is not enough. *)
      run_without_error "ps" ["-p"; pids_str; "-o"; "%cpu=,%mem=,vsz=,rss="];
      run_without_error "which" ["du"];
    ]

module Process = struct
  let get_ps pid =
    try
      let pids_str = get_pids_str pid in
      let ((ic, _, _) as p) =
        Unix.open_process_args_full
          "ps"
          [|"ps"; "-p"; pids_str; "-o"; "%cpu=,%mem=,vsz=,rss="|]
          (Array.append (Unix.environment ()) [|"LC_ALL=C"|])
      in
      let res =
        In_channel.fold_lines
          (fun acc s ->
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
          None
          ic
      in
      Unix.close_process_full p |> ignore ;
      res
    with _exn -> None

  let directory_size path =
    try
      let ic = Unix.open_process_args_in "du" [|"du"; "-sk"; path|] in
      let s = In_channel.input_line ic in
      Unix.close_process_in ic |> ignore ;
      match s with
      | None -> None
      | Some s -> (
          match String.split_on_char '\t' s with
          | [] -> None
          | h :: _ -> Int64.of_string_opt h)
    with _exn -> None

  let get_disk_usage_percentage path =
    try
      let ic = Unix.open_process_args_in "df" [|"df"; path|] in
      let _ = In_channel.input_line ic in
      let s = In_channel.input_line ic in
      Unix.close_process_in ic |> ignore ;
      match s with
      | None -> None
      | Some s -> (
          let l = Str.split (Str.regexp "[ ]+") s in
          let h = List.nth_opt l 4 in
          match h with
          | Some str ->
              let len = String.length str in
              let e = String.sub str 0 (len - 1) in
              Int64.of_string_opt e
          | None -> None)
    with _exn -> None

  let get_file_descriptors pid =
    try
      let pids_str = get_pids_str pid in
      let ic = Unix.open_process_args_in "lsof" [|"lsof"; "-wap"; pids_str|] in
      let rec count fd conn =
        try
          let s = In_channel.input_line ic in
          match s with
          | None -> (fd, conn)
          | Some s ->
              let l = String.split_on_char ' ' s in
              let conn =
                if List.mem ~equal:String.equal "TCP" l then conn + 1 else conn
              in
              count (fd + 1) conn
        with _ -> (fd, conn)
      in
      let fd, conn = count 0 0 in
      Unix.close_process_in ic |> ignore ;
      Some (fd - 1, conn)
    with _exn -> None
end

include Performance_metrics.Make (Monad) (Process)
