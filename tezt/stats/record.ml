(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Tezt_core

type summed_durations = {total_time : int64; count : int}

let zero = {total_time = 0L; count = 0}

let decode_summed_durations (json : JSON.t) =
  if JSON.is_null json then zero
  else
    {
      total_time = JSON.(json |-> "total_time" |> as_int64);
      count = JSON.(json |-> "count" |> as_int);
    }

type test = {
  file : string;
  title : string;
  tags : string list;
  successful_runs : summed_durations;
  failed_runs : summed_durations;
  peak_memory_usage : int option;
}

let decode_test (json : JSON.t) : test =
  {
    file = JSON.(json |-> "file" |> as_string);
    title = JSON.(json |-> "title" |> as_string);
    tags = JSON.(json |-> "tags" |> as_list |> List.map as_string);
    successful_runs = decode_summed_durations JSON.(json |-> "successful_runs");
    failed_runs = decode_summed_durations JSON.(json |-> "failed_runs");
    peak_memory_usage = JSON.(json |-> "peak_memory_usage" |> as_int_opt);
  }

let duration_ns test =
  if test.successful_runs.count = 0 then
    (* Default to one second (this is what Tezt does when auto-balancing). *)
    1_000_000L
  else
    Int64.div
      test.successful_runs.total_time
      (Int64.of_int test.successful_runs.count)

let duration_minutes test = Int64.to_float (duration_ns test) /. 60_000_000.

type t = test list

let decode (json : JSON.t) : t = JSON.(json |> as_list |> List.map decode_test)

let input_file acc filename = (JSON.parse_file filename |> decode) :: acc

let input ~recursive paths =
  let rec input_file_or_directory ~recursive acc path =
    if Sys.is_directory path then
      let sub_paths = Sys.readdir path |> Array.map (Filename.concat path) in
      Array.fold_left
        (fun acc path ->
          if recursive then input_file_or_directory ~recursive:true acc path
          else if Filename.check_suffix path ".json" then input_file acc path
          else acc)
        acc
        sub_paths
    else if Filename.check_suffix path ".json" then input_file acc path
    else []
  in
  List.fold_left (input_file_or_directory ~recursive) [] paths |> List.flatten

let matches tsl test =
  TSL.eval
    {
      file = test.file;
      title = test.title;
      tags = test.tags;
      memory = test.peak_memory_usage |> Option.value ~default:0;
      duration = Int64.to_float (duration_ns test) /. 1_000_000.;
    }
    tsl
