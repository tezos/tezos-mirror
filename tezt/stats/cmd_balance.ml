(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

module String_map = Map.Make (String)

let sf = Printf.sprintf

let echo x = Printf.ksprintf print_endline x

let ( // ) = Filename.concat

type tezt_job = {
  pipeline : [`merge_request | `scheduled];
  name : string;
  component : string option;
  variant : string option;
  directory : string;
  parallel_jobs : int;
  parallel_tests : int;
}

let parse_job_line line =
  match String.split_on_char ',' line with
  | [""] -> []
  | [pipeline; component; variant; parallel_jobs; parallel_tests] ->
      let pipeline =
        match pipeline with
        | "merge_request" -> `merge_request
        | "scheduled" -> `scheduled
        | _ ->
            failwith
            @@ sf
                 "failed to parse the list of jobs: invalid pipeline type: %S"
                 pipeline
      in
      let component = if component = "" then None else Some component in
      let variant = if variant = "" then None else Some variant in
      let name =
        (match component with None -> "" | Some component -> component ^ ".")
        ^ "tezt"
        ^ match variant with None -> "" | Some variant -> "-" ^ variant
      in
      let directory =
        let base = "tezt/records" in
        let component_base =
          match component with
          | None -> base
          | Some component -> base // component
        in
        match variant with
        | None -> component_base
        | Some variant -> component_base // variant
      in
      let parallel_jobs =
        match int_of_string_opt parallel_jobs with
        | Some n -> n
        | None ->
            failwith
              "failed to parse the list of jobs: invalid integer for \
               parallel_jobs"
      in
      let parallel_tests =
        match int_of_string_opt parallel_tests with
        | Some n -> n
        | None ->
            failwith
              "failed to parse the list of jobs: invalid integer for \
               parallel_tests"
      in
      let job =
        {
          pipeline;
          name;
          component;
          variant;
          directory;
          parallel_jobs;
          parallel_tests;
        }
      in
      [job]
  | _ ->
      failwith
        "failed to parse the list of jobs: unexpected number of column in CSV \
         file"

let jobs () =
  let ch = open_in "script-inputs/cacio-tezt-jobs" in
  Fun.protect ~finally:(fun () -> close_in ch) @@ fun () ->
  let rec read acc =
    match input_line ch with
    | exception End_of_file -> List.rev acc
    | line -> read (parse_job_line line @ acc)
  in
  read []

let only_keep_one_occurrence_of_each_job jobs =
  let check_and_add_to_map acc job =
    match String_map.find_opt job.name acc with
    | None -> String_map.add job.name job acc
    | Some same_job ->
        if same_job.parallel_jobs <> job.parallel_jobs then
          (* This is not very important, it only affects the display of the current
             value of parallel_jobs, and the emoji. Still odd since jobs usually
             are defined the same in all pipeline types. So it is worth a warning. *)
          Printf.eprintf
            "Warning: job %S is defined multiple times with different parallel \
             jobs in cacio-tezt-jobs.\n\
             %!"
            job.name ;
        acc
  in
  let map = List.fold_left check_and_add_to_map String_map.empty jobs in
  List.map snd (String_map.bindings map)

let balance_job job ~target ~verbose =
  let tests =
    try Record.input ~recursive:false [job.directory]
    with Failure message | Sys_error message ->
      failwith
      @@ sf
           "failed to input records for job %s from directory %s: %s"
           job.name
           job.directory
           message
  in
  let stats = Stats.make tests in
  let effective_tezt_job_count =
    (* Cannot run [job.tezt_job_count] tests in parallel
       if there are less than [job.tezt_job_count] tests. *)
    min stats.count job.parallel_tests
  in
  let recommended_job_count =
    let ideal =
      Stats.Duration.minutes stats.total_duration
      /. target
      /. float effective_tezt_job_count
    in
    (* If each job runs [effective_tezt_job_count] tests in parallel,
       it is useless to run more than [stats.count / effective_tezt_job_count] jobs. *)
    ceil (min ideal (float stats.count /. float effective_tezt_job_count))
  in
  let expected_minutes_per_job =
    Stats.Duration.minutes stats.total_duration
    /. recommended_job_count
    /. float effective_tezt_job_count
  in
  if verbose then (
    echo "%s:" job.name ;
    echo "- test count: %d" stats.count ;
    echo
      "- total duration: %.2f minutes (%.2f hours)"
      (Stats.Duration.minutes stats.total_duration)
      (Stats.Duration.hours stats.total_duration) ;
    echo
      "- average test duration: %.2f seconds"
      (Stats.Duration.seconds stats.average_duration) ;
    echo "- current job count: %d" job.parallel_jobs ;
    echo
      "- recommended job count: %g (about %.2f minutes per job with -j %d)"
      recommended_job_count
      expected_minutes_per_job
      job.parallel_tests)
  else
    let emoji =
      if abs_float (float job.parallel_jobs -. recommended_job_count) < 0.001
      then "✅"
      else "❌"
    in
    echo
      "%s %s: %g (current: %d) (~%.2f min/job with -j %d)"
      emoji
      job.name
      recommended_job_count
      job.parallel_jobs
      expected_minutes_per_job
      job.parallel_tests

let run ~target ~verbose =
  jobs () |> only_keep_one_occurrence_of_each_job
  |> List.iter (balance_job ~target ~verbose)
