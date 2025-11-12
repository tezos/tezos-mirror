(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let sf = Printf.sprintf

let echo x = Printf.ksprintf print_endline x

let ( // ) = Filename.concat

type tezt_job = {
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
  | [component; variant; parallel_jobs; parallel_tests] ->
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
        {name; component; variant; directory; parallel_jobs; parallel_tests}
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

let run ~target ~verbose = List.iter (balance_job ~target ~verbose) (jobs ())
