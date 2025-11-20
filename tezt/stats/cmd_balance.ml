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
  tezt_job_count : int;
}

let job ?component ?variant ~j () =
  let name =
    (match component with None -> "" | Some component -> component ^ ".")
    ^ "tezt"
    ^ match variant with None -> "" | Some variant -> "-" ^ variant
  in
  let directory =
    let base = "tezt/records" in
    let component_base =
      match component with None -> base | Some component -> base // component
    in
    match variant with
    | None -> component_base
    | Some variant -> component_base // variant
  in
  {name; component; variant; directory; tezt_job_count = j}

(* List of Tezt jobs to balance. *)
(* TODO: Cacio could generate a file in script-inputs/ with this information. *)
let jobs =
  [
    job ~j:6 ();
    job ~variant:"extra" ~j:6 ();
    job ~variant:"flaky" ~j:1 ();
    job ~variant:"riscv-slow-sequential" ~j:1 ();
    job ~variant:"slow" ~j:3 ();
    job ~variant:"static-binaries" ~j:3 ();
    job ~variant:"time-sensitive" ~j:1 ();
    job ~component:"etherlink" ~j:6 ();
    job ~component:"etherlink" ~variant:"extra" ~j:6 ();
    job ~component:"etherlink" ~variant:"flaky" ~j:1 ();
    job ~component:"etherlink" ~variant:"slow" ~j:3 ();
  ]

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
    min stats.count job.tezt_job_count
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
    echo
      "- recommended job count: %g (about %.2f minutes per job with -j %d)"
      recommended_job_count
      expected_minutes_per_job
      job.tezt_job_count)
  else
    echo
      "%s: %g (~%.2f min/job with -j %d)"
      job.name
      recommended_job_count
      expected_minutes_per_job
      job.tezt_job_count

let run ~target ~verbose = List.iter (balance_job ~target ~verbose) jobs
