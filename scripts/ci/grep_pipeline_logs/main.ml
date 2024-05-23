open Tezt
open Tezt.Base
open Tezt_gitlab

let section =
  Clap.section
    "THIS SCRIPT"
    ~description:
      "Usage:\n\n\
      \  dune exec scripts/ci/grep_pipeline_logs/main.exe -- PIPELINE_ID\n\n\
       This fetches the console logs of all jobs whose name match a given \
       filter (by default, all Tezt jobs) for pipeline PIPELINE_ID, and \
       searches for occurences of a given pattern (by default, Tezt warnings), \
       and outputs those occurences.\n\n\
       You should wait for the pipeline (or at least filtered jobs) to be \
       finished before running this script. Also, it takes a bit of time to \
       fetch all jobs, so be patient.\n\n\
       Here is an issue with some of the warnings that this script can find, \
       with hints to fix them: https://gitlab.com/tezos/tezos/-/issues/6657"

let project =
  Clap.default_string
    ~section
    ~long:"project"
    ~description:"GitLab project name."
    "tezos/tezos"

let clap_rex =
  Clap.typ
    ~name:"rex"
    ~dummy:(rex "^dummy$")
    ~parse:(fun s -> Some (rex s))
    ~show:Base.show_rex

let job_filter =
  Clap.default
    clap_rex
    ~section
    ~placeholder:"JOB_FILTER"
    ~long:"job-filter"
    ~description:
      "Filter the names of jobs to fetch logs from. By default, fetches logs \
       from all tezt jobs."
    (rex "^tezt-?([^ ]*) (\\d+)/\\d+$")

let log_pattern =
  Clap.default
    clap_rex
    ~section
    ~long:"log-pattern"
    ~placeholder:"PATTERN"
    ~description:
      "Pattern containing exactly one capture group to search for in logs. By \
       default, searches for Tezt warnings. Only the contents of the capture \
       group is output."
    (rex "^\\[\\d+:\\d+:\\d+\\.\\d+\\] \027\\[31m\\[warn\\] (.*)\027\\[0m$")

let jobs_with_matches =
  Clap.flag
    ~section
    ~set_long:"jobs-with-matches"
    ~description:
      "Print only the names and URL of jobs with matching log lines. Do not \
       print matches to PATTERN."
    false

let parallel =
  Clap.flag
    ~section
    ~set_long:"parallel"
    ~description:
      "Fetches job logs in parallel. Faster, but be aware of the GitLab API \
       rate limit."
    false

let pipeline =
  Clap.mandatory_int
    ~section
    ~placeholder:"PIPELINE"
    ~description:"ID of the pipeline to fetch logs from."
    ()

let () =
  Test.register ~__FILE__ ~title:"list warnings" ~tags:[] @@ fun () ->
  let* all_jobs =
    Gitlab.(project_pipeline_jobs ~project ~pipeline () |> get_all)
  in
  let tezt_jobs =
    all_jobs
    |> List.filter_map @@ fun job ->
       let job_name = JSON.(job |-> "name" |> as_string) in
       if job_name =~ job_filter then
         Some JSON.(job |-> "id" |> as_int, job |-> "name" |> as_string)
       else None
  in
  let* () =
    let iter =
      Fun.flip (if parallel then Lwt_list.iter_p else Lwt_list.iter_s)
    in
    iter tezt_jobs @@ fun (job_id, job_name) ->
    Log.debug "Fetching logs for: %d" job_id ;
    let* logs = Gitlab.get_job_logs ~project ~job_id () in
    let lines = String.split_on_char '\n' logs in
    let warnings = List.filter_map (fun line -> line =~* log_pattern) lines in
    if warnings <> [] then (
      Log.report
        "Matches in: https://gitlab.com/tezos/tezos/-/jobs/%d (%s)"
        job_id
        job_name ;
      if not jobs_with_matches then List.iter (Log.warn "%s") warnings) ;
    unit
  in
  unit

let () = Test.run ()
