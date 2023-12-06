open Tezt
open Tezt.Base
open Tezt_gitlab

let section =
  Clap.section
    "THIS SCRIPT"
    ~description:
      "Usage:\n\n\
      \  dune exec scripts/ci/list_tezt_warnings/main.exe -- PIPELINE_ID\n\n\
       This fetches the console logs of all Tezt jobs for pipeline \
       PIPELINE_ID, filters them to extract warnings, and outputs those \
       warnings.\n\n\
       You should wait for the pipeline (or at least Tezt jobs) to be finished \
       before running this script. Also, it takes a bit of time to fetch all \
       jobs, so be patient.\n\n\
       Here is an issue with some of the warnings that this script can find, \
       with hints to fix them: https://gitlab.com/tezos/tezos/-/issues/6657"

let project =
  Clap.default_string
    ~section
    ~long:"project"
    ~description:"GitLab project name."
    "tezos/tezos"

let pipeline =
  Clap.mandatory_int
    ~section
    ~placeholder:"PIPELINE"
    ~description:"ID of the pipeline to fetch logs from."
    ()

let tezt_job_name_rex = rex "^tezt-?([^ ]*) (\\d+)/\\d+$"

let warn_rex =
  rex "^\\[\\d+:\\d+:\\d+\\.\\d+\\] \027\\[31m\\[warn\\] (.*)\027\\[0m$"

let () =
  Test.register ~__FILE__ ~title:"list warnings" ~tags:[] @@ fun () ->
  let* all_jobs =
    Gitlab.(project_pipeline_jobs ~project ~pipeline () |> get_all)
  in
  let tezt_job_ids =
    all_jobs
    |> List.filter_map @@ fun job ->
       let job_name = JSON.(job |-> "name" |> as_string) in
       if job_name =~ tezt_job_name_rex then Some JSON.(job |-> "id" |> as_int)
       else None
  in
  let* () =
    tezt_job_ids
    |> Lwt_list.iter_s @@ fun job_id ->
       Log.debug "Fetching logs for: %d" job_id ;
       let* logs = Gitlab.get_job_logs ~project ~job_id () in
       let lines = String.split_on_char '\n' logs in
       let warnings = List.filter_map (fun line -> line =~* warn_rex) lines in
       if warnings <> [] then (
         Log.warn
           "Found warnings in: https://gitlab.com/tezos/tezos/-/jobs/%d"
           job_id ;
         List.iter (Log.warn "%s") warnings) ;
       unit
  in
  unit

let () = Test.run ()
