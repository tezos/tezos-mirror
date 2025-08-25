open Tezt
open Base
open Tezt_gitlab

let section =
  Clap.section
    "DOWNLOAD COVERAGE TRACES"
    ~description:
      {|Download coverage traces from a pipeline.

Example: to fetch coverage traces from
https://gitlab.com/tezos/tezos/-/pipelines/426773806, run
(from the root of the repository):

dune exec scripts/ci/download_coverage/download.exe -- --from 426773806

You can use the PROJECT environment variable to specify which GitLab
repository to fetch coverage traces from. Default is: tezos/tezos

The script can also be used to fetch coverage traces from the last successful pipeline on the
latest MR merged to the default branch (configurable through the DEFAULT_BRANCH
environment variable) for a given PROJECT:

dune exec scripts/ci/download_coverage/download.exe -- --from last-merged-pipeline

|}

let project = Sys.getenv_opt "PROJECT" |> Option.value ~default:"tezos/tezos"

let default_branch =
  Sys.getenv_opt "DEFAULT_BRANCH" |> Option.value ~default:"master"

let coverage_traces_directory =
  Sys.getenv_opt "COVERAGE_OUTPUT" |> Option.value ~default:"_coverage_output"

let coverage_jobs_path = "script-inputs/ci-coverage-producing-jobs"

(** Read {!coverage_jobs_path} to find the set of jobs from which to
    collect coverage traces *)
let coverage_jobs =
  (* Note: the contents of this file only includes the stem of each job name. *)
  let coverage_jobs =
    Base.read_file coverage_jobs_path
    |> String.split_on_char '\n'
    |> List.filter (( <> ) "")
  in
  Log.debug
    "From %s, read coverage jobs: [%s]"
    coverage_jobs_path
    (String.concat ", " coverage_jobs) ;
  coverage_jobs

let fetch_coverage_trace (uri, job_name, artifact_name) =
  let local = coverage_traces_directory // artifact_name in
  let* () = Gitlab.get_output uri ~output_path:local in
  Log.info "Downloaded %s: %s" job_name local ;
  return local

let fetch_pipeline_coverage_from_jobs pipeline =
  Log.info
    "Fetching coverage traces from test executions in %d in %s"
    pipeline
    project ;
  let* jobs = Gitlab.(project_pipeline_jobs ~project ~pipeline () |> get_all) in
  let get_coverage_trace job =
    let job_id = JSON.(job |-> "id" |> as_int) in
    let job_name = JSON.(job |-> "name" |> as_string) in
    let job_status = JSON.(job |-> "status" |> as_string) in
    (* Based on [Utils.slugify] from
       https://gitlab.com/gitlab-org/gitlab/blob/master/gems/gitlab-utils/lib/gitlab/utils.rb#L58 *)
    let slugify str =
      let str =
        String.map
          (function
            | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9') as c -> c | _ -> '-')
          str
      in
      let str = String.sub str 0 (min (String.length job_name) 63) in
      Base.replace_string (rex "^-+|-+$") ~by:"" str
    in
    let artifact_name =
      (* Coverage traces are stored in [${CI_JOB_NAME_SLUG}.coverage]
         (see [scripts/ci/merge_coverage.sh]) for full details,
         and see https://docs.gitlab.com/ee/ci/variables/predefined_variables.html
         for the details on CI_JOB_NAME_SLUG. *)
      slugify job_name ^ ".coverage"
    in
    let artifact_path = coverage_traces_directory // artifact_name in
    if
      (job_status = "success" || job_status = "failed")
      && List.exists
           (fun coverage_job_stem ->
             job_name = coverage_job_stem
             (* vector parallel *)
             || job_name =~ rex ("^" ^ coverage_job_stem ^ " \\d+/\\d+$")
             ||
             (* matrix parallel *)
             job_name =~ rex ("^" ^ coverage_job_stem ^ ": \\[.*\\]$"))
           coverage_jobs
    then
      Some
        ( Gitlab.project_job_artifact ~project ~job_id ~artifact_path (),
          job_name,
          artifact_name )
    else None
  in
  let coverage_traces = List.filter_map get_coverage_trace jobs in
  Log.info "Found %d coverage traces." (List.length coverage_traces) ;
  (* Return the list of new coverage_traces *)
  Lwt_list.map_p fetch_coverage_trace coverage_traces

type from = Pipeline of int | Last_merged_pipeline

let cli_from_type =
  let parse = function
    | "last-merged-pipeline" -> Some Last_merged_pipeline
    | s -> Option.map (fun x -> Pipeline x) (int_of_string_opt s)
  in
  let show = function
    | Pipeline id -> string_of_int id
    | Last_merged_pipeline -> "last-merged-pipeline"
  in
  Clap.typ ~name:"from" ~dummy:Last_merged_pipeline ~parse ~show

let cli_from =
  Clap.default
    cli_from_type
    ~section
    ~long:"from"
    ~placeholder:"PIPELINE"
    ~description:
      "The ID of the pipeline to fetch records from. Also accepts \
       'last-merged-pipeline', which denotes the last pipeline for the latest \
       merge commit on the default branch."
    Last_merged_pipeline

let () =
  (* Register a test to benefit from error handling of Test.run,
     as well as [Background.start] etc. *)
  ( Test.register ~__FILE__ ~title:"download coverage" ~tags:["update"]
  @@ fun () ->
    let* _new_coverage_traces =
      match cli_from with
      | Pipeline pipeline_id -> fetch_pipeline_coverage_from_jobs pipeline_id
      | Last_merged_pipeline ->
          let* pipeline_id =
            Gitlab_util.get_last_merged_pipeline ~project ~default_branch ()
          in
          fetch_pipeline_coverage_from_jobs pipeline_id
    in
    (* remove_existing_coverage_traces new_coverage_traces ; *)
    unit ) ;
  Test.run ()
