open Tezt
open Base
open Tezt_gitlab

let usage () =
  prerr_endline
    {|Usage: dune exec scripts/ci/download_coverage/download.exe -- [-a from=last-merged-pipeline | -a from=<PIPELINE_ID>]

Example: to fetch coverage traces from
https://gitlab.com/tezos/tezos/-/pipelines/426773806, run
(from the root of the repository):

dune exec scripts/ci/download_coverage/download.exe -- -a from=426773806

You can use the PROJECT environment variable to specify which GitLab
repository to fetch coverage traces from. Default is: tezos/tezos

The script can also be used to fetch coverage traces from the last successful pipeline on the
latest MR merged to the default branch (configurable through the DEFAULT_BRANCH
environment variable) for a given PROJECT:

dune exec scripts/ci/download_coverage/download.exe -- -a from=last-merged-pipeline

|} ;
  exit 1

let project = Sys.getenv_opt "PROJECT" |> Option.value ~default:"tezos/tezos"

let default_branch =
  Sys.getenv_opt "DEFAULT_BRANCH" |> Option.value ~default:"master"

let coverage_traces_directory =
  Sys.getenv_opt "COVERAGE_OUTPUT" |> Option.value ~default:"_coverage_output"

(* Read [.gitlab/ci/jobs/coverage/coverage.yml] to find the set of
   jobs from which to collect coverage traces *)
let coverage_jobs =
  let coverage_yml_path = ".gitlab/ci/jobs/coverage/coverage.yml" in
  let coverage_yml =
    Base.read_file coverage_yml_path |> String.split_on_char '\n'
  in
  let _, coverage_yml =
    Base.span (fun line -> not (line =~ rex "^\\s+dependencies:$")) coverage_yml
  in
  let coverage_jobs, _ =
    Base.span (fun line -> not (line =~ rex "^\\s+script:$")) coverage_yml
  in
  let coverage_jobs =
    List.map
      (function
        | line -> (
            match line =~* rex {|- "(.*)"|} with
            | Some job_name -> job_name
            | None ->
                Test.fail "Unexpected line %S in %s" line coverage_yml_path))
      (List.tl coverage_jobs)
  in
  Log.debug
    "From %s, read coverage jobs: [%s]"
    coverage_yml_path
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
    let artifact_name =
      Base.replace_string (rex "[\\\\/_ @\\[\\]]+") job_name ~by:"-"
      ^ ".coverage"
      (* See for [scripts/ci/merge_coverage.sh] for details on how
         job_names are mangled into coverage trace artifact paths. *)
    in
    let artifact_path = coverage_traces_directory // artifact_name in
    if List.mem job_name coverage_jobs then
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

let cli_get_from =
  match Cli.get_string_opt "from" with
  | Some "last-merged-pipeline" -> Last_merged_pipeline
  | Some s -> (
      match int_of_string_opt s with Some i -> Pipeline i | None -> usage ())
  | None -> usage ()

let () =
  (* Register a test to benefit from error handling of Test.run,
     as well as [Background.start] etc. *)
  ( Test.register ~__FILE__ ~title:"download coverage" ~tags:["update"]
  @@ fun () ->
    let* _new_coverage_traces =
      match cli_get_from with
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
