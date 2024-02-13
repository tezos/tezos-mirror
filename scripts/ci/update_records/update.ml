open Tezt
open Base
open Tezt_gitlab

let section =
  Clap.section
    "UPDATE TEZT RECORDS"
    ~description:
      {|Update records in tezt/records by fetching them from a pipeline.

Example: to fetch test result records from https://gitlab.com/tezos/tezos/-/pipelines/426773806, run (from the root of the repository):

dune exec scripts/ci/update_records/update.exe -- --from 426773806

You can use the PROJECT environment variable to specify which GitLab repository to fetch records from. Default is: tezos/tezos

The script can also be used to fetch records from the last successful pipeline on the latest MR merged to the default branch (configurable through the DEFAULT_BRANCH environment variable) for a given PROJECT:

dune exec scripts/ci/update_records/update.exe -- --from last-merged-pipeline|}

let project = Sys.getenv_opt "PROJECT" |> Option.value ~default:"tezos/tezos"

let default_branch =
  Sys.getenv_opt "DEFAULT_BRANCH" |> Option.value ~default:"master"

let fetch_record records_directory (uri, index, variant) =
  let local_filename = string_of_int index ^ ".json" in
  let local_dir =
    match variant with
    | None -> records_directory
    | Some variant -> records_directory // variant
  in
  let local = local_dir // local_filename in
  if not @@ Sys.file_exists local_dir then Sys.mkdir local_dir 0o755 ;
  Lwt.catch
    (fun () ->
      let* () = Gitlab.get_output uri ~output_path:local in
      Log.info "Downloaded: %s" local ;
      let (_ : JSON.t) = JSON.parse_file local in
      return (Some local_filename))
    (fun exn ->
      Log.error
        "Failed to fetch record: %s: %s"
        (Uri.to_string uri)
        (Printexc.to_string exn) ;
      return None)

let remove_existing_records records_directory new_records =
  let remove_if_looks_like_an_old_record filename =
    if filename =~ rex "^\\d+\\.json$" && not (List.mem filename new_records)
    then (
      let filename = records_directory // filename in
      Sys.remove filename ;
      Log.info "Removed outdated record: %s" filename)
  in
  Array.iter remove_if_looks_like_an_old_record (Sys.readdir records_directory)

let parse_tezt_job_name =
  let with_no_variant = rex "^tezt (\\d+)/\\d+$" in
  let with_variant = rex "^tezt-([a-zA-Z0-9-_]*) (\\d+)/\\d+$" in
  let with_variant_no_index = rex "^tezt-([a-zA-Z0-9-_]*)$" in
  fun name ->
    match name =~* with_no_variant with
    | Some index -> Some (None, int_of_string index)
    | None -> (
        match name =~** with_variant with
        | Some (variant, index) -> Some (Some variant, int_of_string index)
        | None -> (
            match name =~* with_variant_no_index with
            | Some variant -> Some (Some variant, 1)
            | None -> None))

let fetch_pipeline_records_from_jobs records_directory pipeline =
  Log.info "Fetching records from tezt executions in %d in %s" pipeline project ;
  let* jobs = Gitlab.(project_pipeline_jobs ~project ~pipeline () |> get_all) in
  let get_record job =
    let job_id = JSON.(job |-> "id" |> as_int) in
    let name = JSON.(job |-> "name" |> as_string) in
    match parse_tezt_job_name name with
    | None -> None
    | Some (variant, index) ->
        let artifact_path =
          sf
            "tezt-results-%d%s.json"
            index
            (match variant with
            | None -> ""
            | Some variant ->
                "-" ^ String.map (function '-' -> '_' | c -> c) variant)
        in
        Log.info "Will fetch %s from job #%d (%s)" artifact_path job_id name ;
        Some
          ( Gitlab.project_job_artifact ~project ~job_id ~artifact_path (),
            index,
            variant )
  in
  let records = List.filter_map get_record jobs in
  Log.info "Found %d Tezt jobs." (List.length records) ;
  (* Return the list of new records *)
  Lwt_list.map_p (fetch_record records_directory) records

type from =
  | Pipeline of int
  | Last_merged_pipeline
  | Last_successful_schedule_extended_test

let cli_from_type =
  let parse = function
    | "last-merged-pipeline" -> Some Last_merged_pipeline
    | "last-successful-schedule-extended-test" ->
        Some Last_successful_schedule_extended_test
    | s -> Option.map (fun x -> Pipeline x) (int_of_string_opt s)
  in
  let show = function
    | Pipeline id -> string_of_int id
    | Last_merged_pipeline -> "last-merged-pipeline"
    | Last_successful_schedule_extended_test ->
        "last-successful-schedule-extended-test"
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

let cli_dry_run =
  Clap.flag
    ~section
    ~set_long:"dry-run"
    ~description:
      "If set, tries downloading and parsing records from the given pipeline, \
       but does not modify the records in the working directory. Records are \
       downloaded to a temporary directory that can be kept by passing \
       --keep-temp."
    false

let schedule_extended_test_rex = rex "^\\[schedule_extended_test\\] "

let () =
  (* Register a test to benefit from error handling of Test.run,
     as well as [Background.start] etc. *)
  ( Test.register ~__FILE__ ~title:"update records" ~tags:["update"] @@ fun () ->
    let* pipeline_id =
      match cli_from with
      | Pipeline pipeline_id -> return pipeline_id
      | Last_merged_pipeline ->
          Gitlab_util.get_last_merged_pipeline ~project ~default_branch ()
      | Last_successful_schedule_extended_test ->
          Gitlab_util.get_last_schedule_pipeline
            ~status:"success"
            ~project
            ~matching:schedule_extended_test_rex
            ()
    in
    let records_directory =
      if cli_dry_run then Temp.dir "tezt_records" else "tezt/records"
    in
    Log.info "Records will be stored in %s" records_directory ;
    let* new_records =
      fetch_pipeline_records_from_jobs records_directory pipeline_id
    in
    if not cli_dry_run then
      remove_existing_records
        records_directory
        (List.filter_map Fun.id new_records) ;
    (* Now we can fail if we failed to download a record.
       We did not want to fail earlier because in the CI we want to fetch
       as many records as possible so that it can choose to continue anyway
       (with a warning icon). *)
    let failure_count =
      List.filter (function None -> true | Some _ -> false) new_records
      |> List.length
    in
    Log.info
      "Successfully fetched %d record(s)."
      (List.length new_records - failure_count) ;
    if failure_count > 0 then
      Test.fail
        "%d record(s) could not be fetched; see errors above."
        failure_count ;
    unit ) ;
  Test.run ()
