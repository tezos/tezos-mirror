(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

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

(* Same as [mkdir -p]. *)
let rec mkdir_p path =
  if not (Sys.file_exists path) then
    let parent = Filename.dirname path in
    if String.length parent < String.length path then (
      mkdir_p parent ;
      Sys.mkdir path 0o755)

let fetch_record records_directory
    (uri, {Tezt_job_name.component; index; variant}) =
  let local_filename =
    string_of_int (Option.value index ~default:1) ^ ".json"
  in
  let local_dir =
    (* Note: there is an issue where if both a job named "x.tezt"
       and a job named "tezt-x" exist, for the same "x",
       the files will end up in the same directory.
       So we need to make sure that no component has a name that is equal
       to a Tezt job variant. *)
    let dir =
      match component with
      | None -> records_directory
      | Some component -> records_directory // component
    in
    match variant with None -> dir | Some variant -> dir // variant
  in
  let local = local_dir // local_filename in
  mkdir_p local_dir ;
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

let fetch_pipeline_records_from_jobs records_directory pipeline =
  Log.info "Fetching records from tezt executions in %d in %s" pipeline project ;
  let* jobs = Gitlab.(project_pipeline_jobs ~project ~pipeline () |> get_all) in
  let get_record job =
    let job_id = JSON.(job |-> "id" |> as_int) in
    let name = JSON.(job |-> "name" |> as_string) in
    match Tezt_job_name.parse_ (Lexing.from_string name) with
    | None -> None
    | Some tezt_job_name ->
        let artifact_path = "tezt-results.json" in
        Log.info "Will fetch %s from job #%d (%s)" artifact_path job_id name ;
        Some
          ( Gitlab.project_job_artifact ~project ~job_id ~artifact_path (),
            tezt_job_name )
  in
  let records = List.filter_map get_record jobs in
  Log.info "Found %d Tezt jobs." (List.length records) ;
  (* Return the list of new records *)
  Lwt_list.map_p (fetch_record records_directory) records

type from =
  | Pipeline of int
  | Last_merged_pipeline
  | Last_scheduled of string
  | Last_successful_scheduled of string

let cli_from_type =
  let delete_prefix ~prefix str =
    if String.starts_with ~prefix str then
      let str_len = String.length str in
      let prefix_len = String.length prefix in
      Some (String.sub str prefix_len (str_len - prefix_len))
    else None
  in
  let parse = function
    | "last-merged-pipeline" -> Some Last_merged_pipeline
    | "last-schedule-extended-test" ->
        (* Deprecated, use last-scheduled:schedule_extended_test instead. *)
        Some (Last_scheduled "schedule_extended_test")
    | "last-successful-schedule-extended-test" ->
        (* Deprecated, use last-successful-scheduled:schedule_extended_test instead. *)
        Some (Last_successful_scheduled "schedule_extended_test")
    | s -> (
        match int_of_string_opt s with
        | Some id -> Some (Pipeline id)
        | None -> (
            match delete_prefix ~prefix:"last-scheduled:" s with
            | Some name -> Some (Last_scheduled name)
            | None -> (
                match delete_prefix ~prefix:"last-successful-scheduled:" s with
                | Some name -> Some (Last_successful_scheduled name)
                | None -> None)))
  in
  let show = function
    | Pipeline id -> string_of_int id
    | Last_merged_pipeline -> "last-merged-pipeline"
    | Last_scheduled name -> "last-scheduled:" ^ name
    | Last_successful_scheduled name -> "last-successful-scheduled:" ^ name
  in
  Clap.typ ~name:"from" ~dummy:Last_merged_pipeline ~parse ~show

let cli_from =
  Clap.list
    cli_from_type
    ~section
    ~long:"from"
    ~placeholder:"PIPELINE"
    ~description:
      "Pipelines to fetch records from. PIPELINE can be a pipeline identifier, \
       'last-merged-pipeline' (last pipeline for the last merge commit of the \
       default branch), 'last-scheduled:NAME' (last scheduled pipeline with \
       name containing '[NAME]'), or 'last-successful-scheduled:NAME' (last \
       successful scheduled pipeline with name containing '[NAME]'). If no \
       --from is specified, the script defaults to 'last-merged-pipeline'."
    ()

let cli_from =
  match cli_from with [] -> [Last_merged_pipeline] | _ :: _ -> cli_from

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

let pipeline_name_rex name = rex ("^\\[" ^ name ^ "\\] ")

let () =
  (* Register a test to benefit from error handling of Test.run,
     as well as [Background.start] etc. *)
  ( Test.register ~__FILE__ ~title:"update Tezt test records" ~tags:["update"]
  @@ fun () ->
    let* pipeline_ids =
      Fun.flip Lwt_list.map_p cli_from @@ function
      | Pipeline pipeline_id -> return pipeline_id
      | Last_merged_pipeline ->
          Gitlab_util.get_last_merged_pipeline ~project ~default_branch ()
      | Last_scheduled name ->
          Gitlab_util.get_last_schedule_pipeline
            ~project
            ~matching:(pipeline_name_rex name)
            ()
      | Last_successful_scheduled name ->
          Gitlab_util.get_last_schedule_pipeline
            ~status:"success"
            ~project
            ~matching:(pipeline_name_rex name)
            ()
    in
    let records_directory =
      if cli_dry_run then Temp.dir "tezt_records" else "tezt/records"
    in
    Log.info "Records will be stored in %s" records_directory ;
    let* new_records =
      Fun.flip
        Lwt_list.map_p
        pipeline_ids
        (fetch_pipeline_records_from_jobs records_directory)
    in
    let new_records = List.flatten new_records in
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
