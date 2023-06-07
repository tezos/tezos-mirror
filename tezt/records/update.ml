open Tezt
open Base

let usage () =
  prerr_endline
    {|Usage: PIPELINE=<PIPELINE_ID> dune exec tezt/records/update.exe

Example: to fetch test result records from
https://gitlab.com/tezos/tezos/-/pipelines/426773806, run
(from the root of the repository):

dune exec tezt/records/update.exe -- -a from=426773806

You can use the PROJECT environment variable to specify which GitLab
repository to fetch records from. Default is: tezos/tezos

The script can also be used to fetch records from the last successful pipeline on the
latest MR merged to the default branch (configurable through the DEFAULT_BRANCH
environment variable) for a given PROJECT:

dune exec tezt/records/update.exe -- -a from=last-merged-pipeline

|} ;
  exit 1

let project = Sys.getenv_opt "PROJECT" |> Option.value ~default:"tezos/tezos"

let default_branch =
  Sys.getenv_opt "DEFAULT_BRANCH" |> Option.value ~default:"master"

module Gitlab = struct
  let make path =
    Uri.make ~scheme:"https" ~host:"gitlab.com" ~path:("api/v4/" ^ path)

  let query_opt name value_opt =
    match value_opt with None -> [] | Some value -> [(name, [value])]

  let project_pipelines ?order_by ?sort ?source ?ref_ ?sha ?status =
    make
      (sf "projects/%s/pipelines/" (Uri.pct_encode project))
      ~query:
        (query_opt "ref" ref_ @ query_opt "source" source @ query_opt "sha" sha
       @ query_opt "status" status
        @ query_opt "order_by" order_by
        @ query_opt "sort" sort)

  let project_pipeline_jobs ~pipeline =
    make (sf "projects/%s/pipelines/%d/jobs" (Uri.pct_encode project) pipeline)

  let project_commits ?ref_name =
    make
      (sf "projects/%s/repository/commits" (Uri.pct_encode project))
      ~query:(query_opt "ref_name" ref_name)

  let project_job_artifact ~job_id ~artifact_path =
    make
      (sf
         "projects/%s/jobs/%d/artifacts/%s"
         (Uri.pct_encode project)
         job_id
         artifact_path)

  let curl_params ?(fail_on_http_errors = true) ?output_path ?(location = false)
      uri =
    (if fail_on_http_errors then ["--fail"] else [])
    @ (match output_path with
      | Some output_path -> ["--output"; output_path]
      | None -> [])
    @ (if location then ["--location"] else [])
    @ [Uri.to_string uri]

  let get ?fail_on_http_errors uri =
    let* raw_json =
      Process.run_and_read_stdout "curl" (curl_params ?fail_on_http_errors uri)
    in
    return (JSON.parse ~origin:(Uri.to_string uri) raw_json)

  let get_output ?fail_on_http_errors ~output_path uri =
    Process.run
      "curl"
      (curl_params ?fail_on_http_errors ~location:true ~output_path uri)

  let get_all uri =
    let rec aux from acc =
      let full_uri =
        Uri.(
          add_query_params'
            uri
            [("per_page", "200"); ("page", string_of_int from)])
      in
      (* GitLab uses a lot of redirections so we use --location to follow them. *)
      let* response_body =
        Process.run_and_read_stdout "curl" (curl_params ~location:true full_uri)
      in
      let list =
        JSON.parse ~origin:Uri.(to_string full_uri) response_body
        |> JSON.as_list
      in
      Log.info
        "Found %d items in page %d of %s."
        (List.length list)
        from
        (Uri.to_string uri) ;
      match list with
      | [] -> return (List.rev acc)
      | _ :: _ -> aux (from + 1) (List.rev_append list acc)
    in
    aux 1 []
end

let records_directory = "tezt/records"

let fetch_record (uri, index) =
  let local_filename = index ^ ".json" in
  let local = records_directory // local_filename in
  let* () = Gitlab.get_output uri ~output_path:local in
  Log.info "Downloaded: %s" local ;
  match JSON.parse_file local with
  | exception (JSON.Error _ as exn) ->
      Log.error
        "Failed to parse downloaded JSON file, maybe the artifact has expired?" ;
      raise exn
  | (_ : JSON.t) -> return local_filename

let remove_existing_records new_records =
  let remove_if_looks_like_an_old_record filename =
    if filename =~ rex "^\\d+\\.json$" && not (List.mem filename new_records)
    then (
      let filename = records_directory // filename in
      Sys.remove filename ;
      Log.info "Removed outdated record: %s" filename)
  in
  Array.iter remove_if_looks_like_an_old_record (Sys.readdir records_directory)

let fetch_pipeline_records_from_jobs pipeline =
  Log.info "Fetching records from tezt executions in %d in %s" pipeline project ;
  let* jobs = Gitlab.(project_pipeline_jobs ~pipeline () |> get_all) in
  let get_record job =
    let job_id = JSON.(job |-> "id" |> as_int) in
    let name = JSON.(job |-> "name" |> as_string) in
    match name =~* rex "^tezt (\\d+)/\\d+$" with
    | None -> None
    | Some index ->
        Some
          ( Gitlab.project_job_artifact
              ~job_id
              ~artifact_path:("tezt-results-" ^ index ^ ".json")
              (),
            index )
  in
  let records = List.filter_map get_record jobs in
  Log.info "Found %d Tezt jobs." (List.length records) ;
  (* Return the list of new records *)
  Lwt_list.map_p fetch_record records

let get_last_merged_pipeline () =
  let is_merge_commit commit =
    (* This script assumes that the start commit is part of a branch following
       a semi-linear history and that merged branches are linear (this should
       be the case for the default branch on tezos/tezos).  In that setting,
       only merge commits have two parents. *)
    List.length JSON.(commit |-> "parent_ids" |> as_list) = 2
  in
  Log.info
    "Searching for latest merge commit in project %s on branch %s"
    project
    default_branch ;
  let commit_hash commit = JSON.(commit |-> "id" |> as_string) in
  let* commits = Gitlab.(project_commits ~ref_name:default_branch () |> get) in
  let commits = JSON.as_list commits in
  let rec aux = function
    | [] | [_] ->
        Test.fail
          "Could not find a merge commit in the last %d commits on '%s'"
          (List.length commits)
          default_branch
    | commit :: commit_parent :: commits ->
        if is_merge_commit commit then (
          Log.info "%s is a merge commit parent" (commit_hash commit_parent) ;
          let* pipelines =
            (* Fetches the latest pipelines by descending id. So the
               first element in the result should be the pipeline with
               the highest id and so most recent. *)
            Gitlab.(
              project_pipelines
                ~order_by:"id"
                ~sort:"desc"
                ~sha:(commit_hash commit_parent)
                ()
              |> get)
          in
          match JSON.as_list pipelines with
          | pipeline :: _ ->
              let pipeline = JSON.(pipeline |-> "id" |> as_int) in
              Log.info
                "%s has a pipeline %d"
                (commit_hash commit_parent)
                pipeline ;
              return pipeline
          | [] ->
              Log.info "%s has no pipelines, skipping" (commit_hash commit) ;
              aux (commit_parent :: commits))
        else (
          Log.info "%s is not a merge commit, skipping" (commit_hash commit) ;
          aux (commit_parent :: commits))
  in
  aux commits

type from = Pipeline of int | Last_merged_pipeline

let cli_get_from =
  match Cli.get ~default:None (fun s -> Some (Some s)) "from" with
  | Some "last-merged-pipeline" -> Last_merged_pipeline
  | Some s -> (
      match int_of_string_opt s with Some i -> Pipeline i | None -> usage ())
  | None -> usage ()

let () =
  (* Register a test to benefit from error handling of Test.run,
     as well as [Background.start] etc. *)
  ( Test.register ~__FILE__ ~title:"update records" ~tags:["update"] @@ fun () ->
    let* new_records =
      match cli_get_from with
      | Pipeline pipeline_id -> fetch_pipeline_records_from_jobs pipeline_id
      | Last_merged_pipeline ->
          let* pipeline_id = get_last_merged_pipeline () in
          fetch_pipeline_records_from_jobs pipeline_id
    in
    remove_existing_records new_records ;
    unit ) ;
  Test.run ()
