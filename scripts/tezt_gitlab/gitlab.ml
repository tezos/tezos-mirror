open Tezt
open Tezt.Base

let make path =
  Uri.make ~scheme:"https" ~host:"gitlab.com" ~path:("api/v4/" ^ path)

let query_opt name value_opt =
  match value_opt with None -> [] | Some value -> [(name, [value])]

let project_pipelines ~project ?order_by ?sort ?source ?ref_ ?sha ?status =
  make
    (sf "projects/%s/pipelines/" (Uri.pct_encode project))
    ~query:
      (query_opt "ref" ref_ @ query_opt "source" source @ query_opt "sha" sha
     @ query_opt "status" status
      @ query_opt "order_by" order_by
      @ query_opt "sort" sort)

let project_pipeline_jobs ~project ~pipeline =
  make (sf "projects/%s/pipelines/%d/jobs" (Uri.pct_encode project) pipeline)

let project_commits ~project ?ref_name =
  make
    (sf "projects/%s/repository/commits" (Uri.pct_encode project))
    ~query:(query_opt "ref_name" ref_name)

let project_job_artifact ~project ~job_id ~artifact_path =
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
      JSON.parse ~origin:Uri.(to_string full_uri) response_body |> JSON.as_list
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
