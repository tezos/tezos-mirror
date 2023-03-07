open Tezt
open Base

let usage () =
  prerr_endline
    {|Usage: PIPELINE=<PIPELINE_ID> dune exec tezt/records/update.exe

Example: to fetch test result records from
https://gitlab.com/tezos/tezos/-/pipelines/426773806, run
(from the root of the repository):

PIPELINE=426773806 dune exec tezt/records/update.exe

You can use the PROJECT environment variable to specify which GitLab
repository to fetch records from. Default is: tezos/tezos|} ;
  exit 1

let pipeline =
  match Sys.getenv_opt "PIPELINE" with
  | None -> usage ()
  | Some value -> (
      match int_of_string_opt value with
      | None ->
          prerr_endline "Invalid pipeline ID (not an integer).\n" ;
          usage ()
      | Some value -> value)

let project = Sys.getenv_opt "PROJECT" |> Option.value ~default:"tezos/tezos"

let rec get_all_pages ?(from = 1) ?(acc = []) url =
  let full_url = url ^ "?per_page=200&page=" ^ string_of_int from in
  let* response_body = Process.run_and_read_stdout "curl" [full_url] in
  let list = JSON.parse ~origin:url response_body |> JSON.as_list in
  Log.info "Found %d jobs in page %d." (List.length list) from ;
  match list with
  | [] -> return (List.rev acc)
  | _ :: _ -> get_all_pages ~from:(from + 1) ~acc:(List.rev_append list acc) url

let records_directory = "tezt/records"

let fetch_record (url, index) =
  let local = records_directory // (index ^ ".json") in
  (* GitLab uses a lot of redirections so we use --location to follow them. *)
  let* () = Process.run "curl" [url; "--location"; "--output"; local] in
  Log.info "Downloaded: %s" local ;
  match JSON.parse_file local with
  | exception (JSON.Error _ as exn) ->
      Log.error
        "Failed to parse downloaded JSON file, maybe the artifact has expired?" ;
      raise exn
  | (_ : JSON.t) -> unit

let remove_existing_records () =
  let remove_if_looks_like_a_record filename =
    if filename =~ rex "^\\d+\\.json$" then (
      let filename = records_directory // filename in
      Sys.remove filename ;
      Log.info "Removed outdated record: %s" filename)
  in
  Array.iter remove_if_looks_like_a_record (Sys.readdir records_directory)

let fetch_pipeline_records () =
  let project = Uri.pct_encode project in
  let* jobs =
    get_all_pages
      ("https://gitlab.com/api/v4/projects/" ^ project ^ "/pipelines/"
     ^ string_of_int pipeline ^ "/jobs")
  in
  let get_record job =
    let job_id = JSON.(job |-> "id" |> as_int) in
    let name = JSON.(job |-> "name" |> as_string) in
    match name =~* rex "^tezt (\\d+)/\\d+$" with
    | None -> None
    | Some index ->
        Some
          ( "https://gitlab.com/api/v4/projects/" ^ project ^ "/jobs/"
            ^ string_of_int job_id ^ "/artifacts/tezt-results-" ^ index
            ^ ".json",
            index )
  in
  let records = List.filter_map get_record jobs in
  Log.info "Found %d Tezt jobs." (List.length records) ;
  Lwt_list.iter_p fetch_record records

let () =
  (* Register a test to benefit from error handling of Test.run,
     as well as [Background.start] etc. *)
  ( Test.register ~__FILE__ ~title:"update records" ~tags:["update"] @@ fun () ->
    remove_existing_records () ;
    fetch_pipeline_records () ) ;
  Test.run ()
