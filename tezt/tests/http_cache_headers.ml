(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Http cache headers RPC Middleware
   Invocation:   dune exec tezt/tests/main.exe -- --file http_cache_headers.ml
   Subject:      Test Http cache headers RPC middleware behaves correctly
*)

(** [check_max_age_in_headers ?expects_missing_header ~__LOC__ headers] returns 
   [unit] or fails if "cache-control: max-age" is missing from [headers]. If 
   [expects_missing_header] is set to [true], it returns [unit] instead of 
   failing when "max-age" is missing. *)
let check_max_age_in_headers ?(expects_missing_header = false) ~__LOC__ headers
    =
  match RPC_core.HeaderMap.find_opt "cache-control" headers with
  | None ->
      if expects_missing_header then Lwt.return_unit
      else Test.fail ~__LOC__ "cache-control header not found"
  | Some cache_control -> (
      let cache_control_parts =
        List.map
          (fun s -> String.trim s)
          (String.split_on_char ',' cache_control)
      in
      let max_age_opt =
        List.find_opt
          (fun s -> String.starts_with ~prefix:"max-age" s)
          cache_control_parts
      in
      match max_age_opt with
      | Some _ -> Lwt.return_unit
      | None -> Test.fail ~__LOC__ "max-age not found in cache-control header")

let get_etag ~__LOC__ headers =
  match RPC_core.HeaderMap.find_opt "etag" headers with
  | None -> Test.fail ~__LOC__ "etag not found"
  | Some etag -> etag

(* [test_max_age] tests the presence of max-age header field
    when the round duration has not yet elapsed and the absence
    of the header field when the round duration has elapsed and
    no new block has arrived. *)
let test_max_age ~rpc_external =
  let title =
    "max-age header for " ^ if rpc_external then "external rpc" else "local rpc"
  in
  Protocol.register_test
    ~__FILE__
    ~title
    ~tags:["rpc"; "middleware"; "http_cache_headers"]
    ~supports:(From_protocol 19)
  @@ fun protocol ->
  Log.info "Initialize client, node and baker" ;
  let node =
    Node.create
      ~rpc_external
      [Connections 0; Synchronisation_threshold 0; Enable_http_cache_headers]
  in
  let http_cache_headers_enabled_event =
    if rpc_external then
      Node.wait_for node "enable_http_cache_headers_for_external.v0" Option.some
    else Node.wait_for node "enable_http_cache_headers_for_local.v0" Option.some
  in
  let* () = Node.run node [] in
  let* () = Node.wait_for_ready node in
  let* client = Client.init ~endpoint:(Client.Node node) () in
  let* _ = http_cache_headers_enabled_event in
  let delegates =
    Array.to_list
    @@ Array.map (fun key -> Account.(key.alias)) Account.Bootstrap.keys
  in
  Log.info "Activate protocol" ;
  let block_time = 2 in
  let* parameter_file =
    Protocol.write_parameter_file
      ~base:(Right (protocol, None))
      [(["minimal_block_delay"], `String (string_of_int block_time))]
  in
  let* () =
    Client.activate_protocol ~timestamp:Now ~parameter_file ~protocol client
  in
  Log.info "Bake and wait for block" ;
  let* () =
    Client.bake_for_and_wait ~minimal_timestamp:true ~keys:delegates client
  in
  Log.info "Check max-age is present" ;
  let* {headers; _} = Node.RPC.call_raw node (RPC.get_chain_block_hash ()) in
  let* () = check_max_age_in_headers ~__LOC__ headers in
  Log.info "Check max-age not present after max-age duration" ;
  let* () = Lwt_unix.sleep (Float.of_int (block_time + 1)) in
  let* {headers; _} = Node.RPC.call_raw node (RPC.get_chain_block_hash ()) in
  let* () =
    check_max_age_in_headers ~expects_missing_header:true ~__LOC__ headers
  in
  let* () = Node.terminate node in
  unit

(* [test_if_none_match] does the following:
    1. Test the presence of etag header and asserts etag = block hash
    2. Test rpc returns the response body when etag for current query
       is not found in the if-none-match field
    3. Test rpc returns 304 modified when etag for current query is
       found in the if-none-match field *)
let test_if_none_match ~rpc_external =
  let title =
    "if-none-match header for "
    ^ if rpc_external then "external rpc" else "local rpc"
  in
  Protocol.register_test
    ~__FILE__
    ~title
    ~tags:["rpc"; "middleware"; "http_cache_headers"]
    ~supports:(From_protocol 19)
    ~uses:(fun _protocol -> [Constant.octez_agnostic_baker])
  @@ fun protocol ->
  Log.info "Initialize client, node and baker" ;
  let* node, client =
    Client.init_with_node
      ~nodes_args:
        [Connections 0; Synchronisation_threshold 0; Enable_http_cache_headers]
      ~rpc_external
      `Client
      ()
  in
  let delegates =
    Array.to_list
    @@ Array.map (fun key -> Account.(key.alias)) Account.Bootstrap.keys
  in
  Log.info "Activate protocol" ;
  let block_time = 2 in
  let* parameter_file =
    Protocol.write_parameter_file
      ~base:(Right (protocol, None))
      [(["minimal_block_delay"], `String (string_of_int block_time))]
  in
  let* () =
    Client.activate_protocol ~timestamp:Now ~parameter_file ~protocol client
  in
  Log.info "Bake and wait for block" ;
  let* baker = Agnostic_baker.init ~delegates node client in

  Log.info "Check etag found in query" ;
  let* _ = Node.wait_for_level node 2 in
  let* {headers; body; _} =
    Node.RPC.call_raw node (RPC.get_chain_block_hash ())
  in
  let etag = get_etag ~__LOC__ headers in
  Check.(
    (etag = JSON.(parse ~origin:body body |> as_string))
      string
      ~__LOC__
      ~error_msg:
        (Format.sprintf "Expected etag to equal block hash: %s <> %s" etag body)) ;

  Log.info "Check response is returned when etag not found in if-none-match" ;
  let* _ = Node.wait_for_level node 3 in
  let previous_level_etag = etag in
  let* {headers; body; _} =
    Node.RPC.call_raw
      ~extra_headers:[("if-none-match", previous_level_etag)]
      node
      (RPC.get_chain_block_hash ())
  in
  let* () = Agnostic_baker.terminate baker in
  Check.(
    (String.length body > 0)
      int
      ~__LOC__
      ~error_msg:(Format.sprintf "Expected body to be non empty")) ;

  Log.info "Check 304 not modified response when etag in if-none-match" ;
  let current_level_etag = get_etag ~__LOC__ headers in
  let* {code; _} =
    Node.RPC.call_raw
      node
      ~extra_headers:[("if-none-match", current_level_etag)]
      (RPC.get_chain_block_hash ())
  in
  Check.(
    (code = 304)
      int
      ~__LOC__
      ~error_msg:
        (Format.sprintf
           "Expected error code to be 304 Not Modified, got %d"
           code)) ;
  let* () = Node.terminate node in
  unit

let register ~protocols =
  test_max_age ~rpc_external:true protocols ;
  test_max_age ~rpc_external:false protocols ;
  test_if_none_match ~rpc_external:true protocols ;
  test_if_none_match ~rpc_external:false protocols
