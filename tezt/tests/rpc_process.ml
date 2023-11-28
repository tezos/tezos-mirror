(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Node's RPC_process
   Invocation:   dune exec tezt/tests/main.exe -- --file rpc_process.ml
   Subject:      Tests the resilience of the RPC process
*)

let wait_for_RPC_process_pid node =
  let filter json = JSON.(json |> as_int_opt) in
  Node.wait_for node "rpc_process_started.v0" filter

type signal = SIGABRT | SIGINT | SIGKILL | SIGQUIT | SIGTERM

let signal_to_int = function
  | SIGABRT -> Sys.sigabrt
  | SIGINT -> Sys.sigint
  | SIGKILL -> Sys.sigkill
  | SIGQUIT -> Sys.sigquit
  | SIGTERM -> Sys.sigterm

let pp_signal ppf signal =
  let str =
    match signal with
    | SIGABRT -> "sigabrt"
    | SIGINT -> "sigint"
    | SIGKILL -> "sigkill"
    | SIGQUIT -> "sigquit"
    | SIGTERM -> "sigterm"
  in
  Format.fprintf ppf "%s" str

let kill_process ~pid ~signal =
  Log.info "Kill the rpc process (pid %d) with signal %a" pid pp_signal signal ;
  Unix.kill pid (signal_to_int signal)

let head_can_be_requested ~expected_level client =
  let* json = Client.RPC.call client @@ RPC.get_chain_block_header () in
  let v = JSON.(json |-> "level" |> as_int) in
  return (v = expected_level)

let test_kill =
  Protocol.register_test
    ~__FILE__
    ~title:"RPC process kill"
    ~tags:["rpc"; "process"; "kill"]
  @@ fun protocol ->
  let node = Node.create [] in
  let wait_RPC_process_pid = wait_for_RPC_process_pid node in
  let* () = Node.run node [] in
  let* () = Node.wait_for_ready node in
  let* client = Client.init ~endpoint:(Node node) () in
  let* rpc_process_pid = wait_RPC_process_pid in
  Log.info "RPC process is now runnig on pid %d" rpc_process_pid ;
  let* () = Client.activate_protocol_and_wait ~protocol client in
  Log.info "Wait for level 1" ;
  let* (_ : int) = Node.wait_for_level node 1 in
  Log.info "Head can be requested" ;
  let* (_ : bool) = head_can_be_requested ~expected_level:1 client in
  let wait_new_rpc_process_pid = wait_for_RPC_process_pid node in
  let () = kill_process ~pid:rpc_process_pid ~signal:SIGKILL in
  let* new_rpc_process_pid = wait_new_rpc_process_pid in
  Log.info "The new RPC process is now runnig on pid %d" new_rpc_process_pid ;
  Log.info "Head can still be requested" ;
  let* (_ : bool) = head_can_be_requested ~expected_level:1 client in
  unit

type expected_behavior = Forward | Handle

let test_forward =
  Protocol.register_test
    ~__FILE__
    ~title:"RPC process forward"
    ~tags:["rpc"; "process"; "forward"]
  @@ fun protocol ->
  Log.info
    "Test whether some specific RPCs are handled directly by the RPC process \
     or forwarded to the node." ;
  let* node, client =
    Client.init_with_protocol
      ~event_sections_levels:[("rpc-process", `Debug)]
      ~protocol
      `Client
      ()
  in
  let wait_for expected_behavior ~rpc_prefix =
    let filter json =
      if String.starts_with ~prefix:rpc_prefix (JSON.as_string json) then
        Some ()
      else None
    in
    let where = sf "rpc_prefix = %s" rpc_prefix in
    let event_name =
      match expected_behavior with
      | Forward -> "forwarding_rpc.v0"
      | Handle -> "locally_handled_rpc.v0"
    in
    Node.wait_for ~where node event_name filter
  in
  let test_rpc expected_behavior ?error ~rpc_prefix rpc =
    Log.info "Test %s" rpc_prefix ;
    let waiter = wait_for expected_behavior ~rpc_prefix in
    let* () =
      match error with
      | None ->
          let* _ = Node.RPC.call node rpc in
          unit
      | Some msg ->
          let*? process = Client.RPC.spawn client rpc in
          Process.check_error ~msg:(rex msg) process
    in
    waiter
  in
  let* () = test_rpc Handle RPC.get_version ~rpc_prefix:"/version" in
  let* () = test_rpc Handle RPC.get_config ~rpc_prefix:"/config" in
  let* () =
    test_rpc
      Forward
      (RPC.get_chain_chain_id ())
      ~rpc_prefix:"/chains/main/chain_id"
  in
  let* () =
    test_rpc
      Forward
      (RPC.get_chain_chain_id ~chain:"test" ())
      ~rpc_prefix:"/chains/test/chain_id"
      ~error:"No service found at this URL"
  in
  let* () =
    test_rpc
      Forward
      (RPC.get_chain_chain_id ~chain:"nonexistent" ())
      ~rpc_prefix:"/chains/nonexistent/chain_id"
      ~error:"Cannot parse chain identifier"
  in
  let* () =
    test_rpc
      Forward
      (RPC.post_chain_mempool_filter ~data:(Data (Ezjsonm.from_string "{}")) ())
      ~rpc_prefix:"/chains/main/mempool/filter"
  in
  let* () =
    test_rpc
      Forward
      RPC.nonexistent_path
      ~rpc_prefix:"/nonexistent/path"
      ~error:"No service found at this URL"
  in
  let test_streaming_rpc ~rpc_prefix rpc =
    Log.info "Test streaming RPC: %s" rpc_prefix ;
    let waiter = wait_for Forward ~rpc_prefix in
    let url =
      RPC_core.make_uri (Node.as_rpc_endpoint node) rpc |> Uri.to_string
    in
    let*? _process = Curl.get url in
    waiter
  in
  let* () =
    test_streaming_rpc
      (RPC.get_chain_mempool_monitor_operations ())
      ~rpc_prefix:"/chains/main/mempool/monitor_operations"
  in
  let* () =
    test_streaming_rpc
      (RPC.get_monitor_heads_chain ())
      ~rpc_prefix:"/monitor/heads/main"
  in
  let* () =
    test_streaming_rpc
      (RPC.get_monitor_heads_chain ~chain:"test" ())
      ~rpc_prefix:"/monitor/heads/test"
  in
  let* () =
    test_streaming_rpc
      RPC.get_monitor_validated_blocks
      ~rpc_prefix:"/monitor/validated_blocks"
  in
  test_streaming_rpc
    RPC.get_monitor_applied_blocks
    ~rpc_prefix:"/monitor/applied_blocks"

let point_of_port port = "127.0.0.1:" ^ Int.to_string port

let wait_for_starting_rpc_server_event ~local ?fail node port =
  let filter =
    match fail with
    | None ->
        fun json ->
          let event_port = JSON.(json |-> "port" |> as_int) in
          if port = event_port then Some () else None
    | Some fail_msg -> fun _ -> Test.fail fail_msg
  in
  Node.wait_for
    node
    (if local then "starting_local_rpc_server.v0" else "starting_rpc_server.v0")
    filter

let make_endpoint port =
  Client.Foreign_endpoint Endpoint.{host = "127.0.0.1"; scheme = "http"; port}

let test_local_rpc_server =
  Test.register
    ~__FILE__
    ~title:"RPC local server"
    ~tags:["rpc"; "process"; "local_server"]
  @@ fun () ->
  let node = Node.create ~rpc_local:true [] in
  (* Register event watchers for local RPC server before the node is running to
     ensure they will not be missed. *)
  let local_event_promise =
    wait_for_starting_rpc_server_event ~local:true node (Node.rpc_port node)
  in
  (* Register event watchers for process RPC server that will make the test
     fails if detected. *)
  let _ =
    wait_for_starting_rpc_server_event
      ~local:false
      ~fail:"Process RPC server detected"
      node
      (Node.rpc_port node)
  in
  (* Run the node *)
  let* () = Node.run node [] in
  let* () = Node.wait_for_ready node in
  Log.info "Node ready." ;
  let* client = Client.init () in
  Log.info "Checking if host is available." ;
  let* _ =
    Client.RPC.call ~endpoint:(make_endpoint (Node.rpc_port node)) client
    @@ RPC.get_version
  in
  Log.info "Checking if local RPC server has been well started" ;
  local_event_promise

let test_process_rpc_server =
  Test.register
    ~__FILE__
    ~title:"RPC process server"
    ~tags:["rpc"; "process"; "local_server"]
  @@ fun () ->
  (* By default Node module start a process RPC server. *)
  let node = Node.create [] in
  (* Register event watchers for process RPC server before the node is running
     to ensure they will not be missed. *)
  let process_event_promise =
    wait_for_starting_rpc_server_event ~local:false node (Node.rpc_port node)
  in
  (* Register event watchers for local RPC server that will make the test fails
     if detected. *)
  let _ =
    wait_for_starting_rpc_server_event
      ~local:true
      ~fail:"Local RPC server detected"
      node
      (Node.rpc_port node)
  in
  (* Run the node *)
  let* () = Node.run node [] in
  let* () = Node.wait_for_ready node in
  Log.info "Node ready." ;
  let* client = Client.init () in
  Log.info "Checking if host is available." ;
  let* _ =
    Client.RPC.call ~endpoint:(make_endpoint (Node.rpc_port node)) client
    @@ RPC.get_version
  in
  Log.info "Checking if process RPC server has been well started" ;
  process_event_promise

let test_local_and_process_rpc_servers =
  Test.register
    ~__FILE__
    ~title:"RPC local servers and process servers"
    ~tags:["rpc"; "process"; "local_server"]
  @@ fun () ->
  (* Generate some ports for additional process RPC servers *)
  let process_rpc_ports = List.init 2 (fun _ -> Port.fresh ()) in
  (* Generate some ports for additional local RPC servers *)
  let local_rpc_ports = List.init 2 (fun _ -> Port.fresh ()) in
  let node =
    Node.create
      (List.map
         (fun port -> Node.RPC_additional_addr (point_of_port port))
         process_rpc_ports
      @ List.map
          (fun port -> Node.RPC_additional_addr_local (point_of_port port))
          local_rpc_ports)
  in
  (* Add the RPC server created by default in Node module. *)
  let process_rpc_ports = Node.rpc_port node :: process_rpc_ports in
  (* Register event watchers for both process and local RPC servers before the
     node is running to ensure they will not be missed. *)
  let process_event_promises =
    List.map
      (wait_for_starting_rpc_server_event ~local:false node)
      process_rpc_ports
  in
  let local_event_promises =
    List.map
      (wait_for_starting_rpc_server_event ~local:true node)
      local_rpc_ports
  in
  (* Run the node *)
  let* () = Node.run node [] in
  let* () = Node.wait_for_ready node in
  Log.info "Node ready." ;
  let* client = Client.init () in
  Log.info "Checking if process hosts are available." ;
  let* _ =
    Lwt_list.map_p
      (fun port ->
        Client.RPC.call ~endpoint:(make_endpoint port) client @@ RPC.get_version)
      process_rpc_ports
  in
  Log.info "Checking if local hosts are available." ;
  let* _ =
    Lwt_list.map_p
      (fun port ->
        Client.RPC.call ~endpoint:(make_endpoint port) client @@ RPC.get_version)
      local_rpc_ports
  in
  Log.info "Checking if process RPC servers have been well started" ;
  let* () = Lwt_list.iter_p (fun p -> p) process_event_promises in
  Log.info "Checking if local RPC servers have been well started" ;
  Lwt_list.iter_p (fun p -> p) local_event_promises

let register ~protocols =
  test_kill protocols ;
  test_forward protocols
