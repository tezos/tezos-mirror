(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let simple () =
  Cloud.register
    ~vms:(fun () ->
      return [Agent.Configuration.make (); Agent.Configuration.make ()])
    ~__FILE__
    ~tags:["simple"; "health"]
    ~title:"Simple health check to check local configuration"
  @@ fun t ->
  let agents = Cloud.agents t in
  let agent0 = List.nth agents 0 in
  let agent1 = List.nth agents (1 mod List.length agents) in
  let* output =
    Process.spawn
      ~name:(Agent.name agent0)
      ?runner:(Agent.runner agent0)
      "echo"
      ["Hello world"]
    |> Process.check_and_read_stdout
  in
  Log.info "%s from agent 0" (String.trim output) ;
  let* output =
    Process.spawn
      ~name:(Agent.name agent1)
      ?runner:(Agent.runner agent1)
      "echo"
      ["Hello world"]
    |> Process.check_and_read_stdout
  in
  Log.info "%s from agent 1" (String.trim output) ;
  unit

let run_vm () =
  Cloud.register
    ~vms:(fun () -> return [Agent.Configuration.make ()])
    ~__FILE__
    ~tags:["run"; "vm"]
    ~title:"Run a new VM"
  @@ fun t ->
  let agents = Cloud.agents t in
  let agent = List.nth agents 0 in
  let* _ =
    Agent.host_run_command agent "echo" ["Hello world"]
    |> Process.check_and_read_stdout
  in
  Log.info "VM is ready" ;
  let cmd =
    match Agent.cmd_wrapper agent with
    | None -> assert false
    | Some {cmd; args} -> cmd :: args
  in
  Log.report
    "You can connect onto the VM via the following command:@.%s"
    (String.concat " " cmd) ;
  unit

let run_detached () =
  Cloud.register
    ~vms:(fun () -> return [Agent.Configuration.make ()])
    ~__FILE__
    ~tags:["run"; "detach"]
    ~title:"Run a command and detach in a vm"
  @@ fun t ->
  let agents = Cloud.agents t in
  let agent = List.nth agents 0 in
  Log.info "Run a command and detach. You should not wait" ;
  let* _ =
    Agent.docker_run_command ~detach:true agent "sleep" ["10"] |> Process.wait
  in
  Log.info "OK" ;
  Log.info "Run a command without detaching. You should wait 10sec" ;
  let* _ = Agent.docker_run_command agent "sleep" ["10"] |> Process.wait in
  Log.info "OK" ;
  unit

(** Check that a URL returns the expected HTTP status code. *)
let check_http_status ~url ~expected =
  let* output =
    Curl.get_raw ~args:["-o"; "/dev/null"; "-w"; "%{http_code}"] url
    |> Runnable.run
  in
  let status = String.trim output in
  if status <> expected then
    Test.fail
      "Health check failed for %s: expected %s, got %s"
      url
      expected
      status ;
  Lwt.return_unit

(** [string_mem ~sub s] returns [true] if [sub] is a substring of [s]. *)
let string_mem ~sub s =
  let slen = String.length s in
  let sublen = String.length sub in
  let rec check i =
    if i > slen - sublen then false
    else if String.sub s i sublen = sub then true
    else check (i + 1)
  in
  sublen <= slen && check 0

let check_core_services t =
  (* Default ports used by monitoring services in --localhost mode.
     These are the values from cli.ml defaults. *)
  let website_port = 8080 in
  let prometheus_port = 9090 in
  let grafana_port = 3000 in
  (* 1. Website health *)
  let* () =
    check_http_status
      ~url:(sf "http://localhost:%d/" website_port)
      ~expected:"200"
  in
  Log.info "Website is healthy" ;
  (* 2. Prometheus health *)
  let* () =
    check_http_status
      ~url:(sf "http://localhost:%d/-/ready" prometheus_port)
      ~expected:"200"
  in
  Log.info "Prometheus is healthy" ;
  (* 3. Grafana health *)
  let* () =
    check_http_status
      ~url:(sf "http://localhost:%d/api/health" grafana_port)
      ~expected:"200"
  in
  Log.info "Grafana is healthy" ;
  (* 4. Push a metric and verify it appears in /metrics *)
  Cloud.push_metric t ~name:"tezt_monitoring_test" 1.0 ;
  let* metrics =
    Curl.get_raw (sf "http://localhost:%d/metrics" website_port) |> Runnable.run
  in
  if not (string_mem ~sub:"tezt_monitoring_test" metrics) then
    Test.fail "Pushed metric not found in /metrics endpoint" ;
  Lwt.return_unit

let monitoring () =
  Cloud.register
    ~vms:(fun () -> return [Agent.Configuration.make ()])
    ~__FILE__
    ~tags:["monitoring"; "health"]
    ~title:"Health check for monitoring services"
  @@ fun t ->
  let* () = check_core_services t in
  Log.report "All monitoring services healthy" ;
  unit

let monitoring_full () =
  Cloud.register
    ~vms:(fun () -> return [Agent.Configuration.make ()])
    ~__FILE__
    ~tags:["monitoring_full"; "health"]
    ~title:"Full health check for all monitoring services"
  @@ fun t ->
  let* () = check_core_services t in
  (* OTel health *)
  let* () = check_http_status ~url:"http://localhost:13133" ~expected:"200" in
  Log.info "OTel collector is healthy" ;
  (* Jaeger UI *)
  let* () = check_http_status ~url:"http://localhost:16686/" ~expected:"200" in
  Log.info "Jaeger UI is healthy" ;
  (* Netdata on the agent *)
  let agents = Cloud.agents t in
  let agent = List.hd agents in
  let host = Runner.address (Agent.runner agent) in
  let* () =
    check_http_status ~url:(sf "http://%s:19999/" host) ~expected:"200"
  in
  Log.info "Netdata is healthy" ;
  Log.report "All monitoring services healthy (full)" ;
  unit

let register () =
  simple () ;
  run_vm () ;
  run_detached () ;
  monitoring () ;
  monitoring_full ()
