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

(** Check that a URL returns the expected HTTP status code.
    [extra_args] are passed to curl before the URL (e.g. ["-u"; "admin:secret"]). *)
let check_http_status ?(extra_args = []) ~url ~expected () =
  let* output =
    Curl.get_raw
      ~args:(extra_args @ ["-o"; "/dev/null"; "-w"; "%{http_code}"])
      url
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

let check_core_services ~extra_args ~expected t =
  let website_port = Tezt_cloud_cli.website_port in
  let prometheus_port = Tezt_cloud_cli.prometheus_port in
  let grafana_port = 3000 in
  (* 1. Website health *)
  let* () =
    check_http_status
      ~extra_args
      ~url:(sf "http://localhost:%d/" website_port)
      ~expected
      ()
  in
  Log.info "Website is healthy" ;
  (* 2. Prometheus health *)
  let* () =
    check_http_status
      ~extra_args
      ~url:(sf "http://localhost:%d/-/ready" prometheus_port)
      ~expected
      ()
  in
  Log.info "Prometheus is healthy" ;
  (* 3. Grafana health *)
  let* () =
    check_http_status
      ~extra_args
      ~url:(sf "http://localhost:%d/api/health" grafana_port)
      ~expected
      ()
  in
  Log.info "Grafana is healthy" ;
  (* 4. Check /metrics endpoint *)
  let metrics_url = sf "http://localhost:%d/metrics" website_port in
  let* () = check_http_status ~extra_args ~url:metrics_url ~expected () in
  (* When we can read the body, also verify that pushed metrics appear. *)
  if expected = "200" then (
    Cloud.push_metric t ~name:"tezt_monitoring_test" 1.0 ;
    let* metrics = Curl.get_raw ~args:extra_args metrics_url |> Runnable.run in
    if not (string_mem ~sub:"tezt_monitoring_test" metrics) then
      Test.fail "Pushed metric not found in /metrics endpoint" ;
    Lwt.return_unit)
  else Lwt.return_unit

let monitoring () =
  Cloud.register
    ~vms:(fun () -> return [Agent.Configuration.make ()])
    ~__FILE__
    ~tags:["monitoring"; "health"]
    ~title:"Health check for monitoring services"
  @@ fun t ->
  let* () = check_core_services ~extra_args:[] ~expected:"200" t in
  Log.report "All monitoring services healthy" ;
  unit

let monitoring_full ~expected ~with_auth () =
  Cloud.register
    ~vms:(fun () -> return [Agent.Configuration.make ()])
    ~__FILE__
    ~tags:["monitoring_full"; "health"]
    ~title:
      (Format.sprintf
         "Full health check for all monitoring services (expected_code:%s%s)"
         expected
         (if with_auth then ", with auth" else ""))
  @@ fun t ->
  let extra_args = if with_auth then ["-u"; "admin:secret"] else [] in
  let* () = check_core_services ~extra_args ~expected t in
  (* OTel health — port 13133 is a data-ingestion endpoint, not proxied
     by nginx, so it always returns 200 regardless of auth. *)
  let* () =
    check_http_status ~url:"http://localhost:13133" ~expected:"200" ()
  in
  Log.info "OTel collector is healthy" ;
  (* Jaeger UI — proxied by nginx when auth is enabled *)
  let* () =
    check_http_status ~extra_args ~url:"http://localhost:16686/" ~expected ()
  in
  Log.info "Jaeger UI is healthy" ;
  (* Netdata on the agent.
     - On localhost: Netdata runs inside the agent container and is not
       exposed on the host. The nginx proxy cannot reach it either -> skip.
     - Without auth on GCP: Netdata is directly accessible on agent-ip:19999.
     - With auth on GCP/ssh-host: Netdata is proxied through nginx on the
       orchestrator (port [Env.netdata_proxy_base_port]+). We check via the proxy — both the
       "401 without creds" and "200 with creds" variants go through nginx. *)
  let auth_enabled = with_auth || expected = "401" in
  let* () =
    if Tezt_cloud_cli.localhost then (
      Log.info "Skipping Netdata check (localhost mode, not exposed on host)" ;
      Lwt.return_unit)
    else if auth_enabled then (
      (* Netdata is behind nginx on the orchestrator. We only check the
         first agent — if the proxy works for one, the mechanism
         (add_service + reload) is validated for all. *)
      let netdata_port = Netdata.proxy_base_port in
      let* () =
        check_http_status
          ~extra_args
          ~url:(sf "http://localhost:%d/" netdata_port)
          ~expected
          ()
      in
      Log.info "Netdata is healthy (via nginx proxy on port %d)" netdata_port ;
      Lwt.return_unit)
    else
      let agents = Cloud.agents t in
      let agent = List.hd agents in
      let host = Runner.address (Agent.runner agent) in
      let* () =
        check_http_status
          ~extra_args
          ~url:(sf "http://%s:19999/" host)
          ~expected
          ()
      in
      Log.info "Netdata is healthy" ;
      Lwt.return_unit
  in
  Log.report "All monitoring services healthy (full)" ;
  unit

let nginx () =
  Cloud.register
    ~__FILE__
    ~tags:["nginx"; "auth"]
    ~title:"Nginx reverse proxy with basic auth"
  @@ fun _t ->
  (* Start a dummy HTTP backend via Docker on port 18080.
     We provide a minimal config because the default nginx:alpine listens
     on port 80 which would conflict with the nginx-auth proxy container
     (both use --network host). *)
  let dummy_name = "dummy-backend" in
  let dummy_port = 18080 in
  let dummy_conf = Path.tmp_dir // "dummy-backend.conf" in
  let () =
    with_open_out dummy_conf (fun oc ->
        output_string
          oc
          (sf
             {|server { listen %d; location / { return 200 'ok\n'; } }|}
             dummy_port))
  in
  let* () =
    Process.run
      "docker"
      [
        "run";
        "-d";
        "--rm";
        "--name";
        dummy_name;
        "--network";
        "host";
        "-v";
        sf "%s:/etc/nginx/conf.d/default.conf:ro" dummy_conf;
        "nginx:alpine";
      ]
  in
  let cleanup_dummy () = Process.run "docker" ["kill"; dummy_name] in
  (* Wait for dummy backend to be ready. We use Process.spawn directly
     because curl returns exit code 7 when the backend is not yet
     listening, and Runnable.run / Process.check would fail the test. *)
  let rec wait_ready retries =
    let process =
      Process.spawn
        "curl"
        [
          "-s";
          "-o";
          "/dev/null";
          "-w";
          "%{http_code}";
          sf "http://localhost:%d/" dummy_port;
        ]
    in
    let* status = Process.wait process in
    let* output = Lwt_io.read (Process.stdout process) in
    match status with
    | Unix.WEXITED 0 when String.trim output = "200" -> Lwt.return_unit
    | _ ->
        if retries <= 0 then Test.fail "Dummy backend did not become ready"
        else
          let* () = Lwt_unix.sleep 3. in
          wait_ready (retries - 1)
  in
  let* () = wait_ready 10 in
  Log.info "Dummy backend is ready on port %d" dummy_port ;
  (* Start nginx with basic auth proxying port 28080 -> dummy on 18080 *)
  let proxy_port = 28080 in
  let username = "admin" in
  let password = "test123" in
  let* nginx =
    Nginx.run
      ~username
      ~password
      ~services:
        [
          {
            listen_port = proxy_port;
            proxy_target = sf "http://127.0.0.1:%d" dummy_port;
          };
        ]
      ()
  in
  Lwt.finalize
    (fun () ->
      (* Test 1: Without auth -> 401 *)
      let* () =
        check_http_status
          ~url:(sf "http://localhost:%d/" proxy_port)
          ~expected:"401"
          ()
      in
      Log.info "Without auth: got 401 as expected" ;
      (* Test 2: With auth -> 200 *)
      let* () =
        check_http_status
          ~extra_args:["-u"; sf "%s:%s" username password]
          ~url:(sf "http://localhost:%d/" proxy_port)
          ~expected:"200"
          ()
      in
      Log.info "With auth: got 200 as expected" ;
      (* Test 3: Add a second service dynamically *)
      let proxy_port_2 = 28081 in
      let* () =
        Nginx.add_service
          nginx
          {
            listen_port = proxy_port_2;
            proxy_target = sf "http://127.0.0.1:%d" dummy_port;
          }
      in
      (* Verify the new port also requires auth *)
      let* () =
        check_http_status
          ~url:(sf "http://localhost:%d/" proxy_port_2)
          ~expected:"401"
          ()
      in
      Log.info "Dynamic service: got 401 without auth" ;
      (* Verify original port still works *)
      let* () =
        check_http_status
          ~extra_args:["-u"; sf "%s:%s" username password]
          ~url:(sf "http://localhost:%d/" proxy_port)
          ~expected:"200"
          ()
      in
      Log.info "Original port still works after add_service" ;
      (* Test 4: Config generation — add a third service, then verify
     /tmp/nginx/default.conf contains 3 server{} blocks with correct
     ports and proxy targets. *)
      let proxy_port_3 = 28082 in
      let* () =
        Nginx.add_service
          nginx
          {
            listen_port = proxy_port_3;
            proxy_target = sf "http://127.0.0.1:%d" dummy_port;
          }
      in
      let config_path = Path.tmp_dir // "nginx" // "default.conf" in
      let config = read_file config_path in
      let expected_ports = [proxy_port; proxy_port_2; proxy_port_3] in
      let expected_target = sf "http://127.0.0.1:%d" dummy_port in
      List.iter
        (fun port ->
          let listen = sf "listen 0.0.0.0:%d" port in
          if not (string_mem ~sub:listen config) then
            Test.fail "Config missing server block for port %d" port ;
          if not (string_mem ~sub:(sf "proxy_pass %s" expected_target) config)
          then
            Test.fail "Config missing proxy_pass for target %s" expected_target)
        expected_ports ;
      (* Count server{} blocks *)
      let server_count =
        let sub = "server {" in
        let sublen = String.length sub in
        let rec count i acc =
          if i > String.length config - sublen then acc
          else if String.sub config i sublen = sub then count (i + 1) (acc + 1)
          else count (i + 1) acc
        in
        count 0 0
      in
      if server_count <> 3 then
        Test.fail "Expected 3 server blocks, found %d" server_count ;
      Log.info
        "Config generation: 3 server blocks with correct ports and targets" ;
      Log.report "Nginx auth tests passed" ;
      unit)
    (fun () ->
      let* () = Nginx.shutdown nginx in
      cleanup_dummy ())

let register () =
  simple () ;
  run_vm () ;
  run_detached () ;
  monitoring () ;
  (* This test passes only if authentication is off. *)
  monitoring_full ~expected:"200" ~with_auth:false () ;
  (* This test passes only if authentication is on. *)
  monitoring_full ~expected:"401" ~with_auth:false () ;
  (* This test passes only if authentication is on and username:password are admin:secret. *)
  monitoring_full ~expected:"200" ~with_auth:true () ;
  nginx ()
