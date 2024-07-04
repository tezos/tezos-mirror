(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type target = {address : string; port : int; app_name : string}

type source = {job_name : string; metric_path : string; targets : target list}

type t = {
  configuration_file : string;
  mutable sources : source list;
  scrape_interval : int;
  snapshot_filename : string option;
  port : int;
}

let netdata_source_of_agents agents =
  let job_name = "netdata" in
  let metric_path = "/api/v1/allmetrics?format=prometheus&help=yes" in
  let target agent =
    let app_name = Agent.name agent in
    let address = agent |> Agent.runner |> Option.some |> Runner.address in
    {address; port = 19999; app_name}
  in
  let targets = List.map target agents in
  {job_name; metric_path; targets}

let prefix ~scrape_interval () =
  Format.asprintf
    {|
global:
  scrape_interval: %ds
scrape_configs:  
|}
    scrape_interval

let str_of_target {address; port; app_name} =
  Format.asprintf
    {|
    - targets: ['%s:%d']
      labels:
        app: '%s'
    |}
    address
    port
    app_name

let str_of_source {job_name; metric_path; targets} =
  Format.asprintf
    {|
  - job_name: %s
    metrics_path: %s
    params:
      format: ['prometheus'] 
    static_configs:      
%s      
|}
    job_name
    metric_path
    (targets |> List.map str_of_target |> String.concat "")

let tezt_source =
  {
    job_name = "tezt_metrics";
    metric_path = "/metrics.txt";
    targets = [{address = "localhost"; port = 8080; app_name = "tezt"}];
  }

let config ~scrape_interval sources =
  let sources = List.map str_of_source sources |> String.concat "" in
  prefix ~scrape_interval () ^ sources

let write_configuration_file {scrape_interval; configuration_file; sources; _} =
  let config = config ~scrape_interval sources in
  with_open_out configuration_file (fun oc ->
      Stdlib.seek_out oc 0 ;
      output_string oc config)

(* Prometheus can reload its configuration by first sending the POST RPC and
   then the signal SIGHUP. *)
let reload _t =
  let* () = Process.run "curl" ["-XPOST"; "http://localhost:9090/-/reload"] in
  Process.run "docker" ["kill"; "--signal"; "SIGHUP"; "prometheus"]

let add_source t ?(metric_path = "/metrics") ~job_name targets =
  let source = {job_name; metric_path; targets} in
  t.sources <- source :: t.sources ;
  write_configuration_file t ;
  reload t

let start agents =
  let sources =
    if Env.monitoring then [tezt_source; netdata_source_of_agents agents]
    else [tezt_source]
  in
  let configuration_file = Temp.file "prometheus.yml" in
  let snapshot_filename = Env.prometheus_snapshot_filename in
  let port = Env.prometheus_port in
  let scrape_interval = Env.prometheus_scrape_interval in
  let t =
    {configuration_file; sources; scrape_interval; snapshot_filename; port}
  in
  write_configuration_file t ;
  let process =
    Process.spawn
      "docker"
      [
        "run";
        "--rm";
        "-d";
        "--name";
        "prometheus";
        "--network";
        "host";
        (* We use the host mode so that in [localhost], prometheus can see the
           metrics endpoint run by other docker containers. *)
        "-v";
        Format.asprintf "%s:/etc/prometheus/prometheus.yml" configuration_file;
        "prom/prometheus";
        "--config.file=/etc/prometheus/prometheus.yml";
        "--web.enable-admin-api";
        (* To export a snapshot. *)
        "--web.enable-lifecycle";
        (* To reload the configuration while prometheus is running.*)
        Format.asprintf "--web.listen-address=:%d" port;
        (* Specify the port on which the prometheus instance runs. *)
      ]
  in
  let* status = Process.wait process in
  let* () =
    match status with
    | WEXITED 0 -> Lwt.return_unit
    | WEXITED 125 ->
        Log.warn
          "A prometheus instance is already running. It was not properly \
           closed last time" ;
        Lwt.return_unit
    | _ ->
        (* Fail if something unexpected happens. *)
        Process.check process
  in
  let is_ready output = String.trim output = "200" in
  let run () =
    Process.spawn
      "curl"
      [
        "-s";
        "-o";
        "/dev/null";
        "-w";
        "%{http_code}";
        "http://localhost:9090/-/ready";
      ]
  in
  let* _ = Env.wait_process ~is_ready ~run () in
  Lwt.return t

let snapshots_path = "/prometheus" // "data" // "snapshots"

let export_snapshot {snapshot_filename; _} =
  Log.info "Exporting snapshot..." ;
  let* stdout =
    Process.run_and_read_stdout
      "curl"
      ["-XPOST"; "http://localhost:9090/api/v1/admin/tsdb/snapshot"]
  in
  let json = JSON.parse ~origin:"Prometheus.export" stdout in
  let snapshot_name = JSON.(json |-> "data" |-> "name" |> as_string) in
  let destination =
    Option.value
      ~default:(Filename.get_temp_dir_name () // snapshot_name)
      snapshot_filename
  in
  let* () =
    Docker.cp
      "prometheus"
      ~kind:`To_host
      ~source:(snapshots_path // snapshot_name)
      ~destination
    |> Process.check
  in
  Log.info "You can find the prometheus snapshot at %s" destination ;
  Lwt.return_unit

let shutdown {configuration_file = _; _} =
  let* () = Docker.kill "prometheus" |> Process.check in
  Lwt.return_unit

let run_with_snapshot () =
  (* No need for a configuration file here. *)
  let configuration_file = "" in
  let port = Env.prometheus_port in
  let snapshot_filename =
    match Env.prometheus_snapshot_filename with
    | None ->
        Test.fail
          "You must specify the snapshot filename via \
           --prometheus-snapshot-filename"
    | Some file -> file
  in
  Log.info
    "You can find the prometheus instance at http://localhost:%d"
    Env.prometheus_port ;
  Log.info "Use Ctrl+C to end the scenario and kill the prometheus instance." ;
  let* () =
    Process.run
      "docker"
      [
        "run";
        "-uroot";
        "-v";
        Format.asprintf "%s:/prometheus" snapshot_filename;
        "-p";
        Format.asprintf "%d:9090" port;
        "prom/prometheus";
        "--config.file=/etc/prometheus/prometheus.yml";
        "--storage.tsdb.path=/prometheus";
      ]
  in
  Lwt.return
    {
      configuration_file;
      sources = [];
      scrape_interval = 0;
      snapshot_filename = Some snapshot_filename;
      port;
    }
