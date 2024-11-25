(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type target = {address : string; port : int; app_name : string}

let target_jingoo_template target =
  let open Jingoo.Jg_types in
  Tobj
    [
      ("point", Tstr (Format.asprintf "%s:%d" target.address target.port));
      ("app", Tstr target.app_name);
    ]

type job = {name : string; metrics_path : string; targets : target list}

let job_jingoo_template job =
  let open Jingoo.Jg_types in
  Tobj
    [
      ("name", Tstr job.name);
      ("metrics_path", Tstr job.metrics_path);
      ("targets", Tlist (List.map target_jingoo_template job.targets));
    ]

type alert = {name : string; expr : string; for_ : string option}

let alert_jingoo_template alert =
  let open Jingoo.Jg_types in
  Tobj
    ([("name", Tstr alert.name); ("expr", Tstr alert.expr)]
    @ match alert.for_ with None -> [] | Some for_ -> [("for_", Tstr for_)])

type t = {
  configuration_file : string;
  rules_file : string;
  alert_manager : bool;
  mutable jobs : job list;
  scrape_interval : int;
  snapshot_filename : string option;
  port : int;
  mutable alerts : alert list;
}

let netdata_source_of_agents agents =
  let name = "netdata" in
  let metrics_path = "/api/v1/allmetrics?format=prometheus&help=yes" in
  let target agent =
    let app_name = Agent.name agent in
    let address = agent |> Agent.runner |> Runner.address in
    {address; port = 19999; app_name}
  in
  let targets = List.map target agents in
  {name; metrics_path; targets}

let tezt_source =
  {
    name = "tezt_metrics";
    metrics_path = "/metrics.txt";
    targets =
      [
        {
          address = "localhost";
          port = (if Env.mode = `Orchestrator then 80 else 8080);
          app_name = "tezt";
        };
      ];
  }

let jingoo_configuration_template t =
  let open Jingoo.Jg_types in
  [
    ("scrape_interval", Tint t.scrape_interval);
    ("jobs", Tlist (List.map job_jingoo_template t.jobs));
    ("alert_manager", Tbool t.alert_manager);
  ]

let write_configuration_file t =
  let content =
    Jingoo.Jg_template.from_file
      Path.prometheus_configuration
      ~models:(jingoo_configuration_template t)
  in
  with_open_out t.configuration_file (fun oc ->
      Stdlib.seek_out oc 0 ;
      output_string oc content)

let jingoo_alert_template t =
  let open Jingoo.Jg_types in
  [("alerts", Tlist (List.map alert_jingoo_template t.alerts))]

let write_rules_file t =
  let content =
    Jingoo.Jg_template.from_file
      ~env:{Jingoo.Jg_types.std_env with autoescape = false}
      Path.prometheus_rules_configuration
      ~models:(jingoo_alert_template t)
  in
  with_open_out t.rules_file (fun oc ->
      Stdlib.seek_out oc 0 ;
      output_string oc content)

(* Prometheus can reload its configuration by first sending the POST RPC and
   then the signal SIGHUP. *)
let reload _t =
  let* () = Process.run "curl" ["-XPOST"; "http://localhost:9090/-/reload"] in
  Process.run "docker" ["kill"; "--signal"; "SIGHUP"; "prometheus"]

let add_job t ?(metrics_path = "/metrics") ~name targets =
  let source = {name; metrics_path; targets} in
  t.jobs <- source :: t.jobs ;
  write_configuration_file t ;
  reload t

let add_alert t ?for_ ~name ~expr () =
  let alert = {name; expr; for_} in
  t.alerts <- alert :: t.alerts ;
  write_rules_file t ;
  ()

let start agents =
  let jobs =
    if Env.monitoring then [tezt_source; netdata_source_of_agents agents]
    else [tezt_source]
  in
  let* () =
    Process.run
      "mkdir"
      ["-p"; Filename.get_temp_dir_name () // "prometheus" // "rules"]
  in
  (* We do not use the Temp.dir so that the base directory is predictable and
     can be mounted by the proxy VM if [--proxy] is used. *)
  let configuration_file =
    Filename.get_temp_dir_name () // "prometheus" // "prometheus.yml"
  in
  let rules_file =
    Filename.get_temp_dir_name () // "prometheus" // "rules" // "tezt.rules"
  in
  let snapshot_filename = Env.prometheus_snapshot_filename in
  let port = Env.prometheus_port in
  let scrape_interval = Env.prometheus_scrape_interval in
  let t =
    {
      configuration_file;
      rules_file;
      jobs;
      scrape_interval;
      snapshot_filename;
      port;
      alert_manager = Env.alert_handlers <> [];
      alerts = [];
    }
  in
  write_rules_file t ;
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
        Format.asprintf
          "%s:/etc/prometheus"
          (Filename.dirname configuration_file);
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
      rules_file = "";
      alert_manager = false;
      jobs = [];
      scrape_interval = 0;
      snapshot_filename = Some snapshot_filename;
      port;
      alerts = [];
    }
