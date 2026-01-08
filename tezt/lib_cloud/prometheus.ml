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

let pp_job fmt (job : job) =
  Format.fprintf
    fmt
    "{%a}"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
       (fun fmt (target : target) ->
         Format.fprintf
           fmt
           "%s(%s:%d)"
           target.app_name
           target.address
           target.port))
    job.targets

let job_jingoo_template job =
  let open Jingoo.Jg_types in
  Tobj
    [
      ("name", Tstr job.name);
      ("metrics_path", Tstr job.metrics_path);
      ("targets", Tlist (List.map target_jingoo_template job.targets));
    ]

type record = {name : string; query : string}

type severity = Critical | Warning | Info

let string_of_severity = function
  | Critical -> "critical"
  | Warning -> "warning"
  | Info -> "info"

type alert = {
  name : string;
  expr : string;
  group_name : string option;
  interval : string option;
  severity : severity option;
  for_ : string option;
  description : string option;
  summary : string option;
}

type rule = Record of record | Alert of alert

type group = {name : string; interval : string option; rules : rule list}

type groups = (string, group) Hashtbl.t

type t = {
  name : string;
  configuration_file : string;
  rules_file : string;
  mutable jobs : job list;
  scrape_interval : int;
  snapshot_filename : string option;
  port : int;
  groups : groups;
}

let get_name (t : t) = t.name

let get_port (t : t) = t.port

let get_query_endpoint ~query =
  if Env.prometheus then
    Some
      (Uri.make
         ~scheme:"http"
         ~host:"localhost"
         ~port:Env.prometheus_port
         ~path:"/api/v1/query"
         ~query:[("query", [query])]
         ())
  else None

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

let opentelemetry_source =
  let name = "open-telemetry" in
  let metrics_path = "/metrics" in
  let address = "localhost" in
  let port = 8888 in
  let app_name = "otel-collector" in
  let targets = [{address; port; app_name}] in
  {name; metrics_path; targets}

let tezt_source =
  {
    name = "tezt_metrics";
    metrics_path = "/metrics";
    targets =
      [
        {
          address = "localhost";
          port =
            (if Env.mode = `Remote_orchestrator_local_agents then 80 else 8080);
          app_name = "tezt";
        };
      ];
  }

let jingoo_configuration_template t =
  let is_alert = function Alert _ -> true | _ -> false in
  let exists_in_hashtbl f table =
    let exception Found in
    try
      Hashtbl.iter (fun x y -> if f x y then raise Found) table ;
      false
    with Found -> true
  in
  let alert_on =
    exists_in_hashtbl (fun _ group -> List.exists is_alert group.rules) t.groups
  in
  let open Jingoo.Jg_types in
  [
    ("scrape_interval", Tint t.scrape_interval);
    ("jobs", Tlist (List.map job_jingoo_template t.jobs));
    ("alert_manager", Tbool alert_on);
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

let record_template (record : record) =
  let open Jingoo.Jg_types in
  let payload = [("record", Tstr record.name); ("query", Tstr record.query)] in
  Tobj payload

let alert_template (alert : alert) =
  let open Jingoo.Jg_types in
  let payload =
    [
      ("name", Tstr alert.name);
      ("expr", Tstr alert.expr);
      ( "severity",
        Tstr (Option.fold ~none:"none" ~some:string_of_severity alert.severity)
      );
    ]
    @ Option.fold
        ~none:[]
        ~some:(fun v -> [("description", Tstr v)])
        alert.description
    @ Option.fold ~none:[] ~some:(fun v -> [("for", Tstr v)]) alert.for_
    @ Option.fold ~none:[] ~some:(fun v -> [("summary", Tstr v)]) alert.summary
  in
  Tobj payload

let rule_template rule =
  let open Jingoo.Jg_types in
  let payload =
    match rule with
    | Alert alert -> [("alert", alert_template alert)]
    | Record record -> [("record", record_template record)]
  in
  Tobj payload

let group_template (group : group) =
  let open Jingoo.Jg_types in
  let payload =
    [
      ("name", Tstr group.name);
      ("rules", Tlist (List.map rule_template group.rules));
    ]
    @ Option.fold
        ~none:[]
        ~some:(fun time -> [("interval", Tstr time)])
        group.interval
  in
  Tobj payload

let groups_template t =
  let open Jingoo.Jg_types in
  let groups = t.groups |> Hashtbl.to_seq_values |> List.of_seq in
  [("groups", Tlist (List.map group_template groups))]

let write_rules_file t =
  let content =
    Jingoo.Jg_template.from_file
      ~env:{Jingoo.Jg_types.std_env with autoescape = false}
      Path.prometheus_rules_configuration
      ~models:(groups_template t)
  in
  with_open_out t.rules_file (fun oc ->
      Stdlib.seek_out oc 0 ;
      output_string oc content)

(* Prometheus can reload its configuration by first sending the POST RPC and
   then the signal SIGHUP. *)
let reload t =
  let* () =
    Process.run "curl" ["-XPOST"; sf "http://localhost:%d/-/reload" t.port]
  in
  Process.run "docker" ["kill"; "--signal"; "SIGHUP"; t.name]

let add_job (t : t) ?(metrics_path = "/metrics") ~name targets =
  let new_job = {name; metrics_path; targets} in
  let jobs =
    match List.find_opt (fun (job : job) -> name = job.name) t.jobs with
    | None ->
        Log.report
          "Prometheus: adding job %s with %a"
          new_job.name
          pp_job
          new_job ;
        List.rev (new_job :: t.jobs)
    | Some job ->
        let targets = List.sort_uniq compare (targets @ job.targets) in
        let new_job = {new_job with targets} in
        Log.report
          "Prometheus: replacing job %s with %a"
          new_job.name
          pp_job
          new_job ;
        List.map
          (fun (job : job) -> if job.name = name then new_job else job)
          t.jobs
  in
  t.jobs <- jobs ;
  write_configuration_file t ;
  reload t

let default_group_name = "tezt"

let add_group groups {name; interval; rules} =
  match Hashtbl.find_opt groups name with
  | None -> Hashtbl.add groups name {name; interval; rules}
  | Some group ->
      let min_of left right =
        match (left, right) with
        | None, right -> right
        | left, None -> left
        | Some left, Some right ->
            if Duration.(compare (of_string left) (of_string right)) > 0 then
              Some right
            else Some left
      in
      Hashtbl.replace
        groups
        name
        {
          name;
          interval = min_of interval group.interval;
          rules = List.sort_uniq compare (group.rules @ rules);
        }

let add_rule groups (alert : alert) =
  let interval = alert.interval in
  let group_name = Option.value ~default:default_group_name alert.group_name in
  let group = {name = group_name; interval; rules = [Alert alert]} in
  add_group groups group

let make_alert ?for_ ?description ?summary ?severity ?group_name ?interval ~name
    ~expr () =
  {name; expr; severity; for_; description; summary; group_name; interval}

let make_record ~name ~query = {name; query}

let rule_of_alert x = Alert x

let rule_of_record x = Record x

let name_of_alert ({name; _} : alert) = name

let register_rules ?(group_name = default_group_name) ?interval rules t =
  let group = {name = group_name; interval; rules} in
  add_group t.groups group ;
  write_rules_file t ;
  reload t

let start ~alerts agents =
  (* We do not use the Temp.dir so that the base directory is predictable and
     can be mounted by the proxy VM if [--proxy] is used. *)
  let dir = Path.tmp_dir // "prometheus" in
  (* Group alerts by group name. *)
  let groups =
    let groups = Hashtbl.create 10 in
    List.iter (add_rule groups) alerts ;
    groups
  in
  let jobs =
    if Env.monitoring then [tezt_source; netdata_source_of_agents agents]
    else [tezt_source]
  in
  let jobs =
    if Env.open_telemetry then opentelemetry_source :: jobs else jobs
  in
  let* () = Process.run "mkdir" ["-p"; dir // "rules"] in
  let configuration_file = dir // "prometheus.yml" in
  let rules_file = dir // "rules" // "tezt.rules" in
  let snapshot_filename = Env.prometheus_export_path in
  let port = Env.prometheus_port in
  let name = sf "prometheus-%d" port in
  let scrape_interval = Env.prometheus_scrape_interval in
  let t =
    {
      name;
      configuration_file;
      rules_file;
      jobs;
      scrape_interval;
      snapshot_filename;
      port;
      groups;
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
        name;
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
        sf "http://localhost:%d/-/ready" port;
      ]
  in
  let* _ = Env.wait_process ~is_ready ~run () in
  Lwt.return t

let snapshots_path = "/prometheus" // "data" // "snapshots"

let export_snapshot {snapshot_filename; name; port; _} =
  Log.info "Exporting snapshot..." ;
  let* stdout =
    Process.run_and_read_stdout
      "curl"
      ["-XPOST"; sf "http://localhost:%d/api/v1/admin/tsdb/snapshot" port]
  in
  let json = JSON.parse ~origin:"Prometheus.export" stdout in
  let snapshot_name = JSON.(json |-> "data" |-> "name" |> as_string) in
  let destination =
    Option.value ~default:(Path.tmp_dir // snapshot_name) snapshot_filename
  in
  let* () =
    Docker.cp
      name
      ~kind:`To_host
      ~source:(snapshots_path // snapshot_name)
      ~destination
    |> Process.check
  in
  Log.info "You can find the prometheus snapshot at %s" destination ;
  Lwt.return_unit

let shutdown (t : t) = Docker.kill t.name |> Process.check

let run_with_snapshot port snapshot_filename =
  let name = sf "prometheus-%d" port in
  let _ =
    Process.spawn
      "docker"
      [
        "run";
        "--rm";
        "--name";
        name;
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
  Log.info "You can find the prometheus instance at http://localhost:%d" port ;
  Lwt.return
    {
      name;
      configuration_file = "";
      rules_file = "";
      jobs = [];
      scrape_interval = 0;
      snapshot_filename = Some snapshot_filename;
      port;
      groups = Hashtbl.create 0;
    }
