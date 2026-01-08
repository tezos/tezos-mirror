(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Tezt
include Cli
include Types

type config = {
  localhost : bool option;
  ssh_host : string option;
  monitoring : bool option;
  destroy : bool option;
  keep_alive : bool option;
  project_id : string option;
  vms : int option;
  vm_base_port : int option;
  ports_per_vm : int option;
  proxy : bool option;
  os : Os.t option;
  grafana : bool option;
  grafana_legacy_source : bool option;
  alert_handlers : string list;
  prometheus : bool option;
  prometheus_export : bool option;
  prometheus_port : int option;
  prometheus_export_path : string option;
  prometheus_snapshots : (string * int option) list;
  prometheus_scrape_interval : int option;
  process_monitoring : bool option;
  website : bool option;
  machine_type : string option;
  disk_type : string option;
  disk_size_gb : int option;
  dockerfile_alias : string option;
  website_port : int option;
  max_run_duration : int option;
  no_max_run_duration : bool option;
  tezt_cloud : string option;
  dns_domains : string list;
  no_dns : bool option;
  open_telemetry : bool option;
  macosx : bool option;
  check_file_consistency : bool option;
  docker_host_network : bool option;
  push_docker : bool option;
  auto_approve : bool option;
  faketime : string option;
  binaries_path : string option;
  log_rotation : int option;
  slack_channel_id : string option;
  slack_bot_token : string option;
  retrieve_daily_logs : bool option;
  retrieve_ppx_profiling_traces : bool option;
  scenario_specific : (string * Data_encoding.Json.t) option;
  tc_delay : (float * float) option;
  tc_jitter : (float * float) option;
  artifacts_dir : string option;
  teztale_artifacts : bool option;
}

let encoding =
  let open Data_encoding in
  conv
    (fun {
           localhost;
           ssh_host;
           monitoring;
           destroy;
           keep_alive;
           project_id;
           vms;
           vm_base_port;
           ports_per_vm;
           proxy;
           os;
           grafana;
           grafana_legacy_source;
           alert_handlers;
           prometheus;
           prometheus_export;
           prometheus_port;
           prometheus_export_path;
           prometheus_snapshots;
           prometheus_scrape_interval;
           process_monitoring;
           website;
           machine_type;
           disk_type;
           disk_size_gb;
           dockerfile_alias;
           website_port;
           max_run_duration;
           no_max_run_duration;
           tezt_cloud;
           dns_domains;
           no_dns;
           open_telemetry;
           macosx;
           check_file_consistency;
           docker_host_network;
           push_docker;
           auto_approve;
           faketime;
           binaries_path;
           log_rotation;
           slack_channel_id;
           slack_bot_token;
           retrieve_daily_logs;
           retrieve_ppx_profiling_traces;
           scenario_specific;
           tc_delay;
           tc_jitter;
           artifacts_dir;
           teztale_artifacts;
         }
       ->
      ( ( ( localhost,
            ssh_host,
            monitoring,
            destroy,
            keep_alive,
            project_id,
            vms,
            vm_base_port,
            ports_per_vm,
            proxy ),
          ( os,
            grafana,
            grafana_legacy_source,
            alert_handlers,
            prometheus,
            prometheus_export,
            prometheus_port,
            prometheus_export_path,
            prometheus_snapshots,
            prometheus_scrape_interval ) ),
        ( ( ( process_monitoring,
              website,
              machine_type,
              dockerfile_alias,
              website_port,
              max_run_duration,
              no_max_run_duration,
              tezt_cloud,
              dns_domains,
              no_dns ),
            ( open_telemetry,
              macosx,
              check_file_consistency,
              docker_host_network,
              push_docker,
              auto_approve,
              faketime,
              binaries_path,
              log_rotation,
              slack_channel_id ) ),
          ( slack_bot_token,
            retrieve_daily_logs,
            retrieve_ppx_profiling_traces,
            scenario_specific,
            tc_delay,
            tc_jitter,
            artifacts_dir,
            teztale_artifacts,
            disk_type,
            disk_size_gb ) ) ))
    (fun ( ( ( localhost,
               ssh_host,
               monitoring,
               destroy,
               keep_alive,
               project_id,
               vms,
               vm_base_port,
               ports_per_vm,
               proxy ),
             ( os,
               grafana,
               grafana_legacy_source,
               alert_handlers,
               prometheus,
               prometheus_export,
               prometheus_port,
               prometheus_export_path,
               prometheus_snapshots,
               prometheus_scrape_interval ) ),
           ( ( ( process_monitoring,
                 website,
                 machine_type,
                 dockerfile_alias,
                 website_port,
                 max_run_duration,
                 no_max_run_duration,
                 tezt_cloud,
                 dns_domains,
                 no_dns ),
               ( open_telemetry,
                 macosx,
                 check_file_consistency,
                 docker_host_network,
                 push_docker,
                 auto_approve,
                 faketime,
                 binaries_path,
                 log_rotation,
                 slack_channel_id ) ),
             ( slack_bot_token,
               retrieve_daily_logs,
               retrieve_ppx_profiling_traces,
               scenario_specific,
               tc_delay,
               tc_jitter,
               artifacts_dir,
               teztale_artifacts,
               disk_type,
               disk_size_gb ) ) )
       ->
      {
        localhost;
        ssh_host;
        monitoring;
        destroy;
        keep_alive;
        project_id;
        vms;
        vm_base_port;
        ports_per_vm;
        proxy;
        os;
        grafana;
        grafana_legacy_source;
        alert_handlers;
        prometheus;
        prometheus_export;
        prometheus_port;
        prometheus_export_path;
        prometheus_snapshots;
        prometheus_scrape_interval;
        process_monitoring;
        website;
        machine_type;
        disk_type;
        disk_size_gb;
        dockerfile_alias;
        website_port;
        max_run_duration;
        no_max_run_duration;
        tezt_cloud;
        dns_domains;
        no_dns;
        open_telemetry;
        macosx;
        check_file_consistency;
        docker_host_network;
        push_docker;
        auto_approve;
        faketime;
        binaries_path;
        log_rotation;
        slack_channel_id;
        slack_bot_token;
        retrieve_daily_logs;
        retrieve_ppx_profiling_traces;
        artifacts_dir;
        scenario_specific;
        tc_delay;
        tc_jitter;
        teztale_artifacts;
      })
    (merge_objs
       (merge_objs
          (obj10
             (opt "localhost" bool)
             (opt "ssh_host" string)
             (opt "monitoring" bool)
             (opt "destroy" bool)
             (opt "keep_alive" bool)
             (opt "project_id" string)
             (opt "vms" int31)
             (opt "vm_base_port" int31)
             (opt "ports_per_vm" int31)
             (opt "proxy" bool))
          (obj10
             (opt "os" Os.encoding)
             (opt "grafana" bool)
             (opt "grafana_legacy_source" bool)
             (dft "alert_handlers" (list string) [])
             (opt "prometheus" bool)
             (opt "prometheus_export" bool)
             (opt "prometheus_port" int31)
             (opt "prometheus_export_path" string)
             (dft "prometheus_snapshots" (list (tup2 string (option int31))) [])
             (opt "prometheus_scrape_interval" int31)))
       (merge_objs
          (merge_objs
             (obj10
                (opt "process_monitoring" bool)
                (opt "website" bool)
                (opt "machine_type" string)
                (opt "dockerfile_alias" string)
                (opt "website_port" int31)
                (opt "max_run_duration" int31)
                (opt "no_max_run_duration" bool)
                (opt "tezt_cloud" string)
                (dft "dns_domains" (list string) [])
                (opt "no_dns" bool))
             (obj10
                (opt "open_telemetry" bool)
                (opt "macosx" bool)
                (opt "check_file_consistency" bool)
                (opt "docker_host_network" bool)
                (opt "push_docker" bool)
                (opt "auto_approve" bool)
                (opt "faketime" string)
                (opt "binaries_path" string)
                (opt "log_rotation" int31)
                (opt "slack_channel_id" string)))
          (obj10
             (opt "slack_bot_token" string)
             (opt "daily_logs_artifacts" bool)
             (opt "retrieve_ppx_profiling_traces" bool)
             (opt "scenario_specific" (tup2 string Data_encoding.Json.encoding))
             (opt "tc_delay" (tup2 float float))
             (opt "tc_jitter" (tup2 float float))
             (opt "artifacts_dir" string)
             (opt "teztale_artifacts" bool)
             (opt "disk_type" string)
             (opt "disk_size_gb" int31))))

let section =
  Clap.section
    ~description:"All the options related to Tezt cloud library"
    "Cloud"

let config_file =
  Clap.optional_string
    ~section
    ~long:"config-file"
    ~description:"Configuration file for the tezt_cloud scenario"
    ()

let config =
  match config_file with
  | None -> Data_encoding.Json.destruct encoding (`O [])
  | Some file -> (
      let json = Ezjsonm.from_channel (In_channel.open_text file) in
      Format.printf "%s@." (Ezjsonm.value_to_string json) ;
      try Data_encoding.Json.destruct encoding json with
      | Json_encoding.Cannot_destruct (_, e) as exn ->
          Log.error
            "Cannot load config file: %s - %s"
            (Printexc.to_string exn)
            (Printexc.to_string e) ;
          raise exn
      | e -> raise e)

let scenario_specific = config.scenario_specific

let some = Option.some

let localhost =
  Clap.flag
    ~section
    ~set_long:"localhost"
    ~unset_long:"cloud"
    ~description:"If set, the test is run locally"
    (Option.value ~default:false config.localhost)

let ssh_host =
  let from_cli =
    Clap.optional_string
      ~section
      ~long:"ssh-host"
      ~description:"Whether to provision a non-gcp vm host via ssh"
      ()
  in
  Option.fold ~none:config.ssh_host ~some from_cli

let ssh_private_key =
  Clap.optional_string
    ~section
    ~long:"ssh-private-key"
    ~description:"The ssh private key to use on permanent ssh hosts"
    ()

let monitoring =
  Clap.flag
    ~section
    ~set_long:"monitoring"
    ~unset_long:"no-monitoring"
    ~description:
      "If set, all the VMs are monitored with netdata. The monitoring \
       dashboard is accessible on port 13999 of the monitored VM."
    (Option.value ~default:false config.monitoring)

let destroy =
  Clap.flag
    ~section
    ~set_long:"destroy"
    ~unset_long:"no-destroy"
    ~description:"If set, the machines are destroyed at the end of the test"
    (Option.value ~default:false config.destroy)

let keep_alive =
  Clap.flag
    ~section
    ~set_long:"keep-alive"
    ~description:
      "If set, the test will prompt the user to press <enter> to end the test. \
       This option can be used to inspect VMs state at the end of a scenrio."
    (Option.value ~default:false config.keep_alive)

let project_id =
  let from_cli =
    Clap.optional_string
      ~section
      ~long:"project-id"
      ~description:
        "Allows to specify a given project id. Otherwise, a default one will \
         be fetched with `gcloud config get-value project`"
      ()
  in
  Option.fold ~none:config.project_id ~some from_cli

let vms =
  let from_cli =
    Clap.optional_int
      ~section
      ~long:"vms-limit"
      ~description:"Maximum number of VMs running during the test."
      ()
  in
  Option.fold ~none:config.vms ~some from_cli

let vm_base_port =
  Clap.default_int
    ~section
    ~long:"vm-base-port"
    ~description:"The first available port on the VM"
    (Option.value ~default:30_000 config.vm_base_port)

let ports_per_vm =
  Clap.default_int
    ~section
    ~long:"ports-per-vm"
    ~description:"Number of opened ports per VM (default is 50)"
    (Option.value ~default:50 config.ports_per_vm)

let proxy =
  Clap.flag
    ~section
    ~set_long:"proxy"
    ~description:
      "Enables to run the orchestrator on a VM instead of the host machine"
    (Option.value ~default:false config.proxy)

let os =
  Clap.default
    Os.typ
    ~section
    ~long:"os"
    ~description:
      "The OS to be used for the VM (default is cos). Other possible value is \
       'debian'."
    (Option.value ~default:Os.Cos config.os)

let grafana =
  Clap.flag
    ~section
    ~set_long:"grafana"
    ~unset_long:"no-grafana"
    ~description:"Flag to set whether to run grafana"
    (Option.value
       ~default:(((not localhost) || proxy) && os = Os.Cos)
       config.grafana)

let grafana_legacy_source =
  Clap.flag
    ~section
    ~set_long:"grafana-legacy-source"
    ~description:
      "Flag to indicate to use grafana legacy 'Prometheus' source name when \
       importing a prometheus snapshot. Newer dashboards should use \
       ${datasource} as source instead of the hardcoded 'Prometheus' name."
    (Option.value ~default:false config.grafana_legacy_source)

let alert_handlers =
  config.alert_handlers
  @ Clap.list_string
      ~section
      ~long:"alert-handler"
      ~description:"Specify an alert handler to be registered by alert manager."
      ()

let prometheus =
  Clap.flag
    ~section
    ~set_long:"prometheus"
    ~unset_long:"no-prometheus"
    ~description:"Flag to set whether metrics are exported into prometheus"
    (Option.value ~default:(grafana || alert_handlers <> []) config.prometheus)

let prometheus_export =
  Clap.flag
    ~section
    ~set_long:"prometheus-export"
    ~unset_long:"no-prometheus-export"
    ~description:"Export a Prometheus snapshot at the end of the scenario"
    (Option.value ~default:true config.prometheus_export)

let default_prometheus_port = 9090

let prometheus_port =
  Clap.default_int
    ~section
    ~long:"prometheus-port"
    ~description:
      (sf
         "Set the port on which the prometheus instance will run (default: %d)."
         default_prometheus_port)
    (Option.value ~default:default_prometheus_port config.prometheus_port)

let prometheus_export_path =
  let from_cli =
    Clap.optional_string
      ~section
      ~long:"prometheus-export-path"
      ~description:"Path of the prometheus snapshot"
      ()
  in
  Option.fold ~none:config.prometheus_export_path ~some from_cli

let prometheus_snapshots =
  let parse str =
    match String.split_on_char ':' str with
    | [file; port] -> Some (file, Some (int_of_string port))
    | [file] -> Some (file, None)
    | _ -> None
  in
  let show = function
    | filename, Some port -> sf "%s:%d" filename port
    | filename, None -> filename
  in
  let typ =
    Clap.typ ~name:"prometheus-snapshot-imported" ~dummy:("", None) ~parse ~show
  in
  config.prometheus_snapshots
  @ Clap.list
      typ
      ~section
      ~long:"prometheus-snapshots"
      ~placeholder:"/path/to/snapshot[:port]"
      ~description:
        (sf
           "Path to the snapshot to load in a prometheus docker container. If \
            no port is specified, a port will be automatically assigned \
            starting with the value of --prometheus-port (default: %d)"
           default_prometheus_port)
      ()

let prometheus_scrape_interval =
  Clap.default_int
    ~section
    ~long:"prometheus-scrape-interval"
    ~description:
      "Set the scraping interval of the prometheus instance (default: 5)"
    (Option.value ~default:5 config.prometheus_scrape_interval)

let process_monitoring =
  Clap.flag
    ~section
    ~set_long:"process-monitoring"
    ~unset_long:"no-process-monitoring"
    ~description:
      "Flag to set process monitoring through prometheus process exporter. \
       Default is set when prometheus is set"
    (Option.value ~default:prometheus config.process_monitoring)
(* Automatically enable when enabling prometheus *)

let website =
  Clap.flag
    ~section
    ~set_long:"website"
    ~unset_long:"no-website"
    ~description:
      "A webpage is up on localhost to summarize various information related \
       to the experiment"
    (Option.value ~default:prometheus config.website)

let machine_type =
  Clap.default_string
    ~section
    ~long:"machine-type"
    ~description:
      (Format.sprintf
         "Can specify a GCP machine type (see \
          https://cloud.google.com/compute/docs/general-purpose-machines#n2_series). \
          Default is %s."
         Types.Agent_configuration.default_gcp_machine_type)
    (Option.value
       ~default:Types.Agent_configuration.default_gcp_machine_type
       config.machine_type)

let disk_type =
  Clap.optional_string
    ~section
    ~long:"disk-type"
    ~description:
      "Can specify a disk type to use for GCP machines (see \
       https://docs.cloud.google.com/compute/docs/disks). Default is pd-ssd."
    ()

let disk_size_gb =
  Clap.optional_int
    ~section
    ~long:"disk-size-gb"
    ~description:
      "Can specify the size of disk to use for GCP machines. Default is 200."
    ()

let dockerfile_alias =
  let from_cli =
    Clap.optional_string
      ~section
      ~long:"dockerfile-alias"
      ~description:
        "Specify the name of the dockerfile alias to use (default is given by \
         the environment variable `TEZT_CLOUD`)"
      ()
  in
  Option.fold ~none:config.dockerfile_alias ~some from_cli

let website_port =
  Clap.default_int
    ~section
    ~long:"website-port"
    ~description:"Set the port used for the website. Default is 8080"
    (* When the website is run by the orchestrator, it is easier to
       use port 80. We could decide in the future to use 80 by
       default. *)
    (Option.value
       ~default:(if proxy && localhost then 80 else 8080)
       config.website_port)

let max_run_duration =
  Clap.default_int
    ~section
    ~long:"max-run-duration"
    ~description:
      "Specify the maximum time (in seconds) of a VM (from the first time it \
       was created)."
    (Option.value ~default:7200 config.max_run_duration)

let no_max_run_duration =
  Clap.flag
    ~section
    ~set_long:"no-max-run-duration"
    ~description:"Ensure the VM can only be destroyed manually."
    (* If the proxy mode is active, we don't want to use [max_run_duration]
       since it aims to run long running tests. *)
    (Option.value ~default:(proxy || os <> Os.Cos) config.no_max_run_duration)

let tezt_cloud =
  let from_cli =
    Clap.optional_string
      ~section
      ~long:"tezt-cloud"
      ~description:"Overwrite the TEZT_CLOUD variable"
      ()
  in
  Option.fold ~none:config.tezt_cloud ~some from_cli

let dns_domains =
  config.dns_domains
  @ Clap.list_string
      ~section
      ~long:"dns-domain"
      ~long_synonyms:["dns"]
      ~description:
        "Register a list of DNS domains. By default a domain is registered \
         when using the proxy mode. In that case the domain will be prepended \
         by the value of the `tezt-cloud` parameter and suffixed by the domain \
         registered under the zone name `tezt-cloud` (check the README to get \
         more details). The format expects domains that have suffixes matching \
         domains registered with the GCP project."
      ()

let no_dns =
  Clap.flag
    ~section
    ~set_long:"no-dns"
    ~set_long_synonyms:["no-dns-domain"]
    ~description:
      "Prevent from adding any DNS domain associated with the experiment. This \
       cancel any effect of [--dns-domain]."
    (Option.value ~default:false config.no_dns)

let open_telemetry =
  Clap.flag
    ~section
    ~set_long:"open-telemetry"
    ~unset_long:"no-open-telemetry"
    ~set_long_synonyms:["otel"]
    ~description:"Run the Open Telemetry stack"
    (Option.value ~default:false config.open_telemetry)

let macosx =
  Clap.flag
    ~section
    ~set_long:"macosx"
    ~description:"Use this flag if you run tezt-cloud from Mac OS/X"
    (Option.value ~default:false config.macosx)

let check_file_consistency =
  Clap.flag
    ~section
    ~set_long:"check-file-consistency"
    ~unset_long:"no-check-file-consistency"
    ~description:
      "By default, `tezt-cloud` ensures that files copied from the host \
       machine to a VM are consistent (i.e., they have the same hash). \
       Although a scenario can opt in or opt out, this flag sets the default \
       behavior. Its default value is [true] unless [--macosx] is used."
    (Option.value ~default:(not macosx) config.check_file_consistency)

let docker_host_network =
  Clap.flag
    ~section
    ~set_long:"docker-host-network"
    ~unset_long:"no-docker-host-network"
    ~description:
      "In Local_orchestrator_local_agents mode, whether to use the host \
       network or not. This is not available on Mac OS/X. By default [true] \
       unless Mac OS/X is set. If not set, a dedicated docker network is \
       created."
    (Option.value ~default:(not macosx) config.docker_host_network)

let push_docker =
  Clap.flag
    ~section
    ~unset_long:"no-docker-push"
    ~description:
      "When used in cloud mode (--cloud) or proxy mode (--proxy), by default, \
       `tezt-cloud` pushes the Docker containers on GCP. To save some \
       bandwidth or some time during a redeployment that won't add or remove \
       VMs that are already running, it is useful to prevent those containers \
       to be pushed."
    (Option.value ~default:true config.push_docker)

let auto_approve =
  Clap.flag
    ~section
    ~set_long:"auto-approve"
    ~unset_long:"no-auto-approve"
    ~description:
      "If set to true (default), don't ask confirmation before updating a \
       deployment via terraform."
    (Option.value ~default:true config.auto_approve)

let faketime =
  let from_cli =
    Clap.optional_string
      ~section
      ~long:"faketime"
      ~description:
        "This argument specifies the value used to set [FAKETIME] environment \
         variable for the Docker images. Use it with a docker image that uses \
         [libfaketime]. For instance, use [--faketime \"-10d\"] if you want to \
         set your experminent 10 days in the past. See [libfaketime] for a \
         complete overview about the [FAKETIME] available formats."
      ()
  in
  Option.fold ~none:config.faketime ~some from_cli

let binaries_path =
  Clap.default_string
    ~section
    ~long:"binaries-path"
    ~description:
      "Where to find binaries in the docker image by default (default is: \
       '/tmp/tezt-runners')"
    (Option.value
       ~default:Types.Agent_configuration.default_gcp_binaries_path
       config.binaries_path)

let log_rotation =
  Clap.default_int
    ~section
    ~long:"log-rotation"
    ~description:
      "Maximum number of log rotations before removing older log files. \
       Default is 300 if a log-file is specified.\n\
      \       Set to 0 to completely disable log-rotation"
    (Option.value ~default:300 config.log_rotation)

let dir_typ ~name ~cli_parameter : string option Clap.typ =
  Clap.typ
    ~name
    ~dummy:None
    ~parse:(fun s ->
      if Sys.file_exists s then (
        Log.error
          "The destination folder of --%s already exists: %s"
          cli_parameter
          s ;
        None)
      else if proxy then (
        Log.warn
          "The --%s option is not available when --proxy is used."
          cli_parameter ;
        Some None)
      else Some (Some s))
    ~show:(function Some s -> s | None -> "empty")

let artifacts_dir =
  let long = "artifacts-dir" in
  let from_cli =
    Clap.default
      ~section
      ~long
      ~description:
        "Directory where the artifacts are retrieved (configuration files, \
         logs, etc). This can represent quite a huge quantity of data. \
         Disabled by default."
      (dir_typ ~name:"artifacts" ~cli_parameter:long)
      None
  in
  Option.fold ~none:config.artifacts_dir ~some from_cli

let check_artifact_option name flag =
  if flag && artifacts_dir = None then
    Log.warn
      "Cannot retrieve %s: --artifacts-dir must be set up to retrieve %s"
      name
      name ;
  flag

let retrieve_daily_logs =
  Clap.flag
    ~section
    ~set_long:"daily-logs-artifacts"
    ~unset_long:"no-daily-logs-artifacts"
    ~description:
      "Retrieves the daily logs, usually info logs, that are generated by the \
       daemons, and stores it in the artifact directory given by \
       `--artifacts-dir`. This can represent quite a huge quantity of data. \
       Set to [false] by default."
    (Option.value ~default:false config.retrieve_daily_logs)
  |> check_artifact_option "daily-logs"

let teztale_artifacts =
  Clap.flag
    ~section
    ~set_long:"teztale-artifacts"
    ~unset_long:"no-teztale-artifacts"
    ~description:
      "Retrieves the Teztale database if `--artifacts-dir <dir>` is set, and \
       stores them in <dir>. Set to [false] by default."
    (Option.value ~default:false config.teztale_artifacts)
  |> check_artifact_option "teztale"

let retrieve_ppx_profiling_traces =
  Clap.flag
    ~section
    ~set_long:"ppx-profiling-traces-artifacts"
    ~unset_long:"ppx-profiling-traces-artifacts"
    ~description:
      "Retrieves the PPX profiling traces that are generated by the daemons, \
       and stores it in the artifact directory given by `--artifacts-dir`. \
       This can represent quite a huge quantity of data. Set to [false] by \
       default."
    (Option.value ~default:false config.retrieve_ppx_profiling_traces)
  |> check_artifact_option "profiling-traces"

let float_float =
  let parse string =
    try Scanf.sscanf string "%f,%f" (fun min max -> Some (min, max))
    with _ -> None
  in
  let show (min, max) = Printf.sprintf "%f,%f" min max in
  Clap.typ ~name:"float*float" ~dummy:(0., 0.) ~parse ~show

let tc_delay : (float * float) option =
  Option.fold ~none:config.tc_delay ~some
  @@ Clap.optional
       ~section
       ~long:"tc-delay"
       ~placeholder:"MIN,MAX"
       ~description:
         "Add random network delay to outgoing messages (between MIN and MAX \
          seconds)."
       float_float
       ()

let tc_jitter : (float * float) option =
  Option.fold ~none:config.tc_jitter ~some
  @@ Clap.optional
       ~section
       ~long:"tc-jitter"
       ~placeholder:"MIN,MAX"
       ~description:
         "Add random network jitter to outgoing messages, (between MIN and MAX \
          seconds)."
       float_float
       ()

let section =
  Clap.section
    ~description:"Define report and alert managing options"
    "Cloud reporting and alerting options"

let slack_channel_id =
  let from_cli =
    Clap.optional_string
      ~section
      ~long:"slack-channel-id"
      ~description:"The Slack channel id to send reports and alerts on"
      ()
  in
  Option.fold ~none:config.slack_channel_id ~some from_cli

let slack_bot_token =
  let from_cli =
    Clap.optional_string
      ~section
      ~long:"slack-bot-token"
      ~description:"The Slack bot token used to send reports and alerts"
      ()
  in
  Option.fold ~none:config.slack_bot_token ~some from_cli

module Types = struct
  let dir_typ = dir_typ
end

let to_json_config ?scenario_config () =
  let scenario_specific =
    match scenario_config with
    | None -> scenario_specific
    | Some _ -> scenario_config
  in
  {
    localhost = Some localhost;
    ssh_host;
    monitoring = Some monitoring;
    destroy = Some destroy;
    keep_alive = Some keep_alive;
    project_id;
    vms;
    vm_base_port = Some vm_base_port;
    ports_per_vm = Some ports_per_vm;
    proxy = Some proxy;
    os = Some os;
    grafana = Some grafana;
    grafana_legacy_source = Some grafana_legacy_source;
    alert_handlers;
    prometheus = Some prometheus;
    prometheus_export = Some prometheus_export;
    prometheus_port = Some prometheus_port;
    prometheus_export_path;
    prometheus_snapshots;
    prometheus_scrape_interval = Some prometheus_scrape_interval;
    process_monitoring = Some process_monitoring;
    website = Some website;
    machine_type = Some machine_type;
    disk_type;
    disk_size_gb;
    dockerfile_alias;
    website_port = Some website_port;
    max_run_duration = Some max_run_duration;
    no_max_run_duration = Some no_max_run_duration;
    tezt_cloud;
    dns_domains;
    no_dns = Some no_dns;
    open_telemetry = Some open_telemetry;
    macosx = Some macosx;
    check_file_consistency = Some check_file_consistency;
    docker_host_network = Some docker_host_network;
    push_docker = Some push_docker;
    auto_approve = Some auto_approve;
    faketime;
    binaries_path = Some binaries_path;
    log_rotation = Some log_rotation;
    slack_channel_id;
    slack_bot_token;
    retrieve_daily_logs = Some retrieve_daily_logs;
    retrieve_ppx_profiling_traces = Some retrieve_ppx_profiling_traces;
    scenario_specific;
    tc_delay;
    tc_jitter;
    artifacts_dir;
    teztale_artifacts = Some teztale_artifacts;
  }
  |> Data_encoding.Json.construct encoding
