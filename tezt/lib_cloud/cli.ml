(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Tezt
include Cli
include Types

let section =
  Clap.section
    ~description:"All the options related to Tezt cloud library"
    "Cloud"

let localhost =
  Clap.flag
    ~section
    ~set_long:"localhost"
    ~unset_long:"cloud"
    ~description:"If set, the test is run locally"
    false

let ssh_host =
  Clap.optional_string
    ~section
    ~long:"ssh-host"
    ~description:"Whether to provision a non-gcp vm host via ssh"
    ()

let monitoring =
  Clap.flag
    ~section
    ~set_long:"monitoring"
    ~unset_long:"no-monitoring"
    ~description:
      "If set, all the VMs are monitored with netdata. The monitoring \
       dashboard is accessible on port 13999 of the monitored VM."
    false

let destroy =
  Clap.flag
    ~section
    ~set_long:"destroy"
    ~unset_long:"no-destroy"
    ~description:"If set, the machines are destroyed at the end of the test"
    false

let keep_alive =
  Clap.flag
    ~section
    ~set_long:"keep-alive"
    ~description:
      "If set, the test will prompt the user to press <enter> to end the test. \
       This option can be used to inspect VMs state at the end of a scenrio."
    false

let project_id =
  Clap.optional_string
    ~section
    ~long:"project-id"
    ~description:
      "Allows to specify a given project id. Otherwise, a default one will be \
       fetched with `gcloud config get-value project`"
    ()

let vms =
  Clap.optional_int
    ~section
    ~long:"vms-limit"
    ~description:"Maximum number of VMs running during the test."
    ()

let vm_base_port =
  Clap.default_int
    ~section
    ~long:"vm-base-port"
    ~description:"The first available port on the VM"
    30_000

let ports_per_vm =
  Clap.default_int
    ~section
    ~long:"ports-per-vm"
    ~description:"Number of opened ports per VM (default is 50)"
    50

let proxy =
  Clap.flag
    ~section
    ~set_long:"proxy"
    ~description:
      "Enables to run the orchestrator on a VM instead of the host machine"
    false

let os =
  Clap.default
    Os.typ
    ~section
    ~long:"os"
    ~description:
      "The OS to be used for the VM (default is cos). Other possible value is \
       'debian'."
    Os.Cos

let grafana =
  Clap.flag
    ~section
    ~set_long:"grafana"
    ~unset_long:"no-grafana"
    ~description:"Flag to set whether to run grafana"
    (((not localhost) || proxy) && os = Os.Cos)

let grafana_legacy_source =
  Clap.flag
    ~section
    ~set_long:"grafana-legacy-source"
    ~description:
      "Flag to indicate to use grafana legacy 'Prometheus' source name when \
       importing a prometheus snapshot. Newer dashboards should use \
       ${datasource} as source instead of the hardcoded 'Prometheus' name."
    false

let alert_handlers =
  Clap.list_string
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
    (grafana || alert_handlers <> [])

let prometheus_export =
  Clap.flag
    ~section
    ~set_long:"prometheus-export"
    ~unset_long:"no-prometheus-export"
    ~description:"Export a Prometheus snapshot at the end of the scenario"
    true

let default_prometheus_port = 9090

let prometheus_port =
  Clap.default_int
    ~section
    ~long:"prometheus-port"
    ~description:
      (sf
         "Set the port on which the prometheus instance will run (default: %d)."
         default_prometheus_port)
    default_prometheus_port

let prometheus_export_path =
  Clap.optional_string
    ~section
    ~long:"prometheus-export-path"
    ~description:"Path of the prometheus snapshot"
    ()

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
  Clap.list
    typ
    ~section
    ~long:"prometheus-snapshots"
    ~placeholder:"/path/to/snapshot[:port]"
    ~description:
      (sf
         "Path to the snapshot to load in a prometheus docker container. If no \
          port is specified, a port will be automatically assigned starting \
          with the value of --prometheus-port (default: %d)"
         default_prometheus_port)
    ()

let prometheus_scrape_interval =
  Clap.default_int
    ~section
    ~long:"prometheus-scrape-interval"
    ~description:
      "Set the scraping interval of the prometheus instance (default: 5)"
    5

let process_monitoring =
  Clap.flag
    ~section
    ~set_long:"process-monitoring"
    ~unset_long:"no-process-monitoring"
    ~description:
      "Flag to set process monitoring through prometheus process exporter. \
       Default is set when prometheus is set"
    prometheus (* Automatically enable when enabling prometheus *)

let website =
  Clap.flag
    ~section
    ~set_long:"website"
    ~unset_long:"no-website"
    ~description:
      "A webpage is up on localhost to summarize various information related \
       to the experiment"
    prometheus

let machine_type =
  Clap.default_string
    ~section
    ~long:"machine-type"
    ~description:
      "Can specify a GCP machine type (see \
       https://cloud.google.com/compute/docs/general-purpose-machines#c3d_series)"
    Types.Agent_configuration.default_gcp_machine_type

let dockerfile_alias =
  Clap.optional_string
    ~section
    ~long:"dockerfile-alias"
    ~description:
      "Specify the name of the dockerfile alias to use (default is given by \
       the environment variable `TEZT_CLOUD`)"
    ()

let website_port =
  Clap.default_int
    ~section
    ~long:"website-port"
    ~description:"Set the port used for the website. Default is 8080"
    (* When the website is run by the orchestrator, it is easier to
       use port 80. We could decide in the future to use 80 by
       default. *)
    (if proxy && localhost then 80 else 8080)

let max_run_duration =
  Clap.default_int
    ~section
    ~long:"max-run-duration"
    ~description:
      "Specify the maximum time (in seconds) of a VM (from the first time it \
       was created)."
    7200

let no_max_run_duration =
  Clap.flag
    ~section
    ~set_long:"no-max-run-duration"
    ~description:"Ensure the VM can only be destroyed manually."
    (* If the proxy mode is active, we don't want to use [max_run_duration]
       since it aims to run long running tests. *)
    (proxy || os <> Os.Cos)

let tezt_cloud =
  Clap.optional_string
    ~section
    ~long:"tezt-cloud"
    ~description:"Overwrite the TEZT_CLOUD variable"
    ()

let dns_domains =
  Clap.list_string
    ~section
    ~long:"dns-domain"
    ~long_synonyms:["dns"]
    ~description:
      "Register a list of DNS domains. By default a domain is registered when \
       using the proxy mode. In that case the domain will be prepended by the \
       value of the `tezt-cloud` parameter and suffixed by the domain \
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
    false

let open_telemetry =
  Clap.flag
    ~section
    ~set_long:"open-telemetry"
    ~unset_long:"no-open-telemetry"
    ~set_long_synonyms:["otel"]
    ~description:"Run the Open Telemetry stack"
    false

let macosx =
  Clap.flag
    ~section
    ~set_long:"macosx"
    ~description:"Use this flag if you run tezt-cloud from Mac OS/X"
    false

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
    (not macosx)

let docker_host_network =
  Clap.flag
    ~section
    ~set_long:"docker-host-network"
    ~unset_long:"no-docker-host-network"
    ~description:
      "In Localhost host mode, whether to use the host network or not. This is \
       not available on Mac OS/X. By default [true] unless Mac OS/X is set. If \
       not set, a dedicated docker network is created."
    (not macosx)

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
    true

let auto_approve =
  Clap.flag
    ~section
    ~set_long:"auto-approve"
    ~unset_long:"no-auto-approve"
    ~description:
      "If set to true (default), don't ask confirmation before updating a \
       deployment via terraform."
    true

let faketime =
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

let binaries_path =
  Clap.default_string
    ~section
    ~long:"binaries-path"
    ~description:
      "Where to find binaries in the docker image by default (default is: \
       '/tmp/tezt-runners')"
    Types.Agent_configuration.default_gcp_binaries_path

let log_rotation =
  Clap.default_int
    ~section
    ~long:"log-rotation"
    ~description:
      "Maximum number of log rotations before removing older log files. \
       Default is 300 if a log-file is specified.\n\
      \       Set to 0 to completely disable log-rotation"
    300
