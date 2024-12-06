(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Tezt
include Cli

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
    ~long:"vms"
    ~description:"Number of VMs running for the test."
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
  Clap.default_string
    ~section
    ~long:"os"
    ~description:
      "The OS to be used for the VM (default is cos). Other possible value is \
       'debian'."
    "cos"

let grafana =
  Clap.flag
    ~section
    ~set_long:"grafana"
    ~unset_long:"no-grafana"
    ~description:"Flag to set whether to run grafana"
    (((not localhost) || proxy) && os = "cos")

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

let prometheus_port =
  Clap.default_int
    ~section
    ~long:"prometheus-port"
    ~description:
      "Set the port on which the prometheus instance will run (default: 9090)."
    9090

let prometheus_snapshot_filename =
  Clap.optional_string
    ~section
    ~long:"prometheus-snapshot-filename"
    ~description:"Name of the prometheus snapshot file"
    ()

let prometheus_scrape_interval =
  Clap.default_int
    ~section
    ~long:"prometheus-scrape-interval"
    ~description:
      "Set the scraping interval of the prometheus instance (default: 5)"
    5

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
    "n1-standard-2"

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
    (proxy || os <> "cos")

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

let push_docker =
  Clap.flag
    ~section
    ~unset_long:"no-docker-push"
    ~description:
      "When used with a remote machine, by default, `tezt-cloud` pushes the \
       Docker container to the created VM. When one tries to reuse an existing \
       VM, and do not use the updated content of the Docker container, pushing \
       this container takes a significant time for no purpose."
    true
