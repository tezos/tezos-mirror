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
    ~description:
      "If set, all the VMs are monitored with netdata. The monitoring \
       dashboard is accessible on port 13999 of the monitored VM."
    false

let destroy =
  Clap.flag
    ~section
    ~set_long:"destroy"
    ~description:"If set, the machines are destroyed at the end of the test"
    false

let keep_alive =
  Clap.flag
    ~section
    ~set_long:"keep-alive"
    ~description:
      "If set, the test will promt the user to press <enter> to end the test. \
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
    ~description:"Number of opened port per VM (default is 50)"
    50

let grafana =
  Clap.flag
    ~section
    ~set_long:"grafana"
    ~description:"Flag to set whether to run grafana"
    true

let prometheus =
  Clap.flag
    ~section
    ~set_long:"prometheus"
    ~description:"Flag to set whether metrics are exported into prometheus"
    grafana

let prometheus_export =
  Clap.flag
    ~section
    ~set_long:"prometheus-export"
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
    ~description:
      "A webpage is up on localhost to summarize various informations related \
       to the experiment"
    prometheus

let website_port =
  Clap.default_int
    ~section
    ~long:"website-port"
    ~description:"Set the port used for the website. Default is 8080"
    8080

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

let max_run_duration =
  Clap.default_int
    ~section
    ~long:"max-run-duration"
    ~description:
      "Specify the maximum time (in second) of a VM (from the first time it \
       was created)."
    7200

let no_max_run_duration =
  Clap.flag
    ~section
    ~set_long:"no-max-run-duration"
    ~description:"Ensure the VM can only be destroyed manually."
    false
