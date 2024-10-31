(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type docker_image = Gcp of {alias : string} | Octez_release of {tag : string}

let tezt_cloud =
  match Cli.tezt_cloud with
  | Some tezt_cloud -> tezt_cloud
  | None -> (
      (* This is a lazy value to be sure that this is evaluated only inside a Tezt test. *)
      match Sys.getenv_opt "TEZT_CLOUD" with None -> "" | Some value -> value)

let ssh_private_key_filename ?(home = Sys.getenv "HOME") () =
  home // ".ssh" // Format.asprintf "%s-tf" tezt_cloud

let ssh_public_key_filename ?home () =
  let ssh_key = ssh_private_key_filename ?home () in
  Format.asprintf "%s.pub" ssh_key

let docker_registry = Format.asprintf "%s-docker-registry" tezt_cloud

let mode =
  match (Cli.localhost, Cli.proxy) with
  | true, true -> `Orchestrator
  | true, false -> `Localhost
  | false, true -> `Host
  | false, false -> `Cloud

let prometheus = Cli.prometheus

let prometheus_export = Cli.prometheus_export

let prometheus_port = Cli.prometheus_port

let prometheus_snapshot_filename = Cli.prometheus_snapshot_filename

let prometheus_scrape_interval = Cli.prometheus_scrape_interval

let grafana = Cli.grafana

let website = Cli.website

let website_port = Cli.website_port

let destroy = Cli.destroy

let monitoring = Cli.monitoring

let keep_alive = Cli.keep_alive

let vms = Cli.vms

let vm_base_port = Cli.vm_base_port

let ports_per_vm = Cli.ports_per_vm

let machine_type = Cli.machine_type

let max_run_duration = Cli.max_run_duration

let no_max_run_duration = Cli.no_max_run_duration

let dns_domains = Cli.dns_domains

let os = Cli.os

let open_telemetry = Cli.open_telemetry

let docker_image =
  (* In localhost mode, we don't want to interact with GCP. The image is taken
     locally. *)
  match Cli.dockerfile_alias with
  | None -> Gcp {alias = tezt_cloud}
  | Some alias -> Gcp {alias}

let dockerfile_alias = Option.value ~default:tezt_cloud Cli.dockerfile_alias

let dockerfile = Path.dockerfile ~alias:dockerfile_alias

let project_id = Gcloud.project_id

let init () =
  if tezt_cloud = "" then
    Test.fail
      "The tezt-cloud value should be set. Either via the CLI or via the \
       environment variable 'TEZT_CLOUD'" ;
  match mode with
  | `Localhost | `Orchestrator -> Lwt.return_unit
  | `Host | `Cloud ->
      let tezt_cloud = tezt_cloud in
      let* project_id = project_id () in
      Log.info "Initializing docker registry..." ;
      let* () = Terraform.Docker_registry.init () in
      Log.info "Deploying docker registry..." ;
      let* () = Terraform.Docker_registry.deploy ~tezt_cloud ~project_id in
      Lwt.return_unit

(* Even though we could get this information locally, it is interesting to fetch
   it through terraform to get the correct value if a scenario was launch with
   different parameters. *)
let hostname =
  let hostname = ref "" in
  fun () ->
    if !hostname = "" then (
      let tezt_cloud = tezt_cloud in
      let* hostname' = Terraform.Docker_registry.get_hostname ~tezt_cloud in
      hostname := hostname' ;
      Log.info "Authenticate docker hostname..." ;
      let* () = Gcloud.auth_configure_docker ~hostname:!hostname in
      return !hostname)
    else Lwt.return !hostname

let zone =
  let zone = ref "" in
  fun () ->
    (* To ensure the docker registry is deployed. *)
    let* zone' = Terraform.VM.zone () in
    zone := zone' ;
    Lwt.return !zone

let registry_uri () =
  let* hostname = hostname () in
  let* project_id = project_id () in
  let uri = Format.asprintf "%s/%s/%s" hostname project_id docker_registry in
  Lwt.return uri

let uri_of_docker_image docker_image =
  match (docker_image, mode) with
  | Gcp {alias}, (`Cloud | `Host | `Orchestrator) ->
      let* registry_uri = registry_uri () in
      Lwt.return (Format.asprintf "%s/%s" registry_uri alias)
  | Gcp {alias}, `Localhost -> Lwt.return alias
  | Octez_release _, (`Cloud | `Host | `Orchestrator) ->
      let* registry_uri = registry_uri () in
      Lwt.return (Format.asprintf "%s/octez" registry_uri)
  | Octez_release _, `Localhost -> Lwt.return "octez"

let rec wait_process ?(sleep = 4) ~is_ready ~run () =
  let process = run () in
  let* status = Process.wait process in
  match status with
  | Unix.WEXITED 0 ->
      let* output = Process.check_and_read_stdout process in
      if is_ready output then Lwt.return output
      else (
        Log.info
          "Process '%s' is not ready. Let's wait %d seconds"
          (Process.name process)
          sleep ;
        let* () = Lwt_unix.sleep (float_of_int sleep) in
        wait_process ~sleep ~is_ready ~run ())
  | _ ->
      Log.info
        "Process '%s' failed. Let's wait %d seconds"
        (Process.name process)
        sleep ;
      let* () = Lwt_unix.sleep (float_of_int sleep) in
      wait_process ~sleep ~is_ready ~run ()

let run_command ?cmd_wrapper cmd args =
  match cmd_wrapper with
  | None -> Process.spawn cmd args
  | Some cmd_wrapper ->
      Process.spawn cmd_wrapper.Gcloud.cmd (cmd_wrapper.args @ [cmd] @ args)
