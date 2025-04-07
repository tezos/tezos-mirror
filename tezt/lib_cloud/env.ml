(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

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

let mode =
  match (Cli.localhost, Cli.proxy) with
  | true, true -> `Orchestrator
  | true, false -> `Localhost
  | false, true -> `Host
  | false, false -> `Cloud

let prometheus = Cli.prometheus

let prometheus_export = Cli.prometheus_export

let prometheus_port = Cli.prometheus_port

let prometheus_export_path = Cli.prometheus_export_path

let prometheus_snapshots = Cli.prometheus_snapshots

let prometheus_scrape_interval = Cli.prometheus_scrape_interval

let grafana = Cli.grafana

let grafana_legacy_source = Cli.grafana_legacy_source

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

let open_telemetry = Cli.open_telemetry

let alert_handlers = Cli.alert_handlers

let dockerfile_alias = Option.value ~default:tezt_cloud Cli.dockerfile_alias

let dockerfile = Path.dockerfile ~alias:dockerfile_alias

let docker_registry = Format.asprintf "%s-docker-registry" tezt_cloud

let macosx = Cli.macosx

let check_file_consistency = Cli.check_file_consistency

let docker_host_network = Cli.docker_host_network

let push_docker = Cli.push_docker

let auto_approve = Cli.auto_approve

let project_id = Gcloud.project_id

let faketime = Cli.faketime

let binaries_path = Cli.binaries_path

let process_monitoring = Cli.process_monitoring

let init () =
  if tezt_cloud = "" then
    Test.fail
      "The tezt-cloud value should be set. Either via the CLI or via the \
       environment variable 'TEZT_CLOUD'" ;
  match mode with
  | `Localhost | `Orchestrator -> Lwt.return_unit
  | `Host | `Cloud ->
      let* project_id = project_id () in
      Log.info "Initializing docker registry..." ;
      let* () = Terraform.Docker_registry.init () in
      Log.info "Deploying docker registry..." ;
      let* () = Terraform.Docker_registry.deploy ~tezt_cloud ~project_id in
      Lwt.return_unit

(* Even though we could get this information locally, it is interesting to fetch
   it through terraform to get the correct value if a scenario was launched with
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

let dns_domains () =
  (* When we use the proxy mode, by default a domain name is
     registered for the `tezt-cloud` zone. *)
  let* domains =
    if Cli.no_dns then Lwt.return_nil
    else
      match mode with
      | `Host -> (
          let* domain =
            Gcloud.DNS.get_fqdn ~name:tezt_cloud ~zone:"tezt-cloud"
          in
          match domain with
          | None -> Lwt.return Cli.dns_domains
          | Some domain -> Lwt.return (domain :: Cli.dns_domains))
      | `Orchestrator | `Localhost | `Cloud -> Lwt.return Cli.dns_domains
  in
  (* A fully-qualified domain name requires to end with a dot.
     However, the usage tends to omit this final dot. Because having a
     domain name ending with a dot is always valid, we add one if
     there is none.

     See http://www.dns-sd.org/trailingdotsindomainnames.html for more
     details.
  *)
  domains
  |> List.map (fun domain ->
         if String.ends_with ~suffix:"." domain then domain else domain ^ ".")
  |> Lwt.return
