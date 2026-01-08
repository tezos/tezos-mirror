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
      match Sys.getenv_opt "TEZT_CLOUD" with
      | None -> ""
      | Some value -> value)

let mode =
  match (Cli.proxy, Cli.localhost, Cli.ssh_host) with
  | true, true, None -> `Remote_orchestrator_local_agents
  | true, false, None -> `Remote_orchestrator_remote_agents
  | false, true, None -> `Local_orchestrator_local_agents
  | false, false, None -> `Local_orchestrator_remote_agents
  | true, _, Some _ | _, true, Some _ ->
      Test.fail
        "Unexpected combination of options: ssh_host and (proxy or localhost)"
  | _, _, Some endpoint ->
      let uri = Uri.of_string (Format.asprintf "tcp://%s" endpoint) in
      let host =
        match Uri.host uri with
        | None -> Test.fail "No valid hostname specified: %s" endpoint
        | Some host -> host
      in
      let user = Option.value ~default:(Sys.getenv "USER") (Uri.user uri) in
      let port = Option.value ~default:22 (Uri.port uri) in
      `Ssh_host (user, host, port)

let ssh_private_key_filename ?(home = Sys.getenv "HOME") () =
  match Cli.ssh_private_key with
  | None -> home // ".ssh" // Format.asprintf "%s-tf" tezt_cloud
  | Some key -> key

let ssh_public_key_filename ?home () =
  let ssh_key = ssh_private_key_filename ?home () in
  Format.asprintf "%s.pub" ssh_key

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

let log_rotation = Cli.log_rotation

let retrieve_daily_logs = Cli.retrieve_daily_logs

let tc_delay = Cli.tc_delay

let tc_jitter = Cli.tc_jitter

let artifacts_dir = Cli.artifacts_dir

let teztale_artifacts = Cli.teztale_artifacts

let init () =
  if tezt_cloud = "" then
    Test.fail
      "The tezt-cloud value should be set. Either via the CLI or via the \
       environment variable 'TEZT_CLOUD'" ;
  (* If using logfile, installs a signal handler to close and reopen the logfile.
     This allows logrotate to use a better strategy than copytruncate
     https://incoherency.co.uk/blog/stories/logrotate-copytruncate-race-condition.html
  *)
  (match Tezt_core.Cli.Logs.file with
  | None -> ()
  | Some logfile ->
      let sighup_flag = ref false in
      (* loop every second to check if sighup_flag was set *)
      let logfile_reopen =
        let rec loop () =
          let* () =
            if !sighup_flag then (
              sighup_flag := false ;
              Log.report "Reopening logfile : %s" logfile ;
              Tezt_core.Log.set_file logfile ;
              Lwt.return_unit)
            else Lwt_unix.sleep 1.
          in
          loop ()
        in
        Log.report "Starting reopen logfile background handler" ;
        loop ()
      in
      (* signal handler that now just defer Log.set_file *)
      let signal_hup_handler signal =
        if signal = Sys.sighup then sighup_flag := true
      in
      let _ =
        Lwt_main.Exit_hooks.add_first (fun () ->
            let () = Lwt.cancel logfile_reopen in
            Lwt.return_unit)
      in
      Log.report "Installing signal handler for reopening logfile" ;
      Sys.set_signal Sys.sighup (Sys.Signal_handle signal_hup_handler)) ;
  match mode with
  | `Local_orchestrator_local_agents | `Ssh_host _ -> Lwt.return_unit
  | `Remote_orchestrator_local_agents ->
      (* In orchestrator mode, expose the PID of tezt-cloud in a file as the
         process can be considered as a daemon
         It will be useful for logrotate *)
      let* pidfile =
        match Unix.getuid () with
        | 0 -> Lwt.return "/var/run/tezt-cloud.pid"
        | uid ->
            Format.asprintf "/var/run/user/%d/tezt-cloud.pid" uid |> Lwt.return
      in
      let pid = Unix.getpid () in
      Log.report "Writing %d to pidfile : %s" pid pidfile ;
      ( with_open_out pidfile @@ fun fd ->
        output_string fd (Format.asprintf "%d\n" pid) ) ;
      Lwt.return_unit
  | `Remote_orchestrator_remote_agents | `Local_orchestrator_remote_agents ->
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

let pp_process_status fmt = function
  | Unix.WEXITED n -> Format.fprintf fmt "EXITED %n" n
  | Unix.WSIGNALED n -> Format.fprintf fmt "SIGNALED %n" n
  | Unix.WSTOPPED n -> Format.fprintf fmt "STOPPED %n" n

exception Process_failed of Unix.process_status

let rec wait_process ?(sleep = 4) ~is_ready ~run ?(propagate_error = false) () =
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
        wait_process ~sleep ~is_ready ~run ~propagate_error ())
  | e ->
      Log.info
        "Process '%s' failed with %a. Let's wait %d seconds"
        (Process.name process)
        pp_process_status
        e
        sleep ;
      let* () = Lwt_unix.sleep (float_of_int sleep) in
      if propagate_error then Lwt.fail (Process_failed e)
      else wait_process ~sleep ~is_ready ~run ()

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
      | `Remote_orchestrator_remote_agents -> (
          let* domain =
            Gcloud.DNS.get_fqdn ~name:tezt_cloud ~zone:"tezt-cloud"
          in
          match domain with
          | None -> Lwt.return Cli.dns_domains
          | Some domain -> Lwt.return (domain :: Cli.dns_domains))
      | `Remote_orchestrator_local_agents | `Local_orchestrator_local_agents
      | `Local_orchestrator_remote_agents | `Ssh_host _ ->
          Lwt.return Cli.dns_domains
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

let notifier =
  let open Types in
  match (Cli.slack_bot_token, Cli.slack_channel_id) with
  | None, None -> Notifier_null
  | Some _, None ->
      Log.warn
        "A Slack bot token has been provided but no Slack channel id. No \
         reports or alerts will be sent." ;
      Notifier_null
  | None, Some slack_channel_id -> (
      match Sys.getenv_opt "TEZTCLOUD_SLACK_BOT_TOKEN" with
      | None ->
          Log.warn
            "A Slack channel ID has been provided but no --slack-bot-token is \
             specified or TEZTCLOUD_SLACK_BOT_TOKEN environment variable is \
             defined. No reports or alerts will be sent." ;
          Notifier_null
      | Some slack_bot_token ->
          Notifier_slack
            {name = "default-slack"; slack_channel_id; slack_bot_token})
  | Some slack_bot_token, Some slack_channel_id ->
      Notifier_slack {name = "default-slack"; slack_channel_id; slack_bot_token}
