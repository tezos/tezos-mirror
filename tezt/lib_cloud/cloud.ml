(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Tezt-cloud requires to bypass the clean-up process of Tezt. Hence, when the
   user press Ctrl+C, tezt-cloud needs to catch-up the signal before Tezt.

   Tezt-cloud registers his own handler during Ctrl+C that will trigger the
   clean-up of tezt-cloud. If Ctrl+C is sent a second time, it is the
   clean-up of Tezt that will take over.
*)
let sigint =
  let received_sigint = ref false in
  fun () ->
    if !received_sigint then Lwt.return_unit
    else
      let promise, resolver = Lwt.task () in
      let previous_behaviour = ref Sys.Signal_default in
      let handler _ =
        Sys.set_signal Sys.sigint !previous_behaviour ;
        Lwt.wakeup resolver ()
      in
      let previous_handler = Sys.(signal sigint (Signal_handle handler)) in
      previous_behaviour := previous_handler ;
      promise

(* This exception is raised when the test is interrupted by Ctrl+C. *)
exception Interrupted

type t = {
  agents : Agent.t list;
  website : Web.t option;
  prometheus : Prometheus.t option;
  grafana : Grafana.t option;
  alert_manager : Alert_manager.t option;
  chronos : Chronos.t option;
  otel : Otel.t option;
  jaeger : Jaeger.t option;
  deployement : Deployement.t option;
  notifier : Types.notifier;
}

let notifier t = t.notifier

let shutdown ?exn t =
  let* () =
    if Env.keep_alive then (
      Log.info "Please press <enter> to terminate the scenario." ;
      let* _ = Input.next () in
      Lwt.return_unit)
    else Lwt.return_unit
  in
  (* Shutdown the service managers before alert_manager *)
  let* () =
    Lwt_list.iter_s
      (fun agent ->
        match Agent.service_manager agent with
        | None -> Lwt.return_unit
        | Some sm -> Service_manager.shutdown sm)
      t.agents
  in
  Log.info "Shutting down processes..." ;
  let* () =
    Lwt.catch
      (fun () ->
        match t.deployement with
        | None -> Lwt.return_unit
        | Some deployement ->
            let agents = Deployement.agents deployement in
            agents
            |> List.map (fun agent ->
                   match Agent.point agent with
                   | None -> Lwt.return_unit
                   | Some point ->
                       Process.run
                         "ssh"
                         [
                           "-S";
                           Format.asprintf
                             "~/.ssh/sockets/root@%s-%d"
                             (fst point)
                             (snd point);
                           "-O";
                           "exit";
                           Format.asprintf "root@%s" (fst point);
                         ])
            |> Lwt.join)
      (fun _exn -> Lwt.return_unit)
  in
  let* () =
    Lwt.catch
      (fun () -> Process.clean_up ())
      (fun exn ->
        Log.warn
          "Tezt failed to clean up processes: %s"
          (Printexc.to_string exn) ;
        Log.warn
          "in case you did not destroy VMs, you should execute the 'clean up' \
           job" ;
        Lwt.return_unit)
  in
  let* () =
    Lwt.catch
      (fun () ->
        Option.fold
          ~none:Lwt.return_unit
          ~some:Prometheus.export_snapshot
          t.prometheus)
      (fun exn ->
        Log.warn "Prometheus snapshot export fails: %s" (Printexc.to_string exn) ;
        Lwt.return_unit)
  in
  let* () =
    Lwt.catch
      (fun () ->
        Option.fold ~none:Lwt.return_unit ~some:Prometheus.shutdown t.prometheus)
      (fun exn ->
        Log.warn
          "unable to shutdown Prometheus properly: %s"
          (Printexc.to_string exn) ;
        Lwt.return_unit)
  in
  let* () =
    if Option.is_some t.alert_manager then Alert_manager.shutdown ()
    else Lwt.return_unit
  in
  let* () =
    Option.fold ~none:Lwt.return_unit ~some:Grafana.shutdown t.grafana
  in
  let* () = Option.fold ~none:Lwt.return_unit ~some:Otel.shutdown t.otel in
  let* () = Option.fold ~none:Lwt.return_unit ~some:Jaeger.shutdown t.jaeger in
  let* () =
    Option.fold
      ~none:Lwt.return_unit
      ~some:(Deployement.terminate ?exn)
      t.deployement
  in
  let () = Option.iter Chronos.shutdown t.chronos in
  (* This is not necessary because [Process.clean_up] already killed it.
  *)
  let* () = Option.fold ~none:Lwt.return_unit ~some:Web.shutdown t.website in
  match exn with
  | None -> Lwt.return_unit
  | Some exn -> (* The exception is raised to Tezt. *) Lwt.reraise exn

(* This function is used to ensure we can connect to the docker image on the VM. *)
let wait_ssh_server_running agent =
  if (Agent.configuration agent).vm.os = Debian then Lwt.return_unit
  else
    match Agent.runner agent with
    | None -> Lwt.return_unit
    | Some runner ->
        let is_ready _output = true in
        let run () =
          let cmd, args =
            Runner.wrap_with_ssh
              runner
              (Runner.Shell.cmd [] "echo" ["-n"; "check"])
          in
          Process.spawn cmd (runner.options @ args)
        in
        let* _ = Env.wait_process ~is_ready ~run () in
        Lwt.return_unit

let orchestrator ?(alerts = []) ?(tasks = []) deployement f =
  let agents = Deployement.agents deployement in
  let* website =
    if Env.website then
      let* website = Web.start ~agents in
      Lwt.return_some website
    else Lwt.return_none
  in
  let* prometheus =
    if Env.prometheus then
      (* Alerts requires to update prometheus configuration. *)
      let alerts = List.map (fun Alert_manager.{alert; _} -> alert) alerts in
      let* prometheus = Prometheus.start ~alerts agents in
      Lwt.return_some prometheus
    else Lwt.return_none
  in
  (* Enable logrotate if --log_rotation was not set to 0... *)
  let logrotate = Env.log_rotation <> 0 in
  let* logrotate =
    (* ... and if log-file was specified *)
    match (logrotate, Tezt.Cli.Logs.file) with
    (* If no logfile, do not enable logrotate *)
    | _, None -> Lwt.return false
    (* If logfile and logrotate, configure each agent *)
    | true, Some target_file ->
        let* () =
          (* Logrotate: write a configuration file inside each container *)
          Lwt_list.iter_s
            (fun agent ->
              let* () =
                Logrotate.write_config
                  ~name:"tezt-cloud"
                    (* Use hardcoded name as of now: will not work in localhost mode *)
                  ~pidfile:"/var/run/tezt-cloud.pid"
                  ~target_file
                  ~max_size:204800
                  ~max_rotations:Env.log_rotation
                  agent
              in
              Logrotate.write_config
              (* Note: the process name seems to be "screen"
                 when using pgrep or pkill, despite appearing in ps as "SCREEN" *)
                ~name:"screen"
                ~target_file:"/root/screenlog.0"
                ~max_size:100
                ~max_rotations:1
                agent)
            agents
        in
        Lwt.return true
    (* If no logrotate, return false *)
    | false, _ -> Lwt.return_false
  in
  let tasks =
    if logrotate then
      let name = "logrotate" in
      (* chronos: triggers logrotate every 4 hours.
         if each 4 hours, the criteria to trigger a rotation is met, they
         will be rotated.
         Note that rotation may involve an important IO stress for the machine *)
      let tm = "0 0-23/4 * * *" in
      let action () =
        Lwt_list.iter_s
          (fun agent ->
            let* () = Logrotate.run ~name:"tezt-cloud" agent in
            Logrotate.run ~name:"screen" agent)
          agents
      in
      let task = Chronos.task ~name ~tm ~action () in
      task :: tasks
    else tasks
  in
  let chronos =
    if List.is_empty tasks then None
    else
      let chronos = Chronos.init ~tasks in
      let () = Chronos.start chronos in
      Some chronos
  in
  let notifier = Env.notifier in
  (* TODO: change Alert_manager to accept notifier directly *)
  let default_receiver =
    match notifier with
    | Notifier_null -> Alert_manager.null_receiver
    | Notifier_slack {name; slack_bot_token; slack_channel_id} ->
        Alert_manager.slack_bottoken_receiver
          ~name
          ~channel:slack_channel_id
          ~bot_token:slack_bot_token
          ()
  in
  let* alert_manager = Alert_manager.run ~default_receiver alerts in
  let* grafana =
    if Env.grafana then
      let* grafana = Grafana.run () in
      Lwt.return_some grafana
    else Lwt.return_none
  in
  let* otel, jaeger =
    if Env.open_telemetry then
      let* otel = Otel.run ~jaeger:true in
      let* jaeger = Jaeger.run () in
      Lwt.return (Some otel, Some jaeger)
    else Lwt.return (None, None)
  in
  let t =
    {
      website;
      agents;
      prometheus;
      grafana;
      alert_manager;
      chronos;
      otel;
      jaeger;
      deployement = Some deployement;
      notifier;
    }
  in
  let sigint = sigint () in
  let main_promise =
    (* We also catch error raised from the scenario directly. *)
    Lwt.catch
      (fun () ->
        let* () = f t in
        Lwt.return_none)
      (fun exn -> Lwt.return_some exn)
  in
  (* This part is tricky! We want to catch Ctrl+C so that tezt does not
     kill all the VMs directly before tezt-cloud termination tasks are
     over. When the signal is caught, tezt-cloud takes over. Processes are
     cleaned up manually via [Process.clean_up ()].
  *)
  let* exn =
    Lwt.pick
      [
        (let* () = sigint in
         Lwt.return_some Interrupted);
        main_promise;
      ]
  in
  shutdown ?exn t

let attach agent =
  (* The proxy agent has to have a runner attached to it. *)
  let runner = Agent.runner agent |> Option.get in
  let hooks =
    Process.
      {
        on_log =
          (fun str -> Log.debug ~color:Log.Color.reset "%s" (String.trim str));
        on_spawn = (fun _ _ -> ());
      }
  in
  let has_sigint = ref false in
  let on_sigint =
    let* () = sigint () in
    has_sigint := true ;
    Log.debug "Sigint triggered..." ;
    let cmd, args =
      Runner.wrap_with_ssh
        runner
        (Runner.Shell.cmd [] "screen" ["-S"; "tezt-cloud"; "-X"; "stuff"; "^C"])
    in
    let* () =
      Process.spawn ~hooks cmd (runner.options @ args) |> Process.check
    in
    let cmd, args =
      Runner.wrap_with_ssh
        runner
        (Runner.Shell.cmd [] "stdbuf" ["-oL"; "tail"; "-F"; "screenlog.0"])
    in
    let _p =
      Process.spawn ~hooks cmd (runner.options @ args) |> Process.check
    in
    let* _ = Input.eof in
    let* () =
      let process = Process.spawn ~runner "pkill" ["screen"] in
      let* _ = Process.wait process in
      Lwt.return_unit
    in
    let* () =
      let process = Process.spawn ~runner "rm" ["-f"; "screenlog.0"] in
      let* _ = Process.wait process in
      Lwt.return_unit
    in
    let* () =
      let process =
        Process.spawn "rm" [Path.proxy_deployement ~tezt_cloud:Env.tezt_cloud]
      in
      let* _ = Process.wait process in
      Lwt.return_unit
    in
    Lwt.return_unit
  in
  let on_eof =
    let* () = Input.eof in
    Log.info "Detach from the proxy process." ;
    if !has_sigint then on_sigint
    else
      let* domain =
        Gcloud.DNS.get_fqdn ~name:Env.tezt_cloud ~zone:"tezt-cloud"
      in
      let uri =
        match domain with
        | None ->
            Format.asprintf "http://%s" (Agent.point agent |> Option.get |> fst)
        | Some domain -> Format.asprintf "http://%s" domain
      in
      Log.info "Deployment website can be accessed here: %s" uri ;
      Lwt.return_unit
  in
  Log.Style.set_prefix Log.Style.Hidden ;
  Log.Style.set_timestamp Log.Style.Hidden ;
  let cmd, args =
    Runner.wrap_with_ssh
      runner
      (Runner.Shell.cmd [] "stdbuf" ["-oL"; "tail"; "-F"; "screenlog.0"])
  in
  let logger =
    Lwt.catch
      (fun () ->
        let* () =
          Process.spawn ~hooks cmd (runner.options @ args) |> Process.check
        in
        Lwt.return_unit)
      (fun exn ->
        Log.debug "Interrupted: %s" (Printexc.to_string exn) ;
        on_sigint)
  in
  Lwt.choose [logger; on_sigint; on_eof]

let try_reattach () =
  let tezt_cloud = Env.tezt_cloud in
  if Sys.file_exists (Path.proxy_deployement ~tezt_cloud) then
    let contents = Base.read_file (Path.proxy_deployement ~tezt_cloud) in
    let json = Data_encoding.Json.from_string contents |> Result.get_ok in
    let deployement =
      Data_encoding.Json.destruct (Data_encoding.list Agent.encoding) json
      |> Deployement.of_agents
    in
    let agents = Deployement.agents deployement in
    let proxy_agent = Proxy.get_agent agents in
    let* is_ssh_server_running =
      Lwt.pick
        [
          (let* () = Lwt_unix.sleep 5. in
           Lwt.return_false);
          (let* () = wait_ssh_server_running proxy_agent in
           Lwt.return_true);
        ]
    in
    if is_ssh_server_running then
      let* is_running =
        Lwt.catch
          (fun () ->
            let* status =
              Process.spawn
                ?runner:(Agent.runner proxy_agent)
                "ls"
                ["screenlog.0"]
              |> Process.wait
            in
            match status with
            | WEXITED 0 -> Lwt.return_true
            | _ -> Lwt.return_false)
          (fun _exn -> Lwt.return_false)
      in
      if is_running then
        let* () =
          Lwt.catch
            (fun () -> attach proxy_agent)
            (fun exn ->
              Log.debug "INFO: %s" (Printexc.to_string exn) ;
              Lwt.return_unit)
        in
        Lwt.return_true
      else Lwt.return_false
    else Lwt.return_false
  else Lwt.return_false

let init_proxy ?(proxy_files = []) ?(proxy_args = []) deployement =
  let agents = Deployement.agents deployement in
  let proxy_agent = Proxy.get_agent agents in
  let* () = wait_ssh_server_running proxy_agent in
  let destination =
    (Agent.configuration proxy_agent).vm.binaries_path
    // Filename.basename Path.self
  in
  let* self = Agent.copy ~destination proxy_agent ~source:Path.self in
  let json =
    Data_encoding.Json.construct (Data_encoding.list Agent.encoding) agents
  in
  let contents = Data_encoding.Json.to_string json in
  let tezt_cloud = Env.tezt_cloud in
  let proxy_deployement = Path.proxy_deployement ~tezt_cloud in
  Base.write_file proxy_deployement ~contents ;
  let* () =
    Proxy.copy_files proxy_agent ~scenario_files:proxy_files ~proxy_deployement
  in
  let runner = Agent.runner proxy_agent in
  let* () =
    (* This should not be necessary, this is to ensure there is no [screen] leftover. *)
    let process = Process.spawn ?runner "pkill" ["screen"] in
    let* _ = Process.wait process in
    Lwt.return_unit
  in
  let* () =
    (* We start a screen session in detached mode. The orchestrator will run in this session. *)
    Process.spawn ?runner "screen" ["-S"; "tezt-cloud"; "-d"; "-m"]
    |> Process.check
  in
  let process =
    let args =
      (* remove "--ssh-host host" from the commande line *)
      let rec filter_ssh acc args =
        match args with
        | [] -> List.rev acc
        (* We need to remove the private key argument. This permanent key
           is only used to connect to the proxy. The key that is generated
           and uploaded to the proxy allow to connect to the containers. *)
        | "--ssh-private-key" :: _priv :: args -> filter_ssh acc args
        (* FIXME: remove proxy-localhost when agent name bug is fixed *)
        | "--ssh-host" :: _host :: args ->
            filter_ssh ("--proxy-localhost" :: "--proxy" :: acc) args
        | arg :: args -> filter_ssh (arg :: acc) args
      in
      let args = Sys.argv |> Array.to_list |> List.tl in
      let args = filter_ssh [] args in
      args @ ["--localhost"; "--tezt-cloud"; Env.tezt_cloud]
      (* [--localhost] will be combined with [--proxy], this enables to detect we
         want to run in [`Remote_orchestrator_local_agents] mode.

         [--tezt-cloud] is used so that the [`Remote_orchestrator_local_agents]
         mode knows this value. *)
    in
    (* We execute a command in a screen session that will start the orchestrator. *)
    Agent.docker_run_command
      proxy_agent
      "screen"
      (["-S"; "tezt-cloud"; "-X"; "exec"] @ (self :: args) @ proxy_args)
  in
  let* () =
    Agent.docker_run_command
      proxy_agent
      "screen"
      ["-S"; "tezt-cloud"; "-X"; "log"]
    |> Process.check
  in
  let* () =
    Lwt.catch
      (fun () -> attach proxy_agent)
      (fun exn ->
        Test.fail "Unable to attach process: %s" (Printexc.to_string exn))
  in
  let* () =
    let* status = Process.wait process in
    match status with
    | WEXITED 0 -> Lwt.return_unit
    | _ -> Test.fail "Proxy scenario has failed"
  in
  if Env.destroy then Deployement.terminate deployement else Lwt.return_unit

let set_faketime agent faketime =
  match Agent.runner agent with
  | None -> Lwt.return_unit (* ? *)
  | Some runner ->
      let open Runner.Shell in
      let* home =
        (* Get the directory where you can (hopefully) find .ssh *)
        let cmd = cmd [] "pwd" [] in
        let cmd, args = Runner.wrap_with_ssh runner cmd in
        Process.run_and_read_stdout cmd args
      in
      let env_file = Filename.concat (String.trim home) ".ssh/environment" in
      let* () =
        (* Avoid error if the environment file does not exist *)
        let cmd = cmd [] "touch" [env_file] in
        let cmd, args = Runner.wrap_with_ssh runner cmd in
        Process.run cmd args
      in
      let* contents =
        (* Read the environment file content
               and append FAKETIME definition to the result *)
        let process, stdin =
          Process.spawn_with_stdin ~runner "cat" [env_file; "-"]
        in
        let* () = Lwt_io.write_line stdin (sf "FAKETIME=%s" faketime) in
        let* () = Lwt_io.close stdin in
        Process.check_and_read_stdout process
      in
      (* Write the final environment content *)
      let cmd = redirect_stdout (cmd [] "echo" [contents]) env_file in
      let cmd, args = Runner.wrap_with_ssh runner cmd in
      Process.run cmd args

(** Optionally add some latency and jitter to network connection *)
let adjust_traffic_control i agent =
  match (Env.tc_delay, Env.tc_jitter) with
  | None, None -> Lwt.return_unit
  | tc_delay, tc_jitter ->
      let () =
        Option.iter (fun seed -> Random.init (seed + i)) Tezt.Cli.Options.seed
      in
      let rand = function
        | Some (min, max) -> min +. Random.float (max -. min)
        | None -> 0.
      in
      let delay = rand tc_delay in
      let jitter = rand tc_jitter in
      (* Get the interface name in the docker container (among other infos) *)
      let* ip_output =
        Agent.docker_run_command agent "ip" ["route"; "show"; "default"]
        |> Process.check_and_read_stdout
      in
      (* Parse the ip output in order to retrieve the interface name *)
      let iface_name =
        let rec loop = function
          | [] ->
              Test.fail
                "Failed to retrieve interface name (%s)"
                (Agent.name agent)
          | "dev" :: name :: _ -> name
          | _ :: tl -> loop tl
        in
        loop (String.split_on_char ' ' ip_output)
      in
      (* Run [tc] on this interface *)
      Agent.docker_run_command
        agent
        "tc"
        [
          "qdisc";
          "add";
          "dev";
          iface_name;
          "root";
          "netem";
          "delay";
          Printf.sprintf "%.3fs" delay;
          Printf.sprintf "%.3fs" jitter;
        ]
      |> Process.check

let register ?proxy_files ?proxy_args ?vms ?dockerbuild_args ~__FILE__ ~title
    ~tags ?seed ?alerts ?tasks f =
  Test.register ~__FILE__ ~title ~tags ?seed @@ fun () ->
  let* () = Env.init () in
  let* vms =
    match vms with
    | None -> Lwt.return_none
    | Some vms -> Lwt.map Option.some (vms ())
  in
  let vms =
    match (vms, Env.vms) with
    | None, None | None, Some _ -> None
    | Some _vms, Some 0 -> (
        Log.warn
          "The legacy behaviour with '--vms-limit 0' may be removed in the \
           future." ;
        match Env.mode with
        | `Local_orchestrator_local_agents | `Local_orchestrator_remote_agents
          ->
            None
        | `Remote_orchestrator_remote_agents | `Remote_orchestrator_local_agents
        | `Ssh_host _ ->
            (* In [Remote_orchestrator_remote_agents] mode, we want to run a
               deployment deploying the Proxy VM. In
               [Remote_orchestrator_local_agents] or in [Ssh_host] mode, there is
               few initialisation steps needed. By using [Some []], we ensure
               they will be done. When the scenario asks for an agent and do not
               find it there is fallback to a default agent. This works but this
               is hackish and should be removed in the near future (famous last
               words). *)
            Some [])
    | Some vms, Some vms_limit ->
        let number_of_vms = List.length vms in
        if vms_limit < number_of_vms then
          Test.fail
            "The number limits of VM '%d' is less than the number of VMs \
             specified by the scenario: '%d'"
            vms_limit
            number_of_vms
        else Some vms
    | Some vms, None -> Some vms
  in
  match vms with
  | None ->
      let default_agent =
        let configuration =
          Agent.Configuration.make ~name:"default" ?dockerbuild_args ()
        in
        let next_available_port =
          let cpt = ref 30_000 in
          fun () ->
            incr cpt ;
            !cpt
        in
        let process_monitor =
          if Env.process_monitoring then
            Some (Process_monitor.init ~listening_port:(next_available_port ()))
          else None
        in
        Agent.make
          ~configuration
          ~next_available_port
          ~vm_name:None
          ~process_monitor
          ~artifacts_dir:Env.artifacts_dir
          ()
      in
      f
        {
          agents = [default_agent];
          website = None;
          grafana = None;
          otel = None;
          jaeger = None;
          prometheus = None;
          alert_manager = None;
          chronos = None;
          deployement = None;
          notifier = Env.notifier;
        }
  | Some configurations -> (
      let* ssh_public_key = Ssh.public_key () in
      let sorted_names =
        configurations
        |> List.map (fun Agent.Configuration.{name; _} -> name)
        |> List.sort_uniq compare
      in
      if List.length sorted_names < List.length configurations then
        Test.fail
          "Duplicate found in the agent names used by the scenario: %s"
          (String.concat " " sorted_names)
      else
        let tezt_cloud = Env.tezt_cloud in
        let ensure_ready =
          let ensure_ready i agent =
            let* () = wait_ssh_server_running agent in
            let* () = adjust_traffic_control i agent in
            let* () =
              match Env.faketime with
              | None -> Lwt.return_unit
              | Some faketime -> set_faketime agent faketime
            in
            Lwt.return_unit
          in
          fun deployement ->
            Deployement.agents deployement |> List.mapi ensure_ready |> Lwt.join
        in
        match Env.mode with
        | `Remote_orchestrator_local_agents ->
            (* The scenario is executed locally on the proxy VM. *)
            let contents =
              Base.read_file (Path.proxy_deployement ~tezt_cloud)
            in
            let json =
              Data_encoding.Json.from_string contents |> Result.get_ok
            in
            let deployement =
              Data_encoding.Json.destruct
                (Data_encoding.list Agent.encoding)
                json
              |> Deployement.of_agents
            in
            let* () = ensure_ready deployement in
            orchestrator ?alerts ?tasks deployement f
        | `Local_orchestrator_local_agents ->
            (* The scenario is executed locally and the VM are on the host machine. *)
            let* () =
              Jobs.docker_build
                ?args:dockerbuild_args
                ~push:false
                ~ssh_public_key
                ()
            in
            let* deployement =
              Deployement.deploy ?dockerbuild_args ~configurations ()
            in
            let* () = ensure_ready deployement in
            orchestrator ?alerts ?tasks deployement f
        | `Local_orchestrator_remote_agents ->
            (* The scenario is executed locally and the VMs are on the cloud. *)
            let* () = Jobs.deploy_docker_registry () in
            let* () =
              Jobs.docker_build
                ?args:dockerbuild_args
                ~push:Env.push_docker
                ~ssh_public_key
                ()
            in
            let* deployement =
              Deployement.deploy ?dockerbuild_args ~configurations ()
            in
            let* () = ensure_ready deployement in
            orchestrator ?alerts ?tasks deployement f
        | `Remote_orchestrator_remote_agents | `Ssh_host _ ->
            (* The scenario is executed remotely. *)
            let* proxy_running = try_reattach () in
            if not proxy_running then
              let* () = Jobs.deploy_docker_registry () in
              let* () =
                Jobs.docker_build
                  ?args:dockerbuild_args
                  ~push:Env.push_docker
                  ~ssh_public_key
                  ()
              in
              let* deployement =
                Deployement.deploy ?dockerbuild_args ~configurations ()
              in
              let* () = ensure_ready deployement in
              init_proxy ?proxy_files ?proxy_args deployement
            else Lwt.return_unit)

let agents t =
  match Env.mode with
  | `Remote_orchestrator_local_agents -> (
      let proxy_agent = Proxy.get_agent t.agents in
      let proxy_name = Agent.name proxy_agent in
      match
        t.agents |> List.filter (fun agent -> Agent.name agent <> proxy_name)
      with
      | [] ->
          let configuration = Proxy.make_config () in
          let next_available_port =
            let cpt = ref 30_000 in
            fun () ->
              incr cpt ;
              !cpt
          in
          let process_monitor =
            if Env.process_monitoring then
              Some
                (Process_monitor.init ~listening_port:(next_available_port ()))
            else None
          in
          let default_agent =
            Agent.make
              ~configuration
              ~next_available_port
              ~vm_name:(Some (Format.asprintf "%s-orchestrator" Env.tezt_cloud))
              ~process_monitor
              ~artifacts_dir:Env.artifacts_dir
              ()
          in
          [default_agent]
      | agents -> agents)
  | `Remote_orchestrator_remote_agents | `Local_orchestrator_remote_agents
  | `Local_orchestrator_local_agents | `Ssh_host _ ->
      t.agents

let write_website t =
  match t.website with
  | None -> Lwt.return_unit
  | Some website -> Web.write website ~agents:t.agents

let push_metric t ?help ?typ ?labels ~name value =
  match t.website with
  | None -> ()
  | Some website -> Web.push_metric website ?help ?typ ?labels ~name value

type target = {agent : Agent.t; port : int; app_name : string}

let add_prometheus_source t ?metrics_path ~name targets =
  match t.prometheus with
  | None -> Lwt.return_unit
  | Some prometheus ->
      let prometheus_target {agent; port; app_name} =
        let address = agent |> Agent.runner |> Runner.address in
        Prometheus.{address; port; app_name}
      in
      let targets = List.map prometheus_target targets in
      Prometheus.add_job prometheus ?metrics_path ~name targets

let add_service t ~name ~url =
  match t.website with
  | None -> Lwt.return_unit
  | Some web -> Web.add_service web ~agents:t.agents {name; url}

let open_telemetry_endpoint t =
  match t.otel with
  | None -> None
  | Some _otel -> (
      match Env.mode with
      | `Remote_orchestrator_local_agents ->
          let agent = Proxy.get_agent t.agents in
          let address = Agent.point agent |> Option.get |> fst in
          let port = 55681 in
          Some (Format.asprintf "http://%s:%d" address port)
      | _ ->
          (* It likely won't work in [Local_orchestrator_remote_agents] mode. *)
          let address = "localhost" in
          let port = 55681 in
          Some (Format.asprintf "http://%s:%d" address port))

let register_binary cloud ?agents ?(group = "tezt-cloud") ~name () =
  if Env.process_monitoring then
    let agents =
      match agents with None -> cloud.agents | Some agents -> agents
    in
    Lwt_list.iter_p
      (fun agent ->
        match Agent.process_monitor agent with
        | None -> Lwt.return_unit
        | Some process_monitor ->
            let changed =
              Process_monitor.add_binary process_monitor ~group ~name
            in
            if changed then
              let* () =
                Process_monitor.reload process_monitor (fun ~detach cmd args ->
                    Agent.docker_run_command agent ~detach cmd args)
              in
              let app_name =
                Format.asprintf
                  "%s:prometheus-process-exporter"
                  (Agent.name agent)
              in
              let target =
                let address = agent |> Agent.runner |> Runner.address in
                Prometheus.
                  {
                    address;
                    port = Process_monitor.get_port process_monitor;
                    app_name;
                  }
              in
              (* Reload prometheus *)
              let* () =
                match cloud.prometheus with
                | None -> Lwt.return_unit
                | Some prometheus ->
                    Prometheus.add_job prometheus ~name:app_name [target]
              in
              (* Reload the website *)
              write_website cloud
            else Lwt.return_unit)
      agents
  else Lwt.return_unit

(* FIXME: remove the need for this table by being able to properly associate
   node to the corresponding agent *)
let agents_by_service_name = Hashtbl.create 10

let service_name agent name = Format.asprintf "%s-%s" (Agent.name agent) name

let service_register ~name ~executable ?on_alive_callback ~on_shutdown agent =
  match Agent.service_manager agent with
  | None -> ()
  | Some service_manager ->
      let () = Hashtbl.add agents_by_service_name name agent in
      let name = service_name agent name in
      Service_manager.register_service
        ~name
        ~executable
        ?on_alive_callback
        ~on_shutdown
        service_manager

let notify_service_start ~name ~pid =
  match Hashtbl.find_opt agents_by_service_name name with
  | None -> ()
  | Some agent -> (
      match Agent.service_manager agent with
      | None -> ()
      | Some service_manager ->
          let name = service_name agent name in
          Service_manager.notify_start_service ~name ~pid service_manager)

let notify_service_stop ~name =
  match Hashtbl.find_opt agents_by_service_name name with
  | None -> ()
  | Some agent -> (
      match Agent.service_manager agent with
      | None -> ()
      | Some service_manager ->
          let name = service_name agent name in
          Service_manager.notify_stop_service ~name service_manager)

let register_chronos_task t task =
  match t.chronos with
  | None -> ()
  | Some chronos -> Chronos.add_task chronos task

let add_alert cloud ~alert =
  match cloud.alert_manager with
  | None -> Lwt.return_unit
  | Some alert_manager -> (
      match cloud.prometheus with
      | None ->
          Log.warn
            "Alert_manager is enabled without prometheus. No alerts will be \
             fired" ;
          Alert_manager.add_alert alert_manager ~alert
      | Some prometheus ->
          (* TODO: clean the prometheus and alert_manager api, this is ugly *)
          let Alert_manager.{alert = alert'; _} = alert in
          let* () =
            Prometheus.register_rules
              [Prometheus.rule_of_alert alert']
              prometheus
          in
          Alert_manager.add_alert alert_manager ~alert)
