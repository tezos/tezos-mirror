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

module Input : sig
  (** This module should be the only one that reads on [stdin]. *)

  (** [next ()] returns the next line on stdin or none if stdin is closed. *)
  val next : unit -> string option Lwt.t
end = struct
  type t = {
    mutable resolvers : string option Lwt.u list;
    mutable stdin_closed : bool;
  }

  let state = {resolvers = []; stdin_closed = false}

  let next () =
    if state.stdin_closed then Lwt.return_none
    else
      let t, u = Lwt.task () in
      state.resolvers <- u :: state.resolvers ;
      t

  let rec loop () =
    let* input = Lwt_io.read_line Lwt_io.stdin in
    state.resolvers
    |> List.iter (fun resolver -> Lwt.wakeup_later resolver (Some input)) ;
    state.resolvers <- [] ;
    loop ()

  let _ =
    Lwt.catch
      (fun () -> loop ())
      (fun _exn ->
        state.resolvers
        |> List.iter (fun resolver -> Lwt.wakeup_later resolver None) ;
        state.stdin_closed <- true ;
        Lwt.return_unit)
end

let eof =
  let promise, resolver = Lwt.task () in
  Lwt.dont_wait
    (fun () ->
      let rec loop () =
        let* input = Input.next () in
        match input with
        | None ->
            Lwt.wakeup resolver () ;
            Lwt.return_unit
        | Some _ -> loop ()
      in
      loop ())
    (fun _ -> Lwt.wakeup resolver ()) ;
  promise

(* This exception is raised when the test is interrupted by Ctrl+C. *)
exception Interrupted

type t = {
  agents : Agent.t list;
  website : Web.t option;
  prometheus : Prometheus.t option;
  grafana : Grafana.t option;
  deployement : Deployement.t option;
}

let shutdown ?exn t =
  let* () =
    if Env.keep_alive then (
      Log.info "Please press <enter> to terminate the scenario." ;
      let* _ = Input.next () in
      Lwt.return_unit)
    else Lwt.return_unit
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
                   let point = Agent.point agent in
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
    Option.fold ~none:Lwt.return_unit ~some:Grafana.shutdown t.grafana
  in
  let* () =
    Option.fold
      ~none:Lwt.return_unit
      ~some:(Deployement.terminate ?exn)
      t.deployement
  in
  (* This is not necessary because [Process.clean_up] already killed it.
  *)
  let* () = Option.fold ~none:Lwt.return_unit ~some:Web.shutdown t.website in
  match exn with
  | None -> Lwt.return_unit
  | Some exn -> (* The exception is raised to Tezt. *) Lwt.reraise exn

(* This function is used to ensure we can connect to the docker image on the VM. *)
let wait_ssh_server_running agent =
  if (Agent.configuration agent).os = "debian" then Lwt.return_unit
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
          Process.spawn cmd (["-o"; "StrictHostKeyChecking=no"] @ args)
        in
        let* _ = Env.wait_process ~is_ready ~run () in
        Lwt.return_unit

let orchestrator deployement f =
  let agents = Deployement.agents deployement in
  let* website =
    if Env.website then
      let* website = Web.start ~agents in
      Lwt.return_some website
    else Lwt.return_none
  in
  let* prometheus =
    if Env.prometheus then
      let* prometheus = Prometheus.start agents in
      Lwt.return_some prometheus
    else Lwt.return_none
  in
  let* grafana =
    if Env.grafana then
      let* grafana = Grafana.run () in
      Lwt.return_some grafana
    else Lwt.return_none
  in
  Log.info "Post prometheus" ;
  let t =
    {website; agents; prometheus; grafana; deployement = Some deployement}
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
      Process.spawn ~hooks cmd (["-o"; "StrictHostKeyChecking=no"] @ args)
      |> Process.check
    in
    let cmd, args =
      Runner.wrap_with_ssh
        runner
        (Runner.Shell.cmd [] "stdbuf" ["-oL"; "tail"; "-f"; "screenlog.0"])
    in
    let _p =
      Process.spawn ~hooks cmd (["-o"; "StrictHostKeyChecking=no"] @ args)
      |> Process.check
    in
    let* _ = eof in
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
    let* () = eof in
    Log.info "Detach from the proxy process." ;
    if !has_sigint then on_sigint
    else
      let* uri =
        if Env.dns then
          let* domain =
            Gcloud.DNS.get_domain ~tezt_cloud:Env.tezt_cloud ~zone:"tezt-cloud"
          in
          Lwt.return (Format.asprintf "http://%s" domain)
        else Lwt.return (Format.asprintf "http://%s" (Agent.point agent |> fst))
      in
      Log.info "Deployement website can be accessed here: %s" uri ;
      Lwt.return_unit
  in
  Log.Style.set_prefix Log.Style.Hidden ;
  Log.Style.set_timestamp Log.Style.Hidden ;
  let cmd, args =
    Runner.wrap_with_ssh
      runner
      (Runner.Shell.cmd [] "stdbuf" ["-oL"; "tail"; "-f"; "screenlog.0"])
  in
  let logger =
    Lwt.catch
      (fun () ->
        let* () =
          Process.spawn ~hooks cmd (["-o"; "StrictHostKeyChecking=no"] @ args)
          |> Process.check
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
    let proxy_agent =
      agents
      |> List.find (fun agent ->
             let proxy_agent_prefix =
               Format.asprintf "%s-proxy" Env.tezt_cloud
             in
             String.starts_with ~prefix:proxy_agent_prefix (Agent.name agent))
    in
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

let init_proxy ?(proxy_files = []) deployement =
  let agents = Deployement.agents deployement in
  let proxy_agent =
    agents
    |> List.find (fun agent ->
           let proxy_agent_prefix = Format.asprintf "%s-proxy" Env.tezt_cloud in
           String.starts_with ~prefix:proxy_agent_prefix (Agent.name agent))
  in
  let* () = wait_ssh_server_running proxy_agent in
  let destination =
    (Agent.configuration proxy_agent).binaries_path
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
      let args = Sys.argv |> Array.to_list |> List.tl in
      args @ ["--localhost"; "--tezt-cloud"; Env.tezt_cloud]
      (* [--localhost] will be combined with --proxy, this enables to detect we want to run in [`Orchestrator].

         [--tezt-cloud] is used so that the [`Orchestrator] mode knows this value.
      *)
    in
    (* We execute a command in a screen session that will start the orchestrator. *)
    Agent.docker_run_command
      proxy_agent
      "screen"
      (["-S"; "tezt-cloud"; "-X"; "exec"] @ (self :: args))
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

let register ?proxy_files ?vms ~__FILE__ ~title ~tags ?seed f =
  Test.register ~__FILE__ ~title ~tags ?seed @@ fun () ->
  let* () = Env.init () in
  let vms =
    (* The Cli arguments by-pass the argument given here. This enable the user
       to always have decide precisely the number of vms to be run. *)
    match (vms, Env.vms) with
    | None, None -> None
    | None, Some i | Some _, Some i ->
        let vms = List.init i (fun _ -> Configuration.make ()) in
        Some vms
    | Some vms, None -> Some vms
  in
  match vms with
  | None | Some [] ->
      (* If there is no configuration, it is a similar scenario as if there were not agent. *)
      f
        {
          agents = [];
          website = None;
          grafana = None;
          prometheus = None;
          deployement = None;
        }
  | Some configurations -> (
      let tezt_cloud = Env.tezt_cloud in
      match Env.mode with
      | `Orchestrator ->
          (* The scenario is executed locally on the proxy VM. *)
          let contents = Base.read_file (Path.proxy_deployement ~tezt_cloud) in
          let json = Data_encoding.Json.from_string contents |> Result.get_ok in
          let deployement =
            Data_encoding.Json.destruct (Data_encoding.list Agent.encoding) json
            |> Deployement.of_agents
          in
          let* () =
            Deployement.agents deployement
            |> List.map wait_ssh_server_running
            |> Lwt.join
          in
          orchestrator deployement f
      | `Localhost ->
          (* The scenario is executed locally and the VM are on the host machine. *)
          let* () = Jobs.docker_build ~push:false () in
          let* deployement = Deployement.deploy ~configurations in
          let* () =
            Deployement.agents deployement
            |> List.map wait_ssh_server_running
            |> Lwt.join
          in
          orchestrator deployement f
      | `Cloud ->
          (* The scenario is executed locally and the VMs are on the cloud. *)
          let* () = Jobs.deploy_docker_registry () in
          let* () = Jobs.docker_build ~push:true () in
          let* deployement = Deployement.deploy ~configurations in
          let* () =
            Deployement.agents deployement
            |> List.map wait_ssh_server_running
            |> Lwt.join
          in
          orchestrator deployement f
      | `Host ->
          (* The scenario is executed remotely. *)
          let* proxy_running = try_reattach () in
          if not proxy_running then
            let* () = Jobs.deploy_docker_registry () in
            let* () = Jobs.docker_build ~push:true () in
            let* deployement = Deployement.deploy ~configurations in
            let* () =
              Deployement.agents deployement
              |> List.map wait_ssh_server_running
              |> Lwt.join
            in
            init_proxy ?proxy_files deployement
          else Lwt.return_unit)

let agents t =
  match Env.mode with
  | `Orchestrator ->
      let proxy_agent = Proxy.get_agent t.agents in
      let proxy_vm_name = Agent.vm_name proxy_agent in
      t.agents
      |> List.filter (fun agent -> Agent.vm_name agent <> proxy_vm_name)
  | `Host | `Cloud | `Localhost -> t.agents

let get_configuration = Agent.configuration

let set_agent_name t agent name =
  Agent.set_name agent name ;
  let* () =
    match t.website with
    | None -> Lwt.return_unit
    | Some website -> Web.write website ~agents:t.agents
  in
  match t.prometheus with
  | None -> Lwt.return_unit
  | Some prometheus -> Prometheus.reload prometheus

let push_metric t ?labels ~name value =
  match t.website with
  | None -> ()
  | Some website -> Web.push_metric website ?labels ~name value

type target = {agent : Agent.t; port : int; app_name : string}

let add_prometheus_source t ?metric_path ~job_name targets =
  match t.prometheus with
  | None -> Lwt.return_unit
  | Some prometheus ->
      let prometheus_target {agent; port; app_name} =
        let address = agent |> Agent.runner |> Runner.address in
        Prometheus.{address; port; app_name}
      in
      let targets = List.map prometheus_target targets in
      Prometheus.add_source prometheus ?metric_path ~job_name targets
