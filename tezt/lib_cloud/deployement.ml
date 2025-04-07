(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Infrastructure to deploy on Google Cloud *)
module Remote = struct
  type point_info = {workspace_name : string; gcp_name : string}

  type t = {agents : Agent.t list}

  let wait_docker_running ~vm_name () =
    let ssh_private_key_filename = Env.ssh_private_key_filename () in
    let* zone = Env.zone () in
    let is_ready _output = true in
    let run () =
      (* Try to get the docker images up. *)
      Gcloud.compute_ssh
        ~zone
        ~vm_name
        ~ssh_private_key_filename
        "docker"
        ["ps"; "--format"; "{{.Names}}"]
    in
    let* output = Env.wait_process ~is_ready ~run () in
    let images_name =
      output |> String.split_on_char '\n' |> List.filter (fun s -> s <> "")
    in
    let is_ready output = String.trim output |> Stdlib.bool_of_string in
    let run image_name () =
      (* Try to get the docker images that are actually running. *)
      Gcloud.compute_ssh
        ~zone
        ~vm_name
        ~ssh_private_key_filename
        "docker"
        ["inspect"; "--format"; "{{.State.Running}}"; image_name]
    in
    let* _ =
      images_name
      |> List.map (fun image_name ->
             Env.wait_process ~is_ready ~run:(run image_name) ())
      |> Lwt.all
    in
    Lwt.return_unit

  let workspace_deploy ~workspace_name ~number_of_vms ~vm_configuration
      ~configurations =
    let* () = Terraform.VM.Workspace.select workspace_name in
    let* docker_image =
      Agent.Configuration.uri_of_docker_image
        vm_configuration.Agent.Configuration.docker_image
    in
    let machine_type = vm_configuration.machine_type in
    let max_run_duration = vm_configuration.max_run_duration in
    let ports_per_vm = Env.ports_per_vm in
    let base_port = Env.vm_base_port in
    let os = vm_configuration.os in
    let auto_approve = Env.auto_approve in
    let prometheus_port = Env.prometheus_port in
    let* () =
      Terraform.VM.deploy
        ~auto_approve
        ~max_run_duration
        ~machine_type
        ~base_port
        ~ports_per_vm
        ~number_of_vms
        ~docker_image
        ~prometheus_port
        ~os
    in
    let names =
      List.init number_of_vms (fun i ->
          Format.asprintf "%s-%03d" workspace_name (i + 1))
    in
    let* zone = Env.zone () in
    let* () =
      if vm_configuration.os = Cos then
        List.map (fun vm_name -> wait_docker_running ~vm_name ()) names
        |> Lwt.join
      else Lwt.return_unit
    in
    let ssh_private_key_filename = Env.ssh_private_key_filename () in
    let make_agent (vm_name, configuration) =
      let* ip = Gcloud.get_ip_address_from_name ~zone vm_name in
      let point = (ip, base_port) in
      let next_available_port =
        let port = ref base_port in
        fun () ->
          incr port ;
          !port
      in
      let cmd_wrapper =
        Gcloud.cmd_wrapper ~zone ~ssh_private_key_filename ~vm_name
      in
      let* () =
        if Env.monitoring then Monitoring.run ~cmd_wrapper ()
        else Lwt.return_unit
      in
      let process_monitor =
        if Env.process_monitoring then
          Some (Process_monitor.init ~listening_port:(next_available_port ()))
        else None
      in
      Agent.make
        ~ssh_id:ssh_private_key_filename
        ~zone
        ~point
        ~configuration
        ~next_available_port
        ~name:configuration.name
        ~process_monitor
        ()
      |> Lwt.return
    in
    let* agents =
      List.combine names configurations |> Lwt_list.map_p make_agent
    in
    Lwt.return agents

  let order_agents agents configurations =
    let bindings =
      agents
      |> List.map (fun agent ->
             let configuration = Agent.configuration agent in
             (configuration, agent))
    in
    let rec order configurations bindings =
      match configurations with
      | [] -> []
      | configuration :: configurations ->
          let agent = List.assoc configuration bindings in
          let bindings = List.remove_assoc configuration bindings in
          agent :: order configurations bindings
    in
    order configurations bindings

  let deploy_proxy () =
    let workspace_name = Format.asprintf "%s-proxy" Env.tezt_cloud in
    let configuration = Proxy.make_config () in
    let tezt_cloud = Env.tezt_cloud in
    let* () = Terraform.VM.Workspace.init ~tezt_cloud [workspace_name] in
    let* agents =
      workspace_deploy
        ~workspace_name
        ~vm_configuration:configuration.Agent.Configuration.vm
        ~configurations:[configuration]
        ~number_of_vms:1
    in
    match agents with [agent] -> Lwt.return agent | _ -> assert false

  (* Register the specified domain in an appropriate GCP zone *)
  let dns_add_record agent domain =
    let* res = Gcloud.DNS.find_zone_for_subdomain domain in
    match res with
    | None ->
        let () =
          Log.report
            ~color:Log.Color.FG.yellow
            "The domain '%s' is not a subdomain of an authorized GCP zone. \
             Skipping."
            domain
        in
        Lwt.return_unit
    | Some (zone, _) ->
        let* ip = Gcloud.DNS.get_value ~zone ~domain in
        let* () =
          match ip with
          | None -> Lwt.return_unit
          | Some ip -> Gcloud.DNS.remove_subdomain ~zone ~name:domain ~value:ip
        in
        let ip = Agent.point agent |> Option.get |> fst in
        let* () = Gcloud.DNS.add_subdomain ~zone ~name:domain ~value:ip in
        let () =
          Log.report
            ~color:Log.Color.FG.green
            "DNS registrered successfully: '%s'"
            domain
        in
        Lwt.return_unit

  (*
      Deployment requires to create new VMs and organizing them per group of
      configuration. Each configuration leads to one terraform workspace.
    *)
  let deploy ~proxy ~configurations =
    let agents_info = Hashtbl.create 11 in
    let workspaces_info =
      (* VMs are grouped per group of configuration. Each group leads to one workspace. *)
      List.to_seq configurations
      |> Seq.group (fun Agent.Configuration.{vm; name = _} configuration ->
             vm = configuration.vm)
      |> Seq.mapi (fun i seq ->
             let Agent.Configuration.{vm = vm_configuration; _} =
               Seq.uncons seq |> Option.get |> fst
             in
             let workspace_name = Format.asprintf "%s-%d" Env.tezt_cloud i in
             ( workspace_name,
               (vm_configuration, List.of_seq seq, Seq.length seq) ))
    in
    let* () = Terraform.Docker_registry.init () in
    let* () = Terraform.VM.init () in
    let workspaces_names = workspaces_info |> Seq.map fst |> List.of_seq in
    let tezt_cloud = Env.tezt_cloud in
    let* () = Terraform.VM.Workspace.init ~tezt_cloud workspaces_names in
    let* agents =
      workspaces_info |> List.of_seq
      |> Lwt_list.map_s
           (fun
             (workspace_name, (vm_configuration, configurations, number_of_vms))
           ->
             let* () =
               Jobs.docker_build
                 ~docker_image:vm_configuration.Agent.Configuration.docker_image
                 ~push:Env.push_docker
                 ()
             in
             let* () = Terraform.VM.Workspace.select workspace_name in
             let* () = Terraform.VM.init () in
             let* agents =
               workspace_deploy
                 ~workspace_name
                 ~number_of_vms
                 ~vm_configuration
                 ~configurations
             in
             agents
             |> List.iter (fun agent ->
                    (* We index the table per address to identify uniquely the agent. *)
                    let address = agent |> Agent.runner |> Runner.address in
                    Hashtbl.add
                      agents_info
                      address
                      {workspace_name; gcp_name = Agent.name agent}) ;
             Lwt.return agents)
    in
    let agents =
      (* We want to ensure agents are given in the same order than the
         configurations. *)
      order_agents (List.concat agents) configurations
    in
    let* t =
      if proxy then
        let* agent = deploy_proxy () in
        let* domains = Env.dns_domains () in
        let* () =
          Lwt_list.iter_s
            (fun domainname -> dns_add_record agent domainname)
            domains
        in
        Lwt.return {agents = agent :: agents}
      else Lwt.return {agents}
    in
    Lwt.return t

  let agents t = t.agents

  let terminate ?exn _t =
    (match exn with
    | None ->
        Log.report ~color:Log.Color.FG.green "Scenario ended successfully."
    | Some exn ->
        Log.report
          ~color:Log.Color.FG.red
          "Scenario terminated unexpectedly:'%s'"
          (Printexc.to_string exn)) ;
    if Env.destroy then (
      Log.report "Destroying VMs, this may take a while..." ;
      let tezt_cloud = Env.tezt_cloud in
      let* workspaces = Terraform.VM.Workspace.list ~tezt_cloud in
      let* project_id = Gcloud.project_id () in
      let* () = Terraform.VM.destroy workspaces ~project_id in
      Terraform.VM.Workspace.destroy ~tezt_cloud)
    else (
      Log.report
        "No VM destroyed! Don't forget to destroy them when you are done with \
         your experiments" ;
      Lwt.return_unit)
end

(* Infrastructure to deploy locally using Docker *)
module Localhost = struct
  type t = {
    number_of_vms : int;
    processes : Process.t list;
    base_port : int;
    ports_per_vm : int;
    agents : Agent.t list;
  }

  let container_name configuration =
    Format.asprintf
      "teztcloud-%s-%s"
      Env.tezt_cloud
      configuration.Agent.Configuration.name

  let deploy ~configurations () =
    let number_of_vms = List.length configurations in
    let base_port = Env.vm_base_port in
    let ports_per_vm = Env.ports_per_vm in
    let docker_network = Env.tezt_cloud ^ "-net" in
    let* network =
      if Env.docker_host_network then Lwt.return "host"
      else
        let* () =
          Docker.network ~command:"create" ~network_name:docker_network
          |> Process.check
        in
        Lwt.return docker_network
    in
    let* processes =
      List.to_seq configurations
      |> Seq.mapi (fun i configuration ->
             let start = base_port + (i * ports_per_vm) |> string_of_int in
             let stop =
               base_port + ((i + 1) * ports_per_vm) - 1 |> string_of_int
             in
             let publish_ports = (start, stop, start, stop) in
             let* () =
               Jobs.docker_build
                 ~docker_image:configuration.Agent.Configuration.vm.docker_image
                 ~push:false
                 ()
             in
             let* docker_image =
               Agent.Configuration.uri_of_docker_image
                 configuration.vm.docker_image
             in
             let process =
               Docker.run
                 ~rm:true
                 ~name:(container_name configuration)
                 ~network
                 ~publish_ports
                 docker_image
                 ["-D"; "-p"; start; "-e"]
             in
             Lwt.return process)
      |> List.of_seq |> Lwt.all
    in
    let address configuration =
      if Env.docker_host_network then Lwt.return "127.0.0.1"
      else
        let* output =
          Process.run_and_read_stdout
            "docker"
            [
              "inspect";
              "-f";
              "{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}";
              container_name configuration;
            ]
        in
        Lwt.return (String.trim output)
    in
    (* We need to wait a little the machine is up. As for the remote case, we
       could be more robust to handle that. *)
    let* () = Lwt_unix.sleep 5. in
    let addresses_table = Hashtbl.create number_of_vms in
    let ports_table = Hashtbl.create number_of_vms in
    let* () =
      Lwt_list.iteri_s
        (fun i configuration ->
          let* addr = address configuration in
          let () = Hashtbl.replace addresses_table i addr in
          let port = base_port + (i * ports_per_vm) in
          let () = Hashtbl.replace ports_table (addr, port) (port + 1) in
          Lwt.return_unit)
        configurations
    in
    let ssh_id = Env.ssh_private_key_filename () in
    let get_point i =
      let port = base_port + (i * ports_per_vm) in
      let addr = Hashtbl.find addresses_table i in
      (addr, port)
    in
    let next_port point =
      let port = Hashtbl.find ports_table point in
      Hashtbl.replace ports_table point (port + 1) ;
      port
    in
    let* () = if Env.monitoring then Monitoring.run () else Lwt.return_unit in
    let agents =
      configurations
      |> List.mapi (fun i configuration ->
             let point = get_point i in
             let process_monitor =
               if Env.process_monitoring then
                 Some (Process_monitor.init ~listening_port:(next_port point))
               else None
             in
             Agent.make
               ~ssh_id
               ~point
               ~configuration
               ~next_available_port:(fun () -> next_port point)
               ~name:configuration.Agent.Configuration.name
               ~process_monitor
               ())
    in
    Lwt.return {number_of_vms; processes; base_port; ports_per_vm; agents}

  let agents t = t.agents

  let terminate ?exn t =
    (match exn with
    | None ->
        Log.report ~color:Log.Color.FG.green "Scenario ended successfully."
    | Some exn ->
        Log.report
          ~color:Log.Color.FG.red
          "Scenario terminated unexpectedly:'%s'"
          (Printexc.to_string exn)) ;
    Log.report "Terminate test: tearing down docker containers..." ;
    let* () =
      t.agents
      |> List.map (fun agent ->
             let* _ =
               Docker.kill (container_name (Agent.configuration agent))
               |> Process.wait
             in
             Lwt.return_unit)
      |> Lwt.join
    in
    if Env.monitoring then
      let* () = Docker.kill "netdata" |> Process.check in
      Lwt.return_unit
    else Lwt.return_unit
end

type t = Remote of Remote.t | Localhost of Localhost.t

let deploy ~configurations =
  match Env.mode with
  | `Localhost ->
      let* localhost = Localhost.deploy ~configurations () in
      Lwt.return (Localhost localhost)
  | `Cloud ->
      let* remote = Remote.deploy ~proxy:false ~configurations in
      Lwt.return (Remote remote)
  | `Host ->
      let* remote = Remote.deploy ~proxy:true ~configurations in
      Lwt.return (Remote remote)
  | `Orchestrator -> assert false

let agents t =
  match t with
  | Remote remote -> Remote.agents remote
  | Localhost localhost -> Localhost.agents localhost

let terminate ?exn t =
  match t with
  | Remote remote -> Remote.terminate ?exn remote
  | Localhost localhost -> Localhost.terminate ?exn localhost

let of_agents agents = Remote {agents}
