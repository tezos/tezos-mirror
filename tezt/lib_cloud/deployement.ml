(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let dns_set_subdomain agent domain =
  let agent_ip = Agent.point agent |> Option.get |> fst in
  Gcloud.DNS.set_subdomain ~agent_ip ~domain

let merge_dockerbuild_args global_dockerbuild_args vm_configuration =
  let args = vm_configuration.Agent.Configuration.dockerbuild_args in
  let global_args =
    List.filter
      (fun (arg, _) -> not (List.mem_assoc arg args))
      global_dockerbuild_args
  in
  global_args @ args

(* Infrastructure to deploy on Google Cloud *)
module Remote = struct
  type point_info = {workspace_name : string; gcp_name : string}

  type t = {agents : Agent.t list}

  let rec wait_docker_running ~vm_name () =
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
    Lwt.catch
      (fun () ->
        let* (_ : string list) =
          images_name
          |> Lwt_list.map_p (fun image_name ->
                 Env.wait_process
                   ~is_ready
                   ~run:(run image_name)
                   ~propagate_error:true
                   ())
        in
        unit)
      (fun _ ->
        (* If [Env.wait_process] failed, this is presumably because the docker image
           we are trying to inspect does not exist anymore.
           Hence, we relaunch the function from the start to perform [docker ps] again. *)
        wait_docker_running ~vm_name ())

  let workspace_deploy ~workspace_name ~number_of_vms ~vm_configuration
      ~configurations =
    let* () = Terraform.VM.Workspace.select workspace_name in
    let* docker_image =
      Agent.Configuration.uri_of_docker_image
        vm_configuration.Agent.Configuration.docker_image
    in
    let {
      Agent.Configuration.machine_type;
      disk_type;
      disk_size_gb;
      max_run_duration;
      os;
      _;
    } =
      vm_configuration
    in
    let ports_per_vm = Env.ports_per_vm in
    let base_port = Env.vm_base_port in
    let auto_approve = Env.auto_approve in
    let prometheus_port = Env.prometheus_port in
    let artifacts_dir = Env.artifacts_dir in
    let* () =
      Terraform.VM.deploy
        ~auto_approve
        ~max_run_duration
        ~machine_type
        ~disk_type
        ~disk_size_gb
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
        ~vm_name:(Some vm_name)
        ~process_monitor
        ~artifacts_dir
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

  (*
      Deployment requires to create new VMs and organizing them per group of
      configuration. Each configuration leads to one terraform workspace.
    *)
  let deploy ~dockerbuild_args ~proxy ~configurations =
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
    let* () = Terraform.VM.Workspace.select "default" in
    let workspaces_names = workspaces_info |> Seq.map fst |> List.of_seq in
    let tezt_cloud = Env.tezt_cloud in
    let* () = Terraform.VM.Workspace.init ~tezt_cloud workspaces_names in
    let* agents =
      workspaces_info |> List.of_seq
      |> Lwt_list.map_s
           (fun
             (workspace_name, (vm_configuration, configurations, number_of_vms))
           ->
             let* ssh_public_key = Ssh.public_key () in
             let args =
               merge_dockerbuild_args dockerbuild_args vm_configuration
             in
             let* () =
               Jobs.docker_build
                 ~docker_image:vm_configuration.Agent.Configuration.docker_image
                 ~args
                 ~push:Env.push_docker
                 ~ssh_public_key
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
                      {
                        workspace_name;
                        gcp_name = Option.get (Agent.vm_name agent);
                      }) ;
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
            (fun domainname -> dns_set_subdomain agent domainname)
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

(* Deployment on a SSH reachable host. At this moment, it is expected to be a
   debian bookworm, and apt must be executable by the user connecting with sudo.
   At this moment, this deployment launches a proxy mode docker. *)
module Ssh_host = struct
  type t = {point : string * string * int; agents : Agent.t list}

  (* This function allows to setup the prerequisites to run the deployment
     of containers on a vm which can be connected to via ssh.
     Multiple hosts are currently supported:
       - a gcp debian vm
       - a qemu vm
       - a physical machine.
     All that is required is that the host can be contacted either via root
     account or a sudo enabled account. *)
  let initial_host_provisionning user host ?ssh_id port =
    let ssh_id =
      Option.value ~default:(Env.ssh_private_key_filename ()) ssh_id
    in
    let runner =
      Runner.create ~ssh_user:user ~ssh_port:port ~address:host ~ssh_id ()
    in
    let* () =
      (* Allows direct connections as root on debian, using key authentication.
         This is not unsecure (as key logging is secure) as long as users
         logging in as root are careful. This allows to not be embarrassed with sudo *)
      if user = "root" then Lwt.return_unit
      else
        (* TODO? might check with ssh-add that the provided key is loaded in agent *)
        let* _ =
          Process.spawn
            ~runner
            "sudo"
            [
              "sed";
              "-i";
              "s/PermitRootLogin no/PermitRootLogin prohibit-password/";
              "/etc/ssh/sshd_config";
            ]
          |> Process.wait
        in
        with_open_in (Env.ssh_public_key_filename ()) @@ fun fd ->
        let ssh_public_key_content = input_line fd in
        let* () =
          Process.spawn ~runner "sudo" ["mkdir"; "-p"; "/root/.ssh"]
          |> Process.check
        in
        let* () =
          Process.spawn
            ~runner
            "sh"
            [
              "-c";
              Format.asprintf
                "echo %s | sudo tee -a /root/.ssh/authorized_keys"
                ssh_public_key_content;
            ]
          |> Process.check
        in
        Process.spawn ~runner "sudo" ["systemctl"; "restart"; "ssh.service"]
        |> Process.check
    in
    (* Setup a new runner for root connections *)
    let runner =
      Runner.create
        ~ssh_user:"root"
        ~ssh_port:port
        ~address:host
        ~ssh_id:(Env.ssh_private_key_filename ())
        ()
    in
    let user = "root" in
    (* Installs docker *)
    Log.report "Installing docker" ;
    let* () = Process.spawn ~runner "apt-get" ["update"] |> Process.check in
    let* () =
      Process.spawn ~runner "apt-get" ["install"; "-y"; "docker.io"; "libev4"]
      |> Process.check
    in
    let* project = Env.project_id () in
    (* Generate locally an access token to access to gcp docker registry *)
    let* iam_key_filename =
      let service_account_name = Format.asprintf "%s-id" Env.tezt_cloud in
      let* service_account =
        (* FIXME: service_account_fullname is strictly bound to dal team and should
           be configured and stored in a tezt-cloud ~/.config directory.
           Here, preserving backward compatibility with terraform naming *)
        let service_account_fullname =
          Format.asprintf "%s-id@nl-dal.iam.gserviceaccount.com" Env.tezt_cloud
        in
        (* Delete service account if exists. Do not fail if it does not *)
        let* _ =
          Process.spawn
            "gcloud"
            ["iam"; "service-accounts"; "delete"; service_account_fullname]
          |> Process.wait
        in
        let* () =
          Process.spawn
            "gcloud"
            [
              "iam";
              "service-accounts";
              "create";
              "--project";
              project;
              service_account_name;
              "--display-name";
              service_account_name;
            ]
          |> Process.check
        in
        Lwt.return service_account_fullname
      in
      Log.report "waiting for the iam account to become valid" ;
      let* () = Lwt_unix.sleep 3.0 in
      let iam_key_filename = "/tmp/iam-keys" in
      let* () =
        Process.spawn
          "gcloud"
          [
            "iam";
            "service-accounts";
            "keys";
            "create";
            iam_key_filename;
            "--iam-account";
            service_account;
            "--project";
            project;
          ]
        |> Process.check
      in
      Log.report "waiting for the iam key to become valid" ;
      let* () = Lwt_unix.sleep 3.0 in
      let* () =
        Process.spawn
          "gcloud"
          [
            "artifacts";
            "repositories";
            "add-iam-policy-binding";
            Format.asprintf "%s-docker-registry" Env.tezt_cloud;
            "--location";
            "europe-west1";
            "--member";
            Format.asprintf "serviceAccount:%s" service_account;
            "--role";
            "roles/artifactregistry.reader";
          ]
        |> Process.check
      in
      Log.report "waiting for the iam key binding to become valid" ;
      let* () = Lwt_unix.sleep 3.0 in
      Lwt.return iam_key_filename
    in
    (* Upload the key to the host via ssh *)
    let* () =
      Process.run
        "scp"
        (["-i"; Env.ssh_private_key_filename (); iam_key_filename]
        @ (if port <> 22 then ["-p"; string_of_int port] else [])
        @ [Format.asprintf "%s@%s:%s" user host iam_key_filename])
    in
    let* () =
      let rec retry () =
        let* status =
          Process.spawn
            ~runner
            "sh"
            [
              "-c";
              Format.asprintf
                "cat %s | docker login -u _json_key --password-stdin  \
                 https://europe-west1-docker.pkg.dev"
                iam_key_filename;
            ]
          |> Process.wait
        in
        match status with WEXITED 0 -> Lwt.return_unit | _ -> retry ()
      in
      retry ()
    in
    Lwt.return_unit

  let deploy_proxy runner =
    let configuration = Proxy.make_config () in
    let next_available_port =
      let cpt = ref 30_000 in
      fun () ->
        incr cpt ;
        !cpt
    in
    let name = configuration.name in
    let* docker_image =
      Agent.Configuration.uri_of_docker_image configuration.vm.docker_image
    in
    (* Remove any existing proxy first *)
    let* () = Docker.rm ~runner ~force:true name |> Process.check in
    (* Prepare the arguments for docker run *)
    let ssh_listening_port = next_available_port () in
    let guest_port = string_of_int ssh_listening_port in
    let publish_ports = (guest_port, guest_port, guest_port, guest_port) in
    let volumes =
      [
        ("/var/run/docker.sock", "/var/run/docker.sock");
        ("/tmp/prometheus", "/tmp/prometheus");
        ("/tmp/website", "/tmp/website");
        ("/tmp/grafana", "/tmp/grafana");
        ("/tmp/alert_manager", "/tmp/alert_manager");
        ("/tmp/otel", "/tmp/otel");
      ]
    in
    let* registry_uri_opt =
      Agent.Configuration.registry_uri_of_docker_image
        configuration.vm.docker_image
    in
    let image_name =
      Agent.Configuration.docker_image_name configuration.vm.docker_image
    in
    let* () =
      match registry_uri_opt with
      | Some registry_uri ->
          Docker.pull ~runner ~image_name ~registry_uri () |> Process.check
      | None -> return ()
    in
    let* () =
      Docker.run
        ~runner
        ~rm:true
        ~detach:true
        ~network:"host"
        ~custom_docker_options:["--ulimit"; "nofile=65535:65535"]
        ~volumes
        ~publish_ports
        ~name
        docker_image
        ["-D"; "-p"; guest_port]
      |> Process.check
    in
    let agent =
      Agent.make
        ~vm_name:None
        ~configuration
        ~next_available_port
        ~point:(Runner.address (Some runner), ssh_listening_port)
        ~ssh_id:(Env.ssh_private_key_filename ())
        ~process_monitor:None
        ~artifacts_dir:Env.artifacts_dir
        ()
    in
    Lwt.return agent

  let deploy ~user ?ssh_id ~host ~port ~dockerbuild_args
      ~(configurations : Agent.Configuration.t list) () =
    let* () = initial_host_provisionning user ?ssh_id host port in
    (* At this time, we only support deploying with root user.
       The initial provisionning provide ssh root access by keypair.
       TODO: support deploying using sudo and remove the "root" user requirement *)
    let user = "root" in
    let proxy_runner =
      Runner.create
        ~ssh_user:user
        ~ssh_port:port
        ~address:host
        ~ssh_id:(Env.ssh_private_key_filename ())
        ()
    in
    (* Deploys the proxy *)
    let* proxy = deploy_proxy proxy_runner in
    (* Deploys all agents *)
    let* agents =
      Lwt_list.mapi_p
        (fun i (configuration : Agent.Configuration.t) ->
          let* docker_image =
            Agent.Configuration.uri_of_docker_image
              configuration.vm.docker_image
          in
          let build_args =
            merge_dockerbuild_args dockerbuild_args configuration.vm
            |> List.map (fun (k, v) ->
                   ["--build-arg"; Format.asprintf "%s=%s" k v])
            |> List.concat
          in
          let ssh_port = Agent.next_available_port proxy in
          (* FIXME move this constants elsewhere *)
          let base_port = 30_050 in
          let range = 50 in
          let runner =
            Runner.create
              ~ssh_user:user
              ~ssh_port:port
              ~address:host
              ~ssh_id:(Env.ssh_private_key_filename ())
              ()
          in
          let publish_ports =
            ( string_of_int (base_port + (i * range)),
              string_of_int (base_port + ((i + 1) * range) - 1),
              string_of_int (base_port + (i * range)),
              string_of_int (base_port + ((i + 1) * range) - 1) )
          in

          let* registry_uri_opt =
            Agent.Configuration.registry_uri_of_docker_image
              configuration.vm.docker_image
          in
          let image_name =
            Agent.Configuration.docker_image_name configuration.vm.docker_image
          in
          let* () =
            match registry_uri_opt with
            | Some registry_uri ->
                Docker.pull ~runner ~image_name ~registry_uri ()
                |> Process.check
            | None -> return ()
          in
          let* () =
            Docker.run
              ~runner
              ~detach:true
              ~publish_ports
              ~network:"host"
              ~name:configuration.name
              docker_image
              (["-D"; "-p"; string_of_int ssh_port] @ build_args)
            |> Process.check
          in
          let () =
            Log.warn
              "Deployed agent: %s on (%s, %d)"
              configuration.name
              host
              ssh_port
          in
          let agent =
            Agent.make
              ~vm_name:None
              ~configuration
              ~next_available_port:
                (let cpt = ref (base_port + (i * range)) in
                 fun () ->
                   incr cpt ;
                   !cpt)
              ~process_monitor:None
              ~point:(host, ssh_port)
              ~ssh_id:(Env.ssh_private_key_filename ())
              ~artifacts_dir:Env.artifacts_dir
              ()
          in
          Lwt.return agent)
        configurations
    in
    let* domains = Env.dns_domains () in
    let* () =
      Lwt_list.iter_s
        (fun domainname -> dns_set_subdomain proxy domainname)
        domains
    in
    let agents = proxy :: agents in
    Lwt.return {point = (user, host, port); agents}

  let agents t = t.agents

  let terminate {point; agents} =
    let _user, host, port = point in
    let* () =
      Lwt_list.iter_p
        (fun agent ->
          let name = Agent.name agent in
          let runner =
            Runner.create
              ~ssh_port:port
              ~address:host
              ~ssh_id:(Env.ssh_private_key_filename ())
              ()
          in
          let* _ = Docker.rm ~runner name |> Process.check in
          Lwt.return_unit)
        agents
    in
    Lwt.return_unit
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

  let deploy ~configurations ~dockerbuild_args () =
    let number_of_vms = List.length configurations in
    let base_port = Env.vm_base_port in
    let ports_per_vm = Env.ports_per_vm in
    let docker_network = Env.tezt_cloud ^ "-net" in
    let* network =
      if Env.docker_host_network then Lwt.return "host"
      else
        (* Remove existing network if it exists (ignore errors) *)
        let* () =
          Docker.network ~command:"rm" ~network_name:docker_network
          |> Process.wait
          |> Lwt.map (fun _ -> ())
        in
        let* () =
          Docker.network ~command:"create" ~network_name:docker_network
          |> Process.check
        in
        Lwt.return docker_network
    in
    let* ssh_public_key = Ssh.public_key () in
    let* processes =
      List.to_seq configurations
      |> Seq.mapi (fun i configuration ->
             let start = base_port + (i * ports_per_vm) |> string_of_int in
             let stop =
               base_port + ((i + 1) * ports_per_vm) - 1 |> string_of_int
             in
             let publish_ports = (start, stop, start, stop) in
             let args =
               merge_dockerbuild_args
                 dockerbuild_args
                 configuration.Agent.Configuration.vm
             in
             let* () =
               Jobs.docker_build
                 ~docker_image:configuration.Agent.Configuration.vm.docker_image
                 ~args
                 ~push:false
                 ~ssh_public_key
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
      (* On macOS, container IPs are not routable from the host.
         We must use 127.0.0.1 with published ports instead. *)
      if Env.docker_host_network || Env.macosx then Lwt.return "127.0.0.1"
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
               ~vm_name:None
               ~process_monitor
               ~artifacts_dir:Env.artifacts_dir
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

type t =
  | Remote of Remote.t
  | Ssh_host of Ssh_host.t
  | Localhost of Localhost.t

let deploy ?(dockerbuild_args = []) ~configurations () =
  match Env.mode with
  | `Local_orchestrator_local_agents ->
      let* localhost = Localhost.deploy ~dockerbuild_args ~configurations () in
      Lwt.return (Localhost localhost)
  | `Local_orchestrator_remote_agents ->
      let* remote =
        Remote.deploy ~proxy:false ~dockerbuild_args ~configurations
      in
      Lwt.return (Remote remote)
  | `Remote_orchestrator_remote_agents ->
      let* remote =
        Remote.deploy ~proxy:true ~dockerbuild_args ~configurations
      in
      Lwt.return (Remote remote)
  | `Remote_orchestrator_local_agents -> assert false
  | `Ssh_host (user, host, port) ->
      let ssh_id = Env.ssh_private_key_filename () in
      let* host =
        Ssh_host.deploy
          ~user
          ~ssh_id
          ~host
          ~port
          ~dockerbuild_args
          ~configurations
          ()
      in
      Lwt.return (Ssh_host host)

let agents t =
  match t with
  | Remote remote -> Remote.agents remote
  | Localhost localhost -> Localhost.agents localhost
  | Ssh_host remote -> Ssh_host.agents remote

let terminate ?exn t =
  match t with
  | Remote remote -> Remote.terminate ?exn remote
  | Localhost localhost -> Localhost.terminate ?exn localhost
  | Ssh_host remote -> Ssh_host.terminate remote

let of_agents agents = Remote {agents}
