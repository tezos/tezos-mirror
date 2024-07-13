(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Remote = struct
  type workspace_info = {configuration : Configuration.t; number_of_vms : int}

  type point_info = {workspace_name : string; gcp_name : string}

  type address = string

  type t = {
    base_port : int;
    ports_per_vm : int;
    agents_info : (address, point_info) Hashtbl.t;
    agents : Agent.t list;
    workspaces_info : (string, workspace_info) Hashtbl.t;
    zone : string;
  }

  let rec wait_docker_running ~zone ~vm_name =
    let*? process =
      Gcloud.compute_ssh
        ~zone
        ~vm_name
        "docker"
        ["ps"; "--format"; "{{.Names}}"]
    in
    let* status = Process.wait process in
    match status with
    | Unix.WEXITED 0 ->
        let* images_name = Process.check_and_read_stdout process in
        String.split_on_char '\n' images_name
        |> List.filter (fun s -> s <> "")
        |> List.map (fun image_name ->
               let*? process =
                 Gcloud.compute_ssh
                   ~zone
                   ~vm_name
                   "docker"
                   ["inspect"; "--format"; "{{.State.Running}}"; image_name]
               in
               let* status = Process.wait process in
               match status with
               | Unix.WEXITED 0 ->
                   let* is_running =
                     let* status = Process.check_and_read_stdout process in
                     String.trim status |> Stdlib.bool_of_string |> Lwt.return
                   in
                   if is_running then Lwt.return_unit
                   else (
                     Log.info
                       "Docker image is not ready, let's wait 2 seconds and \
                        check again..." ;
                     wait_docker_running ~zone ~vm_name)
               | _ ->
                   Log.info
                     "Docker image is not ready, let's wait 2 seconds and \
                      check again..." ;
                   let* () = Lwt_unix.sleep 2. in
                   wait_docker_running ~zone ~vm_name)
        |> Lwt.join
    | _ ->
        Log.info
          "Docker image is not ready, let's wait 2 seconds and check again..." ;
        let* () = Lwt_unix.sleep 2. in
        wait_docker_running ~zone ~vm_name

  let workspace_deploy ?(base_port = 30_000) ?(ports_per_vm = 50)
      ~max_run_duration ~workspace_name ~machine_type ~number_of_vms
      ~docker_image () =
    let* () = Terraform.VM.Workspace.select workspace_name in
    let* project_id = Gcloud.project_id () in
    let docker_image =
      Configuration.string_of_docker_image ~project_id docker_image
    in
    let* () =
      Terraform.VM.deploy
        ~max_run_duration
        ~machine_type
        ~base_port
        ~ports_per_vm
        ~number_of_vms
        ~docker_image
    in
    let names =
      List.init number_of_vms (fun i ->
          Format.asprintf "%s-%03d" workspace_name (i + 1))
    in
    let* zone = Terraform.VM.zone () in
    let* () =
      List.map (fun vm_name -> wait_docker_running ~zone ~vm_name) names
      |> Lwt.join
    in
    let* () =
      let run_command ~vm_name cmd args =
        Gcloud.compute_ssh ~zone ~vm_name cmd args
      in
      if Cli.monitoring then
        List.map
          (fun vm_name -> Monitoring.run ~run_command:(run_command ~vm_name))
          names
        |> Lwt.join
      else Lwt.return_unit
    in
    let ssh_id = Lazy.force Env.ssh_private_key in
    let binaries_path = Path.default_binaries_path () in
    let agent_of_name name =
      let* ip = Gcloud.get_ip_address_from_name ~zone name in
      let point = (ip, base_port) in
      let next_available_port =
        let port = ref base_port in
        fun () ->
          incr port ;
          !port
      in
      Agent.make ~ssh_id ~point ~binaries_path ~next_available_port ~name ()
      |> Lwt.return
    in
    let* agents = names |> Lwt_list.map_p agent_of_name in
    Lwt.return agents

  let get_configuration agents_info workspaces_info agent =
    let address = agent |> Agent.runner |> Option.some |> Runner.address in
    let {workspace_name; _} = Hashtbl.find agents_info address in
    let {configuration; _} = Hashtbl.find workspaces_info workspace_name in
    configuration

  let order_agents agents_info workspaces_info agents configurations =
    let bindings =
      agents
      |> List.map (fun agent ->
             let configuration =
               get_configuration agents_info workspaces_info agent
             in
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

  let deploy ?(base_port = 30_000) ?(ports_per_vm = 50) ~configurations () =
    let tezt_cloud = Lazy.force Env.tezt_cloud in
    let workspaces_info = Hashtbl.create 11 in
    let agents_info = Hashtbl.create 11 in
    let () =
      List.to_seq configurations |> Seq.group ( = )
      |> Seq.iteri (fun i seq ->
             let configuration = Seq.uncons seq |> Option.get |> fst in
             let name = Format.asprintf "%s-%d" tezt_cloud i in
             Hashtbl.add
               workspaces_info
               name
               {configuration; number_of_vms = Seq.length seq})
    in
    let* () = Terraform.Docker_registry.init () in
    let* () = Terraform.VM.init () in
    let workspaces_names =
      workspaces_info |> Hashtbl.to_seq_keys |> List.of_seq
    in
    let* () = Terraform.VM.Workspace.init workspaces_names in
    let* agents =
      workspaces_info |> Hashtbl.to_seq |> List.of_seq
      |> Lwt_list.map_s
           (fun
             ( workspace_name,
               {
                 configuration = {machine_type; docker_image; max_run_duration};
                 number_of_vms;
               } )
           ->
             let* () = Terraform.VM.Workspace.select workspace_name in
             let* () = Terraform.VM.init () in
             let* agents =
               workspace_deploy
                 ~base_port
                 ~ports_per_vm
                 ~workspace_name
                 ~max_run_duration
                 ~machine_type
                 ~number_of_vms
                 ~docker_image
                 ()
             in
             agents
             |> List.iter (fun agent ->
                    (* We index the table per address to identify uniquely the agent. *)
                    let address =
                      agent |> Agent.runner |> Option.some |> Runner.address
                    in
                    Hashtbl.add
                      agents_info
                      address
                      {workspace_name; gcp_name = Agent.name agent}) ;
             Lwt.return agents)
    in
    let agents =
      order_agents
        agents_info
        workspaces_info
        (List.concat agents)
        configurations
    in
    let* zone = Terraform.VM.zone () in
    Lwt.return
      {base_port; ports_per_vm; zone; agents_info; agents; workspaces_info}

  let get_configuration t agent =
    get_configuration t.agents_info t.workspaces_info agent

  let run_vm_command {agents_info; zone; _} agent cmd args =
    let address = agent |> Agent.runner |> Option.some |> Runner.address in
    let {gcp_name = vm_name; _} = Hashtbl.find agents_info address in
    Gcloud.compute_ssh ~zone ~vm_name cmd args

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
    if Cli.destroy then (
      Log.report "Destroying VMs, this may take a while..." ;
      let workspaces = Hashtbl.to_seq_keys t.workspaces_info |> List.of_seq in
      let* () = Terraform.VM.destroy workspaces in
      Terraform.VM.Workspace.destroy ())
    else (
      Log.report
        "No VM destroyed! Don't forget to destroy them when you are done with \
         your experiments" ;
      Lwt.return_unit)
end

module Localhost = struct
  type t = {
    number_of_vms : int;
    processes : Process.t list;
    base_port : int;
    ports_per_vm : int;
    names : (string, string) Hashtbl.t;
    agents : Agent.t list;
  }

  let deploy ?(base_port = 30_000) ?(ports_per_vm = 50) ~configurations () =
    (* We need to intialize the docker registry even on localhost to fetch the
       docker image. *)
    let* () = Terraform.Docker_registry.init () in
    let* project_id = Gcloud.project_id () in
    let number_of_vms = List.length configurations in
    let tezt_cloud = Lazy.force Env.tezt_cloud in
    let names = Hashtbl.create 11 in
    (* The current configuration is actually unused in localhost. Only the
       number of VMs matters. *)
    let processes =
      List.to_seq configurations
      |> Seq.mapi (fun i configuration ->
             let name = Format.asprintf "%s-%03d" tezt_cloud (i + 1) in
             let start = base_port + (i * ports_per_vm) |> string_of_int in
             let stop =
               base_port + ((i + 1) * ports_per_vm) - 1 |> string_of_int
             in
             let publish_ports = (start, stop, start, stop) in
             let docker_image =
               Configuration.string_of_docker_image
                 ~project_id
                 configuration.Configuration.docker_image
             in
             let*? process =
               Docker.run
                 ~rm:true
                 ~name
                 ~network:"host"
                 ~publish_ports
                 docker_image
                 ["-D"; "-p"; start; "-e"]
             in
             process)
      |> List.of_seq
    in
    (* We need to wait a little the machine is up. As for the remote case, we
       could be more robust to handle that. *)
    let* () = Lwt_unix.sleep 5. in
    let next_port = Hashtbl.create number_of_vms in
    Seq.ints 0 |> Seq.take number_of_vms
    |> Seq.iter (fun i ->
           let port = base_port + (i * ports_per_vm) in
           Hashtbl.replace next_port ("localhost", port) (port + 1)) ;
    let* () =
      let run_command cmd args =
        let value : Process.t = Process.spawn cmd args in
        let run process = Process.check_and_read_stdout process in
        {value; run}
      in
      if Cli.monitoring then Monitoring.run ~run_command else Lwt.return_unit
    in
    let ssh_id = Lazy.force Env.ssh_private_key in
    let get_point i =
      let port = base_port + (i * ports_per_vm) in
      ("localhost", port)
    in
    let next_port point =
      let port = Hashtbl.find next_port point in
      Hashtbl.replace next_port point (port + 1) ;
      port
    in
    let binaries_path = Path.default_binaries_path () in
    let agents =
      List.init number_of_vms (fun i ->
          let name = Format.asprintf "localhost_docker_%d" i in
          let point = get_point i in
          Agent.make
            ~ssh_id
            ~point
            ~binaries_path
            ~next_available_port:(fun () -> next_port point)
            ~name
            ())
    in
    Lwt.return
      {number_of_vms; processes; base_port; ports_per_vm; names; agents}

  (* Since in [localhost] mode, the VM is the host machine, this comes back
     to just run a command on the host machine. *)
  let run_vm_command cmd args =
    let value : Process.t = Process.spawn cmd args in
    let run process = Process.check_and_read_stdout process in
    {value; run}

  let agents t = t.agents

  let get_configuration _t _agent =
    (* The configuration is not used in localhost. *)
    Configuration.make ()

  let terminate ?exn _t =
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
      (* The netdata docker is run in the background. It must be shut
         explicitely. *)
      if Cli.monitoring then
        let*! () = Docker.kill "netdata" in
        let*! () = Docker.rm "netdata" in
        Lwt.return_unit
      else Lwt.return_unit
    in
    Lwt.return_unit
end

type t = Remote of Remote.t | Localhost of Localhost.t

let deploy ?(base_port = 30_000) ?(ports_per_vm = 50) ~configurations ~localhost
    () =
  if localhost then
    let* localhost =
      Localhost.deploy ~base_port ~ports_per_vm ~configurations ()
    in
    Lwt.return (Localhost localhost)
  else
    let* remote = Remote.deploy ~base_port ~ports_per_vm ~configurations () in
    Lwt.return (Remote remote)

let run_vm_command t agent cmd args =
  match t with
  | Remote remote -> Remote.run_vm_command remote agent cmd args
  | Localhost _localhost -> Localhost.run_vm_command cmd args

let agents t =
  match t with
  | Remote remote -> Remote.agents remote
  | Localhost localhost -> Localhost.agents localhost

let get_configuration t =
  match t with
  | Remote remote -> Remote.get_configuration remote
  | Localhost localhost -> Localhost.get_configuration localhost

let terminate ?exn t =
  match t with
  | Remote remote -> Remote.terminate ?exn remote
  | Localhost localhost -> Localhost.terminate ?exn localhost
