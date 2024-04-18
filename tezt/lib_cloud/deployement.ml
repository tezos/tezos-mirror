(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Remote = struct
  type t = {
    base_port : int;
    ports_per_vm : int;
    next_port : (string * int, int) Hashtbl.t;
    names : (string, string) Hashtbl.t;
    zone : string;
  }

  let get_points base_port =
    let* addresses = Terraform.VM.points () in
    List.map (fun address -> (address, base_port)) addresses |> Lwt.return

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

  let deploy ?(base_port = 30_000) ?(ports_per_vm = 50) ~number_of_vms () =
    let tezt_cloud = Lazy.force Env.tezt_cloud in
    let docker_registry = Format.asprintf "%s-docker-registry" tezt_cloud in
    let machine_type = Cli.machine_type in
    let* () = Terraform.Docker_registry.init () in
    let* () = Terraform.VM.Workspace.init [tezt_cloud] in
    let* () = Terraform.VM.Workspace.select tezt_cloud in
    let* () = Terraform.VM.init () in
    let* () =
      Terraform.VM.deploy
        ~machine_type
        ~base_port
        ~ports_per_vm
        ~number_of_vms
        ~docker_registry
    in
    let names =
      Seq.ints 1 |> Seq.take number_of_vms
      |> Seq.map (fun i -> Format.asprintf "%s-%03d" tezt_cloud i)
      |> List.of_seq
    in
    let* zone = Terraform.VM.zone () in
    let* () =
      List.map (fun vm_name -> wait_docker_running ~zone ~vm_name) names
      |> Lwt.join
    in
    let next_port = Hashtbl.create number_of_vms in
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
    let* points = get_points base_port in
    List.iter
      (fun point -> Hashtbl.replace next_port point (base_port + 1))
      points ;
    let* ips =
      names
      |> Lwt_list.map_p (fun name -> Gcloud.get_ip_address_from_name ~zone name)
    in
    let names = List.combine names ips |> List.to_seq |> Hashtbl.of_seq in
    Lwt.return {base_port; ports_per_vm; next_port; names; zone}

  let run_command {names; zone; _} ~address cmd args =
    let vm_name = Hashtbl.find names address in
    Gcloud.compute_ssh ~zone ~vm_name cmd args

  let get_points t = get_points t.base_port

  let next_port t point =
    let port = Hashtbl.find t.next_port point in
    Hashtbl.replace t.next_port point (port + 1) ;
    port

  let terminate ?exn _t =
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
      let* () = Terraform.VM.destroy () in
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
    next_port : (string * int, int) Hashtbl.t;
    names : (string, string) Hashtbl.t;
  }

  let deploy ?(base_port = 30_000) ?(ports_per_vm = 50) ~number_of_vms () =
    (* We need to intialize the docker registry even on localhost to fetch the
       docker image. *)
    let* () = Terraform.Docker_registry.init () in
    let* docker_registry = Terraform.Docker_registry.get_docker_registry () in
    let tezt_cloud = Lazy.force Env.tezt_cloud in
    let image_name =
      Format.asprintf "%s/%s:%s" docker_registry tezt_cloud "latest"
    in
    let names = Hashtbl.create 11 in
    let processes =
      Seq.ints 0 |> Seq.take number_of_vms
      |> Seq.map (fun i ->
             let name = Format.asprintf "%s-%03d" tezt_cloud (i + 1) in
             let start = base_port + (i * ports_per_vm) |> string_of_int in
             let stop =
               base_port + ((i + 1) * ports_per_vm) - 1 |> string_of_int
             in
             let publish_ports = (start, stop, start, stop) in
             let*? process =
               Docker.run
                 ~rm:true
                 ~name
                 ~network:"host"
                 ~publish_ports
                 image_name
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
    Lwt.return
      {number_of_vms; processes; base_port; ports_per_vm; next_port; names}

  let run_command _t ~address:_ cmd args =
    let value : Process.t = Process.spawn cmd args in
    let run process = Process.check_and_read_stdout process in
    {value; run}

  let get_points {number_of_vms; base_port; ports_per_vm; _} =
    Seq.ints 0 |> Seq.take number_of_vms
    |> Seq.map (fun i ->
           let port = base_port + (i * ports_per_vm) in
           ("localhost", port))
    |> List.of_seq |> Lwt.return

  let next_port t point =
    let port = Hashtbl.find t.next_port point in
    Hashtbl.replace t.next_port point (port + 1) ;
    port

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

let deploy ?(base_port = 30_000) ?(ports_per_vm = 50) ~number_of_vms ~localhost
    () =
  if localhost then
    let* localhost =
      Localhost.deploy ~base_port ~ports_per_vm ~number_of_vms ()
    in
    Lwt.return (Localhost localhost)
  else
    let* remote = Remote.deploy ~base_port ~ports_per_vm ~number_of_vms () in
    Lwt.return (Remote remote)

let run_command t ~address cmd args =
  match t with
  | Remote remote -> Remote.run_command remote ~address cmd args
  | Localhost localhost -> Localhost.run_command localhost ~address cmd args

let get_points t =
  match t with
  | Remote remote -> Remote.get_points remote
  | Localhost localhost -> Localhost.get_points localhost

let next_port t point =
  match t with
  | Remote remote -> Remote.next_port remote point
  | Localhost localhost -> Localhost.next_port localhost point

let terminate ?exn t =
  match t with
  | Remote remote -> Remote.terminate ?exn remote
  | Localhost localhost -> Localhost.terminate ?exn localhost
