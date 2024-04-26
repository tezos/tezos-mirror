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
  deployement : Deployement.t option;
}

let shutdown ?exn t =
  let* () =
    if Cli.keep_alive then (
      Log.info "Please press <enter> to terminate the scenario." ;
      let* _ = Lwt_io.read_line Lwt_io.stdin in
      Lwt.return_unit)
    else Lwt.return_unit
  in
  Log.info "Shutting down processes..." ;
  let* () = Process.clean_up () in
  let* () =
    Option.fold
      ~none:Lwt.return_unit
      ~some:Prometheus.export_snapshot
      t.prometheus
  in
  let* () =
    Option.fold ~none:Lwt.return_unit ~some:Prometheus.shutdown t.prometheus
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
let rec wait_ssh_server_running runner =
  let cmd, args =
    Runner.wrap_with_ssh runner (Runner.Shell.cmd [] "echo" ["-n"; "check"])
  in
  let* status =
    Process.spawn cmd (["-o"; "StrictHostKeyChecking=no"] @ args)
    |> Process.wait
  in
  match status with
  | Unix.WEXITED 0 -> Lwt.return_unit
  | _ ->
      Log.info
        "SSH server is not running, let's wait 2 seconds and check again..." ;
      let* () = Lwt_unix.sleep 2. in
      wait_ssh_server_running runner

let register ?vms ~__FILE__ ~title ~tags ?seed f =
  Test.register ~__FILE__ ~title ~tags ?seed @@ fun () ->
  let vms =
    (* The Cli arguments by-pass the argument given here. This enable the user
       to always have decide precisely the number of vms to be run. *)
    match (vms, Cli.vms) with
    | None, None -> None
    | None, Some i | Some _, Some i ->
        let vms =
          List.init i (fun _ -> Deployement.{machine_type = Cli.machine_type})
        in
        Some vms
    | Some vms, None -> Some vms
  in
  match vms with
  | None ->
      (* If there is no configuration, it is a similar scenario as if there were not agent. *)
      f {agents = []; website = None; prometheus = None; deployement = None}
  | Some configurations ->
      let ports_per_vm = Cli.ports_per_vm in
      let* deployement =
        Deployement.deploy
          ~ports_per_vm
          ~configurations
          ~localhost:Cli.localhost
          ()
      in
      let agents = Deployement.agents deployement in
      let* () =
        agents
        |> List.map (fun agent -> Agent.runner agent |> wait_ssh_server_running)
        |> Lwt.join
      in
      if Cli.monitoring then (
        Log.info "Monitoring enabled" ;
        agents
        |> List.iter (fun agent ->
               let address =
                 Agent.runner agent |> fun runner ->
                 Runner.address (Some runner)
               in
               Log.info
                 "Monitoring of agent %s can be accessed at 'http://%s:19999'"
                 (Agent.name agent)
                 address)) ;
      let* website =
        if Cli.website then
          let* website =
            Web.start ~port:Cli.website_port ~deployement ~agents
          in
          Lwt.return_some website
        else Lwt.return_none
      in
      let* prometheus =
        if Cli.prometheus then
          let* prometheus = Prometheus.start agents in
          Lwt.return_some prometheus
        else Lwt.return_none
      in
      let t = {website; agents; prometheus; deployement = Some deployement} in
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

let agents t = t.agents

type vm_configuration = Deployement.configuration = {machine_type : string}

let default_vm_configuration = {machine_type = Cli.machine_type}

let get_configuration t agent =
  match t.deployement with
  | None -> default_vm_configuration
  | Some deployement -> Deployement.get_configuration deployement agent

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
        let address = agent |> Agent.runner |> Option.some |> Runner.address in
        Prometheus.{address; port; app_name}
      in
      let targets = List.map prometheus_target targets in
      Prometheus.add_source prometheus ?metric_path ~job_name targets
