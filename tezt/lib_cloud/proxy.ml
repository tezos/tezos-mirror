(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let agent_name = Format.asprintf "%s-proxy" Env.tezt_cloud

let make_config () = Agent.Configuration.make ~name:agent_name ()

let get_agent agents =
  match List.find_opt (fun agent -> Agent.name agent = agent_name) agents with
  | None -> Test.fail ~__LOC__ "Cannot find agent %s" agent_name
  | Some agent -> agent

let copy_files proxy_agent ~scenario_files ~proxy_deployement =
  (* This file is necessary to get the agents configurations. *)
  let* _ =
    Agent.copy
      proxy_agent
      ~source:proxy_deployement
      ~destination:proxy_deployement
  in
  (* Copying the ssh key is necessary for the proxy agent to connect with the
     other VMs. *)
  let ssh_public_key_filename = Env.ssh_public_key_filename () in
  let ssh_private_key_filename = Env.ssh_private_key_filename () in
  let* _ =
    Agent.copy
      proxy_agent
      ~source:ssh_public_key_filename
      ~destination:
        ("/root" // ".ssh" // Filename.basename ssh_public_key_filename)
  in
  let* _ =
    Agent.copy
      proxy_agent
      ~source:ssh_private_key_filename
      ~destination:
        ("/root" // ".ssh" // Filename.basename ssh_private_key_filename)
  in
  let* _ =
    Agent.copy
      proxy_agent
      ~source:Path.website_index
      ~destination:("/root" // Path.website_index)
  in
  let* _ =
    Agent.copy
      proxy_agent
      ~source:Path.website_style
      ~destination:("/root" // Path.website_style)
  in
  let* _ =
    Agent.copy
      proxy_agent
      ~source:Path.prometheus_configuration
      ~destination:("/root" // Path.prometheus_configuration)
  in
  let* _ =
    Agent.copy
      proxy_agent
      ~source:Path.prometheus_rules_configuration
      ~destination:("/root" // Path.prometheus_rules_configuration)
  in
  let* _ =
    Agent.copy
      proxy_agent
      ~source:Path.alert_manager_configuration
      ~destination:("/root" // Path.alert_manager_configuration)
  in
  (* If the Proxy agent uses grafana, it needs some dashboards. We copy them to
     the proxy VM and then import them.

     Another way to do it, would be to push them from the host machine to the
     grafana instance directly. This is doable and slightly better in theory.
  *)
  let* _ =
    if Env.grafana then
      let* grafana_dashboards = Grafana.dashboards_filepaths () in
      grafana_dashboards
      |> List.map (fun dashboard ->
             let* _ =
               Agent.copy
                 proxy_agent
                 ~source:dashboard
                 ~destination:
                   (Filename.get_temp_dir_name ()
                   // "grafana" // "dashboards"
                   // Filename.basename dashboard)
             in
             Lwt.return_unit)
      |> Lwt.join
    else Lwt.return_unit
  in
  (* The proxy agent requires the daemons to be in the current working
     directory. We add symbolink links to all the known binaries.

     This requires the docker image to contain all the binaries used by the
     proxy agent.
  *)
  let Agent.Configuration.{vm = {binaries_path; _}; name = _} =
    Agent.configuration proxy_agent
  in
  let* output =
    Process.spawn ?runner:(Agent.runner proxy_agent) "ls" [binaries_path]
    |> Process.check_and_read_stdout
  in
  let files = String.trim output |> String.split_on_char '\n' in
  let* () =
    (* By default the SSH server accepts at most 10 connections, this can
       fail if there are more then 10 files. We put 5 to take into
       account potential other connections with the proxy SSH server. *)
    files |> Lwt_stream.of_list
    |> Lwt_stream.iter_n ~max_concurrency:5 (fun file ->
           Process.spawn
             ?runner:(Agent.runner proxy_agent)
             "ln"
             ["-s"; binaries_path // file; file; "-f"]
           |> Process.check)
  in
  (* The scenario itself may need some files that exist on the host machine but
     are not present by default on the proxy machine. Since it may be difficult to
     know ahead which files are needed, instead we require the scenario to provide
     them manually. *)
  let* () =
    List.map
      (fun file ->
        let* _ = Agent.copy proxy_agent ~source:file ~destination:file in
        Lwt.return_unit)
      scenario_files
    |> Lwt.join
  in
  unit
