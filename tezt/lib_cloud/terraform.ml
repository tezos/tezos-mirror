(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Tezt

let color = Log.Color.FG.magenta

let name = "terraform"

let chdir path = [Format.asprintf "-chdir=%s" path]

module Docker_registry = struct
  let init () =
    Process.run
      ~name
      ~color
      "terraform"
      (chdir Path.terraform_docker_registry @ ["init"])

  let deploy ~tezt_cloud ~project_id =
    Process.run
      ~env:(String_map.singleton "TF_WORKSPACE" tezt_cloud)
      ~name
      ~color
      "terraform"
      (chdir Path.terraform_docker_registry
      @ [
          "apply";
          "--auto-approve";
          "--var";
          Format.asprintf "project_id=%s" project_id;
        ])

  let get_docker_registry ~tezt_cloud =
    let* output =
      Process.run_and_read_stdout
        ~env:(String_map.singleton "TF_WORKSPACE" tezt_cloud)
        ~name
        ~color
        "terraform"
        (chdir Path.terraform_docker_registry @ ["output"; "-json"])
    in
    let json = JSON.parse ~origin:"get_docker_registry" output in
    let registry_name =
      JSON.(json |-> "docker_registry" |-> "value" |> as_string)
    in
    Lwt.return registry_name

  let get_hostname ~tezt_cloud =
    let* output =
      Process.run_and_read_stdout
        ~env:(String_map.singleton "TF_WORKSPACE" tezt_cloud)
        ~name
        ~color
        "terraform"
        (chdir Path.terraform_docker_registry @ ["output"; "-json"])
    in
    let json = JSON.parse ~origin:"get_hostname" output in
    let registry_name = JSON.(json |-> "hostname" |-> "value" |> as_string) in
    Lwt.return registry_name

  let get_zone ~tezt_cloud =
    let* output =
      Process.run_and_read_stdout
        ~env:(String_map.singleton "TF_WORKSPACE" tezt_cloud)
        ~name
        ~color
        "terraform"
        (chdir Path.terraform_docker_registry @ ["output"; "-json"])
    in
    let json = JSON.parse ~origin:"get_zone" output in
    let registry_name = JSON.(json |-> "zone" |-> "value" |> as_string) in
    Lwt.return registry_name
end

module VM = struct
  (* A VM is deployed under a workspace. A single tezt cloud environment can use
     multiple workspaces all prefixed by the current tezt cloud environment. *)
  module Workspace = struct
    let select workspace =
      Process.run
        ~name
        ~color
        "terraform"
        (chdir Path.terraform_vm @ ["workspace"; "select"; workspace])

    (* Return all the workspaces associated with the current tezt cloud
       environment. *)
    let list ~tezt_cloud =
      (* We select the default workspace to be sure we can parse correctly the
         output. *)
      let* () = select "default" in
      let* output =
        Process.run_and_read_stdout
          ~name
          ~color
          "terraform"
          (chdir Path.terraform_vm @ ["workspace"; "list"])
      in
      String.split_on_char '\n' output
      |> List.map String.trim
      |> List.filter (fun workspace ->
             String.starts_with ~prefix:tezt_cloud workspace
             (* The workspace named "tezt-cloud" is used for the docker
                registry, it does not need to be listed. *)
             && workspace <> tezt_cloud)
      |> Lwt.return

    (* Create workspaces that will be used for the experiment. *)
    let init ~tezt_cloud workspaces =
      let* existing_workspaces = list ~tezt_cloud in
      let to_create =
        (* Create only new workspaces. *)
        List.filter
          (fun workspace -> not @@ List.mem workspace existing_workspaces)
          workspaces
      in
      let* () =
        to_create
        |> List.map (fun workspace ->
               Process.run
                 ~name
                 ~color
                 "terraform"
                 (chdir Path.terraform_vm @ ["workspace"; "new"; workspace]))
        |> Lwt.join
      in
      (* We want to ensure the last workspace created will not be the
         one selected by default. Instead it should be set when
         deploying the machines. *)
      let* () = select "default" in
      unit

    let destroy ~tezt_cloud =
      (* We ensure we are not using a workspace we want to delete. *)
      let* () = select "default" in
      let* workspaces = list ~tezt_cloud in
      workspaces
      |> List.map (fun workspace ->
             Process.run
               ~name
               ~color
               "terraform"
               (chdir Path.terraform_vm @ ["workspace"; "delete"; workspace]))
      |> Lwt.join
  end

  let init () =
    Process.run ~name ~color "terraform" (chdir Path.terraform_vm @ ["init"])

  let deploy ~max_run_duration ~machine_type ~base_port ~ports_per_vm
      ~number_of_vms ~docker_image =
    let* project_id = Gcloud.project_id () in
    let max_run_duration =
      match max_run_duration with
      | None -> []
      | Some value -> ["--var"; Format.asprintf "max_run_duration=%d" value]
    in
    let args =
      max_run_duration
      @ [
          "--var";
          Format.asprintf "base_port=%d" base_port;
          "--var";
          Format.asprintf "ports_per_vm=%d" ports_per_vm;
          "--var";
          Format.asprintf "number_of_vms=%d" number_of_vms;
          "--var";
          Format.asprintf "project_id=%s" project_id;
          "--var";
          Format.asprintf "machine_type=%s" machine_type;
          "--var";
          Format.asprintf "docker_image=%s" docker_image;
        ]
    in
    Process.run
      ~name
      ~color
      "terraform"
      (chdir Path.terraform_vm @ ["apply"; "--auto-approve"] @ args)

  let points () =
    let* output =
      Process.run_and_read_stdout
        ~name
        ~color
        "terraform"
        (chdir Path.terraform_vm @ ["output"; "-json"])
    in
    let json = JSON.parse ~origin:"VM.points" output in
    let addresses =
      JSON.(json |-> "addresses" |-> "value" |> as_list |> List.map as_string)
    in
    Lwt.return addresses

  let zone () =
    let* output =
      Process.run_and_read_stdout
        ~name
        ~color
        "terraform"
        (chdir Path.terraform_vm @ ["output"; "-json"])
    in
    let json = JSON.parse ~origin:"VM.zone" output in
    let zone = JSON.(json |-> "zone" |-> "value" |> as_string) in
    Lwt.return zone

  let machine_type () =
    let* output =
      Process.run_and_read_stdout
        ~name
        ~color
        "terraform"
        (chdir Path.terraform_vm @ ["output"; "-json"])
    in
    let json = JSON.parse ~origin:"VM.machine_type" output in
    let machine_type =
      JSON.(json |-> "machine_type" |-> "value" |> as_string)
    in
    Lwt.return machine_type

  let destroy workspaces ~project_id =
    workspaces
    |> Lwt_list.iter_s (fun workspace ->
           let* () = Workspace.select workspace in
           let* machine_type = machine_type () in
           let vars =
             [
               "--var";
               Format.asprintf "project_id=%s" project_id;
               "--var";
               Format.asprintf "machine_type=%s" machine_type;
             ]
           in
           let* () =
             (* Remote machines could have been destroyed
                automatically. We synchronize the state to see whether
                they still exist or not. *)
             Process.run
               ~name
               ~color
               "terraform"
               (chdir Path.terraform_vm @ ["refresh"] @ vars)
           in
           Process.run
             ~name
             ~color
             "terraform"
             (chdir Path.terraform_vm @ ["destroy"; "--auto-approve"] @ vars))
end
