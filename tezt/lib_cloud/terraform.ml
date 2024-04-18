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

  let deploy () =
    let* project_id = Gcloud.project_id () in
    Process.run
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

  let get_docker_registry () =
    let* output =
      Process.run_and_read_stdout
        ~name
        ~color
        "terraform"
        (chdir Path.terraform_docker_registry @ ["output"; "-json"])
    in
    let json = JSON.parse ~origin:"docker_registry" output in
    let registry_name =
      JSON.(json |-> "docker_registry" |-> "value" |> as_string)
    in
    Lwt.return registry_name

  let get_hostname () =
    let* output =
      Process.run_and_read_stdout
        ~name
        ~color
        "terraform"
        (chdir Path.terraform_docker_registry @ ["output"; "-json"])
    in
    let json = JSON.parse ~origin:"docker_registry_hostname" output in
    let registry_name = JSON.(json |-> "hostname" |-> "value" |> as_string) in
    Lwt.return registry_name
end

module VM = struct
  module Workspace = struct
    let get () =
      let tezt_cloud = Lazy.force Env.tezt_cloud in
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
             String.starts_with ~prefix:tezt_cloud workspace)
      |> Lwt.return

    let init workspaces =
      let* existing_workspaces = get () in
      let to_create =
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
      let* () =
        Process.run
          ~name
          ~color
          "terraform"
          (chdir Path.terraform_vm @ ["workspace"; "select"; "default"])
      in
      unit

    let select workspace =
      Process.run
        ~name
        ~color
        "terraform"
        (chdir Path.terraform_vm @ ["workspace"; "select"; workspace])

    let destroy () =
      let* workspaces = get () in
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

  let deploy ~machine_type ~base_port ~ports_per_vm ~number_of_vms
      ~docker_registry =
    let* project_id = Gcloud.project_id () in
    let docker_image_name = Lazy.force Env.tezt_cloud in
    let args =
      [
        "--var";
        Format.asprintf "base_port=%d" base_port;
        "--var";
        Format.asprintf "ports_per_vm=%d" ports_per_vm;
        "--var";
        Format.asprintf "number_of_vms=%d" number_of_vms;
        "--var";
        Format.asprintf "docker_registry_name=%s" docker_registry;
        "--var";
        Format.asprintf "project_id=%s" project_id;
        "--var";
        Format.asprintf "machine_type=%s" machine_type;
        "--var";
        Format.asprintf "docker_image_name=%s" docker_image_name;
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

  let destroy () =
    let* project_id = Gcloud.project_id () in
    let* machine_type = machine_type () in
    let docker_image_name = Lazy.force Env.tezt_cloud in
    Process.run
      ~name
      ~color
      "terraform"
      (chdir Path.terraform_vm
      @ [
          "destroy";
          "--auto-approve";
          "--var";
          Format.asprintf "project_id=%s" project_id;
          "--var";
          Format.asprintf "machine_type=%s" machine_type;
          "--var";
          Format.asprintf "docker_image_name=%s" docker_image_name;
        ])
end

module State_bucket = struct
  let init () =
    let* () = Docker_registry.init () in
    Process.run
      ~name
      ~color
      "terraform"
      (chdir Path.terraform_state_bucket @ ["init"])

  let deploy () =
    let* project_id = Gcloud.project_id () in
    Process.run
      ~name
      ~color
      "terraform"
      (chdir Path.terraform_state_bucket
      @ [
          "apply";
          "--auto-approve";
          "--var";
          Format.asprintf "project_id=%s" project_id;
        ])
end
