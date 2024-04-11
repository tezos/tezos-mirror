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
  let init () =
    (* If this is the first time the workspace is used, it needs to be created.
       For all the other cases, terraform will fail, hence we forget the error
       (yeah, this is a bit ugly). This must be run before `terraform init`. *)
    let workspace = Lazy.force Env.workspace in
    let* _ =
      Process.spawn
        ~name
        ~color
        "terraform"
        (chdir Path.terraform_vm @ ["workspace"; "new"; workspace])
      |> Process.wait
    in
    Process.run ~name ~color "terraform" (chdir Path.terraform_vm @ ["init"])

  let deploy ~machine_type ~base_port ~ports_per_vm ~number_of_vms
      ~docker_registry =
    let* project_id = Gcloud.project_id () in
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
