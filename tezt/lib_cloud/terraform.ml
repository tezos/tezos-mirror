(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Tezt
open Types

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
      let () = Log.report "Terraform.VM.Workspace.list" in
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
             let prefix = Format.asprintf "%s-" tezt_cloud in
             String.starts_with ~prefix workspace
             (* The workspace named "tezt-cloud" is used for the docker
                registry, it does not need to be listed. *)
             && workspace <> tezt_cloud)
      |> Lwt.return

    (* Create workspaces that will be used for the experiment. *)
    let init ~tezt_cloud workspaces =
      let () = Log.report "Terraform.VM.Workspace.init" in
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

    let show_existing_resources ~tezt_cloud =
      let filter = Format.asprintf "name=%s" tezt_cloud in
      let* instances_groups = Gcloud.list_instance_groups ~filter () in
      let* services_accounts = Gcloud.list_iam_service_accounts ~filter () in
      let* addresses = Gcloud.list_addresses ~filter () in
      let* firewalls = Gcloud.list_firewalls ~filter () in
      let* subnets = Gcloud.list_subnets ~filter () in
      let* networks = Gcloud.list_networks ~filter () in
      let* disks = Gcloud.list_disks ~filter () in

      let show title resources =
        let names =
          JSON.(
            resources |> as_list
            |> List.map (fun v -> v |-> "name" |> as_string))
        in
        if names = [] then ()
        else
          Log.warn
            "%s: @[<hv 3>%a@;@]"
            title
            (Format.pp_print_list
               ~pp_sep:(fun fmt () -> Format.fprintf fmt "@;")
               Format.pp_print_string)
            names
      in
      let () = show "instances groups" instances_groups in
      let () = show "service_accounts" services_accounts in
      let () = show "addresses" addresses in
      let () = show "firewalls" firewalls in
      let () = show "subnets" subnets in
      let () = show "networks" networks in
      let () = show "disks" disks in
      Lwt.return_unit

    let destroy ~tezt_cloud =
      let () = Log.report "Terraform.VM.Workspace.destroy" in
      (* We ensure we are not using a workspace we want to delete. *)
      Lwt.catch
        (fun () ->
          let* () = select "default" in
          let* workspaces = list ~tezt_cloud in
          workspaces
          |> List.map (fun workspace ->
                 Process.run
                   ~name
                   ~color
                   "terraform"
                   (chdir Path.terraform_vm @ ["workspace"; "delete"; workspace]))
          |> Lwt.join)
        (fun exn ->
          let* () = show_existing_resources ~tezt_cloud in
          raise exn)
  end

  let init () =
    let () = Log.report "Terraform.VM.init" in
    Process.run ~name ~color "terraform" (chdir Path.terraform_vm @ ["init"])

  let deploy ~auto_approve ~max_run_duration ~machine_type ~disk_type
      ~disk_size_gb ~base_port ~ports_per_vm ~number_of_vms ~docker_image ~os
      ~prometheus_port =
    let* project_id = Gcloud.project_id () in
    let max_run_duration =
      match max_run_duration with
      | None -> []
      | Some value -> ["--var"; Format.asprintf "max_run_duration=%d" value]
    in
    let disk_type =
      match disk_type with
      | None -> []
      | Some d -> ["--var"; Format.sprintf "disk_type=%s" d]
    in
    let disk_size_gb =
      match disk_size_gb with
      | None -> []
      | Some s -> ["--var"; Format.sprintf "disk_size_gb=%d" s]
    in
    let args =
      max_run_duration @ disk_type @ disk_size_gb
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
          "--var";
          Format.asprintf "os=%s" (Os.to_string os);
          "--var";
          Format.asprintf "prometheus_port=%d" prometheus_port;
        ]
    in
    if auto_approve then
      Process.run
        ~name
        ~color
        "terraform"
        (chdir Path.terraform_vm @ ["apply"; "--auto-approve"] @ args)
    else
      let process, output_channel =
        Process.spawn_with_stdin
          ~name
          ~color
          "terraform"
          (chdir Path.terraform_vm @ ["apply"] @ args)
      in
      let* input = Input.next () in
      (* If the user pressed Ctrl+D, i.e. input is [None], we don't
         care what the input is. *)
      let input = Option.value ~default:"" input in
      let* () = Lwt_io.write_line output_channel input in
      Process.check process

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

  type machine_specs = {
    machine_type : string;
    disk_type : string option;
    disk_size_gb : int option;
  }

  let machine_specs () =
    let* output =
      Process.run_and_read_stdout
        ~name
        ~color
        "terraform"
        (chdir Path.terraform_vm @ ["output"; "-json"])
    in
    let json = JSON.parse ~origin:"VM.output" output in
    (* TODO: find a better fix *)
    let machine_type =
      match JSON.(json |-> "machine_type" |-> "value" |> as_string_opt) with
      | Some machine_type -> machine_type
      | None -> "n1-standard-2"
    in
    let disk_type = JSON.(json |-> "disk_type" |-> "value" |> as_string_opt) in
    let disk_size_gb =
      JSON.(json |-> "disk_size_gb" |-> "value" |> as_int_opt)
    in
    Lwt.return {machine_type; disk_type; disk_size_gb}

  let destroy workspaces ~project_id =
    workspaces
    |> Lwt_list.iter_s (fun workspace ->
           Lwt.catch
             (fun () ->
               let* () = Workspace.select workspace in
               let* vm = machine_specs () in
               let disk_type =
                 match vm.disk_type with
                 | None -> []
                 | Some d -> ["--var"; Format.sprintf "disk_type=%s" d]
               in
               let disk_size_gb =
                 match vm.disk_size_gb with
                 | None -> []
                 | Some s -> ["--var"; Format.sprintf "disk_size_gb=%d" s]
               in
               let vars =
                 [
                   "--var";
                   Format.asprintf "project_id=%s" project_id;
                   "--var";
                   Format.asprintf "machine_type=%s" vm.machine_type;
                 ]
                 @ disk_type @ disk_size_gb
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
             (fun exn ->
               let* () =
                 Workspace.show_existing_resources ~tezt_cloud:workspace
               in
               raise exn))
end
