(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let name = "gcloud"

let color = Log.Color.FG.gray

let auth_configure_docker ~hostname =
  Process.run ~name ~color "gcloud" ["auth"; "configure-docker"; hostname]

let config_get_value_project () =
  Process.run_and_read_stdout
    ~name
    ~color
    "gcloud"
    ["config"; "get-value"; "project"]

let project_id =
  let project_id = ref "<unset>" in
  fun () ->
    if !project_id = "<unset>" then (
      let* project_id' =
        match Cli.project_id with
        | None -> config_get_value_project ()
        | Some project_id -> Lwt.return project_id
      in
      project_id := String.trim project_id' ;
      Lwt.return !project_id)
    else Lwt.return !project_id

type cmd_wrapper = {cmd : string; args : string list}

let cmd_wrapper ~zone ~vm_name ~ssh_private_key_filename =
  {
    cmd = "gcloud";
    args =
      [
        "compute";
        "ssh";
        "--ssh-key-file";
        ssh_private_key_filename;
        vm_name;
        "--zone";
        zone;
        "--";
      ];
  }

let compute_ssh ~zone ~vm_name ~ssh_private_key_filename cmd args =
  let wrapper = cmd_wrapper ~zone ~vm_name ~ssh_private_key_filename in
  Process.spawn ~name ~color wrapper.cmd (wrapper.args @ [cmd] @ args)

let get_ip_address_from_name ~zone name =
  let* output =
    Process.run_and_read_stdout
      "gcloud"
      [
        "compute";
        "instances";
        "describe";
        name;
        "--format";
        "get(networkInterfaces[0].accessConfigs[0].natIP)";
        "--zone";
        zone;
      ]
  in
  Lwt.return (String.trim output)

let list_vms ~prefix =
  let filter = Format.asprintf "status=RUNNING AND name:%s" prefix in
  let* output =
    Process.run_and_read_stdout
      "gcloud"
      ["compute"; "instances"; "list"; "--filter"; filter]
  in
  Lwt.return (String.trim output)
