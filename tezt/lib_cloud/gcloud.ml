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

module DNS = struct
  let create_zone ~domain ~zone () =
    let dns_name = Format.asprintf "%s.%s" zone domain in
    let description =
      Format.asprintf "Managed zone for tezt-cloud for %s project" zone
    in
    Process.run
      "gcloud"
      [
        "dns";
        "managed-zones";
        "create";
        zone;
        "--dns-name";
        dns_name;
        "--description";
        description;
      ]

  let describe ~zone () =
    Process.run_and_read_stdout
      "gcloud"
      ["dns"; "managed-zones"; "describe"; zone]

  module Transaction = struct
    let start ~zone () =
      Process.run
        "gcloud"
        ["dns"; "record-sets"; "transaction"; "start"; "--zone"; zone]

    let add ?(ttl = 300) ?(typ = "A") ~domain ~zone ~ip () =
      let ttl = ["--ttl"; Format.asprintf "%d" ttl] in
      let typ = ["--type"; Format.asprintf "%s" typ] in
      let domain = ["--name"; domain] in
      let zone = ["--zone"; zone] in
      Process.run
        "gcloud"
        (["dns"; "record-sets"; "transaction"; "add"]
        @ zone @ ttl @ typ @ domain @ [ip])

    let remove ?(ttl = 300) ?(typ = "A") ~domain ~zone ~ip () =
      let ttl = ["--ttl"; Format.asprintf "%d" ttl] in
      let typ = ["--type"; Format.asprintf "%s" typ] in
      let domain = ["--name"; domain] in
      let zone = ["--zone"; zone] in
      Process.run
        "gcloud"
        (["dns"; "record-sets"; "transaction"; "remove"]
        @ zone @ ttl @ typ @ domain @ [ip])

    let execute ~zone () =
      Process.run
        "gcloud"
        ["dns"; "record-sets"; "transaction"; "execute"; "--zone"; zone]
  end

  let get_domain ~tezt_cloud ~zone =
    let* output = describe ~zone () in
    let line =
      String.trim output |> String.split_on_char '\n'
      |> List.find_opt (String.starts_with ~prefix:"dnsName:")
    in
    match line with
    | None -> Test.fail "Unable to find a managed zone. Have you create it?"
    | Some line ->
        Lwt.return
          (Format.asprintf
             "%s.%s"
             tezt_cloud
             (String.split_on_char ' ' line |> Fun.flip List.nth 1))

  let add ~tezt_cloud ~zone ~ip =
    let* domain = get_domain ~tezt_cloud ~zone in
    let* () = Transaction.start ~zone () in
    let* () = Transaction.add ~domain ~zone ~ip () in
    let* () = Transaction.execute ~zone () in
    unit

  let remove ~tezt_cloud ~zone ~ip =
    let* domain = get_domain ~tezt_cloud ~zone in
    let* () = Transaction.start ~zone () in
    let* () = Transaction.remove ~domain ~zone ~ip () in
    let* () = Transaction.execute ~zone () in
    unit
end
