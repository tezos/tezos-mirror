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

  let list ?name_filter ~zone () =
    let name_filter =
      match name_filter with
      | None -> []
      | Some filter -> ["--filter"; Format.asprintf "name=%s" filter]
    in
    Process.run_and_read_stdout
      "gcloud"
      (["dns"; "record-sets"; "list"; "--zone"; zone] @ name_filter)

  module Transaction = struct
    let start ~zone () =
      Process.run
        "gcloud"
        ["dns"; "record-sets"; "transaction"; "start"; "--zone"; zone]

    let add ?(ttl = 300) ?(typ = "A") ~zone ~name ~value () =
      let ttl = ["--ttl"; Format.asprintf "%d" ttl] in
      let typ = ["--type"; Format.asprintf "%s" typ] in
      let name = ["--name"; name] in
      let zone = ["--zone"; zone] in
      Process.run
        "gcloud"
        (["dns"; "record-sets"; "transaction"; "add"]
        @ zone @ ttl @ typ @ name @ [value])

    let remove ?(ttl = 300) ?(typ = "A") ~zone ~name ~value () =
      let ttl = ["--ttl"; Format.asprintf "%d" ttl] in
      let typ = ["--type"; Format.asprintf "%s" typ] in
      let name = ["--name"; name] in
      let zone = ["--zone"; zone] in
      Process.run
        "gcloud"
        (["dns"; "record-sets"; "transaction"; "remove"]
        @ zone @ ttl @ typ @ name @ [value])

    let execute ~zone () =
      Process.run
        "gcloud"
        ["dns"; "record-sets"; "transaction"; "execute"; "--zone"; zone]

    let abort ~zone () =
      Process.run
        "gcloud"
        ["dns"; "record-sets"; "transaction"; "abort"; "--zone"; zone]

    let try_update ~zone fn args =
      Lwt.catch
        (fun () ->
          let* () = start ~zone () in
          let* () = fn args in
          let* () = execute ~zone () in
          unit)
        (fun _ -> abort ~zone ())
  end

  let get_fqdn ~zone ~name =
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
             name
             (String.split_on_char ' ' line |> Fun.flip List.nth 1))

  let get_value ~zone ~domain =
    let* domain = get_fqdn ~zone ~name:domain in
    let* output = list ~name_filter:domain ~zone () in
    (* Example of output
       NAME                           TYPE  TTL  DATA
       user.nl-dal.domain.com.        A     300  35.187.31.38
    *)
    match String.split_on_char '\n' (String.trim output) with
    | [_header; line] -> (
        let columns =
          String.split_on_char ' ' line |> List.filter (fun s -> s <> "")
        in
        match columns with
        | [_domain; _type; _ttl; ip] -> Lwt.return_some ip
        | _ -> assert false)
    | _ -> Lwt.return_none

  let add_subdomain ~zone ~name ~value =
    let* name = get_fqdn ~zone ~name in
    Transaction.(try_update ~zone @@ add ~zone ~name ~value) ()

  let remove_subdomain ~zone ~name ~value =
    let* name = get_fqdn ~zone ~name in
    Transaction.(try_update ~zone @@ remove ~zone ~name ~value) ()
end
