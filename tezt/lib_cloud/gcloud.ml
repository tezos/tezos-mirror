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

(** Retrieves the current Gcloud project ID from local environment. *)
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

let gcloud_run_stdout args =
  let* output =
    Process.run_and_read_stdout "gcloud" (["--format"; "json"] @ args)
  in
  let origin = "gcloud " ^ String.concat " " args in
  let json = JSON.parse ~origin output in
  Lwt.return json

let gcloud_run_stdout_filter ~filter args =
  let filter =
    match filter with
    | None -> []
    | Some filter -> [Format.asprintf "--filter=%s" filter]
  in
  let* json = gcloud_run_stdout (args @ filter) in
  Lwt.return json

let list_firewalls ?filter () =
  gcloud_run_stdout_filter ~filter ["compute"; "firewall-rules"; "list"]

let list_networks ?filter () =
  gcloud_run_stdout_filter ~filter ["compute"; "networks"; "list"]

let list_subnets ?filter () =
  gcloud_run_stdout_filter ~filter ["compute"; "networks"; "subnets"; "list"]

let list_addresses ?filter () =
  gcloud_run_stdout_filter ~filter ["compute"; "addresses"; "list"]

let list_instances ?filter () =
  gcloud_run_stdout_filter ~filter ["compute"; "instances"; "list"]

let list_instance_templates ?filter () =
  gcloud_run_stdout_filter ~filter ["compute"; "instance-templates"; "list"]

let list_instance_groups ?filter () =
  gcloud_run_stdout_filter ~filter ["compute"; "instance-groups"; "list"]

let list_iam_service_accounts ?filter () =
  gcloud_run_stdout_filter ~filter ["iam"; "service-accounts"; "list"]

let list_disks ?filter () =
  gcloud_run_stdout_filter ~filter ["compute"; "disks"; "list"]

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

  let list_zones () =
    let* output =
      Process.run_and_read_stdout
        "gcloud"
        ["dns"; "managed-zones"; "list"; "--format"; "csv(name, dns_name)"]
    in
    let lines = String.trim output |> String.split_on_char '\n' |> List.tl in
    let res =
      List.fold_left
        (fun acc line ->
          match String.trim line |> String.split_on_char ',' with
          | [zone; dnsname] -> (zone, dnsname) :: acc
          | _ -> acc)
        []
        lines
    in
    Lwt.return res

  let describe ~zone () =
    Process.run_and_read_stdout
      "gcloud"
      ["dns"; "managed-zones"; "describe"; zone]

  let list ~zone () =
    Process.run_and_read_stdout
      "gcloud"
      ["dns"; "record-sets"; "list"; "--zone"; zone]

  (** [list_entries ?name ~zone] lists all DNS entries from specified [~zone]
      optionally filtering them by [?name_filter]. *)
  let list_entries ?name_filter ~zone () =
    let name_filter =
      match name_filter with
      | None -> []
      | Some filter -> ["--filter"; Format.asprintf "name=%s" filter]
    in
    Process.run_and_read_stdout
      "gcloud"
      (["dns"; "record-sets"; "list"; "--zone"; zone] @ name_filter)

  module Transaction = struct
    (** [start ~zone ()] starts a DNS transaction in the specified [~zone]. *)
    let start ~zone () =
      Process.run
        "gcloud"
        ["dns"; "record-sets"; "transaction"; "start"; "--zone"; zone]

    (** [add ?ttl ?typ ~zone ~name ~value ()] adds a DNS record to the transaction.
        The record has an optional time-to-live [?ttl], type [?typ]. *)
    let add ?(ttl = 300) ?(typ = "A") ~zone ~name ~value () =
      let ttl = ["--ttl"; Format.asprintf "%d" ttl] in
      let typ = ["--type"; Format.asprintf "%s" typ] in
      let name = ["--name"; name] in
      let zone = ["--zone"; zone] in
      Process.run
        "gcloud"
        (["dns"; "record-sets"; "transaction"; "add"]
        @ zone @ ttl @ typ @ name @ [value])

    (** [remove ?ttl ?typ ~zone ~name ~value ()] removes a DNS record from the transaction.
        The record has an optional time-to-live [?ttl], type [?typ]. *)
    let remove ?(ttl = 300) ?(typ = "A") ~zone ~name ~value () =
      let ttl = ["--ttl"; Format.asprintf "%d" ttl] in
      let typ = ["--type"; Format.asprintf "%s" typ] in
      let name = ["--name"; name] in
      let zone = ["--zone"; zone] in
      Process.run
        "gcloud"
        (["dns"; "record-sets"; "transaction"; "remove"]
        @ zone @ ttl @ typ @ name @ [value])

    (** [execute ~zone ()] executes the current DNS transaction in the specified [~zone]. *)
    let execute ~zone () =
      Process.run
        "gcloud"
        ["dns"; "record-sets"; "transaction"; "execute"; "--zone"; zone]

    (** [abort ~zone ()] aborts the current DNS transaction in the specified [~zone]. *)
    let abort ~zone () =
      Process.run
        "gcloud"
        ["dns"; "record-sets"; "transaction"; "abort"; "--zone"; zone]

    (** [try_update ~zone fn args] tries to perform a DNS update transaction. It starts a
        transaction, applies the changes using the provided function [fn] with arguments
        [args], and executes the transaction. If an error occurs, it aborts the transaction. *)
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
    | None -> Lwt.return_none
    | Some line -> (
        try
          let parent = String.split_on_char ' ' line |> Fun.flip List.nth 1 in
          if String.ends_with name ~suffix:parent then Lwt.return_some name
          else Lwt.return_some (Format.asprintf "%s.%s" name parent)
        with _ -> Lwt.return_none)

  let get_value ~zone ~domain =
    let* output = list_entries ~name_filter:domain ~zone () in
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

  let find_zone_for_subdomain domain =
    (* list_zone will always return domain name with a final dot *)
    let domain =
      if String.ends_with ~suffix:"." domain then domain else domain ^ "."
    in
    let* zones = list_zones () in
    let zone =
      List.find_opt
        (fun (_, zone_domain) -> String.ends_with ~suffix:zone_domain domain)
        zones
    in
    return zone

  let apply_update ~zone ~name ~log transaction =
    let* name = get_fqdn ~zone ~name in
    match name with
    | None ->
        Log.report "No domain found for zone: '%s'" zone ;
        Lwt.return_unit
    | Some name ->
        Log.report "%s" (log name) ;
        Transaction.(try_update ~zone @@ transaction ~name) ()

  let add_subdomain ~zone ~name ~value =
    apply_update
      ~zone
      ~name
      ~log:(Format.sprintf "Adding subdomain '%s'")
      (fun ~name () -> Transaction.add ~zone ~name ~value ())

  let remove_subdomain ~zone ~name ~value =
    apply_update
      ~zone
      ~name
      ~log:(Format.sprintf "Removing subdomain '%s'")
      (fun ~name () -> Transaction.remove ~zone ~name ~value ())

  let set_subdomain ~agent_ip ~domain =
    let* res = find_zone_for_subdomain domain in
    match res with
    | None ->
        let () =
          Log.report
            ~color:Log.Color.FG.yellow
            "The domain '%s' is not a subdomain of an authorized GCP zone. \
             Skipping."
            domain
        in
        Lwt.return_unit
    | Some (zone, _) ->
        let transaction ~name () =
          let* ip = get_value ~zone ~domain in
          let* () =
            match ip with
            | None -> Lwt.return_unit
            | Some ip -> Transaction.remove ~zone ~name ~value:ip ()
          in
          Transaction.add ~zone ~name ~value:agent_ip ()
        in
        let* () =
          apply_update
            ~zone
            ~name:domain
            ~log:(Format.sprintf "Updating subdomain '%s'")
            transaction
        in
        let () =
          Log.report
            ~color:Log.Color.FG.green
            "DNS registered successfully: '%s'"
            domain
        in
        Lwt.return_unit
end
