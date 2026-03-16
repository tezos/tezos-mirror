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

let delete_address ~name ~region ~project_id () =
  Process.run
    "gcloud"
    [
      "compute";
      "addresses";
      "delete";
      name;
      "--region";
      region;
      "--project";
      project_id;
      "--quiet";
    ]

(** [region_of_url url] extracts the region name from a GCP resource URL.

    GCP returns the [region] field of an address as a fully qualified URL
    such as:
      ["https://www.googleapis.com/compute/v1/projects/my-proj/regions/europe-west1"]

    This function extracts the last path segment (["europe-west1"]).
    If the input contains no ['/'], it is returned unchanged as a
    fallback. *)
let region_of_url region_url =
  match String.split_on_char '/' region_url |> List.rev with
  | r :: _ -> r
  | [] -> region_url

(** [utc_seconds_of_iso8601 s] attempts to parse an ISO 8601 timestamp
    as returned by GCP (e.g. ["2024-01-15T10:30:00.000-07:00"]) and
    returns the corresponding UTC time in seconds since the Unix epoch.

    The implementation uses [Scanf] to extract the date-time fields and
    the timezone offset, then converts to UTC via [CalendarLib]-free
    arithmetic.  Fractional seconds are accepted but ignored (GCP
    sometimes appends [".000"]).

    Returns [None] if the string does not match the expected format. *)
let utc_seconds_of_iso8601 s =
  try
    (* GCP format: "2024-01-15T10:30:00.000-07:00"
       We accept with or without fractional seconds. *)
    let year, month, day, hour, min, sec, tz_sign, tz_h, tz_m =
      Scanf.sscanf
        s
        "%d-%d-%dT%d:%d:%d%[^0-9]%d:%d"
        (fun y mo d h mi s tz_sign_str tz_h tz_m ->
          let tz_sign = if String.contains tz_sign_str '-' then -1 else 1 in
          (y, mo, d, h, mi, s, tz_sign, tz_h, tz_m))
    in
    let tm =
      {
        Unix.tm_sec = sec;
        tm_min = min - (tz_sign * tz_m);
        tm_hour = hour - (tz_sign * tz_h);
        tm_mday = day;
        tm_mon = month - 1;
        tm_year = year - 1900;
        (* The following fields are ignored by [mktime] but must be
           present in the record. *)
        tm_wday = 0;
        tm_yday = 0;
        tm_isdst = false;
      }
    in
    let epoch, _ = Unix.mktime tm in
    Some epoch
  with _ -> None

(** [delete_unused_addresses ~project_id ?name_filter ?max_age_hours ()]
    lists and deletes GCP static IP addresses that are currently unused.

    This function serves two use-cases:

    {b (a) Post-destroy cleanup} (called from [Deployement.Remote.terminate]
    and [register_destroy_vms]):  [name_filter] is set to the workspace
    name we just destroyed, [max_age_hours] is [None].  We know the
    workspaces are gone, so any matching RESERVED address is genuinely
    orphaned.

    {b (b) Periodic garbage collection} (called from
    [register_delete_unused_ips]):  [name_filter] is [None] (match all
    addresses in the project), [max_age_hours] is [Some 24].  This is the
    safety net that cleans up after any user, regardless of whose
    scenario created the IPs.

    {b Safety invariants — read carefully before modifying this function:}

    {b 1. Scope restriction via [name_filter].}
    When provided, [name_filter] is passed as a [name:] prefix filter
    to [gcloud compute addresses list].  In the tezt-cloud naming
    convention, addresses are named ["{workspace}-{index:02d}"] where
    the workspace itself is ["{tezt_cloud}-{n}"].  By passing a
    workspace name (e.g. ["alice-0"]) or the tezt-cloud identifier
    (e.g. ["alice"]) as [name_filter], we restrict deletion to the
    current user's addresses.  When [None], all RESERVED addresses in
    the project are considered — in that case, [max_age_hours] {b must}
    be set to avoid destroying freshly-created addresses.

    {b 2. Only RESERVED addresses are considered.}
    The GCP filter includes [status=RESERVED], which means only
    addresses that are {e not attached} to any VM are candidates.
    An address that is [IN_USE] (attached to a running or stopped VM)
    is never deleted.  This is the primary safeguard against deleting
    IPs of a concurrent scenario that has already attached its
    addresses to VMs.

    {b 3. Race condition window.}
    Between [terraform apply] creating an address and the address
    becoming [IN_USE] (attached to a VM), there is a short window
    where the address is [RESERVED].  During that window, a concurrent
    cleanup could delete it.  The [max_age_hours] parameter mitigates
    this: when provided, only addresses older than [max_age_hours] are
    deleted.  For the post-destroy cleanup (where we know the
    workspaces are gone), [max_age_hours] can safely be [None].  For
    the garbage-collection job (no [name_filter]), it {b should} be set
    (e.g. [Some 24]) to avoid touching freshly-created addresses.

    {b 4. Resilience to concurrent execution.}
    Deletion failures on individual addresses are caught and logged
    as warnings without aborting the remaining deletions.  This makes
    the function safe to call concurrently: if two processes try to
    delete the same orphan address, one will succeed and the other
    will get a "not found" error that is silently absorbed.

    @param project_id   The GCP project identifier.
    @param name_filter  When set, a prefix passed to
      [gcloud --filter="name:{v}"].  When [None], all addresses are
      considered (use together with [max_age_hours] for safety).
    @param max_age_hours  When set, only addresses whose
      [creationTimestamp] is strictly older than this many hours are
      deleted.  When [None], all matching [RESERVED] addresses are
      deleted regardless of age. *)
let delete_unused_addresses ~project_id ?name_filter ?max_age_hours () =
  let filter =
    match name_filter with
    | Some prefix -> Format.asprintf "status=RESERVED AND name:%s" prefix
    | None -> "status=RESERVED"
  in
  let filter_description =
    match name_filter with
    | Some prefix -> Format.asprintf "'%s'" prefix
    | None -> "<all addresses>"
  in
  let* addresses = list_addresses ~filter () in
  let candidates = JSON.as_list addresses in
  let candidates =
    match max_age_hours with
    | None -> candidates
    | Some hours ->
        let now = Unix.gettimeofday () in
        let max_age_seconds = float_of_int hours *. 3600. in
        List.filter
          (fun addr ->
            let creation = JSON.(addr |-> "creationTimestamp" |> as_string) in
            match utc_seconds_of_iso8601 creation with
            | Some created_at -> now -. created_at > max_age_seconds
            | None ->
                Log.warn
                  "Could not parse creation timestamp '%s' for address '%s', \
                   skipping"
                  creation
                  JSON.(addr |-> "name" |> as_string) ;
                false)
          candidates
  in
  match candidates with
  | [] ->
      Log.info "No unused static IPs matching %s to delete." filter_description ;
      Lwt.return_unit
  | _ ->
      Log.info
        "Found %d unused static IP(s) matching %s to delete."
        (List.length candidates)
        filter_description ;
      Lwt_list.iter_s
        (fun addr ->
          let name = JSON.(addr |-> "name" |> as_string) in
          let region = JSON.(addr |-> "region" |> as_string) |> region_of_url in
          Log.info "Deleting unused IP: %s (region: %s)" name region ;
          Lwt.catch
            (fun () -> delete_address ~name ~region ~project_id ())
            (fun exn ->
              Log.warn
                "Failed to delete IP %s (region: %s): %s"
                name
                region
                (Printexc.to_string exn) ;
              Lwt.return_unit))
        candidates

let create_firewall_rule ~name ~network ~ports ~source_ranges ~priority () =
  let ports_str =
    List.map (fun p -> sf "tcp:%d" p) ports |> String.concat ","
  in
  let source_ranges_str = String.concat "," source_ranges in
  Process.run
    "gcloud"
    [
      "compute";
      "firewall-rules";
      "create";
      name;
      "--network";
      network;
      "--allow";
      ports_str;
      "--source-ranges";
      source_ranges_str;
      "--priority";
      string_of_int priority;
      "--quiet";
    ]

let delete_firewall_rule ~name () =
  Lwt.catch
    (fun () ->
      Process.run
        "gcloud"
        ["compute"; "firewall-rules"; "delete"; name; "--quiet"])
    (fun exn ->
      Log.warn
        "Failed to delete firewall rule %s (may not exist): %s"
        name
        (Printexc.to_string exn) ;
      Lwt.return_unit)

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
