(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Path = Path
module Agent = Agent
module Types = Types
module Chronos = Chronos
module Ssh = Ssh

module Alert = struct
  include Alert_manager

  type t = Alert_manager.alert

  type severity = Prometheus.severity = Critical | Warning | Info

  let make ?route ?for_ ?description ?summary ?severity ?group_name ?interval
      ~name ~expr () =
    let alert =
      Prometheus.make_alert
        ?for_
        ?description
        ?summary
        ?severity
        ?group_name
        ?interval
        ~name
        ~expr
        ()
    in

    Alert_manager.alert ?route alert
end

module Cloud = Cloud

let register_docker_push ~tags =
  Cloud.register
    ?vms:None
    ~__FILE__
    ~title:"Push the dockerfile to the GCP registry"
    ~tags:("docker" :: "push" :: tags)
  @@ fun _cloud ->
  let* ssh_public_key = Ssh.public_key () in
  Jobs.docker_build ~push:true ~ssh_public_key ()

let register_docker_build ~tags =
  Cloud.register
    ?vms:None
    ~__FILE__
    ~title:"Build the dockerfile"
    ~tags:("docker" :: "build" :: tags)
  @@ fun _cloud ->
  let* ssh_public_key = Ssh.public_key () in
  Jobs.docker_build ~push:false ~ssh_public_key ()

let register_deploy_docker_registry ~tags =
  Cloud.register
    ?vms:None
    ~__FILE__
    ~title:"Deploy docker registry"
    ~tags:("docker" :: "registry" :: "deploy" :: tags)
  @@ fun _cloud -> Jobs.deploy_docker_registry ()

let register_destroy_vms ~tags =
  Cloud.register
    ?vms:None
    ~__FILE__
    ~title:"Destroy terraform VMs"
    ~tags:("terraform" :: "destroy" :: "vms" :: tags)
  @@ fun _cloud ->
  let tezt_cloud = Env.tezt_cloud in
  let* project_id = Gcloud.project_id () in
  let* workspaces = Terraform.VM.Workspace.list ~tezt_cloud in
  let* () = Terraform.VM.destroy workspaces ~project_id in
  Terraform.VM.Workspace.destroy ~tezt_cloud

let register_prometheus_import ~tags =
  Cloud.register
    ?vms:None
    ~__FILE__
    ~title:"Import snapshots into prometheus containers"
    ~tags:("prometheus" :: "import" :: tags)
  @@ fun _cloud ->
  let conf =
    match Env.prometheus_snapshots with
    | [] ->
        Test.fail
          "You must specify the snapshot filename via --prometheus-snapshots"
    | snapshots ->
        List.mapi
          (fun i -> function
            | path, Some port -> (path, port)
            | path, None -> (path, Env.prometheus_port + i))
          snapshots
  in
  let* prometheus =
    Lwt_list.map_s
      (fun (path, port) -> Prometheus.run_with_snapshot port path)
      conf
  in
  let* g =
    if Env.grafana then
      let sources =
        if Env.grafana_legacy_source then
          match prometheus with
          | [p] ->
              (* For legacy support of the existing grafana dashboards, when
                 using only one snapshot, we want to use the default
                 'Prometheus' source name. *)
              let port = Prometheus.get_port p in
              [
                sf
                  {|
- name: Prometheus
  type: prometheus
  access: proxy
  url: http://localhost:%d
  isDefault: true
|}
                  port;
              ]
          | _ ->
              Test.fail
                "You need to provide one and only one snapshot when using the \
                 legacy dashboard source name"
        else
          (* When using multiple snapshots we assume that they are used
             with a dashboard that do not hard code the datasource
             name. *)
          List.map
            (fun p ->
              let name = Prometheus.get_name p in
              let port = Prometheus.get_port p in
              sf
                {|
- name: %s
  type: prometheus
  access: proxy
  url: http://localhost:%d
|}
                name
                port)
            prometheus
      in
      Grafana.run ~sources () |> Lwt.map Option.some
    else Lwt.return_none
  in
  Log.info
    "Prometheus instances are now running. Write 'stop' in order to stop and \
     remove docker containers." ;
  while read_line () <> "stop" do
    ()
  done ;
  let* () = Lwt_list.iter_s Prometheus.shutdown prometheus in
  Option.map Grafana.shutdown g |> Option.value ~default:Lwt.return_unit

let register_clean_up_vms ~tags =
  Cloud.register
    ?vms:None
    ~__FILE__
    ~title:"Clean ups VMs manually"
    ~tags:("clean" :: "up" :: tags)
  @@ fun _cloud -> Jobs.clean_up_vms ()

let register_list_vms ~tags =
  Cloud.register
    ?vms:None
    ~__FILE__
    ~title:"List VMs"
    ~tags:("list" :: "vms" :: tags)
  @@ fun _cloud ->
  Log.info "TEZT_CLOUD environment variable found with value: %s" Env.tezt_cloud ;
  let* _ = Gcloud.list_vms ~prefix:Env.tezt_cloud in
  Lwt.return_unit

let register_create_dns_zone ~tags =
  Cloud.register
    ?vms:None
    ~__FILE__
    ~title:"Create a new DNS zone"
    ~tags:("create" :: "dns" :: "zone" :: tags)
  @@ fun _cloud ->
  let* domains = Env.dns_domains () in
  match domains with
  | [] ->
      Test.fail "You must specify the domains to use via --dns-domain option."
  | domains ->
      Lwt_list.iter_p
        (fun domain ->
          let* res = Gcloud.DNS.find_zone_for_subdomain domain in
          match res with
          | Some (zone, _) ->
              let* () = Gcloud.DNS.create_zone ~domain ~zone () in
              let* _ = Gcloud.DNS.describe ~zone () in
              unit
          | None -> unit)
        domains

let register_describe_dns_zone ~tags =
  Cloud.register
    ?vms:None
    ~__FILE__
    ~title:"Describe a new DNS zone"
    ~tags:("describe" :: "dns" :: "zone" :: tags)
  @@ fun _cloud ->
  let* zones = Gcloud.DNS.list_zones () in
  Lwt_list.iter_s
    (fun (zone, _) ->
      let* _ = Gcloud.DNS.describe ~zone () in
      unit)
    zones

let register_list_dns_domains ~tags =
  Cloud.register
    ?vms:None
    ~__FILE__
    ~title:"List the DNS domains currently in use"
    ~tags:("describe" :: "dns" :: "list" :: tags)
  @@ fun _cloud ->
  let* zones = Gcloud.DNS.list_zones () in
  Lwt_list.iter_s
    (fun (zone, _) ->
      let* _ = Gcloud.DNS.list ~zone () in
      unit)
    zones

let register_dns_add ~tags =
  Cloud.register
    ?vms:None
    ~__FILE__
    ~title:"Register a new DNS entry"
    ~tags:("dns" :: "add" :: tags)
  @@ fun _cloud ->
  let ip =
    match Cli.get_string_opt "ip" with
    | None -> Test.fail "You must provide an IP address via -a ip=<ip>"
    | Some ip -> ip
  in
  let domain =
    match Cli.get_string_opt "dns-domain" with
    | None ->
        Test.fail
          "You must provide a domain name via -a dns-domain=<domain>. The \
           format expected is the same one as the CLI argument '--dns-domain' \
           of tezt-cloud. "
    | Some domain -> domain
  in
  let* res = Gcloud.DNS.find_zone_for_subdomain domain in
  let zone =
    match res with
    | None -> Test.fail "No suitable zone for %s" domain
    | Some (zone, _) -> zone
  in
  Gcloud.DNS.add_subdomain ~zone ~name:domain ~value:ip

let register_dns_remove ~tags =
  Cloud.register
    ?vms:None
    ~__FILE__
    ~title:"Remove a DNS entry"
    ~tags:("dns" :: "remove" :: tags)
  @@ fun _cloud ->
  let* domains = Env.dns_domains () in
  Lwt_list.iter_s
    (fun domain ->
      let* res = Gcloud.DNS.find_zone_for_subdomain domain in
      let zone =
        match res with
        | None -> Test.fail "No suitable zone for %s" domain
        | Some (zone, _) -> zone
      in
      let* ip = Gcloud.DNS.get_value ~zone ~domain in
      match ip with
      | None -> Test.fail "No record found for the current domain"
      | Some ip ->
          let* () = Gcloud.DNS.remove_subdomain ~zone ~name:domain ~value:ip in
          unit)
    domains

let register ~tags =
  register_docker_push ~tags ;
  register_docker_build ~tags ;
  register_deploy_docker_registry ~tags ;
  register_destroy_vms ~tags ;
  register_prometheus_import ~tags ;
  register_clean_up_vms ~tags ;
  register_list_vms ~tags ;
  register_create_dns_zone ~tags ;
  register_describe_dns_zone ~tags ;
  register_list_dns_domains ~tags ;
  register_dns_add ~tags ;
  register_dns_remove ~tags

module Prometheus = struct
  let get_query_endpoint ~query = Prometheus.get_query_endpoint ~query
end

module Tezt_cloud_cli = struct
  module Types = Cli.Types

  let prometheus = Cli.prometheus

  let scenario_specific_json = Cli.scenario_specific

  let retrieve_daily_logs = Cli.retrieve_daily_logs

  let retrieve_ppx_profiling_traces = Cli.retrieve_ppx_profiling_traces

  let artifacts_dir = Cli.artifacts_dir

  let binaries_path = Cli.binaries_path

  let teztale_artifacts = Cli.teztale_artifacts

  let faketime = Cli.faketime

  let to_json_config = Cli.to_json_config

  let localhost = Cli.localhost

  let proxy = Cli.proxy

  let dns_domains = Cli.dns_domains
end

module Artifact_helpers = struct
  let parse_path path =
    let path = String.split_on_char '/' path in
    (* Check if the path is absolute or relative *)
    let starts_from_root = match path with "" :: _ -> true | _ -> false in
    (* Removes empty dir that would be the result of "//" *)
    let path = List.filter (( <> ) "") path in
    match path with
    | dir :: subpath when starts_from_root -> ("/" ^ dir) :: subpath
    | _ -> path

  let local_path path =
    List.fold_left
      (fun prefix subdir ->
        let prefix = prefix // subdir in
        if not (Sys.file_exists prefix) then Sys.mkdir prefix 0o755 ;
        prefix)
      ""
      path

  let prepare_artifacts ?scenario_config () =
    match Tezt_cloud_cli.artifacts_dir with
    | None -> ()
    | Some artifacts_dir ->
        (* Ensures the artifact directory exists and creates it otherwise *)
        let artifacts_dir = local_path (parse_path artifacts_dir) in
        let full_configuration =
          Tezt_cloud_cli.to_json_config ?scenario_config ()
        in
        let full_configuration_string =
          Data_encoding.Json.to_string full_configuration
        in
        write_file
          (artifacts_dir // "configuration.json")
          ~contents:full_configuration_string
end

module Gcloud = Gcloud
