(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Agent = Agent

module Configuration = struct
  include Env
  include Configuration
end

module Cloud = Cloud

let register_docker_push ~tags =
  Cloud.register
    ?vms:None
    ~__FILE__
    ~title:"Push the dockerfile to the GCP registry"
    ~tags:("docker" :: "push" :: tags)
  @@ fun _cloud -> Jobs.docker_build ~push:true ()

let register_docker_build ~tags =
  Cloud.register
    ?vms:None
    ~__FILE__
    ~title:"Build the dockerfile"
    ~tags:("docker" :: "build" :: tags)
  @@ fun _cloud -> Jobs.docker_build ~push:false ()

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
    ~title:"Import a snapshot into a prometheus container"
    ~tags:("prometheus" :: "import" :: tags)
  @@ fun _cloud ->
  let* prometheus = Prometheus.run_with_snapshot () in
  Prometheus.shutdown prometheus

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
  match Env.dns_domains with
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
  let* _ = Gcloud.DNS.describe ~zone:"tezt-cloud" () in
  unit

let register_dns_add ~tags =
  Cloud.register
    ?vms:None
    ~__FILE__
    ~title:"Register a new DNS entry"
    ~tags:("dns" :: "add" :: tags)
  @@ fun _cloud ->
  let tezt_cloud = Env.tezt_cloud in
  let ip = Cli.get_string_opt "ip" in
  match ip with
  | None -> Test.fail "You must provide an IP address via -a ip=<ip>"
  | Some ip ->
      let* () =
        Gcloud.DNS.add_subdomain ~name:tezt_cloud ~zone:"tezt-cloud" ~value:ip
      in
      unit

let register_dns_remove ~tags =
  Cloud.register
    ?vms:None
    ~__FILE__
    ~title:"Remove a DNS entry"
    ~tags:("dns" :: "remove" :: tags)
  @@ fun _cloud ->
  let name = Env.tezt_cloud in
  let* ip = Gcloud.DNS.get_value ~zone:"tezt-cloud" ~domain:name in
  match ip with
  | None -> Test.fail "No record found for the current domain"
  | Some ip ->
      let* () =
        Gcloud.DNS.remove_subdomain ~name ~zone:"tezt-cloud" ~value:ip
      in
      unit

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
  register_dns_add ~tags ;
  register_dns_remove ~tags
