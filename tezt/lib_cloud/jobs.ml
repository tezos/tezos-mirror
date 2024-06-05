(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Small twist to override here the Cli module from Tezt. *)
module C = Cli
open Tezt
module Cli = C

let docker_push ~tags =
  Test.register
    ~__FILE__
    ~title:"Push the dockerfile to the registry"
    ~tags:("docker" :: "push" :: tags)
  @@ fun () ->
  let tezt_cloud = Lazy.force Env.tezt_cloud in
  Log.info "TEZT_CLOUD_BASENAME variable found with value: %s" tezt_cloud ;
  let ssh_public_key = Lazy.force Env.ssh_public_key in
  Log.info "Checking the existence of ssh public key '%s'..." ssh_public_key ;
  let* ssh_public_key =
    let ssh_public_key_file = Lazy.force Env.ssh_public_key in
    if not (Sys.file_exists ssh_public_key_file) then
      Test.fail
        "Could not find SSH key named %s. See READNE for more information why \
         this ssh key is necessary."
        ssh_public_key
    else Process.run_and_read_stdout ~name:"cat" "cat" [ssh_public_key_file]
  in
  Log.info
    "Checking the existence of the docker file %s.Dockerfile..."
    tezt_cloud ;
  let dockerfile = Lazy.force Env.dockerfile in
  if not (Sys.file_exists dockerfile) then
    Test.fail
      "Could not find the dockerfile %s. See README for more information why \
       this dockerfile is necessary."
      dockerfile ;
  Log.info "Checking the existence of the zcash parameters..." ;
  if not (Sys.file_exists Path.zcash_params) then
    Test.fail
      "Could not find the zcash parameters in %s. See READEM for more \
       information why such files are necessary."
      Path.zcash_params ;
  Log.info "Initializing terraform docker state..." ;
  let* () = Terraform.Docker_registry.init () in
  Log.info "Fetching or creating the remote docker registry..." ;
  let* hostname = Terraform.Docker_registry.get_hostname () in
  let* docker_registry = Terraform.Docker_registry.get_docker_registry () in
  Log.info "Authenticate docker hostname..." ;
  let* () = Gcloud.auth_configure_docker ~hostname in
  let args =
    [
      ("SSH_PUBLIC_KEY", ssh_public_key);
      ("ZCASH_PARAMS_PATH", Path.zcash_params);
      ("DAL_TRUSTED_SETUP_PATH", Path.dal_trusted_setup);
      ("BINARIES_DESTINATION_PATH", Agent.default_binaries_path ());
    ]
  in
  Log.info "Building image from %s.Dockerfile..." tezt_cloud ;
  let*! () = Docker.build ~dockerfile ~args () in
  Log.info "Tagging the image..." ;
  let*! () = Docker.tag docker_registry in
  Log.info "Push the image..." ;
  let*! () = Docker.push docker_registry in
  unit

let deploy_docker_registry ~tags =
  Test.register
    ~__FILE__
    ~title:"Deploy docker registry"
    ~tags:("docker" :: "registry" :: "deploy" :: tags)
  @@ fun () ->
  let tezt_cloud = Lazy.force Env.tezt_cloud in
  Log.info "Tezt_Cloud found with value: %s" tezt_cloud ;
  let* () = Terraform.Docker_registry.init () in
  Terraform.Docker_registry.deploy ()

let deploy_terraform_state_bucket ~tags =
  Test.register
    ~__FILE__
    ~title:"Deploy terraform state bucket"
    ~tags:("terraform" :: "state" :: "bucket" :: "deploy" :: tags)
  @@ fun () ->
  let tezt_cloud = Lazy.force Env.tezt_cloud in
  Log.info "Tezt_Cloud found with value: %s" tezt_cloud ;
  let* () = Terraform.State_bucket.init () in
  Terraform.State_bucket.deploy ()

let destroy_vms ~tags =
  Test.register
    ~__FILE__
    ~title:"Destroy terraform VMs"
    ~tags:("terraform" :: "destroy" :: tags)
  @@ fun () ->
  let tezt_cloud = Lazy.force Env.tezt_cloud in
  Log.info "Tezt_Cloud found with value: %s" tezt_cloud ;
  let* () = Terraform.VM.Workspace.select "default" in
  let* workspaces = Terraform.VM.Workspace.get () in
  let* () = Terraform.VM.destroy workspaces in
  Terraform.VM.Workspace.destroy ()

let prometheus_import ~tags =
  Test.register
    ~__FILE__
    ~title:"Import a snapshot into a prometheus container"
    ~tags:("prometheus" :: "import" :: tags)
  @@ fun () ->
  match Cli.prometheus_snapshot with
  | None ->
      Test.fail "Use --prometheus-snapshot to provide the snapshot filename"
  | Some filename ->
      let* prometheus =
        Prometheus.run_with_snapshot
          ~snapshot:(Cli.prometheus_snapshot_directory // filename)
          ~port:9090
      in
      Log.info "You can find the prometheus instance at http://localhost:9090" ;
      Log.info
        "Use Ctrl+C to end the scenario and kill the prometheus instance." ;
      Prometheus.shutdown prometheus

let clean_up_vms ~tags =
  Test.register
    ~__FILE__
    ~title:"Clean ups VMs manually"
    ~tags:("clean" :: "up" :: tags)
  @@ fun () ->
  let* workspaces = Terraform.VM.Workspace.get () in
  workspaces
  |> Lwt_list.iter_s (fun workspace ->
         let* () = Terraform.VM.Workspace.select workspace in
         let* () = Terraform.VM.init () in
         let* points = Terraform.VM.points () in
         let n = List.length points in
         let names =
           Seq.ints 1 |> Seq.take n
           |> Seq.map (fun i -> Format.asprintf "%s-%03d" workspace i)
           |> List.of_seq
         in
         let* zone = Terraform.VM.zone () in
         (* We restart the main docker image and kill/remove all the other ones. *)
         let* () =
           names
           |> Lwt_list.iter_p (fun vm_name ->
                  let*! output =
                    Gcloud.compute_ssh
                      ~zone
                      ~vm_name
                      "docker"
                      ["ps"; "--format"; "{{.Names}}"]
                  in
                  let images_name =
                    String.split_on_char '\n' output
                    |> List.filter (fun str -> str <> "")
                  in
                  let main_image, other_images =
                    List.partition (fun str -> str <> "netdata") images_name
                  in
                  if List.length main_image <> 1 then
                    Test.fail
                      "Unexpected setting. All the docker images found: %s. \
                       There should only be one image which is not 'netdata' \
                       in this list"
                      (String.concat ";" images_name) ;
                  let main_image = List.hd main_image in
                  let*! _ =
                    Gcloud.compute_ssh
                      ~zone
                      ~vm_name
                      "docker"
                      ["stop"; main_image]
                  in
                  let*! _ =
                    Gcloud.compute_ssh
                      ~zone
                      ~vm_name
                      "docker"
                      ["start"; main_image]
                  in
                  let* () =
                    other_images
                    |> Lwt_list.iter_p (fun image ->
                           let*! _ =
                             Gcloud.compute_ssh
                               ~zone
                               ~vm_name
                               "docker"
                               ["kill"; image]
                           in
                           let*! _ =
                             Gcloud.compute_ssh
                               ~zone
                               ~vm_name
                               "docker"
                               ["rm"; image]
                           in
                           Lwt.return_unit)
                  in
                  Lwt.return_unit)
         in
         unit)

let list_vms ~tags =
  Test.register ~__FILE__ ~title:"List VMs" ~tags:("list" :: "vms" :: tags)
  @@ fun () ->
  let tezt_cloud = Lazy.force Env.tezt_cloud in
  Log.info "TEZT_CLOUD environment variable found with value: %s" tezt_cloud ;
  let* _ = Gcloud.list_vms ~prefix:tezt_cloud in
  Lwt.return_unit

let simple ~tags =
  Cloud.register
    ~vms:[{machine_type = Cli.machine_type}; {machine_type = Cli.machine_type}]
    ~__FILE__
    ~tags:("simple" :: "health" :: tags)
    ~title:"Simple health check to check local configuration"
  @@ fun t ->
  let agents = Cloud.agents t in
  let agent0 = List.nth agents 0 in
  let agent1 = List.nth agents 1 in
  let* output =
    Process.spawn
      ~name:"agent0"
      ~runner:(Agent.runner agent0)
      "echo"
      ["Hello world"]
    |> Process.check_and_read_stdout
  in
  Log.info "%s from agent 0" (String.trim output) ;
  let* output =
    Process.spawn
      ~name:"agent1"
      ~runner:(Agent.runner agent1)
      "echo"
      ["Hello world"]
    |> Process.check_and_read_stdout
  in
  Log.info "%s from agent 1" (String.trim output) ;
  unit

let register ~tags =
  docker_push ~tags ;
  deploy_docker_registry ~tags ;
  deploy_terraform_state_bucket ~tags ;
  destroy_vms ~tags ;
  prometheus_import ~tags ;
  clean_up_vms ~tags ;
  list_vms ~tags ;
  simple ~tags
