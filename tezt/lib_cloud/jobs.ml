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

let docker_build =
  let cache = Hashtbl.create 11 in
  fun ?(docker_image = Agent.Configuration.Gcp {alias = Env.dockerfile_alias})
      ?(args = [])
      ~push
      ~ssh_public_key
      ()
    ->
    if Hashtbl.mem cache (docker_image, args) then (
      Log.info "Docker image is already built. Nothing to do" ;
      Lwt.return_unit)
    else (
      Hashtbl.replace cache (docker_image, args) () ;
      let alias =
        match docker_image with
        | Gcp {alias} -> alias
        | Octez_release _ -> "octez"
      in
      let dockerfile = Path.dockerfile ~alias in
      Log.info "Checking the existence of the docker file %s..." dockerfile ;
      if not (Sys.file_exists dockerfile) then
        Test.fail
          "Could not find the dockerfile %s. See README for more information \
           why this dockerfile is necessary."
          dockerfile ;
      Log.info "Checking the existence of the zcash parameters..." ;
      if (not (Sys.file_exists Path.zcash_params)) && alias = Env.tezt_cloud
      then
        Test.fail
          "Could not find the zcash parameters in %s. See README for more \
           information why such files are necessary."
          Path.zcash_params ;
      if
        (not (Sys.file_exists Path.dal_trusted_setup)) && alias = Env.tezt_cloud
      then
        Test.fail
          "Could not find the dal_trusted_setup in %s. See README for more \
           information why such files are necessary."
          Path.dal_trusted_setup ;
      (* For some docker images such as the one for Mac OS/X, it may
         be useful to get access to the commit of the current
         branch. *)
      let* commit =
        let* output = Process.run_and_read_stdout "git" ["rev-parse"; "HEAD"] in
        Lwt.return (String.trim output)
      in
      let auto_args =
        match docker_image with
        | Gcp _ ->
            [
              ("SSH_PUBLIC_KEY", ssh_public_key);
              ("ZCASH_PARAMS_PATH", Path.zcash_params);
              ("DAL_TRUSTED_SETUP_PATH", Path.dal_trusted_setup);
              ("BINARIES_DESTINATION_PATH", Env.binaries_path);
              ("COMMIT", commit);
            ]
        | Octez_release {tag} ->
            [("SSH_PUBLIC_KEY", ssh_public_key); ("RELEASE_TAG", tag)]
      in
      let args = auto_args @ args in
      Log.info "Building image from %s..." Env.tezt_cloud ;
      let* () = Docker.build ~alias ~args () |> Process.check in
      let* () =
        if push then (
          Log.info "Compute the registry uri..." ;
          let* registry_uri = Env.registry_uri () in
          Log.info "Tagging the image..." ;
          let* () = Docker.tag ~alias ~registry_uri () |> Process.check in
          Log.info "Push the image..." ;
          let* () = Docker.push ~alias ~registry_uri () |> Process.check in
          Lwt.return_unit)
        else Lwt.return_unit
      in
      unit)

let deploy_docker_registry () =
  Log.info "Tezt_Cloud found with value: %s" Env.tezt_cloud ;
  let* () = Terraform.Docker_registry.init () in
  let tezt_cloud = Env.tezt_cloud in
  let* project_id = Gcloud.project_id () in
  Terraform.Docker_registry.deploy ~tezt_cloud ~project_id

let clean_up_vms () =
  let tezt_cloud = Env.tezt_cloud in
  let* workspaces = Terraform.VM.Workspace.list ~tezt_cloud in
  let ssh_private_key_filename = Env.ssh_private_key_filename () in
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
                  let* output =
                    Gcloud.compute_ssh
                      ~zone
                      ~vm_name
                      ~ssh_private_key_filename
                      "docker"
                      ["ps"; "--format"; "{{.Names}}"]
                    |> Process.check_and_read_stdout
                  in
                  let images_name =
                    String.split_on_char '\n' output
                    |> List.filter (fun str -> str <> "")
                  in
                  let is_main_image image_name =
                    (* The main image created by Terraform at the
                       moment contains "--" in its name. This enables
                       to identify this image uniquely. While this is
                       not very robust, it should work for now. *)
                    let re = Str.regexp_string "--" in
                    try
                      ignore (Str.search_forward re image_name 0) ;
                      true
                    with Not_found -> false
                  in
                  let main_images, other_images =
                    List.partition is_main_image images_name
                  in
                  if List.length main_images <> 1 then
                    Test.fail
                      "Unexpected setting. All the docker images found: %s. \
                       There should only be one image which contains '--' in \
                       the list"
                      (String.concat ";" images_name) ;
                  let main_image = List.hd main_images in
                  let* _ =
                    Gcloud.compute_ssh
                      ~zone
                      ~vm_name
                      ~ssh_private_key_filename
                      "docker"
                      ["stop"; main_image]
                    |> Process.check
                  in
                  let* _ =
                    Gcloud.compute_ssh
                      ~zone
                      ~vm_name
                      ~ssh_private_key_filename
                      "docker"
                      ["start"; main_image]
                    |> Process.check
                  in
                  let* () =
                    other_images
                    |> Lwt_list.iter_p (fun image ->
                           let* _ =
                             Gcloud.compute_ssh
                               ~zone
                               ~vm_name
                               ~ssh_private_key_filename
                               "docker"
                               ["kill"; image]
                             |> Process.check
                           in
                           Lwt.return_unit)
                  in
                  Lwt.return_unit)
         in
         unit)
