(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let tezt_cloud =
  (* This is a lazy value to be sure that this is evaluated only inside a Tezt test. *)
  Lazy.from_fun @@ fun () ->
  match Sys.getenv_opt "TEZT_CLOUD" with
  | None ->
      Test.fail
        "The environment variable 'TEZT_CLOUD' is not defined. See README for \
         more information why this variable must be defined."
  | Some value -> value

let ssh_private_key =
  Lazy.from_fun (fun () ->
      let home = Sys.getenv "HOME" in
      let tezt_cloud = Lazy.force tezt_cloud in
      home // ".ssh" // Format.asprintf "%s-tf" tezt_cloud)

let ssh_public_key =
  Lazy.from_fun (fun () ->
      let ssh_key = Lazy.force ssh_private_key in
      Format.asprintf "%s.pub" ssh_key)

let dockerfile =
  Lazy.from_fun (fun () ->
      let tezt_cloud = Lazy.force tezt_cloud in
      let basename = Option.value ~default:tezt_cloud Cli.dockerfile in
      Path.docker // Format.asprintf "%s.Dockerfile" basename)

let docker_registry =
  Lazy.from_fun (fun () ->
      let tezt_cloud = Lazy.force tezt_cloud in
      Format.asprintf "%s-docker-registry" tezt_cloud)

let custom_docker_image ?docker_image_name ~project_id () =
  let artifact_registry = "europe-west1-docker.pkg.dev" in
  let docker_registry = Lazy.force docker_registry in
  let docker_image_name =
    Option.value ~default:(Lazy.force tezt_cloud) docker_image_name
  in
  Format.asprintf
    "%s/%s/%s/%s"
    artifact_registry
    project_id
    docker_registry
    docker_image_name
