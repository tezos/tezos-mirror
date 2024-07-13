(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type docker_image =
  | Custom of {tezt_cloud : string}
  | Image of {docker_image : string}

let string_of_docker_image ~project_id = function
  | Custom {tezt_cloud} ->
      Env.custom_docker_image ~docker_image_name:tezt_cloud ~project_id ()
  | Image {docker_image} -> docker_image

type t = {
  machine_type : string;
  docker_image : docker_image;
  max_run_duration : int option;
}

let default_docker_image =
  match (Cli.dockerfile, Cli.localhost) with
  | None, false ->
      let tezt_cloud = Lazy.force Env.tezt_cloud in
      Custom {tezt_cloud}
  | None, true ->
      let tezt_cloud = Lazy.force Env.tezt_cloud in
      let docker_image = Format.asprintf "%s:latest" tezt_cloud in
      Image {docker_image}
  | Some tezt_cloud, false -> Custom {tezt_cloud}
  | Some tezt_cloud, true ->
      let docker_image = Format.asprintf "%s:latest" tezt_cloud in
      Image {docker_image}

let make ?max_run_duration ?(machine_type = Cli.machine_type) ?docker_image () =
  let docker_image = Option.value ~default:default_docker_image docker_image in
  let max_run_duration =
    match max_run_duration with
    | None ->
        if Cli.no_max_run_duration then None else Some Cli.max_run_duration
    | Some max_run_duration -> Some max_run_duration
  in
  {machine_type; docker_image; max_run_duration}
