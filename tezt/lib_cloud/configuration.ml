(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type docker_image = 
| Custom of {tezt_cloud:string}
| Image of {docker_image:string}

type t = {machine_type : string; docker_image : docker_image}

let default_docker_image =
  match Cli.dockerfile with
  | None ->
      let tezt_cloud = Lazy.force Env.tezt_cloud in
      Custom {tezt_cloud}
  | Some tezt_cloud -> Custom {tezt_cloud}


let make ?(machine_type = Cli.machine_type) ?docker_image () =  
  let docker_image = Option.value ~default:default_docker_image docker_image in
   {machine_type;docker_image}
