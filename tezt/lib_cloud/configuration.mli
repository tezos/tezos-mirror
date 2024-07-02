(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type docker_image =
  | Custom of {tezt_cloud : string}
  | Image of {docker_image : string}

val string_of_docker_image : project_id:string -> docker_image -> string

type t = private {machine_type : string; docker_image : docker_image}

val make : ?machine_type:string -> ?docker_image:docker_image -> unit -> t
