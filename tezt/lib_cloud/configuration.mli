(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type vm = private {
  machine_type : string;
  docker_image : Env.docker_image;
  max_run_duration : int option;
  binaries_path : string;
  os : Types.Os.t;
}

type t = private {name : string; vm : vm}

val make :
  ?os:Types.Os.t ->
  ?binaries_path:string ->
  ?max_run_duration:int ->
  ?machine_type:string ->
  ?docker_image:Env.docker_image ->
  ?name:string ->
  unit ->
  t
