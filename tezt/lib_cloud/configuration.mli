(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type t = private {
  machine_type : string;
  docker_image : Env.docker_image;
  max_run_duration : int option;
  binaries_path : string;
  os : string;
}

val make :
  ?os:string ->
  ?binaries_path:string ->
  ?max_run_duration:int ->
  ?machine_type:string ->
  ?docker_image:Env.docker_image ->
  unit ->
  t
