(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type t = {
  machine_type : string;
  docker_image : Env.docker_image;
  max_run_duration : int option;
  binaries_path : string;
  os : string;
}

let make ?os ?binaries_path ?max_run_duration ?machine_type ?docker_image () =
  let os = Option.value ~default:Env.os os in
  let docker_image = Option.value ~default:Env.docker_image docker_image in
  let machine_type = Option.value ~default:Env.machine_type machine_type in
  let default_binaries_path =
    match docker_image with
    | Env.Gcp _ -> Path.default_binaries_path ()
    | Octez_release _ -> "/usr/local/bin"
  in
  let binaries_path =
    Option.value ~default:default_binaries_path binaries_path
  in
  let max_run_duration =
    match max_run_duration with
    | None ->
        if Env.no_max_run_duration then None else Some Env.max_run_duration
    | Some max_run_duration -> Some max_run_duration
  in
  {os; machine_type; docker_image; max_run_duration; binaries_path}
