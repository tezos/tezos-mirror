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
