(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let workspace =
  (* This is a lazy value to be sure that this is evaluated only inside a Tezt test. *)
  Lazy.from_fun @@ fun () ->
  match Sys.getenv_opt "TF_WORKSPACE" with
  | None ->
      Test.fail
        "The environment variable 'TF_WORKSPACE' is not defined. See README \
         for more information why this variable must be defined."
  | Some value -> value

let ssh_private_key =
  Lazy.from_fun (fun () ->
      let home = Sys.getenv "HOME" in
      let workspace = Lazy.force workspace in
      home // ".ssh" // Format.asprintf "%s-tf" workspace)

let ssh_public_key =
  Lazy.from_fun (fun () ->
      let ssh_key = Lazy.force ssh_private_key in
      Format.asprintf "%s.pub" ssh_key)

let dockerfile =
  Lazy.from_fun (fun () ->
      let workspace = Lazy.force workspace in
      Path.docker // Format.asprintf "%s.Dockerfile" workspace)
