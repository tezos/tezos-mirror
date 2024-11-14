(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let ( // ) = Tezt.Base.( // )

(* All paths are given relatively to the Tezos project root. *)

let project = "tezt" // "lib_cloud"

let terraform = project // "terraform"

let terraform_docker_registry = terraform // "docker-registry"

let terraform_state_bucket = terraform // "state-bucket"

let terraform_vm = terraform // "vm"

let dockerfile ~alias =
  project // "dockerfiles" // Format.asprintf "%s.Dockerfile" alias

let zcash_params = "_opam" // "share" // "zcash-params"

let dal_trusted_setup = "_opam" // "share" // "dal-trusted-setup"

let default_binaries_path () = Filename.get_temp_dir_name () // "tezt-runners"

let self = Sys.argv.(0)

let proxy_deployement ~tezt_cloud =
  Filename.get_temp_dir_name ()
  // Format.asprintf "%s-tezt-cloud-deployement" tezt_cloud

let grafana_dashboards = project // "grafana" // "dashboards"
