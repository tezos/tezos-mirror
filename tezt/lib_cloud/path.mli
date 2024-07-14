(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Path to the directory containing the terraform script to setup the
    docker registry. *)
val terraform_docker_registry : string

(** Path to the directory containing the terraform script to generate
    the bucket storing the terraform state. *)
val terraform_state_bucket : string

(** Path to the directory containing the terraform script to generate
    the VMs. *)
val terraform_vm : string

(** Path to the directory containing dockerfiles for each user. *)
val dockerfile : alias:string -> string

(** Path to the zcash_params directory that can be used for the
    dockerfiles. *)
val zcash_params : string

(** Path to the DAL trusted setup directory that can be used for the
  dockerfiles.*)
val dal_trusted_setup : string

(** Default path where images can be found on the docker container. *)
val default_binaries_path : unit -> string

(** Path to the current binary. *)
val self : string

(** Path where to find the encoding of tezt cloud deployement. *)
val proxy_deployement : tezt_cloud:string -> string

(** Path where are stored grafana dashboards. *)
val grafana_dashboards : string
