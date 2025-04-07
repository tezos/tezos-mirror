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
    dockerfiles. *)
val dal_trusted_setup : string

(** Path to the current binary. *)
val self : string

(** Path where to find the encoding of tezt cloud deployement. *)
val proxy_deployement : tezt_cloud:string -> string

(** Path where grafana dashboards are stored. *)
val grafana_dashboards : string

(** Path where the website index is stored. *)
val website_index : string

(** CSS file for the website. *)
val website_style : string

(** Path where the prometheus configuration file is stored. *)
val prometheus_configuration : string

(** Path where the prometheus rules file is stored. *)
val prometheus_rules_configuration : string

(** Path where the alert manager configuration file is stored. *)
val alert_manager_configuration : string
