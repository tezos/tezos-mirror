(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Job [oc.build-data_packages]. *)
val job_build_data_packages : Cacio.job

(** Job [oc.build-debian]. *)
val job_build_debian : Common.Packaging.repository_pipeline -> Cacio.job

(** Job [oc.build-ubuntu]. *)
val job_build_ubuntu : Common.Packaging.repository_pipeline -> Cacio.job

(** Job [apt_repo_debian]. *)
val job_apt_repo_debian : Common.Packaging.repository_pipeline -> Cacio.job

(** Job [apt_repo_ubuntu]. *)
val job_apt_repo_ubuntu : Common.Packaging.repository_pipeline -> Cacio.job

(** Child pipeline [debian_repository_partial]. *)
val child_pipeline_partial : Tezos_ci.Pipeline.child_pipeline

(** Child pipeline [debian_repository_partial_auto]. *)
val child_pipeline_partial_auto : Tezos_ci.Pipeline.child_pipeline
