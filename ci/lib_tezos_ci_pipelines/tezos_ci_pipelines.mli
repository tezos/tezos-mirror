(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Global pipelines of this repository.

    Add jobs to those pipelines with [Cacio.register_jobs].
    They are registered with CIAO by [Cacio.close]. *)

(** Scheduled pipeline publishing a dated master Docker image to Docker Hub. *)
val schedule_docker_master_snapshot : Cacio.global_pipeline

(** Scheduled pipeline that rebuilds Docker images, skipping any cache. *)
val schedule_docker_build_pipeline : Cacio.global_pipeline

(** Pipeline that updates and publishes the test release page. *)
val publish_test_release_page : Cacio.global_pipeline

(** Pipeline that updates and publishes the release page. *)
val publish_release_page : Cacio.global_pipeline
