(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Global pipelines of this repository.

    Add jobs to those pipelines with [Cacio.register_jobs].
    They are registered with CIAO by [Cacio.close]. *)

(** Scheduled, full version of 'before_merging', daily on 'master'. *)
val schedule_extended_test : Cacio.global_pipeline

(** Daily pipeline containing all Debian jobs (build and extended tests). *)
val debian_daily : Cacio.global_pipeline

(** Daily pipeline containing all Homebrew jobs (build and extended tests). *)
val homebrew_daily : Cacio.global_pipeline

(** Refresh pipeline: rebuild the base images from scratch. *)
val base_images_refresh : Cacio.global_pipeline

(** Daily pipeline containing all Base Images jobs (build and merge). *)
val base_images_daily : Cacio.global_pipeline

(** Scheduled run of all tezt tests with external RPC servers. *)
val schedule_extended_rpc_test : Cacio.global_pipeline

(** Scheduled run of all tezt tests with single-process validation. *)
val schedule_extended_validation_test : Cacio.global_pipeline

(** Scheduled run of all tezt tests with baker using remote node. *)
val schedule_extended_baker_remote_mode_test : Cacio.global_pipeline

(** Scheduled run of all tezt tests with dal using baker commands. *)
val schedule_extended_dal_use_baker : Cacio.global_pipeline

(** Add jobs to all the "custom extended test" pipelines.

    Those pipelines all contain exactly the same jobs: they run the same tests
    with different options. *)
val register_custom_extended_test_jobs :
  (Cacio.trigger * Cacio.job) list -> unit

(** Scheduled pipeline that runs a test release pipeline. *)
val schedule_test_release : Cacio.global_pipeline

(** Scheduled pipeline for various security scans. *)
val schedule_security_scans : Cacio.global_pipeline

(** Scheduled pipeline publishing a dated master Docker image to Docker Hub. *)
val schedule_docker_master_snapshot : Cacio.global_pipeline

(** Scheduled pipeline that rebuilds Docker images, skipping any cache. *)
val schedule_docker_build_pipeline : Cacio.global_pipeline

(** Pipeline that updates and publishes the test release page. *)
val publish_test_release_page : Cacio.global_pipeline

(** Pipeline that updates and publishes the release page. *)
val publish_release_page : Cacio.global_pipeline
