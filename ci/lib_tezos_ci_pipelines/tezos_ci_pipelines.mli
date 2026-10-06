(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Global pipelines of this repository.

    Add jobs to those pipelines with [Cacio.register_jobs].
    They are registered with CIAO by [Cacio.close]. *)

(** Lints code in merge requests, checks that it compiles and runs tests. *)
val before_merging : Cacio.global_pipeline

(** A merge-train-specific version of {!before_merging}.

    Manual jobs are not allowed in this pipeline: it must finish ASAP so as to
    not block other MRs, so it does not make sense to have to wait on a manual
    action. *)
val merge_train : Cacio.global_pipeline

(** Add jobs to both the {!before_merging} and the {!merge_train} pipelines.

    Manual jobs are only added to {!before_merging}. *)
val register_merge_request_jobs : (Cacio.trigger * Cacio.job) list -> unit

(** Publishes artifacts (docs, static binaries) from master on each merge. *)
val master_branch : Cacio.global_pipeline

(** Updates the 'latest' tag of the Octez Docker distribution on Docker Hub. *)
val octez_latest_release : Cacio.global_pipeline

(** Dry-run pipeline for {!octez_latest_release}. *)
val octez_latest_release_test : Cacio.global_pipeline

(** Publishes a major Octez release, e.g. [octez-v1.0] or [octez-v2.0-rc4]. *)
val octez_major_release_tag : Cacio.global_pipeline

(** Publishes a minor Octez release, e.g. [octez-v1.2]. *)
val octez_minor_release_tag : Cacio.global_pipeline

(** Publishes a beta Octez release, e.g. [octez-v1.2-beta5]. *)
val octez_beta_release_tag : Cacio.global_pipeline

(** Dry-run pipeline for {!octez_major_release_tag}. *)
val octez_major_release_tag_test : Cacio.global_pipeline

(** Dry-run pipeline for {!octez_minor_release_tag}. *)
val octez_minor_release_tag_test : Cacio.global_pipeline

(** Dry-run pipeline for {!octez_beta_release_tag}. *)
val octez_beta_release_tag_test : Cacio.global_pipeline

(** Publishes a new revision of the packages of an Octez release. *)
val octez_packaging_revision : Cacio.global_pipeline

(** Dry-run pipeline for {!octez_packaging_revision}. *)
val octez_packaging_revision_test : Cacio.global_pipeline

(** Pipeline for tags that are not release tags. *)
val non_release_tag : Cacio.global_pipeline

(** Dry-run pipeline for {!non_release_tag}. *)
val non_release_tag_test : Cacio.global_pipeline

(** Add jobs to the release pipelines.

    This is equivalent to registering the jobs into both
    {!octez_major_release_tag} and {!octez_beta_release_tag}. *)
val register_release_jobs : (Cacio.trigger * Cacio.job) list -> unit

(** Add jobs to the test release pipelines.

    This is equivalent to registering the jobs into both
    {!octez_major_release_tag_test} and {!octez_beta_release_tag_test}. *)
val register_test_release_jobs : (Cacio.trigger * Cacio.job) list -> unit

(** Scheduled, full version of 'before_merging', daily on 'master'. *)
val schedule_extended_test : Cacio.global_pipeline

(** Daily pipeline containing all Debian jobs (build and extended tests). *)
val debian_daily : Cacio.global_pipeline

(** Daily pipeline containing all Homebrew jobs (build and extended tests). *)
val homebrew_daily : Cacio.global_pipeline

(** Refresh pipeline: rebuild the base images from scratch.

    TODO: https://gitlab.com/tezos/tezos/-/work_items/8367 *)
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
