(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* This module defines the jobs of the [release_tag] family of pipelines.

   These pipeline runs on each pushes to the various release tags (see
   [main.ml] for the set of regular expressions that define the
   language of release tags).

   The goal of these pipelines is to create
   {{:https://gitlab.com/tezos/tezos/-/releases}Octez releases on
   GitLab}, the associated artifacts, and to push releases to opam. *)

open Tezos_ci
open Common

(** Type of release tag pipelines.

    The semantics of the type is summed up in this table:

   |                       | Release_tag | Beta_release_tag | Non_release_tag |
   |-----------------------+-------------+------------------+-----------------|
   | GitLab release type   | Release     | Release          | Create          |
   | Experimental binaries | No          | No               | No              |
   | Docker build type     | Release     | Release          | Release         |
   | Publishes to opam     | Yes         | No               | No              |

    - All release tag pipelines types publish [Release] type Docker builds.
    - No release tag pipelines include experimental binaries.
    - [Release_tag] and [Beta_release_tag] pipelines creates GitLab
    and publishes releases. [Non_release_tag] pipelines create the
    GitLab release but do not publish them.
    - Only [Release_tag] pipelines publish to opam. *)
type release_tag_pipeline_type =
  | Release_tag
  | Beta_release_tag
  | Non_release_tag

(** Create an Octez release tag pipeline of type {!release_tag_pipeline_type}.

    If [test] is true (default is [false]), then the Docker images are
    built of the [Test] type and are published to the GitLab registry
    instead of Docker hub.

    On release pipelines these jobs can start immediately *)
let octez_jobs ?(test = false) release_tag_pipeline_type =
  let job_docker_amd64 =
    job_docker_build
      ~dependencies:(Dependent [])
      ~__POS__
      ~arch:Amd64
      (if test then Test else Release)
  in
  let job_docker_arm64 =
    job_docker_build
      ~dependencies:(Dependent [])
      ~__POS__
      ~arch:Arm64
      (if test then Test else Release)
  in
  let job_docker_merge =
    job_docker_merge_manifests
      ~__POS__
      ~ci_docker_hub:(not test)
      ~job_docker_amd64
      ~job_docker_arm64
  in
  (* on release pipelines the static binaries do not have any dependencies
     on previous stages and can start immediately *)
  let job_static_arm64_release =
    job_build_static_binaries
      ~dependencies:(Dependent [])
      ~__POS__
      ~arch:Arm64
      ~release:true
      ()
  in
  let job_static_x86_64_release =
    job_build_static_binaries
      ~dependencies:(Dependent [])
      ~__POS__
      ~arch:Amd64
      ~release:true
      ()
  in
  let job_gitlab_release ~dependencies : Tezos_ci.tezos_job =
    job
      ~__POS__
      ~image:Images.ci_release
      ~stage:Stages.publish_release_gitlab
      ~interruptible:false
      ~dependencies
      ~name:"gitlab:release"
      [
        "./scripts/ci/restrict_export_to_octez_source.sh";
        "./scripts/ci/gitlab-release.sh";
      ]
  in
  let job_gitlab_publish ~dependencies : Tezos_ci.tezos_job =
    job
      ~__POS__
      ~image:Images.ci_release
      ~stage:Stages.publish_package_gitlab
      ~interruptible:false
      ~dependencies
      ~name:"gitlab:publish"
      ["${CI_PROJECT_DIR}/scripts/ci/create_gitlab_package.sh"]
  in
  let job_build_rpm_amd64 = job_build_rpm_amd64 () in
  let ( jobs_debian_repository,
        job_build_ubuntu_package_current_a,
        job_build_debian_package_current_a,
        job_build_ubuntu_package_current_b,
        job_build_debian_package_current_b ) =
    Debian_repository.jobs Release
  in
  let job_gitlab_release_or_publish =
    let dependencies =
      Dependent
        [
          Artifacts job_static_x86_64_release;
          Artifacts job_static_arm64_release;
          Artifacts job_build_rpm_amd64;
          Artifacts job_build_ubuntu_package_current_a;
          Artifacts job_build_debian_package_current_a;
          Artifacts job_build_ubuntu_package_current_b;
          Artifacts job_build_debian_package_current_b;
        ]
    in
    match release_tag_pipeline_type with
    | Non_release_tag -> job_gitlab_publish ~dependencies
    | _ -> job_gitlab_release ~dependencies
  in
  let job_opam_release ?(dry_run = false) () : Tezos_ci.tezos_job =
    job
      ~__POS__
      ~image:Images.CI.prebuild
      ~stage:Stages.publish_release
      ~interruptible:false
      ~name:"opam:release"
      [("./scripts/ci/opam-release.sh" ^ if dry_run then " --dry-run" else "")]
  in
  let job_promote_to_latest_test =
    Common.job_docker_promote_to_latest
      ~ci_docker_hub:false
      ~dependencies:(Dependent [Job job_docker_merge])
      ()
  in
  [
    (* Stage: sanity *)
    job_datadog_pipeline_trace;
    (* Stage: build *)
    job_static_x86_64_release;
    job_static_arm64_release;
    job_docker_amd64;
    job_docker_arm64;
    job_build_rpm_amd64;
    job_docker_merge;
    job_gitlab_release_or_publish;
  ]
  @ jobs_debian_repository
  @
  match (test, release_tag_pipeline_type) with
  (* for the moment the apt repository are not official, so we do not add to the release
     pipeline . *)
  | false, Release_tag -> [job_opam_release ()]
  | true, Release_tag ->
      [
        (* This job normally runs in the {!Octez_latest_release} pipeline
           that is triggered manually after a release is made. However, to
           make release testing easier, we include it here directly. Thus,
           release testers are not required to trigger two separate pipelines
           (indeed, the second `latest_release_test` pipeline is rarely tested). *)
        job_promote_to_latest_test;
        job_opam_release ~dry_run:true ();
      ]
  | _ -> []

(** Create an etherlink release tag pipeline of type {!release_tag_pipeline_type}. *)
let octez_evm_node_jobs ?(test = false) () =
  let job_docker_amd64 =
    job_docker_build
      ~__POS__
      ~arch:Amd64
      (if test then Test else Octez_evm_node_release)
  in
  let job_docker_arm64 =
    job_docker_build
      ~__POS__
      ~arch:Arm64
      (if test then Test else Octez_evm_node_release)
  in
  let job_docker_merge =
    job_docker_merge_manifests
      ~__POS__
      ~ci_docker_hub:(not test)
      ~job_docker_amd64
      ~job_docker_arm64
  in
  let job_static_x86_64_release =
    job_build_static_binaries
      ~__POS__
      ~arch:Amd64
      ~executable_files:"script-inputs/octez-evm-node-executable"
      ~release:true
      ~version_executable:"octez-evm-node"
      ()
  in
  let job_static_arm64_release =
    job_build_static_binaries
      ~__POS__
      ~arch:Arm64
      ~executable_files:"script-inputs/octez-evm-node-executable"
      ~release:true
      ~version_executable:"octez-evm-node"
      ()
  in

  let job_gitlab_release : Tezos_ci.tezos_job =
    let dependencies =
      Dependent
        [
          Artifacts job_static_x86_64_release; Artifacts job_static_arm64_release;
        ]
    in
    job
      ~__POS__
      ~image:Images.ci_release
      ~stage:Stages.publish_release_gitlab
      ~interruptible:false
      ~dependencies
      ~name:"gitlab:octez-evm-node-release"
      ["./scripts/ci/create_gitlab_octez_evm_node_release.sh"]
  in
  [
    job_static_arm64_release;
    job_static_x86_64_release;
    job_docker_amd64;
    job_docker_arm64;
    job_docker_merge;
    job_gitlab_release;
  ]
