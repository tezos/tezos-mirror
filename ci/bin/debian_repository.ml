(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* This module defines the jobs of the [debian_repository] child
   pipeline.

   This pipeline builds the post-v19.1 Debian (and Ubuntu)
   packages. *)

open Gitlab_ci.Types
open Gitlab_ci.Util
open Tezos_ci
open Common

let build_debian_packages_image =
  Image.mk_external ~image_path:"$DEP_IMAGE:${CI_COMMIT_REF_SLUG}"

(** These are the set of Debian release-architecture combinations for
    which we build deb packages in the job
    [job_build_debian_package]. A dependency image will be built once
    for each combination of [RELEASE] and [TAGS]. *)
let debian_package_release_matrix =
  [[("RELEASE", ["unstable"; "bookworm"]); ("TAGS", ["gcp"; "gcp_arm64"])]]

(** These are the set of Ubuntu release-architecture combinations for
    which we build deb packages in the job
    [job_build_ubuntu_package]. See {!debian_package_release_matrix}
    for more information. *)
let ubuntu_package_release_matrix =
  [[("RELEASE", ["focal"; "jammy"]); ("TAGS", ["gcp"; "gcp_arm64"])]]

let jobs =
  let variables add =
    ("DEP_IMAGE", "registry.gitlab.com/tezos/tezos/build-$DISTRIBUTION-$RELEASE")
    :: add
  in
  let make_job_docker_build_debian_dependencies ~__POS__ ~name ~matrix
      ~distribution =
    job_docker_authenticated
      ~__POS__
      ~name
      ~stage:Stages.build
      ~variables:(variables [("DISTRIBUTION", distribution)])
      ~parallel:(Matrix matrix)
      ~tags:["$TAGS"]
      [".gitlab/ci/jobs/packaging/build-debian-packages-dependencies.sh"]
  in
  let job_docker_build_debian_dependencies : tezos_job =
    make_job_docker_build_debian_dependencies
      ~__POS__
      ~name:"oc.docker-build-debian-dependencies"
      ~distribution:"debian"
      ~matrix:debian_package_release_matrix
  in
  let job_docker_build_ubuntu_dependencies : tezos_job =
    make_job_docker_build_debian_dependencies
      ~__POS__
      ~name:"oc.docker-build-ubuntu-dependencies"
      ~distribution:"ubuntu"
      ~matrix:ubuntu_package_release_matrix
  in
  let make_job_build_debian_packages ~__POS__ ~name ~matrix ~distribution =
    job
      ~__POS__
      ~name
      ~image:build_debian_packages_image
      ~stage:Stages.packaging
      ~variables:(variables [("DISTRIBUTION", distribution)])
      ~parallel:(Matrix matrix)
      ~tags:["$TAGS"]
      ~artifacts:(artifacts ["packages/$DISTRIBUTION/$RELEASE"])
      [".gitlab/ci/jobs/packaging/build-debian-packages.sh"]
  in
  let job_build_debian_package : tezos_job =
    make_job_build_debian_packages
      ~__POS__
      ~name:"oc.build-debian"
      ~distribution:"debian"
      ~matrix:debian_package_release_matrix
  in
  let job_build_ubuntu_package : tezos_job =
    make_job_build_debian_packages
      ~__POS__
      ~name:"oc.build-ubuntu"
      ~distribution:"ubuntu"
      ~matrix:ubuntu_package_release_matrix
  in
  [
    job_docker_build_debian_dependencies;
    job_docker_build_ubuntu_dependencies;
    job_build_debian_package;
    job_build_ubuntu_package;
  ]
