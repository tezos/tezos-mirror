(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Definition of the global pipelines of this repository.

   Global pipelines are pipelines that are shared between components:
   components add jobs to them with [Cacio.register_jobs], and no single
   component owns them. They are declared here, and registered with CIAO by
   [Cacio.close].

   This library exists so that Cacio itself does not have to know about the
   pipelines of this particular repository. It cannot be merged into
   [ci/lib_tezos_ci_jobs], which would be the natural place for it, because
   [tezos_ci_jobs] depends on the component libraries (release_site_ci,
   grafazos_ci, teztale_ci, rollup_node_ci and sdk_bindings_ci) while those
   components need to add jobs to global pipelines, i.e. they need to depend
   on this library. This dependency of [tezos_ci_jobs] on components could be
   removed, in which case this library could be merged into it. *)

open Gitlab_ci
open Tezos_ci

let schedule_docker_master_snapshot =
  Cacio.new_global_pipeline
    "schedule_docker_master_snapshot"
    Rules.schedule_docker_master_snapshot
    ~interruptible_pipeline:false
    ~description:
      "Scheduled pipeline publishing a dated master Docker image to Docker \
       Hub.\n\n\
       This pipeline publishes the Octez Docker image tagged as \
       [master-YYYYMMDD] (where the date is computed at build time) to \
       DockerHub (https://hub.docker.com/r/tezos/tezos), then promotes it to \
       the rolling [weekly] tag."

let schedule_docker_build_pipeline =
  Cacio.new_global_pipeline
    "schedule_docker_build_pipeline"
    Rules.schedule_docker_build
    ~variables:[("DOCKER_FORCE_BUILD", "true")]
    ~description:
      "Scheduled pipeline for forcing building fresh Docker image (skipping \
       any cache mechanism) for the current master branch of Octez. The newly \
       built images should contains the latest available Alpine packages"

let publish_test_release_page =
  Cacio.new_global_pipeline
    "publish_test_release_page"
    Rules.(If.(api_release_page && not_on_tezos_namespace))
    ~description:"Pipeline that updates and publishes the test release page."

let publish_release_page =
  Cacio.new_global_pipeline
    "publish_release_page"
    Rules.(If.(api_release_page && on_tezos_namespace))
    ~description:"Pipeline that updates and publishes the release page."
