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

(* The "custom extended test" pipelines test the codebase with some particular options.
   This allows testing behaviors that are not enabled by default on the node,
   and are thus not tested in Tezt jobs of [before_merging] pipelines in particular.

   Note: this method is simple to implement but is not ideal.
   It duplicates a lot of tests that do not actually contribute
   to testing the special options, and there is no reason why at least some tests
   could be run with special options in dedicated jobs in [before_merging] pipelines. *)

let schedule_extended_rpc_test =
  Cacio.new_global_pipeline
    "schedule_extended_rpc_test"
    Rules.schedule_extended_rpc_tests
    ~interruptible_pipeline:false
    ~description:
      "Scheduled run of all tezt tests with external RPC servers, weekly on \
       'master'.\n\n\
       This scheduled pipeline exercices the full tezt tests suites, but with \
       Octez nodes configured to use external RPC servers."

let schedule_extended_validation_test =
  Cacio.new_global_pipeline
    "schedule_extended_validation_test"
    Rules.schedule_extended_validation_tests
    ~interruptible_pipeline:false
    ~description:
      "Scheduled run of all tezt tests with single-process validation, weekly \
       on 'master'.\n\n\
       This scheduled pipeline exercices the full tezt tests suites, but with \
       Octez nodes configured to use single-process validation."

let schedule_extended_baker_remote_mode_test =
  Cacio.new_global_pipeline
    "schedule_extended_baker_remote_mode_test"
    Rules.schedule_extended_baker_remote_mode_tests
    ~interruptible_pipeline:false
    ~description:
      "Scheduled run of all tezt tests with baker using remote node, weekly on \
       'master'.\n\n\
       This scheduled pipeline exercices the full tezt tests suites."

let schedule_extended_dal_use_baker =
  Cacio.new_global_pipeline
    "schedule_extended_dal_use_baker"
    Rules.schedule_extended_dal_use_baker
    ~interruptible_pipeline:false
    ~description:
      "Scheduled run of all tezt tests with dal using baker commands weekly on \
       'master'.\n\n\
       This scheduled pipeline exercices the full tezt tests suites."

let custom_extended_test_pipelines =
  [
    schedule_extended_rpc_test;
    schedule_extended_validation_test;
    schedule_extended_baker_remote_mode_test;
    schedule_extended_dal_use_baker;
  ]

(* Add jobs to all the "custom extended test" pipelines.
   They all contain exactly the same jobs. *)
let register_custom_extended_test_jobs jobs =
  List.iter
    (fun pipeline -> Cacio.register_jobs pipeline jobs)
    custom_extended_test_pipelines

let schedule_test_release =
  Cacio.new_global_pipeline
    "schedule_test_release"
    Rules.schedule_test_release
    ~description:
      "Scheduled pipeline that runs a test release pipeline. The jobs are the \
       same as a release pipeline but run in dry-mode."

let schedule_security_scans =
  Cacio.new_global_pipeline
    "schedule_security_scans"
    Rules.schedule_security_scans
    ~description:
      "Scheduled pipeline for various security scans. Currently scanning for \
       vulnerabilities in Docker images"

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
