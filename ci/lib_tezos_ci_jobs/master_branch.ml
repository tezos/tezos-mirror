(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* This module defines the jobs of the [master_branch] pipeline.

   This pipeline runs for each merge on the [master] branch. To goal
   of this pipeline is to publish artifacts for the latest development
   version of Octez (e.g. the latest version on 'master'), including:

   - docker images,
   - static binaries, and
   - documentation. *)

open Common.Build
open Common.Docker
open Gitlab_ci
open Gitlab_ci.Util
open Tezos_ci
open Tezos_ci.Cache

let rules_always = [job_rule ~when_:Always ()]

(* static binaries *)
let job_static_arm64 =
  job_build_static_binaries
    ~__POS__
    ~arch:Arm64
    ~storage:Ramfs
    ~rules:rules_always
    ()

let job_static_x86_64 =
  job_build_static_binaries
    ~__POS__
    ~arch:Amd64
    ~cpu:Very_high
    ~storage:Ramfs
    ~rules:rules_always
    ()

(* Defines the jobs of the [schedule_docker_build_pipeline] pipeline.

    This pipeline runs scheduled on the [master] branch. The goal
   of this pipeline is to publish fresh Docker images for 'master' using the latest Alpine packages. *)

let octez_distribution_docker_jobs =
  let job_docker_amd64_experimental : tezos_job =
    job_docker_build ~__POS__ ~rules:rules_always ~arch:Amd64 Experimental
  in
  let job_docker_arm64_experimental : tezos_job =
    job_docker_build
      ~__POS__
      ~rules:rules_always
      ~arch:Arm64
      ~storage:Ramfs
      Experimental
  in
  let job_docker_merge_manifests =
    job_docker_merge_manifests
      ~__POS__
      ~ci_docker_hub:true
      ~job_docker_amd64:job_docker_amd64_experimental
      ~job_docker_arm64:job_docker_arm64_experimental
  in
  [
    (* Stage: build *)
    job_docker_amd64_experimental;
    job_docker_arm64_experimental;
    (* Stage: prepare_release *)
    job_docker_merge_manifests;
  ]

let jobs =
  (* Like in the {!Schedule_extended_test} variant of
     {!Code_verification} pipelines, we'd like to run as many jobs as
     possible in [master_branch] pipelines. Therefore, the default
     [when_] should be [Always] for jobs without dependencies.

     [changes:] clauses can be used on [master_branch] pipelines. In
     push pipelines like this one, [changes:]-clauses match against
     the diff of the branch's [HEAD] between two pushes. That is, the
     the difference between the previous state and the new state of
     the branch. In the case of the [master] branch, this is just the
     contents of the most recently merged MR. For more info on
     [changes:] in different pipelines, see
     {{:https://docs.gitlab.com/ee/ci/jobs/job_troubleshooting.html#jobs-or-pipelines-run-unexpectedly-when-using-changes}
     GitLab Docs: Jobs or pipelines run unexpectedly when using changes}. *)
  let job_static_arm64 =
    job_build_static_binaries
      ~__POS__
      ~arch:Arm64
      ~storage:Ramfs
      ~rules:rules_always
      ()
  in
  let job_static_x86_64 =
    job_build_static_binaries
      ~__POS__
      ~arch:Amd64
      ~cpu:Very_high
      ~storage:Ramfs
      ~rules:rules_always
      ()
  in
  let job_unified_coverage_default : tezos_job =
    job
      ~__POS__
      ~image:Images.CI.test
      ~name:"oc.unified_coverage"
      ~stage:Stages.test_coverage
      ~variables:
        [
          ("PROJECT", Predefined_vars.(show ci_project_path));
          ("DEFAULT_BRANCH", Predefined_vars.(show ci_commit_sha));
        ]
      ~allow_failure:Yes
      ~before_script:
        ((* sets COVERAGE_OUTPUT *)
         Common.Helpers.before_script
           ~source_version:true
           ~eval_opam:true
           [])
      ~rules:rules_always
      ~coverage:"/Coverage: ([^%]+%)/"
      [
        (* On the project default branch, we fetch coverage from the last merged MR *)
        "mkdir -p _coverage_report";
        "dune exec scripts/ci/download_coverage/download.exe -- --from \
         last-merged-pipeline --info --log-file \
         _coverage_report/download_coverage.log";
        "./scripts/ci/report_coverage.sh";
      ]
    |> Coverage.enable_location |> Coverage.enable_report
  in

  (* Smart Rollup: Kernel SDK

     See [src/kernel_sdk/RELEASE.md] for more information. *)
  let job_publish_kernel_sdk : tezos_job =
    job
      ~__POS__
      ~name:"publish_kernel_sdk"
      ~image:Images.rust_toolchain
      ~stage:Stages.manual
      ~rules:[job_rule ~when_:Manual ()]
      ~interruptible:false
      [
        "make -f kernels.mk publish-sdk-deps";
        (* Manually set SSL_CERT_DIR as default setting points to empty dir *)
        "SSL_CERT_DIR=/etc/ssl/certs CC=clang make -f kernels.mk publish-sdk";
      ]
    |> enable_cargo_cache |> enable_sccache
  in
  (* arm builds are manual on the master branch pipeline *)
  let build_arm_rules = [job_rule ~when_:Manual ~allow_failure:Yes ()] in
  let job_build_arm64_release =
    job_build_arm64_release ~rules:build_arm_rules ()
  in
  let job_build_arm64_extra_exp =
    job_build_arm64_extra_exp ~rules:build_arm_rules ()
  in
  let job_build_arm64_extra_dev =
    job_build_arm64_extra_dev ~rules:build_arm_rules ()
  in

  [
    (* Stage: build *)
    job_static_x86_64;
    job_static_arm64;
    job_build_arm64_release;
    job_build_arm64_extra_dev;
    job_build_arm64_extra_exp;
    (* Stage: sanity *)
    job_datadog_pipeline_trace;
    (* Stage: test_coverage *)
    job_unified_coverage_default;
  ]
  (* Jobs to build and update on Docker Hub the Octez Docker image.  *)
  @ octez_distribution_docker_jobs
  (* Stage: manual *)
  @ [job_publish_kernel_sdk]
