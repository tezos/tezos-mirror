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

open Common
open Gitlab_ci
open Gitlab_ci.Util
open Tezos_ci

let rules_always = [job_rule ~when_:Always ()]

(* static binaries *)
let job_static_arm64 =
  job_build_static_binaries ~__POS__ ~arch:Arm64 ~rules:rules_always ()

let job_static_x86_64 =
  job_build_static_binaries
    ~__POS__
    ~arch:Amd64
    ~cpu:Very_high
    ~retry:{max = 2; when_ = [Stuck_or_timeout_failure; Runner_system_failure]}
    ~rules:rules_always
    ()

let jobs_documentation : tezos_job list =
  let rules = [job_rule ~changes:(Changeset.encode changeset_octez_docs) ()] in
  let dependencies = Dependent [] in
  let job_odoc = Documentation.job_odoc ~rules ~dependencies () in
  let job_manuals =
    Documentation.job_manuals
      ~rules
      ~dependencies:(Dependent [Artifacts job_static_x86_64])
      ~use_static_executables:true
      ()
  in
  let job_docgen = Documentation.job_docgen ~rules ~dependencies () in
  let job_build_all =
    Documentation.job_build_all ~job_odoc ~job_manuals ~job_docgen ~rules ()
  in
  let job_publish_documentation : tezos_job =
    Documentation.job_publish_documentation ~job_build_all ~rules ()
  in
  [job_odoc; job_manuals; job_docgen; job_build_all; job_publish_documentation]

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
  let job_docker_amd64_experimental : tezos_job =
    job_docker_build ~__POS__ ~rules:rules_always ~arch:Amd64 Experimental
  in
  let job_docker_arm64_experimental : tezos_job =
    job_docker_build ~__POS__ ~rules:rules_always ~arch:Arm64 Experimental
  in
  let job_docker_merge_manifests =
    job_docker_merge_manifests
      ~__POS__
      ~ci_docker_hub:true
      ~job_docker_amd64:job_docker_amd64_experimental
      ~job_docker_arm64:job_docker_arm64_experimental
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
         before_script
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
    |> enable_coverage_location |> enable_coverage_report
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
  let job_build_arm64_exp_dev_extra =
    job_build_arm64_exp_dev_extra ~rules:build_arm_rules ()
  in
  [
    (* Stage: build *)
    job_static_x86_64;
    job_static_arm64;
    job_build_arm64_release;
    job_build_arm64_exp_dev_extra;
    job_docker_amd64_experimental;
    job_docker_arm64_experimental;
    (* Stage: sanity *)
    job_datadog_pipeline_trace;
    (* Stage: test_coverage *)
    job_unified_coverage_default;
  ]
  (* Stage: doc *)
  @ jobs_documentation
  @ [
      (* Stage: prepare_release *)
      job_docker_merge_manifests;
      (* Stage: manual *)
      job_publish_kernel_sdk;
    ]
